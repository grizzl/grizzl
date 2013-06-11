;;; grizzl-read.el --- A fuzzy completing-read backed by grizzl.

;; Copyright © 2013 Chris Corbyn
;;
;; Author:   Chris Corbyn <chris@w3style.co.uk>
;; URL:      https://github.com/d11wtq/grizzl
;; Version:  0.1.0
;; Keywords: convenience, usability

;; This file is NOT part of GNU Emacs.

;;; --- License

;; Licensed under the same terms as Emacs.

;;; --- Commentary

;; grizzl-read.el provides an implementation of the built-in Emacs
;; completing-read function, except it is backed by the grizzl fuzzy
;; search index. The goals are similar to ido-mode and helm, but grizzl
;; is heavily optimized for large data-sets, and as-such uses a
;; persistent fuzzy search index in its algorithm.
;;
;; The indexing and searching algorithm itself is defined in grizzl-core.el
;; with grizzl-read.el simply wrapping the search in a minibuffer with a
;; minor-mode defined.
;;
;; ---- Usage
;;
;; Call `grizzl-completing-read' with an index returned by
;; `grizzl-make-index':
;;
;;    (defvar *index* (grizzl-make-index '("one" "two" "three")))
;;    (grizzl-completing-read "Number: " index)
;;
;; When the user hits ENTER, either one of the strings is returned on
;; success, or nil of nothing matched.
;;
;; The arrow keys can be used to navigate within the results.
;;

(require 'cl)

;;; --- Configuration Variables

(defvar *grizzl-read-max-results* 10
  "The maximum number of results to show in `grizzl-completing-read'.")

;;; --- Runtime Processing Variables

(defvar *grizzl-current-result* nil
  "The search result in `grizzl-completing-read'.")

(defvar *grizzl-current-selection* 0
  "The selected offset in `grizzl-completing-read'.")

;;; --- Minor Mode Definition

(defvar *grizzl-keymap* (make-sparse-keymap)
  "Internal keymap used by the minor-mode in `grizzl-completing-read'.")

(define-key *grizzl-keymap* (kbd "<up>")   'grizzl-set-selection+1)
(define-key *grizzl-keymap* (kbd "<down>") 'grizzl-set-selection-1)

(define-minor-mode grizzl-mode
  "Toggle the internal mode used by `grizzl-completing-read'."
  nil
  " Grizzl"
  *grizzl-keymap*)

;;; --- Public Functions

;;;###autoload
(defun grizzl-completing-read (prompt index)
  "Performs a completing-read in the minibuffer using INDEX to fuzzy search.
Each key pressed in the minibuffer filters down the list of matches."
  (minibuffer-with-setup-hook
      (lambda ()
        (setq *grizzl-current-result* nil)
        (setq *grizzl-current-selection* 0)
        (grizzl-mode 1)
        (lexical-let*
            ((hookfun (lambda ()
                        (setq *grizzl-current-result*
                              (grizzl-search (minibuffer-contents)
                                             index
                                             *grizzl-current-result*))
                        (grizzl-display-result index)))
             (exitfun (lambda ()
                        (grizzl-mode -1)
                        (remove-hook 'post-command-hook    hookfun t))))
          (add-hook 'minibuffer-exit-hook exitfun nil t)
          (add-hook 'post-command-hook    hookfun nil t)))
    (read-from-minibuffer ">>> ")
    (grizzl-selected-result index)))

;;;###autoload
(defun grizzl-selected-result (index)
  "Get the selected string from INDEX in a `grizzl-completing-read'."
  (elt (grizzl-result-strings *grizzl-current-result* index
                              :start 0
                              :end   *grizzl-read-max-results*)
       (grizzl-current-selection)))

;;;###autoload
(defun grizzl-set-selection+1 ()
  "Move the selection up one row in `grizzl-completing-read'."
  (interactive)
  (setq *grizzl-current-selection* (1+ (grizzl-current-selection))))

;;;###autoload
(defun grizzl-set-selection-1 ()
  "Move the selection down one row in `grizzl-completing-read'."
  (interactive)
  (setq *grizzl-current-selection* (1- (grizzl-current-selection))))

;;; --- Private Functions

;; FIXME: All this presentation logic needs cleaning up
(defun grizzl-display-result (index)
  "Renders a series of overlays to list the matches in the result."
  (delete-all-overlays)
  (let ((selection (grizzl-current-selection))
        (overlay (make-overlay (point-min) (point-min)))
        (formatted '())
        (strings (grizzl-result-strings *grizzl-current-result* index
                                        :start 0
                                        :end   *grizzl-read-max-results*)))
    (reduce (lambda (n s)
              (if (= n (grizzl-current-selection))
                  (push (propertize (format "> %s\n" s) 'face 'diredp-symlink) formatted)
                (push (propertize (format "  %s\n" s) 'face 'default) formatted))
                (1+ n))
            strings
            :initial-value 0)
    (overlay-put overlay 'before-string (mapconcat 'identity formatted ""))
    (set-window-text-height nil (+ 1 (length strings)))))

(defun grizzl-current-selection ()
  "Get the currently selected index in `grizzl-completing-read'."
  (let ((max-selection
         (min (1- *grizzl-read-max-results*)
              (1- (grizzl-result-count *grizzl-current-result*)))))
    (max 0 (min max-selection *grizzl-current-selection*))))

(provide 'grizzl-read)

;;; grizzl-read.el ends here
