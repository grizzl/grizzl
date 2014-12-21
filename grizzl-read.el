;;; grizzl-read.el --- A fuzzy completing-read backed by grizzl.
;;
;; Copyright © 2013 Chris Corbyn
;;
;; Author:     Chris Corbyn <chris@w3style.co.uk>
;; Maintainer: boyw165 <boyw165@gmail.com>
;;             bbatsov <bozhidar@batsov.com>
;; URL:        https://github.com/d11wtq/grizzl
;; Version:    0.1.2
;; Keywords:   convenience, usability
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; grizzl-read.el provides an implementation of the built-in Emacs
;; completing-read function, except it is backed by the grizzl fuzzy
;; search index. The goals are similar to ido-mode and helm, but grizzl
;; is heavily optimized for large data-sets, and as-such uses a
;; persistent fuzzy search database in its algorithm.
;;
;; The indexing and searching algorithm itself is defined in grizzl-core.el
;; with grizzl-read.el simply wrapping the search in a minibuffer with a
;; minor-mode defined.
;;
;; Usage:
;; ------
;;
;;    (grizzl-completing-read "Number:" '("one" "two" "three" "four"))
;;
;; or
;;
;;    (setq database (grizzl-make-database '("one" "two" "three" "four")))
;;    (grizzl-completing-read-database "Number:" database)
;;
;; or
;;
;;    ;; Use external FETCHER function to get strings list.
;;    (grizzl-completing-read-gui "Number:" FETCHER)
;;
;; When the user hits ENTER, either one of the strings is returned on
;; success, or nil of nothing matched.
;;
;; The arrow keys can be used to navigate within the results.
;;
;; TODO:
;; -----
;; * Highlight matched characters.
;;   <https://github.com/d11wtq/grizzl/issues/5>
;; * `grizzl-completing-read' support 2nd argument as a fetcher function in case
;;   of someone want to use another process or daemon to do searching job.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2014-11-15
;; * Support `grizzl-completing-read-gui'.
;;
;; 2014-11-14
;; * Add grizzl group.
;; * Support face customization.
;; * Support scrollable result display in minibuffer.
;; * Support `grizzl-read-update-hook' to info hooks the current result.
;; * Slightly improve performance by using idle timer to do searching job instead
;;   of do it in post-command.
;; * Remove `cl' dependency.
;;
;; 2013-05-28
;; * Initial release refer to `fiplr' package.
;;
;;; Code:

;; GNU library.
(eval-when-compile (require 'cl))

;; 3rd party library.
(require 'grizzl-core)

(defface grizzl-read-selection-face
  '((t (:background "darkseagreen2" :foreground "black" :weight bold)))
  "Default face for highlighting keyword in definition window."
  :group 'grizzl)

(defface grizzl-read-default-face
  '((t (:foreground "gray20")))
  "Default face for highlighting keyword in definition window."
  :group 'grizzl)

(defface grizzl-read-warning-face
  '((t (:foreground "red" :weight bold)))
  "Default face for highlighting keyword in definition window."
  :group 'grizzl)

(defcustom grizzl-read-update-hook nil
  "A hook triggered at selection changed in `grizzl-completing-read'. It takes 
2 arguments:
1st is `grizzl-read-selection';
2nd is `grizzl-read-selection-string'."
  :type '(repeat function)
  :group 'grizzl)

(defcustom grizzl-read-display-lines 10
  "The maximum number of results to show in `grizzl-completing-read'."
  :type 'integer
  :group 'grizzl)

(defcustom grizzl-read-search-delay 0.2
  "The maximum number of results to show in `grizzl-completing-read'."
  :type 'integer
  :group 'grizzl)

(defvar grizzl-read-result nil
  "The search result in `grizzl-completing-read'.")

(defvar grizzl-read-result-count 0
  "The count of search result in `grizzl-completing-read'.")

(defvar grizzl-read-selection 0
  "The selected offset in `grizzl-completing-read'.")

(defvar grizzl-read-selection-string nil
  "The selected string in `grizzl-completing-read'.")

(defvar grizzl-read-max 0
  "The maximum index of result for display.")

(defvar grizzl-read-min 0
  "The minimum index of result for display.")

(defvar grizzl-read-prompt ""
  "Cached completing-read prompt.")

(defvar grizzl-read-database nil
  "Cached completing-read database")

(defvar grizzl-read-string nil
  "Cached read string. It is for preventing duplicate searching of same input.")

(defvar grizzl-read-timer nil
  "An idle timer for delayed searching job. It is for performance.")

(defvar grizzl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<up>") 'grizzl-set-selection-1)
    (define-key map (kbd "<down>") 'grizzl-set-selection+1)
    (define-key map (kbd "C-p") 'grizzl-set-selection-1)
    (define-key map (kbd "C-n") 'grizzl-set-selection+1)
    (define-key map (kbd "C-P") 'grizzl-set-selection-page)
    (define-key map (kbd "C-N") 'grizzl-set-selection+page)
    (define-key map (kbd "<prior>") 'grizzl-set-selection-page)
    (define-key map (kbd "<next>") 'grizzl-set-selection+page)
    map)
  "Internal keymap used by the minor-mode in `grizzl-completing-read'.")

(defun grizzl-move-selection (delta)
  "Move the selection by DELTA rows in `grizzl-completing-read'."
  (let* ((total grizzl-read-result-count)
         (new-selection (let ((index (+ grizzl-read-selection delta)))
                          (cond
                           ((< index 0) 0)
                           ((>= index total) (1- total))
                           (t index))))
         (new-max (cond
                   ((>= new-selection grizzl-read-max)
                    (1+ new-selection))
                   ((< new-selection grizzl-read-min)
                    (+ grizzl-read-max
                       (- new-selection grizzl-read-min)))
                   (t grizzl-read-max)))
         (new-min (cond
                   ((< new-selection grizzl-read-min)
                    new-selection)
                   ((>= new-selection grizzl-read-max)
                    (+ grizzl-read-min
                       (1+ (- new-selection grizzl-read-max))))
                   (t grizzl-read-min))))
    (setq grizzl-read-selection new-selection
          grizzl-read-max new-max
          grizzl-read-min new-min)))

(defun grizzl-prompt-line ()
  "Returns a string to render a full-width prompt in `grizzl-completing-read'."
  (let* ((match-info (format " select %s (total %d candidate%s) --"
                             (if (> grizzl-read-result-count 0)
                                 (1+ grizzl-read-selection)
                               "none")
                             grizzl-read-result-count
                             (if (= grizzl-read-result-count 1) "" "s"))))
    (concat (propertize (format "-- %s" grizzl-read-prompt) 'face 'mode-line)
            (propertize " "
                        'face 'mode-line
                        'display `(space :align-to (- right
                                                      ,(1+ (length match-info)))))
            (propertize match-info 'face 'mode-line))))

(defun grizzl-read-display-result ()
  "Renders a series of overlays to list the matches in the result."
  (let* ((lines (min grizzl-read-result-count
                     grizzl-read-display-lines))
         (result-strings (cond
                          ;; A grizzl result ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                          ((grizzl-result-p grizzl-read-result)
                           (reverse
                            (grizzl-result-strings grizzl-read-result
                                                   grizzl-read-database)))
                          ;; A strings list ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                          (t grizzl-read-result)))
         (item (nthcdr grizzl-read-min result-strings))
         (selection (- grizzl-read-max grizzl-read-selection))
         (count lines)
         formated-string)
    (setq grizzl-read-selection-string (nth (- lines selection) item))
    (while (> count 0)
      (setq formated-string (concat formated-string
                                    (if (= selection count)
                                        (propertize (format "* %s" (car item))
                                                    'face 'grizzl-read-selection-face)
                                      (propertize (format "  %s" (car item))
                                                  'face 'grizzl-read-default-face))
                                    (and (> count 1) "\n"))
            item (cdr item)
            count (1- count)))
    (delete-all-overlays)
    (overlay-put (make-overlay (point-min) (point-min))
                 'before-string
                 (format "%s\n%s\n"
                         (or formated-string
                             (propertize "-- NO MATCH --"
                                         'face 'grizzl-read-warning-face))
                         (grizzl-prompt-line)))
    (set-window-text-height nil (max 3 (+ 2 lines)))))

(defun grizzl-is-begin-search ()
  (not (or (equal grizzl-read-string (minibuffer-contents-no-properties))
           (not (minibufferp)))))

(defun grizzl-begin-search ()
  (condition-case err
      (when (grizzl-is-begin-search)
        (setq grizzl-read-string (minibuffer-contents-no-properties)
              grizzl-read-result (cond
                                  ;; A grizzl database ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                  ((grizzl-database-p grizzl-read-database)
                                   (grizzl-search grizzl-read-string
                                                  grizzl-read-database
                                                  grizzl-read-result))
                                  ;; A fetch function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                  ((functionp grizzl-read-database)
                                   (funcall grizzl-read-database)))
              grizzl-read-result-count (cond
                                        ;; A grizzl result ;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ((grizzl-result-p grizzl-read-result)
                                         (grizzl-result-count grizzl-read-result))
                                        ;; A strings list ;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        (t (length grizzl-read-result)))
              grizzl-read-selection 0
              grizzl-read-min 0
              grizzl-read-max (min grizzl-read-display-lines
                                   grizzl-read-result-count))
        (grizzl-read-display-result))
    (error (error "Error in grizzl-begin-search: %s" err)))
  ;; Pass selected result to hooks in `grizzl-read-update-hook'.
  (run-hook-with-args 'grizzl-read-update-hook
                      grizzl-read-selection
                      grizzl-read-selection-string))

(defun grizzl-completing-read-impl (prompt database)
  "Performs a completing-read in the minibuffer:
* Use DATABASE to fuzzy search. Each key pressed in the minibuffer filters down 
the list of matches.
* Use DATABASE as a function to get a strings list."
  (unless (stringp prompt)
    (error "PROMPT must be a string!"))
  (setq grizzl-read-prompt prompt
        grizzl-read-database database
        grizzl-read-result nil
        grizzl-read-string nil)
  (minibuffer-with-setup-hook
      (lambda ()
        (setq truncate-lines t)
        (grizzl-mode 1)
        (grizzl-begin-search))
    ;; TODO: catch exit event.
    (read-from-minibuffer ">>> ")
    (setq-default grizzl-read-update-hook nil)
    (grizzl-selection-result)))

(defun grizzl-exit ()
  (grizzl-mode -1))

(define-minor-mode grizzl-mode
  "Toggle the internal mode used by `grizzl-completing-read'."
  :group 'grizzl
  (unless (minibufferp)
    (error "The grizzl-mode is only for minibuffer!"))
  (if grizzl-mode
      (progn
        (setq grizzl-read-timer (run-with-idle-timer grizzl-read-search-delay t
                                                     'grizzl-begin-search))
        (add-hook 'post-command-hook 'grizzl-read-display-result nil t)
        (add-hook 'minibuffer-exit-hook 'grizzl-exit nil t))
    (when (timerp grizzl-read-timer)
      (cancel-timer grizzl-read-timer)
      (setq grizzl-read-timer nil))
    (remove-hook 'post-command-hook 'grizzl-read-display-result t)
    (remove-hook 'minibuffer-exit-hook 'grizzl-exit t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun grizzl-completing-read-database (prompt database)
  "Performs a completing-read in the minibuffer using DATABASE to fuzzy search."
  (grizzl-completing-read-impl prompt database))

;;;###autoload
(defun grizzl-completing-read (prompt strings)
  "Performs a completing-read in the minibuffer with given STRINGS list."
  (grizzl-completing-read-impl prompt
                               (grizzl-make-database strings)))

;;;###autoload
(defun grizzl-completing-read-gui (prompt func)
  "Performs a completing-read in the minibuffer using given FUNC. It doesn't do 
any search job for you and provides pretty GUI only."
  (grizzl-completing-read-impl prompt func))

;;;###autoload
(defun grizzl-selection-result ()
  "It is the selection string."
  grizzl-read-selection-string)

;;;###autoload
(defun grizzl-selection ()
  "It is the selection index."
  grizzl-read-selection)

;;;###autoload
(defun grizzl-set-selection+1 ()
  "Move the selection up one row in `grizzl-completing-read'."
  (interactive)
  (grizzl-move-selection 1))

;;;###autoload
(defun grizzl-set-selection-1 ()
  "Move the selection down one row in `grizzl-completing-read'."
  (interactive)
  (grizzl-move-selection -1))

;;;###autoload
(defun grizzl-set-selection+page ()
  "Move the selection up one row in `grizzl-completing-read'."
  (interactive)
  (grizzl-move-selection grizzl-read-display-lines))

;;;###autoload
(defun grizzl-set-selection-page ()
  "Move the selection down one row in `grizzl-completing-read'."
  (interactive)
  (grizzl-move-selection (* -1 grizzl-read-display-lines)))

(provide 'grizzl-read)
;;; grizzl-read.el ends here
