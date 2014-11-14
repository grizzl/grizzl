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
;; Call `grizzl-completing-read' with an database returned by
;; `grizzl-make-index':
;;
;;    (defvar database (grizzl-make-database '("one" "two" "three")))
;;    (grizzl-completing-read "Number: " database)
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
;; * Add grizzl group.
;; * Support face customization.
;; * Support scrollable result display in minibuffer.
;; * Slightly improve performance by using idle timer to do searching job instead
;;   of do it in post-command.
;; * Remove `cl' dependency.
;;
;; 2013-05-28
;; * Initial release refer to `fiplr' package.
;;
;;; Code:

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

(defcustom grizzl-read-display-lines 10
  "The maximum number of results to show in `grizzl-completing-read'."
  :type 'integer
  :group 'grizzl)

(defcustom grizzl-idle-delay 0.2
  "The maximum number of results to show in `grizzl-completing-read'."
  :type 'integer
  :group 'grizzl)

(defvar grizzl-result nil
  "The search result in `grizzl-completing-read'.")

(defvar grizzl-read-selection 0
  "The selected offset in `grizzl-completing-read'.")

(defvar grizzl-read-max 0)

(defvar grizzl-read-min 0)

(defvar grizzl-read-prompt "")

(defvar grizzl-read-database nil)

(defvar grizzl-read-string nil)

(defvar grizzl-read-timer nil)

(defvar grizzl-selected-result nil)

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
  (let* ((total (grizzl-result-count grizzl-result))
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
  (let* ((count (grizzl-result-count grizzl-result))
         (match-info (format " select %d (total %d candidate%s) --"
                             (1+ grizzl-read-selection)
                             count
                             (if (= count 1) "" "s"))))
    (concat (propertize (format "-- %s" grizzl-read-prompt) 'face 'mode-line)
            (propertize " "
                        'face 'mode-line
                        'display `(space :align-to (- right
                                                      ,(1+ (length match-info)))))
            (propertize match-info 'face 'mode-line))))

(defun grizzl-display-result ()
  "Renders a series of overlays to list the matches in the result."
  (let* ((total (grizzl-result-count grizzl-result))
         (lines (min total grizzl-read-display-lines))
         (result-strings (grizzl-result-strings grizzl-result grizzl-read-database))
         (item (nthcdr grizzl-read-min result-strings))
         (selection (- grizzl-read-max grizzl-read-selection))
         (count lines)
         formated-string)
    (while (> count 0)
      (and (= selection count)
           (setq grizzl-selected-result (car item)))
      (setq formated-string (concat formated-string
                                    (if (= selection count)
                                        (propertize (format "> %s" (car item))
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
  (when (grizzl-is-begin-search)
    (setq grizzl-read-string (minibuffer-contents-no-properties)
          grizzl-result (grizzl-search grizzl-read-string
                                       grizzl-read-database
                                       grizzl-result)
          grizzl-read-selection 0
          grizzl-read-min 0
          grizzl-read-max (min grizzl-read-display-lines
                               (grizzl-result-count grizzl-result)))
    (grizzl-display-result)))

(defun grizzl-exit ()
  (grizzl-mode -1))

(define-minor-mode grizzl-mode
  "Toggle the internal mode used by `grizzl-completing-read'."
  :group 'grizzl
  (unless (minibufferp)
    (error "The grizzl-mode is only for minibuffer!"))
  (if grizzl-mode
      (progn
        (setq grizzl-read-timer (run-with-idle-timer grizzl-idle-delay t
                                                'grizzl-begin-search))
        (add-hook 'post-command-hook 'grizzl-display-result nil t)
        (add-hook 'minibuffer-exit-hook 'grizzl-exit nil t))
    (when (timerp grizzl-read-timer)
      (cancel-timer grizzl-read-timer)
      (setq grizzl-read-timer nil))
    (remove-hook 'post-command-hook 'grizzl-display-result t)
    (remove-hook 'minibuffer-exit-hook 'grizzl-exit t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun grizzl-completing-read (prompt strings)
  "Performs a completing-read in the minibuffer using INDEX to fuzzy search.
Each key pressed in the minibuffer filters down the list of matches."
  ;; TODO: (prompt database-or-fetcher)
  (unless (stringp prompt)
    (error "PROMPT must be a string!"))
  (unless (listp strings)
    (error "STRINGS must be a string list!"))
  (setq grizzl-read-prompt prompt
        grizzl-read-database (grizzl-make-database strings)
        grizzl-read-string nil)
  (minibuffer-with-setup-hook
      (lambda ()
        (setq truncate-lines t)
        (grizzl-mode 1)
        (grizzl-begin-search))
    (read-from-minibuffer ">>> ")
    grizzl-selected-result))

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
