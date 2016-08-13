;;; grizzl.el --- Fast fuzzy search index for Emacs. -*- lexical-binding: t -*-

;; Copyright © 2013-2014 Chris Corbyn
;; Copyright © 2015 Bozhidar Batsov
;;
;; Author:     Chris Corbyn <chris@w3style.co.uk>
;; Maintainer: Bozhidar Batsov <bozhidar@batsov.com>
;; URL:        https://github.com/grizzl/grizzl
;; Version:    0.1.2
;; Keywords:   convenience, usability
;; Package-Requires: ((cl-lib "0.5") (emacs "24.3"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Grizzl provides a fuzzy completion framework for general purpose
;; use in Emacs Lisp projects.
;;
;; grizzl provides the underlying data structures and sesrch
;; algorithm without any UI attachment.  At the core, a fuzzy search
;; index is created from a list of strings, using `grizzl-make-index'.
;; A fuzzy search term is then used to get a result from this index
;; with `grizzl-search'.  Because grizzl considers the usage of a
;; fuzzy search index to operate in real-time as a user enters a
;; search term in the minibuffer, the framework optimizes for this use
;; case.  Any result can be passed back into `grizzl-search' as a hint
;; to continue searching.  The search algorithm is able to understand
;; insertions and deletions and therefore minimizes the work it needs
;; to do in this case.  The intended use here is to collect a result
;; on each key press and feed that result into the search for the next
;; key press. Once a search is complete, the matched strings are then
;; read, using `grizzl-result-strings'. The results are ordered on the
;; a combination of the Levenshtein Distance and a character-proximity
;; scoring calculation. This means shorter strings are favoured, but
;; adjacent letters are more heavily favoured.
;;
;; It is assumed that the index will be re-used across multiple
;; searches on larger sets of data.
;;
;; Call `grizzl-completing-read' with an index returned by
;; `grizzl-make-index':
;;
;;    (defvar *index* (grizzl-make-index '("one" "two" "three")))
;;    (grizzl-completing-read "Number: " *index*)
;;
;; When the user hits ENTER, either one of the strings is returned on
;; success, or nil of nothing matched.
;;
;; The arrow keys can be used to navigate within the results.


;;; Code:

(eval-when-compile
  (require 'cl-lib))

;;; --- Public Functions

;;;###autoload
(defun grizzl-make-index (strings &rest options)
  "Makes an index from the list STRINGS for use with `grizzl-search'.

If :PROGRESS-FN is given as a keyword argument, it is called repeatedly
with integers N and TOTAL.

If :CASE-SENSITIVE is specified as a non-nil keyword argument, the index
will be created case-sensitive, otherwise it will be case-insensitive."
  (let ((lookup-table (make-hash-table))
        (total-strs (length strings))
        (case-sensitive (plist-get options :case-sensitive))
        (progress-fn (plist-get options :progress-fn))
        (string-data (vconcat (mapcar (lambda (s)
                                        (cons s (length s)))
                                      strings))))
    (cl-reduce (lambda (list-offset str)
                 (grizzl-index-insert str list-offset lookup-table
                                      :case-sensitive case-sensitive)
                 (when progress-fn
                   (funcall progress-fn (1+ list-offset) total-strs))
                 (1+ list-offset))
               strings
               :initial-value 0)
    (maphash (lambda (_char str-map)
               (maphash (lambda (list-offset locations)
                          (puthash list-offset (reverse locations) str-map))
                        str-map)) lookup-table)
    `((case-sensitive . ,case-sensitive)
      (lookup-table   . ,lookup-table)
      (string-data    . ,string-data))))

;;;###autoload
(defun grizzl-search (term index &optional old-result)
  "Fuzzy searches for TERM in INDEX prepared with `grizzl-make-index'.

OLD-RESULT may be specified as an existing search result to increment from.
The result can be read with `grizzl-result-strings'."
  (let* ((cased-term (if (grizzl-index-case-sensitive-p index)
                         term
                       (downcase term)))
         (result (grizzl-rewind-result cased-term index old-result))
         (matches (copy-hash-table (grizzl-result-matches result)))
         (from-pos (length (grizzl-result-term result)))
         (remainder (substring cased-term from-pos))
         (lookup-table (grizzl-lookup-table index)))
    (cl-reduce (lambda (acc-res ch)
                 (let ((sub-table (gethash ch lookup-table)))
                   (if (not sub-table)
                       (clrhash matches)
                     (grizzl-search-increment sub-table matches))
                   (grizzl-cons-result cased-term matches acc-res)))
               remainder
               :initial-value result)))

;;;###autoload
(defun grizzl-result-count (result)
  "Returns the number of matches present in RESULT."
  (hash-table-count (grizzl-result-matches result)))

;;;###autoload
(defun grizzl-result-strings (result index &rest options)
  "Returns the ordered list of matched strings in RESULT, using INDEX.

If the :START option is specified, results are read from the given offset.
If the :END option is specified, up to :END results are returned."
  (let* ((matches (grizzl-result-matches result))
         (strings (grizzl-index-strings index))
         (loaded '()))
    (maphash (lambda (string-offset _char-offset)
               (push string-offset loaded))
             matches)
    (let* ((ordered (sort loaded
                          (lambda (a b)
                            (< (cadr (gethash a matches))
                               (cadr (gethash b matches))))))
           (start (or (plist-get options :start) 0))
           (end (min (plist-get options :end) (length ordered)))
           (best (if (or start end)
                     (cl-delete-if-not 'identity
                                       (cl-subseq ordered start end))
                   ordered)))
      (mapcar (lambda (n)
                (car (elt strings n)))
              best))))

;;; --- Private Functions

(defun grizzl-cons-result (term matches results)
  "Build a new result for TERM and hash-table MATCHES consed with RESULTS."
    (cons (cons term matches) results))

(defun grizzl-rewind-result (term index result)
  "Adjusts RESULT according to TERM, ready for a new search."
  (if result
      (let* ((old-term (grizzl-result-term result))
             (new-len (length term))
             (old-len (length old-term)))
        (if (and (>= new-len old-len)
                 (string-equal old-term (substring term 0 old-len)))
              result
          (grizzl-rewind-result term index (cdr result))))
    (grizzl-cons-result "" (grizzl-base-matches index) nil)))

(defun grizzl-base-matches (index)
  "Returns the full set of matches in INDEX, with an out-of-bound offset."
  (let ((matches (make-hash-table)))
    (cl-reduce (lambda (n s-len)
                 (puthash n (list -1 0 (cdr s-len)) matches)
                 (1+ n))
               (grizzl-index-strings index)
               :initial-value 0)
    matches))

(defun grizzl-result-term (result)
  "Returns the search term used to find the matches in RESULT."
  (car (car result)))

(defun grizzl-result-matches (result)
  "Returns the internal hash used to track the matches in RESULT."
  (cdar result))

(defun grizzl-index-insert (string list-offset index &rest options)
  "Inserts STRING at LIST-OFFSET into INDEX."
  (let ((case-sensitive (plist-get options :case-sensitive)))
    (cl-reduce (lambda (char-offset cs-char)
                 (let* ((char (if case-sensitive
                                  cs-char
                                (downcase cs-char)))
                        (str-map (or (gethash char index)
                                     (puthash char (make-hash-table) index)))
                        (offsets (gethash list-offset str-map)))
                   (puthash list-offset
                            (cons char-offset offsets)
                            str-map)
                   (1+ char-offset)))
               string
               :initial-value 0)))

(defun grizzl-lookup-table (index)
  "Returns the lookup table portion of INDEX."
  (cdr (assoc 'lookup-table index)))

(defun grizzl-index-strings (index)
  "Returns the vector of strings stored in INDEX."
  (cdr (assoc 'string-data index)))

(defun grizzl-index-case-sensitive-p (index)
  "Predicate to test of INDEX is case-sensitive."
  (cdr (assoc 'case-sensitive index)))

(defun grizzl-search-increment (sub-table result)
  "Use the search lookup table to filter already-accumulated results."
  (cl-flet ((next-offset (key current sub-table)
              (cl-find-if (lambda (v)
                         (> v current))
                       (gethash key sub-table))))
    (maphash (lambda (k v)
               (let* ((oldpos (car v))
                      (oldrank (cadr v))
                      (len (cl-caddr v))
                      (newpos (next-offset k oldpos sub-table)))
                 (if newpos
                     (puthash k (list newpos
                                      (grizzl-inc-rank oldrank oldpos newpos len)
                                      len)
                              result)
                   (remhash k result))))
             result)))

(defun grizzl-inc-rank (oldrank oldpos newpos len)
  "Increment the current match distance as a new char is matched."
  (let ((distance (if (< oldpos 0) 1 (- newpos oldpos))))
    (+ oldrank (* len (* distance distance)))))

;;; --- Configuration Variables

(defvar *grizzl-read-max-results* 10
  "The maximum number of results to show in `grizzl-completing-read'.")

;;; --- Runtime Processing Variables

(defvar *grizzl-current-result* nil
  "The search result in `grizzl-completing-read'.")

(defvar *grizzl-current-selection* 0
  "The selected offset in `grizzl-completing-read'.")

(defface grizzl-selection-face
  `((((class color) (background light))
     (:foreground "red"))
    (((class color) (background dark))
     (:foreground "red"))
    (t (:foreground "red")))
  "Face for selected result."
  :group 'grizzl-mode)

(defface grizzl-prompt-face
  `((t :inherit 'mode-line-inactive))
  "Face used for grizzl prompt."
   :group 'grizzl-mode)

;;; --- Minor Mode Definition

(defvar *grizzl-keymap* (make-sparse-keymap)
  "Internal keymap used by the minor-mode in `grizzl-completing-read'.")

(define-key *grizzl-keymap* (kbd "<up>")   'grizzl-set-selection+1)
(define-key *grizzl-keymap* (kbd "C-p")    'grizzl-set-selection+1)
(define-key *grizzl-keymap* (kbd "<down>") 'grizzl-set-selection-1)
(define-key *grizzl-keymap* (kbd "C-n")    'grizzl-set-selection-1)

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
        (let* ((hookfun (lambda ()
                          (setq *grizzl-current-result*
                                (grizzl-search (minibuffer-contents)
                                               index
                                               *grizzl-current-result*))
                          (grizzl-display-result index prompt)))
               (exitfun (lambda ()
                          (grizzl-mode -1)
                          (remove-hook 'post-command-hook    hookfun t))))
          (add-hook 'minibuffer-exit-hook exitfun nil t)
          (add-hook 'post-command-hook    hookfun nil t)))
    (let ((read-value (read-from-minibuffer ">>> ")))
      (or (grizzl-selected-result index) read-value))))

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
  (grizzl-move-selection 1))

;;;###autoload
(defun grizzl-set-selection-1 ()
  "Move the selection down one row in `grizzl-completing-read'."
  (interactive)
  (grizzl-move-selection -1))

;;; --- Private Functions

(defun grizzl-move-selection (delta)
  "Move the selection by DELTA rows in `grizzl-completing-read'."
  (setq *grizzl-current-selection* (+ (grizzl-current-selection) delta))
  (when (not (= (grizzl-current-selection) *grizzl-current-selection*))
    (beep)))

(defun grizzl-display-result (index prompt)
  "Renders a series of overlays to list the matches in the result."
  (let* ((matches (grizzl-result-strings *grizzl-current-result* index
                                         :start 0
                                         :end   *grizzl-read-max-results*)))
    (delete-all-overlays)
    (overlay-put (make-overlay (point-min) (point-min))
                 'before-string
                 (format "%s\n%s\n"
                         (mapconcat 'identity
                                    (grizzl-map-format-matches matches)
                                    "\n")
                         (grizzl-format-prompt-line prompt)))
    (set-window-text-height nil (max 3 (+ 2 (length matches))))))

(defun grizzl-map-format-matches (matches)
  "Convert the set of string MATCHES into propertized text objects."
  (if (= 0 (length matches))
      (list (propertize "-- NO MATCH --" 'face 'outline-3))
    (cdr (cl-reduce (lambda (acc str)
                      (let* ((idx (car acc))
                             (lst (cdr acc))
                             (sel (= idx (grizzl-current-selection))))
                        (cons (1+ idx)
                              (cons (grizzl-format-match str sel) lst))))
                    matches
                    :initial-value '(0)))))

(defun grizzl-format-match (match-str selected)
  "Default match string formatter in `grizzl-completing-read'.

MATCH-STR is the string in the selection list and SELECTED is non-nil
if this is the current selection."
  (let ((margin (if selected "> "            "  "))
        (face   (if selected 'grizzl-selection-face 'default)))
    (propertize (format "%s%s" margin match-str) 'face face)))

(defun grizzl-format-prompt-line (prompt)
  "Returns a string to render a full-width prompt in `grizzl-completing-read'."
  (let* ((count (grizzl-result-count *grizzl-current-result*))
         (match-info (format " (%d candidate%s) ---- *-"
                             count (if (= count 1) "" "s"))))
    (concat (propertize (format "-*%s *-" prompt) 'face 'grizzl-prompt-face)

            (propertize " "
                        'face    'grizzl-prompt-face
                        'display `(space :align-to (- right
                                                      ,(1+ (length match-info)))))
            (propertize match-info 'face 'grizzl-prompt-face))))

(defun grizzl-current-selection ()
  "Get the currently selected index in `grizzl-completing-read'."
  (let ((max-selection
         (min (1- *grizzl-read-max-results*)
              (1- (grizzl-result-count *grizzl-current-result*)))))
    (max 0 (min max-selection *grizzl-current-selection*))))

(provide 'grizzl)

;;; grizzl.el ends here
