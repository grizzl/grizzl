;;; grizzl-core.el --- Fast fuzzy search index for Emacs.

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

;; Overview:
;;
;; Grizzl provides  a fuzzy  completion framework for  general purpose
;; use in Emacs Lisp projects.
;;
;; grizzl-core.el provides  the underlying data structures  and sesrch
;; algorithm without any  UI attachment.  At the core,  a fuzzy search
;; index is created from a list of strings, using `grizzl-make-index'.
;; A fuzzy search  term is then used  to get a result  from this index
;; with  `grizzl-search'.  Because  grizzl  considers the  usage of  a
;; fuzzy  search index  to operate  in real-time  as a  user enters  a
;; search term in the minibuffer, the framework optimizes for this use
;; case.  Any result can be passed back into `grizzl-search' as a hint
;; to continue searching.  The search  algorithm is able to understand
;; insertions and deletions and therefore  minimizes the work it needs
;; to do in this  case.  The intended use here is  to collect a result
;; on each key press and feed that result into the search for the next
;; key press. Once a search is  complete, the matched strings are then
;; read, using `grizzl-result-strings'. The results are ordered on the
;; Levenshtein distance between them and the search term.
;;
;; It  is assumed  that  the  index will  be  re-used across  multiple
;; searches on larger sets of data.
;;
;; A discussion of the algorithm can be found in the docs/ directory.
;;

(require 'cl)

;;; --- Public Functions

;;;###autoload
(defun grizzl-make-index (strings)
  "Makes an index from the list STRINGS for use with `grizzl-search'."
  (let ((lookup-table (make-hash-table)))
    (reduce (lambda (list-offset str)
              (grizzl-index-insert str list-offset lookup-table)
              (1+ list-offset))
            strings
            :initial-value 0)
    (maphash (lambda (char str-map)
               (maphash (lambda (list-offset locations)
                          (puthash list-offset (reverse locations) str-map))
                        str-map)) lookup-table)
    (cons (vconcat strings) lookup-table)))

;;;###autoload
(defun grizzl-search (term index old-result)
  "Fuzzy searches for TERM in INDEX prepared with `grizzl-make-index'.
If OLD-RESULT is non-nil, it is an existing search result to increment from.
The result can be read with `grizzl-result-strings'."
  (let* ((result (grizzl-rewind-result term index old-result))
         (matches (copy-hash-table (grizzl-result-matches result)))
         (from-pos (length (grizzl-result-term result)))
         (remainder (substring term from-pos))
         (table (grizzl-lookup-table index)))
    (reduce (lambda (n ch)
              (let ((sub-table (gethash ch table)))
                (if (not sub-table)
                    (clrhash matches)
                  (grizzl-search-increment sub-table matches))
                (1+ n)))
            remainder
            :initial-value from-pos)
    (grizzl-cons-result term matches result)))

;;;###autoload
(defun grizzl-result-strings (result index)
  "Returns the ordered list of matched strings in RESULT, using INDEX."
  (let* ((matches (grizzl-result-matches result))
         (strings (grizzl-index-strings index))
         (loaded '()))
    (maphash (lambda (string-offset char-offset)
               (push (elt strings string-offset) loaded))
             matches)
    loaded))

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
    (dotimes (n (length (grizzl-index-strings index)))
      (puthash n -1 matches))
    matches))

(defun grizzl-result-term (result)
  "Returns the search term used to find the matches in RESULT."
  (car (car result)))

(defun grizzl-result-matches (result)
  "Returns the internal hash used to track the matches in RESULT."
  (cdar result))

(defun grizzl-result-count (result)
  "Returns the number of matches present in RESULT."
  (hash-table-count (grizzl-result-matches result)))

(defun grizzl-index-insert (string list-offset index)
  "Inserts STRING at LIST-OFFSET into INDEX."
  (reduce (lambda (char-offset char)
            (let* ((str-map (or (gethash char index)
                                (puthash char (make-hash-table) index)))
                   (offsets (gethash list-offset str-map)))
              (puthash list-offset
                       (cons char-offset offsets)
                       str-map)
              (1+ char-offset)))
          string
          :initial-value 0))

(defun grizzl-lookup-table (index)
  "Returns the lookup table portion of INDEX."
  (cdr index))

(defun grizzl-index-strings (index)
  "Returns the vector of strings stored in INDEX."
  (car index))

(defun grizzl-search-increment (sub-table result)
  "Use the search lookup table to filter already-accumulated results."
  (cl-flet ((next-offset (key current sub-table)
              (find-if (lambda (v)
                         (> v current))
                       (gethash key sub-table))))
    (maphash (lambda (k v)
               (let ((offset (next-offset k v sub-table)))
                 (if offset
                     (puthash k offset result)
                   (remhash k result))))
             result)))

(provide 'grizzl-core)

;;; grizzl-core.el ends here
