;;; grizzl-core.el --- Fast fuzzy search database for Emacs.
;;
;; Copyright © 2013 Chris Corbyn
;;
;; Author:   Chris Corbyn <chris@w3style.co.uk>
;; URL:      https://github.com/d11wtq/grizzl
;; Version:  0.1.2
;; Keywords: convenience, usability
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
;; Grizzl provides a fuzzy completion framework for general purpose
;; use in Emacs Lisp projects.
;;
;; grizzl-core.el provides the underlying data structures and sesrch
;; algorithm without any UI attachment.  At the core, a fuzzy search
;; database is created from a list of strings, using `grizzl-make-database'.
;; A fuzzy search term is then used to get a result from this database
;; with `grizzl-search'.  Because grizzl considers the usage of a
;; fuzzy search database to operate in real-time as a user enters a
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
;; It is assumed that the database will be re-used across multiple
;; searches on larger sets of data.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;; Code:

;; GNU library.
(eval-when-compile (require 'cl))

(defgroup grizzl nil
  "A fuzzy completion framework for general purpose use")

(defun grizzl-cons-result (term matches results)
  "Build a new result for TERM and hash-table MATCHES consed with RESULTS."
    (cons (cons term matches) results))

(defun grizzl-rewind-result (term database result)
  "Adjusts RESULT according to TERM, ready for a new search."
  (if result
      (let* ((old-term (grizzl-result-term result))
             (new-len (length term))
             (old-len (length old-term)))
        (if (and (>= new-len old-len)
                 (string-equal old-term (substring term 0 old-len)))
              result
          (grizzl-rewind-result term database (cdr result))))
    (grizzl-cons-result "" (grizzl-base-matches database) nil)))

(defun grizzl-base-matches (database)
  "Returns the full set of matches in INDEX, with an out-of-bound offset."
  (let ((matches (make-hash-table)))
    (cl-reduce (lambda (n s-len)
                 (puthash n (list -1 0 (cdr s-len)) matches)
                 (1+ n))
               (grizzl-index-strings database)
               :initial-value 0)
    matches))

(defun grizzl-result-term (result)
  "Returns the search term used to find the matches in RESULT."
  (car (car result)))

(defun grizzl-result-matches (result)
  "Returns the internal hash used to track the matches in RESULT."
  (cdar result))

(defun grizzl-database-insert (string list-offset database &rest options)
  "Inserts STRING at LIST-OFFSET into INDEX."
  (let ((case-sensitive (plist-get options :case-sensitive)))
    (cl-reduce (lambda (char-offset cs-char)
                 (let* ((char (if case-sensitive
                                  cs-char
                                (downcase cs-char)))
                        (str-map (or (gethash char database)
                                     (puthash char (make-hash-table) database)))
                        (offsets (gethash list-offset str-map)))
                   (puthash list-offset
                            (cons char-offset offsets)
                            str-map)
                   (1+ char-offset)))
               string
               :initial-value 0)))

(defun grizzl-lookup-table (database)
  "Returns the lookup table portion of INDEX."
  (cdr (assoc 'lookup-table database)))

(defun grizzl-index-strings (database)
  "Returns the vector of strings stored in INDEX."
  (cdr (assoc 'string-data database)))

(defun grizzl-index-case-sensitive-p (database)
  "Predicate to test of INDEX is case-sensitive."
  (cdr (assoc 'case-sensitive database)))

(defun grizzl-search-increment (sub-table result)
  "Use the search lookup table to filter already-accumulated results."
  (cl-flet ((next-offset (key current sub-table)
              (find-if (lambda (v)
                         (> v current))
                       (gethash key sub-table))))
    (maphash (lambda (k v)
               (let* ((oldpos (car v))
                      (oldrank (cadr v))
                      (len (caddr v))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun grizzl-make-database (strings &rest options)
  "Makes an database from the list STRINGS for use with `grizzl-search'.

If `:progress-fn' is given as a keyword argument, it is called repeatedly
with integers N and TOTAL.

If `:case-sensitive' is specified as a non-nil keyword argument, the database
will be created case-sensitive, otherwise it will be case-insensitive."
  (let ((lookup-table (make-hash-table))
        (total-strs (length strings))
        (case-sensitive (plist-get options :case-sensitive))
        (progress-fn (plist-get options :progress-fn))
        (string-data (vconcat (mapcar (lambda (s)
                                        (cons s (length s)))
                                      strings))))
    (cl-reduce (lambda (list-offset str)
                 (grizzl-database-insert str list-offset lookup-table
                                         :case-sensitive case-sensitive)
                 (when progress-fn
                   (funcall progress-fn (1+ list-offset) total-strs))
                 (1+ list-offset))
               strings
               :initial-value 0)
    (maphash (lambda (char str-map)
               (maphash (lambda (list-offset locations)
                          (puthash list-offset (reverse locations) str-map))
                        str-map)) lookup-table)
    `((case-sensitive . ,case-sensitive)
      (lookup-table   . ,lookup-table)
      (string-data    . ,string-data))))

;;;###autoload
(defun grizzl-search (term database &optional old-result)
  "Fuzzy searches for TERM in INDEX prepared with `grizzl-make-database'.

OLD-RESULT may be specified as an existing search result to increment from.
The result can be read with `grizzl-result-strings'."
  (let* ((cased-term (if (grizzl-index-case-sensitive-p database)
                         term
                       (downcase term)))
         (result (grizzl-rewind-result cased-term database old-result))
         (matches (copy-hash-table (grizzl-result-matches result)))
         (from-pos (length (grizzl-result-term result)))
         (remainder (substring cased-term from-pos))
         (lookup-table (grizzl-lookup-table database)))
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
(defun grizzl-result-strings (result database &rest options)
  "Returns the ordered list of matched strings in RESULT, using INDEX.

If the :START option is specified, results are read from the given offset.
If the :END option is specified, up to :END results are returned."
  (let* ((matches (grizzl-result-matches result))
         (strings (grizzl-index-strings database))
         (loaded '())
         (start (plist-get options :start))
         (end (plist-get options :end)))
    (maphash (lambda (string-offset char-offset)
               (push string-offset loaded))
             matches)
    (let* ((ordered (sort loaded
                          (lambda (a b)
                            (< (cadr (gethash a matches))
                               (cadr (gethash b matches))))))
           (best (if (or start end)
                     (delete-if-not 'identity
                                    (subseq ordered (or start 0) end))
                   ordered)))
      (mapcar (lambda (n)
                (car (elt strings n)))
              best))))

(provide 'grizzl-core)
;;; grizzl-core.el ends here
