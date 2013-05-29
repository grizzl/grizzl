;;; grizzl-core-test.el --- Fuzzy search index tests.

;; Copyright © 2013 Chris Corbyn

;;; --- Unit Tests

(require 'grizzl-core)

(lexical-let*
    ((index (grizzl-make-index '("models" "controllers" "views")))

     (search (lambda (term &optional prev)
               "Perform a search and nothing more."
               (grizzl-search term index prev)))

     (search-read (lambda (term &optional prev)
               "Perform a search and read the result strings."
               (grizzl-result-strings (funcall search term prev) index)))

     (search-sort (lambda (term &optional prev)
                    "Search, and sort the result lexographically."
                    (sort (funcall search-read term prev)
                          #'string-lessp))))

  (ert-deftest initial-search-test ()
    "Test grizzl can index and fuzzy-search a list of strings."
    (should (equal (funcall search-sort "oe") '("controllers" "models")))
    (should (equal (funcall search-sort "iw") '("views")))
    (should (equal (funcall search-sort "bad") '()))
    (should (equal (funcall search-sort "es") '("controllers" "models" "views"))))

  (ert-deftest increment-search-append-test ()
    "Test grizzl can accept an existing result and search string to search."
    (let ((prev (funcall search "ol")))
      (should (equal (funcall search-sort "olr" prev) '("controllers")))))

  (ert-deftest increment-search-deletion-test ()
    "Test grizzl handles repeated search when backspace was hit."
    (let ((prev (funcall search "olr")))
      (should (equal (funcall search-sort "ol" prev) '("controllers" "models")))))

  (ert-deftest increment-search-substitution-test ()
    "Test grizzl handles repeated search when edits were made."
    (let ((prev (funcall search "els")))
      (should (equal (funcall search-sort "ers" prev) '("controllers")))))

  (ert-deftest distance-ordering-test ()
    "Test grizzl orders the results by closest distance."
    (should (equal (funcall search-read "oe")  '("models" "controllers")))
    (should (equal (funcall search-read "es")  '("views" "models" "controllers")))))
