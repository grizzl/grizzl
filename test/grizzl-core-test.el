;;; grizzl-core-test.el --- Fuzzy search index tests.

;; Copyright © 2013 Chris Corbyn

;;; --- Unit Tests

(require 'grizzl-core)

(lexical-let*
    ((index (grizzl-make-index '("models" "controllers" "views" "registry")))

     (search (lambda (term &rest opts)
               "Perform a search and nothing more."
               (grizzl-search term index (plist-get opts :prev))))

     (search-read (lambda (term &rest opts)
               "Perform a search and read the result strings."
               (grizzl-result-strings (funcall search term
                                               (plist-get opts :prev))
                                      index
                                      :start (plist-get opts :start)
                                      :end   (plist-get opts :end))))

     (search-sort (lambda (term &rest opts)
                    "Search, and sort the result lexographically."
                    (sort (funcall search-read term
                                   (plist-get opts :prev)
                                   :start (plist-get opts :start)
                                   :end   (plist-get opts :end))
                          #'string-lessp))))

  (ert-deftest initial-search-test ()
    "Test grizzl can index and fuzzy-search a list of strings."
    (should (equal (funcall search-sort "oe")
                   '("controllers" "models")))
    (should (equal (funcall search-sort "iw")
                   '("views")))
    (should (equal (funcall search-sort "bad") '()))
    (should (equal (funcall search-sort "es")
                   '("controllers" "models" "registry" "views"))))

  (ert-deftest increment-search-append-test ()
    "Test grizzl can accept an existing result and search string to search."
    (let ((prev (funcall search "ol")))
      (should (equal (funcall search-sort "olr" :prev prev)
                     '("controllers")))))

  (ert-deftest increment-search-deletion-test ()
    "Test grizzl handles repeated search when backspace was hit."
    (let ((prev (funcall search "olr")))
      (should (equal (funcall search-sort "ol" :prev prev)
                     '("controllers" "models")))))

  (ert-deftest increment-search-substitution-test ()
    "Test grizzl handles repeated search when edits were made."
    (let ((prev (funcall search "els")))
      (should (equal (funcall search-sort "ers" :prev prev)
                     '("controllers")))))

  (ert-deftest length-ordering-test ()
    "Test grizzl orders the results by shortest length."
    (should (equal (funcall search-read "oe") '("models" "controllers"))))

  (ert-deftest proximity-scoring-test ()
    "Test grizzl factors in proximity of matched letters."
    (should (equal (funcall search-read "es")
                   '("views" "models" "controllers" "registry"))))

  (ert-deftest limit-results-test ()
    "Test grizzl can limit the number of results returned."
    (should (equal (funcall search-read "es" :end 2)
                   '("views" "models"))))

  (ert-deftest offset-results-test ()
    "Test grizzl can offset the start of results returned."
    (should (equal (funcall search-read "es" :start 1)
                   '("models" "controllers" "registry"))))

  (ert-deftest out-of-bounds-limit-results-test ()
    "Test grizzl silently ignores limits > result count."
    (should (equal (funcall search-read "es" :end 7)
                   '("views" "models" "controllers" "registry"))))

  (ert-deftest offset-and-limit-results-test ()
    "Test grizzl can offset the start of results returned and limit them."
    (should (equal (funcall search-read "es" :start 1 :end 2)
                   '("models")))))
