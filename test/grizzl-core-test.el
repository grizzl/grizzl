;;; grizzl-core-test.el --- Fuzzy search index tests.

;; Copyright © 2013 Chris Corbyn

;;; --- Unit Tests

(load (expand-file-name "../grizzl-core.el"))

(require 'grizzl-core)

(ert-deftest index-search-test ()
  "Test grizzl can index and fuzzy-search a list of strings."
  (let* ((strings '("models" "controllers" "views"))
         (index (grizzl-make-index strings)))
    (cl-flet ((search (term)
                (let ((result (grizzl-search term index nil)))
                  (grizzl-result-strings result index))))
      (should (equal (search "oe")  '("controllers" "models")))
      (should (equal (search "iw")  '("views")))
      (should (equal (search "bad") '()))
      (should (equal (search "es")  '("views" "controllers" "models"))))))

(ert-deftest increment-search-append-test ()
  "Test grizzl can accept an existing result and search string to search."
  (let* ((strings '("models" "controllers" "views"))
         (index (grizzl-make-index strings))
         (result (grizzl-search "ol" index nil))
         (matches (grizzl-result-strings (grizzl-search "olr" index result)
                                     index)))
    (should (equal matches  '("controllers")))))

(ert-deftest increment-search-deletion-test ()
  "Test grizzl handles repeated search when backspace was hit."
  (let* ((strings '("models" "controllers" "views"))
         (index (grizzl-make-index strings))
         (result (grizzl-search "olr" index nil))
         (matches (grizzl-result-strings (grizzl-search "ol" index result)
                                     index)))
    (should (equal matches  '("controllers" "models")))))

(ert-deftest increment-search-substitution-test ()
  "Test grizzl handles repeated search when edits were made."
  (let* ((strings '("models" "controllers" "views"))
         (index (grizzl-make-index strings))
         (result (grizzl-search "els" index nil))
         (matches (grizzl-result-strings (grizzl-search "ers" index result)
                                     index)))
    (should (equal matches  '("controllers")))))

;; TODO: Implement and test Levenshtein ordering
