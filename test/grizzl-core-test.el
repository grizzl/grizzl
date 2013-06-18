;;; grizzl-core-test.el --- Fuzzy search index tests.

;; Copyright © 2013 Chris Corbyn

;;; --- Unit Tests

(require 'grizzl-core)

(lexical-let*
    ((index (lambda (&rest opts)
              (apply 'grizzl-make-index
                     (append '(("Models"
                                "Controllers"
                                "Views"
                                "Registry"))
                             opts))))

     (search (lambda (term &rest opts)
               "Perform a search and nothing more."
               (grizzl-search term
                              (apply index opts)
                              (plist-get opts :prev))))

     (search-read (lambda (term &rest opts)
               "Perform a search and read the result strings."
               (grizzl-result-strings (apply search (append (list term) opts))
                                      (apply index opts)
                                      :start (plist-get opts :start)
                                      :end   (plist-get opts :end))))

     (search-sort (lambda (term &rest opts)
                    "Search, and sort the result lexographically."
                    (sort (apply search-read (append (list term) opts))
                          #'string-lessp))))

  (ert-deftest initial-search-multi-match-test ()
    "Test grizzl can fuzzy-match multiple matches in an index."
    (should (equal (funcall search-sort "oe")
                   '("Controllers" "Models"))))

  (ert-deftest initial-search-single-match-test ()
    "Test grizzl can fuzzy-match a unique match in an index."
    (should (equal (funcall search-sort "iw")
                   '("Views"))))

  (ert-deftest initial-search-non-match-test ()
    "Test grizzl returns nil on no match."
    (should (equal (funcall search-sort "bad") '())))

  (ert-deftest initial-search-all-match-test ()
    "Test grizzl returns all strings if possible."
    (should (equal (funcall search-sort "es")
                   '("Controllers" "Models" "Registry" "Views"))))

  (ert-deftest increment-search-append-test ()
    "Test grizzl can accept an existing result and search string to search."
    (let ((prev (funcall search "ol")))
      (should (equal (funcall search-sort "olr" :prev prev)
                     '("Controllers")))))

  (ert-deftest increment-search-deletion-test ()
    "Test grizzl handles repeated search when backspace was hit."
    (let ((prev (funcall search "olr")))
      (should (equal (funcall search-sort "ol" :prev prev)
                     '("Controllers" "Models")))))

  (ert-deftest increment-search-substitution-test ()
    "Test grizzl handles repeated search when edits were made."
    (let ((prev (funcall search "els")))
      (should (equal (funcall search-sort "ers" :prev prev)
                     '("Controllers")))))

  (ert-deftest length-ordering-test ()
    "Test grizzl orders the results by shortest length."
    (should (equal (funcall search-read "oe")
                   '("Models" "Controllers"))))

  (ert-deftest proximity-scoring-test ()
    "Test grizzl factors in proximity of matched letters."
    (should (equal (funcall search-read "es")
                   '("Views" "Models" "Controllers" "Registry"))))

;  (ert-deftest ambiguous-proximity-scoring-test ()
;    "Test grizzl picks the closest letters for proximity scoring."
;    (should (equal (funcall search-read "ol")
;                   '("Controllers" "Models"))))

  (ert-deftest limit-results-test ()
    "Test grizzl can limit the number of results returned."
    (should (equal (funcall search-read "es" :end 2)
                   '("Views" "Models"))))

  (ert-deftest offset-results-test ()
    "Test grizzl can offset the start of results returned."
    (should (equal (funcall search-read "es" :start 1)
                   '("Models" "Controllers" "Registry"))))

  (ert-deftest out-of-bounds-limit-results-test ()
    "Test grizzl silently ignores limits > result count."
    (should (equal (funcall search-read "es" :end 7)
                   '("Views" "Models" "Controllers" "Registry"))))

  (ert-deftest offset-and-limit-results-test ()
    "Test grizzl can offset the start of results returned and limit them."
    (should (equal (funcall search-read "es" :start 1 :end 2)
                   '("Models"))))

  (ert-deftest case-insensitive-index-lowercase-test ()
    "Test grizzl is case-insensitive base default."
    (should (equal (funcall search-sort "mdl")
                   '("Models"))))

  (ert-deftest case-insensitive-index-uppercase-test ()
    "Test grizzl is case-insensitive base default."
    (should (equal (funcall search-sort "MDL")
                   '("Models"))))

  (ert-deftest case-sensitive-index-match-test ()
    "Test grizzl can be case-sensitive if needed; matching."
    (should (equal (funcall search-sort "Mdl" :case-sensitive t)
                   '("Models"))))

  (ert-deftest case-sensitive-index-non-match-test ()
    "Test grizzl can be case-sensitive if needed; failing."
    (should (equal (funcall search-sort "mdl" :case-sensitive t) '()))))
