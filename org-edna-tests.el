;;; org-edna-tests.el --- Tests for org-edna

;; Author: Ian Dunn

;;; Commentary:

;;; Code:

(require 'org-edna)
(require 'ert)

(ert-deftest org-edna-parse-form-no-arguments ()
  (let* ((input-string "test-string")
         (parsed       (org-edna-parse-form input-string)))
    (should parsed)
    (should (= (length parsed) 4))
    (pcase-let* ((`(,token ,args ,modifier ,pos) parsed))
      (should (eq token 'test-string))
      (should (not args))
      (should (not modifier))
      (should (= pos 11)))))

(ert-deftest org-edna-parse-form-no-arguments-modifier ()
  (let* ((input-string "!test-string")
         (parsed       (org-edna-parse-form input-string)))
    (should parsed)
    (should (= (length parsed) 4))
    (pcase-let* ((`(,token ,args ,modifier ,pos) parsed))
      (should (eq token 'test-string))
      (should (not args))
      (should (eq modifier '!))
      (should (= pos 12)))))

(ert-deftest org-edna-parse-form-single-argument ()
  (let* ((input-string "test-string(abc)")
         (parsed       (org-edna-parse-form input-string)))
    (should parsed)
    (should (= (length parsed) 4))
    (pcase-let* ((`(,token ,args ,modifier ,pos) parsed))
      (should (eq token 'test-string))
      (should (= (length args) 1))
      (should (stringp (nth 0 args)))
      (should (string-equal (nth 0 args) "abc"))
      (should (not modifier))
      (should (= pos (length input-string))))))

(ert-deftest org-edna-parse-form-string-argument ()
  (let* ((input-string "test-string(abc,\"def (ghi)\")")
         (parsed       (org-edna-parse-form input-string)))
    (should parsed)
    (should (= (length parsed) 4))
    (pcase-let* ((`(,token ,args ,modifier ,pos) parsed))
      (should (eq token 'test-string))
      (should (= (length args) 2))
      (should (stringp (nth 0 args)))
      (should (string-equal (nth 0 args) "abc"))
      (should (stringp (nth 1 args)))
      (should (string-equal (nth 1 args) "def (ghi)"))
      (should (not modifier))
      (should (= pos (length input-string))))))

(defconst org-edna-test-dir
  (expand-file-name (file-name-directory (or load-file-name buffer-file-name))))

(defconst org-edna-test-file
  (expand-file-name "org-edna-tests.org" org-edna-test-dir))


;; Finders

(defsubst org-edna-heading (pom)
  (org-with-point-at pom
    (org-get-heading t t t t)))

(ert-deftest org-edna-finder/match-single-arg ()
  (let* ((org-agenda-files `(,org-edna-test-file))
         (targets (org-edna-finder/match "test&1")))
    (should (= (length targets) 2))
    (should (string-equal (org-edna-heading (nth 0 targets)) "Tagged Heading 1"))
    (should (string-equal (org-edna-heading (nth 1 targets)) "Tagged Heading 2"))))

(ert-deftest org-edna-finder/ids-single ()
  (let* ((org-agenda-files `(,org-edna-test-file))
         (test-id "caccd0a6-d400-410a-9018-b0635b07a37e")
         (targets (org-edna-finder/ids test-id)))
    (should (= (length targets) 1))
    (should (string-equal (org-edna-heading (nth 0 targets)) "Blocking Test"))
    (should (string-equal (org-entry-get (nth 0 targets) "ID") test-id))))

(ert-deftest org-edna-finder/ids-multiple ()
  (let* ((org-agenda-files `(,org-edna-test-file))
         (test-ids '("0d491588-7da3-43c5-b51a-87fbd34f79f7"
                     "b010cbad-60dc-46ef-a164-eb155e62cbb2"))
         (targets (apply 'org-edna-finder/ids test-ids)))
    (should (= (length targets) 2))
    (should (string-equal (org-edna-heading (nth 0 targets)) "ID Heading 1"))
    (should (string-equal (org-entry-get (nth 0 targets) "ID") (nth 0 test-ids)))
    (should (string-equal (org-edna-heading (nth 1 targets)) "ID Heading 2"))
    (should (string-equal (org-entry-get (nth 1 targets) "ID") (nth 1 test-ids)))))

(ert-deftest org-edna-finder/match-blocker ()
  (let* ((org-agenda-files `(,org-edna-test-file))
         (heading (org-id-find "caccd0a6-d400-410a-9018-b0635b07a37e" t))
         (blocker (org-entry-get heading "BLOCKER")))
    (should (string-equal "match(test&1)" blocker))
    (org-with-point-at heading
      (org-edna-process-form blocker 'condition))
    (should (string-equal (substring-no-properties org-block-entry-blocking)
                          "TODO Tagged Heading 1 :1:test:"))))

(provide 'org-edna-tests)

;;; org-edna-tests.el ends here
