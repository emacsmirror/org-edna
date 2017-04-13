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

(ert-deftest org-edna-parse-form-multiple-forms ()
  (let ((input-string "test-string1 test-string2")
        pos)
    (pcase-let* ((`(,token1 ,args1 ,modifier1 ,pos1) (org-edna-parse-form input-string)))
      ;; (should (and token1 args1 modifier1 pos1))
      (should (eq token1 'test-string1))
      (should (not args1))
      (should (not modifier1))
      (should (= pos1 13))
      (setq pos pos1))
    (pcase-let* ((`(,token2 ,args2 ,modifier2 ,pos2) (org-edna-parse-form (substring input-string pos))))
      (should (eq token2 'test-string2))
      (should (not args2))
      (should (not modifier2))
      (should (= pos2 12)))))

(ert-deftest org-edna-parse-form-empty-argument-list ()
  (let ((input-string "test-string1()"))
    (pcase-let* ((`(,token1 ,args1 ,modifier1 ,pos1) (org-edna-parse-form input-string)))
      (should (eq token1 'test-string1))
      (should (not args1))
      (should (not modifier1))
      (should (= pos1 (length input-string))))))

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


;; Actions

(ert-deftest org-edna-action/todo-test ()
  (let* ((org-agenda-files `(,org-edna-test-file))
         (target (org-id-find "0d491588-7da3-43c5-b51a-87fbd34f79f7" t)))
    (org-with-point-at target
      (org-edna-action/todo nil "DONE")
      (should (string-equal (org-entry-get nil "TODO") "DONE"))
      (org-edna-action/todo nil "TODO")
      (should (string-equal (org-entry-get nil "TODO") "TODO")))))


;; Conditions


;; Consideration

(ert-deftest org-edna-consideration/all ()
  (let ((blocks-blocking `("a" nil "b"))
        (blocks-no-blocking `(nil nil nil)))
    (should (string-equal (org-edna-handle-consideration 'all blocks-blocking) "a"))
    (should (not (org-edna-handle-consideration 'all blocks-no-blocking)))))

(ert-deftest org-edna-consideration/integer ()
  (let ((blocks-blocking `("a" "c" "b"))
        (blocks-no-blocking `("a" nil "b"))
        (blocks-empty `(nil nil nil)))
    (should (string-equal (org-edna-handle-consideration 1 blocks-blocking) "a"))
    (should (not (org-edna-handle-consideration 1 blocks-no-blocking)))
    (should (not (org-edna-handle-consideration 1 blocks-empty)))))

(ert-deftest org-edna-consideration/float ()
  (let ((blocks-blocking `("a" "c" "b"))
        (blocks-no-blocking `("a" nil "b"))
        (blocks-empty `(nil nil nil)))
    (should (string-equal (org-edna-handle-consideration 0.25 blocks-blocking) "a"))
    (should (not (org-edna-handle-consideration 0.25 blocks-no-blocking)))
    (should (not (org-edna-handle-consideration 0.25 blocks-empty)))))

(provide 'org-edna-tests)

;;; org-edna-tests.el ends here
