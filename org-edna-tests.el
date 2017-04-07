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

(provide 'org-edna-tests)

;;; org-edna-tests.el ends here
