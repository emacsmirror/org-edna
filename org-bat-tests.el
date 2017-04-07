;;; org-bat-tests.el --- Tests for org-bat

;; Author: Ian Dunn

;;; Commentary:

;;; Code:

(require 'org-bat)
(require 'ert)

(ert-deftest org-bat-parse-form-no-arguments ()
  (let* ((input-string "test-string")
         (parsed       (org-bat-parse-form input-string)))
    (should parsed)
    (should (= (length parsed) 4))
    (pcase-let* ((`(,token ,args ,modifier ,pos) parsed))
      (should (eq token 'test-string))
      (should (not args))
      (should (not modifier))
      (should (= pos 11)))))

(ert-deftest org-bat-parse-form-no-arguments-modifier ()
  (let* ((input-string "!test-string")
         (parsed       (org-bat-parse-form input-string)))
    (should parsed)
    (should (= (length parsed) 4))
    (pcase-let* ((`(,token ,args ,modifier ,pos) parsed))
      (should (eq token 'test-string))
      (should (not args))
      (should (eq modifier '!))
      (should (= pos 12)))))

(ert-deftest org-bat-parse-form-single-argument ()
  (let* ((input-string "test-string(abc)")
         (parsed       (org-bat-parse-form input-string)))
    (should parsed)
    (should (= (length parsed) 4))
    (pcase-let* ((`(,token ,args ,modifier ,pos) parsed))
      (should (eq token 'test-string))
      (should (= (length args) 1))
      (should (stringp (nth 0 args)))
      (should (string-equal (nth 0 args) "abc"))
      (should (not modifier))
      (should (= pos (length input-string))))))

(ert-deftest org-bat-parse-form-string-argument ()
  (let* ((input-string "test-string(abc,\"def (ghi)\")")
         (parsed       (org-bat-parse-form input-string)))
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

(provide 'org-bat-tests)

;;; org-bat-tests.el ends here
