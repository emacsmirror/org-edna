;;; org-edna.el --- Extensible Dependencies 'N' Actions -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Ian Dunn

;; Author: Ian Dunn <dunni@gnu.org>
;; Keywords: convenience, text, org
;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (seq "2.19") (org "8.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'org)
(require 'subr-x)
(require 'seq)

(defgroup org-edna nil
  "Extensible Dependencies 'N' Actions"
  :group 'org)

(defcustom org-edna-use-inheritance nil
  "Whether Edna should use inheritance when looking for properties.

This only applies to the BLOCKER and TRIGGER properties, not any
properties used during actions or conditions."
  :group 'org-edna
  :type 'boolean)

(defmacro org-edna--syntax-error (msg form pos)
  `(signal 'invalid-read-syntax (list :msg ,msg :form ,form :pos ,pos)))

(defun org-edna--print-syntax-error (error-plist)
  (let ((msg (plist-get error-plist :msg))
        (form (plist-get error-plist :form))
        (pos (plist-get error-plist :pos)))
    (message
     "Org Edna Syntax Error: %s\n%s\n%s"
     msg form (concat (make-string pos ?\ ) "^"))))

(defun org-edna--transform-arg (arg)
  "Transform ARG.

Currently, the following are handled:

- UUIDs (as determined by `org-uuidgen-p') are converted to strings"
  (pcase arg
    ((and (pred symbolp)
          (let (pred org-uuidgen-p) (symbol-name arg)))
     (symbol-name arg))
    (_
     arg)))

(defun org-edna-parse-form (form &optional start)
  "Parse Edna form FORM."
  (setq start (or start 0))
  (pcase-let* ((`(,token . ,pos) (read-from-string form start))
               (modifier nil)
               (args nil))
    (unless token
      (org-edna--syntax-error "Invalid Token" form start))
    ;; Check for either end of string or an opening parenthesis
    (unless (or (equal pos (length form))
                (equal (string-match-p "\\s-" form pos) pos)
                (equal (string-match-p "(" form pos) pos))
      (org-edna--syntax-error "Invalid character in form" form pos))
    ;; Parse arguments if we have any
    (when (equal (string-match-p "(" form pos) pos)
      (pcase-let* ((`(,new-args . ,new-pos) (read-from-string form pos)))
        (setq pos new-pos
              args (mapcar #'org-edna--transform-arg new-args))))
    ;; Check for a modifier
    (when (string-match "^\\([!]\\)\\(.*\\)" (symbol-name token))
      (setq modifier (intern (match-string 1 (symbol-name token))))
      (setq token    (intern (match-string 2 (symbol-name token)))))
    ;; Move across any whitespace
    (when (string-match "\\s-+" form pos)
      (setq pos (match-end 0)))
    (list token args modifier pos)))

(defconst org-edna--types
  '(finder action condition)
  "Types recognized by org-edna.")

(defun org-edna--function-for-key (key)
  (cond
   ((eq key 'consideration)
    ;; Function is ignored here
    (cons 'consideration 'identity))
   (key
    (when-let ((func-format (format "org-edna-%%s/%s" key))
               (new-sym
                ;; Find the first bound function
                (seq-find
                 (lambda (sym) (fboundp (intern (format func-format sym))))
                 org-edna--types)))
      (cons new-sym (intern (format func-format new-sym)))))))

(defun org-edna--handle-condition (func mod args targets consideration)
  ;; Check the condition at each target
  (when-let ((blocks
              (mapcar
               (lambda (entry-marker)
                 (org-with-point-at entry-marker
                   (apply func mod args)))
               targets)))
    ;; Apply consideration
    (org-edna-handle-consideration consideration blocks)))

(defun org-edna-process-form (form action-or-condition)
  (let ((targets)
        (blocking-entry)
        (consideration 'all)
        (state nil) ;; Type of operation
        ;; Keep track of the current headline
        (last-entry (point-marker))
        (pos 0))
    (while (< pos (length form))
      (pcase-let* ((`(,key ,args ,mod ,new-pos) (org-edna-parse-form form pos))
                   (`(,type . ,func) (org-edna--function-for-key key)))
        (unless (and key type func)
          (org-edna--syntax-error "Unrecognized Form" form pos))
        (pcase type
          ('finder
           (unless (eq state 'finder)
             ;; We just executed some actions, so reset the entries.
             (setq targets nil))
           (setq state 'finder)
           (let ((markers (apply func args)))
             (setq targets (seq-uniq `(,@targets ,@markers)))))
          ('action
           (unless (eq action-or-condition 'action)
             (org-edna--syntax-error "Actions aren't allowed in this context" form pos))
           (unless targets
             (message "Warning: Action specified without targets"))
           (setq state 'action)
           (dolist (target targets)
             (org-with-point-at target
               (apply func last-entry args))))
          ('condition
           (unless (eq action-or-condition 'condition)
             (org-edna--syntax-error "Conditions aren't allowed in this context" form pos))
           (unless targets
             (message "Warning: Condition specified without targets"))
           (setq state 'condition)
           (setq blocking-entry
                 (or blocking-entry  ;; We're already blocking
                     (org-edna--handle-condition func mod args targets consideration))))
          ('consideration
           (unless (= (length args) 1)
             (org-edna--syntax-error "Consideration requires a single argument" form pos))
           ;; Consideration must be at the start of the targets, so clear out
           ;; any old targets.
           (setq targets nil
                 consideration (nth 0 args))))
        (setq pos new-pos)))
    ;; We exhausted the input string, but didn't find a condition when we were
    ;; expecting one.
    (when (and (eq action-or-condition 'condition) ;; Looking for conditions
               (eq state 'finder)                  ;; but haven't found any
               (not blocking-entry))                 ;; ever
      (setq blocking-entry
            (org-edna--handle-condition 'org-edna-condition/done
                                        t nil targets consideration)))
    ;; Only blockers care about the return value, and this will be non-nil if
    ;; the entry should be blocked.
    (setq org-block-entry-blocking blocking-entry)
    (not blocking-entry)))



(defmacro org-edna-run (change-plist &rest body)
  (declare (indent 1))
  `(let* ((pos (plist-get ,change-plist :position))
          (type (plist-get ,change-plist :type))
          (from (plist-get ,change-plist :from))
          (to (plist-get ,change-plist :to)))
     (if (and
          ;; We are only handling todo-state-change
          (eq type 'todo-state-change)
          ;; And only from a TODO state to a DONE state
          (member from (cons 'todo org-not-done-keywords))
          (member to (cons 'done org-done-keywords)))
         (condition-case err
             ,@body
           (error
            (if (eq (car err) 'invalid-syntax-error)
                (org-edna--print-syntax-error (cdr err))
              (message "Edna Error: %s" (error-message-string err)))
            (setq org-block-entry-blocking (org-get-heading))
            ;; Block
            nil))
       ;; Return t for the blocker to let the calling function know that there
       ;; is no block here.
       t)))

(defun org-edna-trigger-function (change-plist)
  (org-edna-run change-plist
    (when-let ((form (org-entry-get pos "TRIGGER" org-edna-use-inheritance)))
      (org-edna-process-form form 'action))))

(defun org-edna-blocker-function (change-plist)
  (org-edna-run change-plist
    (if-let ((form (org-entry-get pos "BLOCKER" org-edna-use-inheritance)))
        (org-edna-process-form form 'condition)
      t)))

;;;###autoload
(defun org-edna-load ()
  (interactive)
  (add-hook 'org-trigger-hook 'org-edna-trigger-function)
  (add-hook 'org-blocker-hook 'org-edna-blocker-function))

;;;###autoload
(defun org-edna-unload ()
  (interactive)
  (remove-hook 'org-trigger-hook 'org-edna-trigger-function)
  (remove-hook 'org-blocker-hook 'org-edna-blocker-function))



;; Tag Finder
(defun org-edna-finder/match (match-spec &optional scope skip)
  "Find entries with match-spec MATCH-SPEC.

MATCH-SPEC may be any valid match string; it is passed straight
into `org-map-entries', with 'agenda as the scope.

SCOPE and SKIP are their counterparts in `org-map-entries', but
as strings.

SCOPE defaults to \"agenda\", and SKIP defaults to nil."
  (setq scope (or scope 'agenda)
  (org-map-entries
   ;; Find all entries in the agenda files that match the given tag.
   (lambda nil (point-marker))
   match-spec scope skip))

;; ID finder
(defun org-edna-finder/ids (&rest ids)
  "Find entries with IDs in IDS.

IDS are all UUIDs as understood by `org-id-find'."
  (mapcar (lambda (id) (org-id-find id 'marker)) ids))

(defun org-edna-finder/self ()
  (list (point-marker)))

(defun org-edna-finder/siblings ()
  (org-with-wide-buffer
   (let ((self (and (ignore-errors (org-back-to-heading t)) (point)))
         (markers))
     (org-up-heading-safe)
     (org-goto-first-child)
     (push (point-marker) markers)
     (while (org-get-next-sibling)
       (unless (equal (point) self)
         (push (point-marker) markers)))
     (nreverse markers))))

(defun org-edna-finder/siblings-wrap ()
  (org-with-wide-buffer
   (let ((self (and (ignore-errors (org-back-to-heading t)) (point)))
         (markers))
     ;; Go from this heading to the end
     (while (org-get-next-sibling)
       (unless (equal (point) self)
         (push (point-marker) markers)))
     ;; Go to the first heading
     (org-up-heading-safe)
     (org-goto-first-child)
     (while (not (equal (point) self))
       (push (point-marker) markers)
       (org-get-next-sibling))
     (nreverse markers))))

(defun org-edna-finder/rest-of-siblings ()
  (org-with-wide-buffer
   (let ((self (and (ignore-errors (org-back-to-heading t)) (point)))
         (markers))
     ;; Go from this heading to the end
     (while (org-get-next-sibling)
       (unless (equal (point) self)
         (push (point-marker) markers)))
     (nreverse markers))))

(defun org-edna-finder/next-sibling ()
  (org-with-wide-buffer
   (and (org-get-next-sibling)
        (list (point-marker)))))

(defun org-edna-finder/previous-sibling ()
  (org-with-wide-buffer
   (and (org-get-last-sibling)
        (list (point-marker)))))

(defun org-edna-finder/first-child ()
  (org-with-wide-buffer
   (and (org-goto-first-child)
        (list (point-marker)))))

(defun org-edna-finder/children ()
  (org-with-wide-buffer
   (let ((markers))
     (org-goto-first-child)
     (push (point-marker) markers)
     (while (org-get-next-sibling)
       (push (point-marker) markers))
     (nreverse markers))))

(defun org-edna-finder/parent ()
  (org-with-wide-buffer
   (and (org-up-heading-safe)
        (list (point-marker)))))

(defun org-edna-finder/descendants ()
  (org-with-wide-buffer
   (org-map-entries
    (lambda nil (point-marker))
    nil 'tree)))

(defun org-edna-finder/ancestors ()
  (org-with-wide-buffer
   (let ((markers))
     (while (org-up-heading-safe)
       (push (point-marker) markers))
     (nreverse markers))))

(defun org-edna-finder/olp (file path)
  (let ((marker (org-find-olp (cons file (split-string-and-unquote path "/")))))
    (when (markerp marker)
      (list marker))))

  ;; TODO: Clean up the buffer when it's finished

(defun org-edna-finder/file (file)
  ;; If there isn't a buffer visiting file, then there's no point in having a
  ;; marker to the start of the file.
  (with-current-buffer (find-file-noselect file)
    (list (point-min-marker))))

(defun org-edna-finder/org-file (file)
  "Finds FILE in `org-directory'."
  (with-current-buffer (find-file-noselect (expand-file-name file org-directory))
    (list (point-min-marker))))

(defun org-edna-finder/chain-find (&rest options)
  ;; sortfun - function to use to sort elements
  ;; filterufn - Function to use to filter elements
  ;; Both should handle positioning point
  (let (targets sortfun filterfun)
    (dolist (opt options)
      (pcase opt
        ('from-top
         (setq targets (org-edna-finder/siblings)))
        ('from-bottom
         (setq targets (seq-reverse (org-edna-finder/siblings))))
        ('from-current
         (setq targets (org-edna-finder/siblings-wrap)))
        ('no-wrap
         (setq targets (org-edna-finder/rest-of-siblings)))
        ('todo-only
         (setq filterfun
               (lambda (target)
                 (org-entry-get target "TODO"))))
        ('todo-and-done-only
         (setq filterfun
               (lambda (target)
                 (member (org-entry-get target "TODO") org-done-keywords))))
        ('priority-up
         (setq sortfun
               (lambda (lhs rhs)
                 (let ((priority-lhs (org-entry-get lhs "PRIORITY"))
                       (priority-rhs (org-entry-get rhs "PRIORITY")))
                   (not (string-lessp priority-lhs priority-rhs))))))
        ('priority-down
         (setq sortfun
               (lambda (lhs rhs)
                 (let ((priority-lhs (org-entry-get lhs "PRIORITY"))
                       (priority-rhs (org-entry-get rhs "PRIORITY")))
                   (string-lessp priority-lhs priority-rhs)))))
        ('effort-up
         (setq sortfun
               (lambda (lhs rhs)
                 (let ((effort-lhs (org-duration-to-minutes (org-entry-get lhs "EFFORT")))
                       (effort-rhs (org-duration-to-minutes (org-entry-get rhs "EFFORT"))))
                   (not (< effort-lhs effort-rhs))))))
        ('effort-down
         (setq sortfun
               (lambda (lhs rhs)
                 (let ((effort-lhs (org-duration-to-minutes (org-entry-get lhs "EFFORT")))
                       (effort-rhs (org-duration-to-minutes (org-entry-get rhs "EFFORT"))))
                   (< effort-lhs effort-rhs)))))))
    (when (and targets sortfun)
      (setq targets (seq-sort sortfun targets)))
    (when (and targets filterfun)
      (setq targets (seq-filter filterfun targets)))
    (when targets
      (list (seq-elt 0 targets)))))



;; Set TODO state
(defun org-edna-action/todo (last-entry new-state)
  (ignore last-entry)
  (org-todo (if (stringp new-state) new-state (symbol-name new-state))))

;; Set planning info

(defun org-edna--mod-timestamp (time-stamp n what)
  (with-temp-buffer
    (insert time-stamp)
    (goto-char (point-min))
    (org-timestamp-change n what)
    (buffer-string)))

(defun org-edna--get-planning-info (what)
  (org-entry-get nil (if (eq what 'scheduled) "SCHEDULED" "DEADLINE")))

(defun org-edna--handle-planning (type last-entry args)
  ;; Need case-fold-search enabled so org-read-date-get-relative will recognize "M"
  (let* ((case-fold-search t)
         (arg (nth 0 args))
         (last-ts (org-with-point-at last-entry (org-edna--get-planning-info type)))
         (this-ts (org-edna--get-planning-info type))
         (this-time (and this-ts (org-parse-time-string this-ts)))
         (current (org-current-time))
         (current-ts (format-time-string (org-time-stamp-format t) current))
         (type-map '(("y" . year)
                     ("m" . month)
                     ("d" . day)
                     ("h" . hour)
                     ("M" . minute))))
    (cond
     ((member arg '('rm 'remove "rm" "remove"))
      (org-add-planning-info nil nil type))
     ((member arg '('cp 'copy "cp" "copy"))
      ;; Copy old time verbatim
      (org-add-planning-info type last-ts))
     ((string-match-p "\\`[+-]" arg)
      ;; We support hours and minutes, so this must be supported separately,
      ;; since org-read-date-analyze doesn't
      ;; Starts with a + or -, so assume we're incrementing a timestamp
      (pcase-let* ((`(,n ,what-string ,def) (org-read-date-get-relative arg this-time current))
                   (ts (if def current-ts this-ts))
                   (what (cdr (assoc-string what-string type-map))))
        (org--deadline-or-schedule nil type (org-edna--mod-timestamp ts n what))))
     (t
      ;; For everything else, assume `org-read-date-analyze' can handle it
      (let* ((parsed-time (org-read-date-analyze arg this-time (decode-time this-time)))
             (final-time (apply 'encode-time parsed-time))
             (new-ts (format-time-string "%F %R" final-time)))
        (org--deadline-or-schedule nil type new-ts))))))

(defun org-edna-action/scheduled (last-entry &rest args)
  (org-edna--handle-planning 'scheduled last-entry args))

(defun org-edna-action/deadline (last-entry &rest args)
  (org-edna--handle-planning 'deadline last-entry args))

(defun org-edna-action/tag (last-entry tags)
  (ignore last-entry)
  (org-set-tags-to tags))

(defun org-edna-action/set-property (last-entry property value)
  (ignore last-entry)
  (org-entry-put nil property value))

(defun org-edna-action/clock-in (last-entry)
  (ignore last-entry)
  (org-clock-in))

(defun org-edna-action/clock-out (last-entry)
  (ignore last-entry)
  (org-clock-out))

(defun org-edna-action/set-priority (last-entry priority-action)
  "PRIORITY-ACTION is passed straight to `org-priority'."
  (ignore last-entry)
  (org-priority (if (stringp priority-action)
                    (string-to-char priority-action)
                  priority-action)))

;; TODO I will likely want to check the arguments
(defun org-edna-action/set-effort (last-entry value increment)
  (ignore last-entry)
  (org-set-effort value increment))

(defun org-edna-action/archive (last-entry)
  (ignore last-entry)
  (org-archive-subtree-default-with-confirmation))

(defun org-edna-action/chain (last-entry property)
  (when-let ((old-prop (org-entry-get last-entry property)))
    (org-entry-put nil property old-prop)))



;; For most conditions, we return true if condition is true and neg is false, or
;; if condition is false and neg is true:

;; | cond | neg | res |
;; |------+-----+-----|
;; | t    | t   | f   |
;; | t    | f   | t   |
;; | f    | t   | t   |
;; | f    | f   | f   |

;; This means that we want to take the exclusive-or of condition and neg.

(defun org-edna-condition/done (neg)
  (when-let ((condition
              (if neg
                  (member (org-entry-get nil "TODO") org-not-done-keywords)
                (member (org-entry-get nil "TODO") org-done-keywords))))
    (org-get-heading)))

(defun org-edna-condition/todo-state (neg state)
  (let ((condition (string-equal (org-entry-get nil "TODO") state)))
    (when (org-xor condition neg)
      (org-get-heading))))

;; Block if there are headings
(defun org-edna-condition/headings (neg)
  (let ((condition (not (seq-empty-p (org-map-entries (lambda nil t))))))
    (when (org-xor condition neg)
      (buffer-name))))

(defun org-edna-condition/variable-set (neg var val)
  (let ((condition (string-equal (symbol-value (intern var)) (read val))))
    (when (org-xor condition neg)
      (format "%s %s= %s" var (or neg "=") val))))

(defun org-edna-condition/has-property (neg prop val)
  (let ((condition (string-equal (org-entry-get nil prop) val)))
    (when (org-xor condition neg)
      (org-get-heading))))

(defun org-edna-condition/re-search (neg match)
  (let ((condition (re-search-forward match nil t)))
    (when (org-xor condition neg)
      (format "Found %s in %s" match (buffer-name)))))



(defun org-edna-handle-consideration (consideration blocks)
  (let ((first-block (seq-find #'identity blocks))
        (total-blocks (seq-length blocks)))
    (pcase consideration
      ('all
       ;; All of them must be fulfilled, so find the first one that isn't.
       first-block)
      ((pred integerp)
       ;; A fixed number of them must be fulfilled, so check how many aren't.
       (let* ((fulfilled (seq-count #'not blocks)))
         (if (>= fulfilled consideration)
             nil
           first-block)))
      ((pred floatp)
       ;; A certain percentage of them must be fulfilled
       (let* ((fulfilled (seq-count #'not blocks)))
         (if (>= (/ (float fulfilled) (float total-blocks)) consideration)
             nil
           first-block))))))

(provide 'org-edna)

;;; org-edna.el ends here
