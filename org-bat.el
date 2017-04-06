;;; org-bat.el --- Extendable Blockers and Triggers -*- lexical-binding: t; -*-

;; Author: Ian Dunn

;;; Commentary:

;; Finders:

;; - self              Self
;; - siblings          Find all siblings of the current entry
;; - next-sibling      Find the next sibling on the same level as the entry
;; - previous-sibling  Find the previous sibling on the same level
;; - first-child       Find the first child of the current entry
;; - children          Find all children of the current entry
;; - parent            Find direct parent of current entry
;; - descendants       Find all descendants of the current entry
;; - ancestors         Find all ancestors of the current entry

;;; Code:

(require 'org)
(require 'subr-x)

(defun org-bat-parse-form (form)
  "Form should be KEY(ARGS)."
  (when (string-match "^\\([!]\\)?\\([a-zA-Z-]+\\)\\(?:(\\([^)]+\\))\\)?" form)
    (list (intern (match-string 2 form))
          (when-let (args (match-string 3 form))
            (save-match-data (split-string args ",")))
          (match-string 1 form)
          (match-end 0))))

(defconst org-bat--types
  '(finder action condition)
  "Types recognized by org-bat.")

(defun org-bat--function-for-key (key)
  (cond
    ((eq key 'consideration)
     ;; Function is ignored here
     (cons 'consideration 'identity))
    (key
     (when-let ((func-format (format "org-bat-%%s/%s" key))
                (new-sym
                 ;; Find the first bound function
                 (seq-find
                  (lambda (sym) (fboundp (intern (format func-format sym))))
                  org-bat--types)))
       (cons new-sym (intern (format func-format new-sym)))))))

(defun org-bat-process-form (form action-or-condition)
  (let ((targets)
        (blocking-entry)
        (form-string form)
        (consideration 'all)
        (state nil) ;; Type of operation
        ;; Keep track of the current headline
        (last-entry (point-marker)))
    (while (not (string-empty-p form-string))
      (pcase-let* ((`(,key ,args ,mod ,new-pos) (org-bat-parse-form form-string))
                   (`(,type . ,func) (org-bat--function-for-key key)))
        (unless (and key type func)
          (user-error "Unrecognized form '%s'" form-string))
        (setq form-string (string-trim-left (substring form-string new-pos)))
        (pcase type
          ('finder
           (unless (eq state 'finder)
             ;; We just executed some actions, so reset the entries.
             (setq targets nil))
           (setq state 'finder)
           (let ((markers (apply func args)))
             ;; Ensure targets is a list so append continues to work
             (setq targets (append targets (if (consp markers) markers (list markers))))))
          ('action
           (unless (eq action-or-condition 'action)
             (user-error "Actions aren't allowed in this context."))
           (unless targets
             (user-error "Action specified without targets"))
           (setq state 'action)
           (dolist (target targets)
             (org-with-point-at target
               (apply func last-entry args))))
          ('condition
           (unless (eq action-or-condition 'condition)
             (user-error "Conditions aren't allowed in this context"))
           (unless targets
             (user-error "Condition specified without targets"))
           (setq state 'condition)
           (unless blocking-entry ;; We're already blocking
             ;; Check the condition at each target
             (when-let ((blocks
                         (mapcar
                          (lambda (entry-marker)
                            (org-with-point-at entry-marker
                              (apply func mod args)))
                          targets)))
               ;; Apply consideration
               (setq blocking-entry
                     (org-bat-handle-consideration consideration blocks)))))
          ('consideration
           ;; Consideration must be at the start of the targets, so clear out
           ;; any old targets.
           (setq targets nil)
           ;; The actual consideration will be the only argument
           (setq consideration (org-bat-transform-consideration (nth 0 args)))))))
    ;; We exhausted the input string, but didn't find a condition when we were
    ;; expecting one.
    (when (and (eq action-or-condition 'condition) ;; Looking for conditions
               (eq state 'finder)                  ;; but haven't found any
               (not blocking-entry))                 ;; ever
      (when-let ((blocks
                  (mapcar
                   (lambda (entry-marker)
                     (org-with-point-at entry-marker
                       (org-bat-condition/done t)))
                   targets)))
        ;; Apply consideration
        (setq blocking-entry
              (org-bat-handle-consideration consideration blocks))))
    ;; Only blockers care about the return value, and this will be non-nil if
    ;; the entry should be blocked.
    (setq org-block-entry-blocking blocking-entry)
    (not blocking-entry)))



(defmacro org-bat-run (change-plist &rest body)
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
         ,@body
       ;; Return t for the blocker to let the calling function know that there
       ;; is no block here.
       t)))

(defun org-bat-trigger-function (change-plist)
  (org-bat-run change-plist
    (when-let ((form (org-entry-get pos "TRIGGER")))
      (org-bat-process-form form 'action))))

(defun org-bat-blocker-function (change-plist)
  (org-bat-run change-plist
    (if-let ((form (org-entry-get pos "BLOCKER")))
        (org-bat-process-form form 'condition)
      t)))

;;;###autoload
(defun org-bat-load ()
  (interactive)
  (add-hook 'org-trigger-hook 'org-bat-trigger-function)
  (add-hook 'org-blocker-hook 'org-bat-blocker-function))

;;;###autoload
(defun org-bat-unload ()
  (interactive)
  (remove-hook 'org-trigger-hook 'org-bat-trigger-function)
  (remove-hook 'org-blocker-hook 'org-bat-blocker-function))



;; Tag Finder
(defun org-bat-finder/match (match-spec &optional scope skip)
  "Find entries with match-spec MATCH-SPEC.

MATCH-SPEC may be any valid match string; it is passed straight
into `org-map-entries', with 'agenda as the scope.

SCOPE and SKIP are their counterparts in `org-map-entries', but
as strings.

SCOPE defaults to \"agenda\", and SKIP defaults to nil."
  (setq scope (if scope (intern scope) 'agenda))
  (when skip (setq skip (intern skip)))
  (org-map-entries
   ;; Find all entries in the agenda files that match the given tag.
   (lambda nil (point-marker))
   match-spec scope skip))

;; ID finder
(defun org-bat-finder/ids (&rest ids)
  "Find entries with IDs in IDS.

IDS are all UUIDs as understood by `org-id-find'."
  (mapcar (lambda (id) (org-id-find id 'marker)) ids))

(defun org-bat-finder/self ()
  (point-marker))

(defun org-bat-finder/siblings ()
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

(defun org-bat-finder/next-sibling ()
  (org-with-wide-buffer
   (and (org-get-next-sibling)
        (point-marker))))

(defun org-bat-finder/previous-sibling ()
  (org-with-wide-buffer
   (and (org-get-last-sibling)
        (point-marker))))

(defun org-bat-finder/first-child ()
  (org-with-wide-buffer
   (and (org-goto-first-child)
        (point-marker))))

(defun org-bat-finder/children ()
  (org-with-wide-buffer
   (let ((markers))
     (org-goto-first-child)
     (push (point-marker) markers)
     (while (org-get-next-sibling)
       (push (point-marker) markers))
     (nreverse markers))))

(defun org-bat-finder/parent ()
  (org-with-wide-buffer
   (and (org-up-heading-safe)
        (point-marker))))

(defun org-bat-finder/descendants ()
  (org-with-wide-buffer
   (org-map-entries
    (lambda nil (point-marker))
    nil 'tree)))

(defun org-bat-finder/ancestors ()
  (org-with-wide-buffer
   (let ((markers))
     (while (org-up-heading-safe)
       (push (point-marker) markers))
     (nreverse markers))))

(defun org-bat-finder/olp (file path)
  (org-find-olp (cons file (split-string-and-unquote path "/"))))

(defun org-bat-finder/file (file)
  ;; If there isn't a buffer visiting file, then there's no point in having a
  ;; marker to the start of the file.
  (with-current-buffer (find-file-noselect file)
    (point-min-marker)))

(defun org-bat-finder/org-file (file)
  "Finds FILE in `org-directory'."
  (with-current-buffer (find-file-noselect (expand-file-name file org-directory))
    (point-min-marker)))



;; Set TODO state
(defun org-bat-action/todo (last-entry new-state)
  (ignore last-entry)
  (org-todo new-state))

;; Set planning info

(defun org-bat--mod-timestamp (time-stamp n what)
  (with-temp-buffer
    (insert time-stamp)
    (goto-char (point-min))
    (org-timestamp-change n what)
    (buffer-string)))

(defun org-bat--get-planning-info (what)
  (org-entry-get nil (if (eq what 'scheduled) "SCHEDULED" "DEADLINE")))

(defun org-bat--handle-planning (type last-entry args)
  (let* ((case-fold-search t)
         (arg (nth 0 args))
         (last-ts (org-with-point-at last-entry (org-bat--get-planning-info type)))
         (this-ts (org-bat--get-planning-info type))
         (this-time (and this-ts (org-parse-time-string this-ts)))
         (current (org-current-time))
         (current-ts (format-time-string (org-time-stamp-format t) current))
         (type-map '(("y" . year)
                     ("m" . month)
                     ("d" . day)
                     ("h" . hour)
                     ("M" . minute))))
    (cond
     ((member arg '("rm" "remove"))
      (org-add-planning-info nil nil type))
     ((member arg '("cp" "copy"))
      ;; Copy old time verbatim
      (org-add-planning-info type last-ts))
     ((string-match-p "\\`[+-]" arg)
      ;; We support hours and minutes, so this must be supported separately,
      ;; since org-read-date-analyze doesn't
      ;; Starts with a + or -, so assume we're incrementing a timestamp
      (pcase-let* ((`(,n ,what-string ,def) (org-read-date-get-relative arg this-time current))
                   (ts (if def current-ts this-ts))
                   (what (cdr (assoc-string what-string type-map))))
        (org--deadline-or-schedule nil type (org-bat--mod-timestamp ts n what))))
     (t
      ;; For everything else, assume `org-read-date-analyze' can handle it
      (let* ((parsed-time (org-read-date-analyze arg this-time (decode-time this-time)))
             (final-time (apply 'encode-time parsed-time))
             (new-ts (format-time-string "%F %R" final-time)))
        (org--deadline-or-schedule nil type new-ts))))))

(defun org-bat-action/scheduled (last-entry &rest args)
  (org-bat--handle-planning 'scheduled last-entry args))

(defun org-bat-action/deadline (last-entry &rest args)
  (org-bat--handle-planning 'deadline last-entry args))

(defun org-bat-action/tag (last-entry tags)
  (ignore last-entry)
  (org-set-tags-to tags))

(defun org-bat-action/set-property (last-entry property value)
  (ignore last-entry)
  (org-entry-put nil property value))

(defun org-bat-action/clock-in (last-entry)
  (ignore last-entry)
  (org-clock-in))

(defun org-bat-action/clock-out (last-entry)
  (ignore last-entry)
  (org-clock-out))

(defun org-bat-action/set-priority (last-entry priority-action)
  "PRIORITY-ACTION is passed straight to `org-priority'."
  (ignore last-entry)
  (org-priority (if (stringp priority-action)
                    (string-to-char priority-action)
                  priority-action)))

;; TODO I will likely want to check the arguments
(defun org-bat-action/set-effort (last-entry value increment)
  (ignore last-entry)
  (org-set-effort value increment))

(defun org-bat-action/archive (last-entry)
  (ignore last-entry)
  (org-archive-subtree-default-with-confirmation))



;; For most conditions, we return true if condition is true and neg is false, or
;; if condition is false and neg is true:

;; | cond | neg | res |
;; |------+-----+-----|
;; | t    | t   | f   |
;; | t    | f   | t   |
;; | f    | t   | t   |
;; | f    | f   | f   |

;; This means that we want to take the exclusive-or of condition and neg.

(defsubst org-bat--xor (lhs rhs)
  (or (and lhs (not rhs))
      (and (not lhs) rhs)))

(defun org-bat-condition/done (neg)
  (when-let ((condition
              (if neg
                  (member (org-entry-get nil "TODO") org-not-done-keywords)
                (member (org-entry-get nil "TODO") org-done-keywords))))
    (org-get-heading)))

(defun org-bat-condition/todo-state (neg state)
  (let ((condition (string-equal (org-entry-get nil "TODO") state)))
    (when (org-bat--xor condition neg)
      (org-get-heading))))

;; Block if there are headings
(defun org-bat-condition/headings (neg)
  (let ((condition (seq-empty-p (org-map-entries (lambda nil t)))))
    (when (org-bat--xor condition neg)
      (buffer-name))))

(defun org-bat-condition/variable-set (neg var val)
  (let ((condition (string-equal (symbol-value (intern var)) (read val))))
    (when (org-bat--xor condition neg)
      (format "%s %s= %s" var (or neg "=") val))))



(defun org-bat-transform-consideration (consideration)
  (pcase consideration
    ;; Leave symbols alone
    ('"all" (intern consideration))
    ;; Change strings into numbers
    ((pred stringp)
     (string-to-number consideration))
    (_
     (user-error "Unrecognized consideration '%s'" consideration))))

(defun org-bat-handle-consideration (consideration blocks)
  (let ((first-block (seq-find #'identity blocks))
        (total-blocks (seq-length blocks)))
    (pcase consideration
      ('all
       ;; All of them must be fulfilled, so find the first one that isn't.
       first-block)
      ((pred integerp)
       ;; A fixed number of them must be fulfilled, so check how many aren't.
       (let* ((unfulfilled (seq-count #'identity blocks))
              (fulfilled   (- total-blocks unfulfilled)))
         (if (> fulfilled consideration)
             nil
           first-block)))
      ((pred floatp)
       ;; A certain percentage of them must be fulfilled
       (let* ((unfulfilled (seq-count #'identity blocks))
              (fulfilled   (- total-blocks unfulfilled)))
         (if (> (/ fulfilled total-blocks) consideration)
             nil
           first-block))))))

(provide 'org-bat)

;;; org-bat.el ends here
