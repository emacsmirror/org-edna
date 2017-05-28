;;; org-edna.el --- Extensible Dependencies 'N' Actions -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Free Software Foundation, Inc.

;; Author: Ian Dunn <dunni@gnu.org>
;; Maintainer: Ian Dunn <dunni@gnu.org>
;; Keywords: convenience, text, org
;; URL: https://savannah.nongnu.org/projects/org-edna-el/
;; Package-Requires: ((emacs "25.1") (seq "2.19") (org "9.0.5"))
;; Version: 1.0alpha1

;; This file is part of GNU Emacs.

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

;; Edna provides an extensible means of specifying conditions which must be
;; fulfilled before a task can be completed and actions to take once it is.

;; Org Edna runs when either the BLOCKER or TRIGGER properties are set on a
;; headline, and when it is changing from a TODO state to a DONE state.

;;; History:

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

(defcustom org-edna-prompt-for-archive t
  "Whether Edna should prompt before archiving a target."
  :group 'org-edna
  :type 'boolean)

(defmacro org-edna--syntax-error (msg form pos)
  "Signal an Edna syntax error.

MSG will be reported to the user and should describe the error.
FORM is the form that generated the error.
POS is the position in FORM at which the error occurred."
  `(signal 'invalid-read-syntax (list :msg ,msg :form ,form :pos ,pos)))

(defun org-edna--print-syntax-error (error-plist)
  "Prints the syntax error from ERROR-PLIST."
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
  "Parse Edna form FORM starting at position START."
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

(defun org-edna--function-for-key (key)
  "Determine the Edna function for KEY.

KEY should be a symbol, the keyword for which to find the Edna
function."
  (cond
   ;; Just return nil if it's not a symbol; `org-edna-process-form' will handle
   ;; the rest
   ((or (not key)
        (not (symbolp key))))
   ((eq key 'consideration)
    ;; Function is ignored here
    (cons 'consideration 'identity))
   ((string-suffix-p "!" (symbol-name key))
    ;; Action
    (let ((func-sym (intern (format "org-edna-action/%s" key))))
      (when (fboundp func-sym)
        (cons 'action func-sym))))
   ((string-suffix-p "?" (symbol-name key))
    ;; Condition
    (let ((func-sym (intern (format "org-edna-condition/%s" key))))
      (when (fboundp func-sym)
        (cons 'condition func-sym))))
   (t
    ;; Everything else is a finder
    (let ((func-sym (intern (format "org-edna-finder/%s" key))))
      (when (fboundp func-sym)
        (cons 'finder func-sym))))))

(defun org-edna--handle-condition (func mod args targets consideration)
  "Handle a condition."
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
  "Process FORM.

ACTION-OR-CONDITION is a symbol, either 'action or 'condition,
indicating whether FORM accepts actions or conditions."
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
            (org-edna--handle-condition 'org-edna-condition/done?
                                        t nil targets consideration)))
    ;; Only blockers care about the return value, and this will be non-nil if
    ;; the entry should be blocked.
    (setq org-block-entry-blocking blocking-entry)
    (not blocking-entry)))



(defmacro org-edna-run (change-plist &rest body)
  "Run a TODO state change.

The state information is held in CHANGE-PLIST.  If the TODO state
is changing from a TODO state to a DONE state, run BODY."
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
            (if (eq (car err) 'invalid-read-syntax)
                (org-edna--print-syntax-error (cdr err))
              (message "Edna Error: %s" (error-message-string err)))
            (setq org-block-entry-blocking (org-get-heading))
            ;; Block
            nil))
       ;; Return t for the blocker to let the calling function know that there
       ;; is no block here.
       t)))

(defun org-edna-trigger-function (change-plist)
  "Trigger function work-horse.

See `org-edna-run' for CHANGE-PLIST explanation.

This shouldn't be run from outside of `org-trigger-hook'."
  (org-edna-run change-plist
    (when-let ((form (org-entry-get pos "TRIGGER" org-edna-use-inheritance)))
      (org-edna-process-form form 'action))))

(defun org-edna-blocker-function (change-plist)
  "Blocker function work-horse.

See `org-edna-run' for CHANGE-PLIST explanation.

This shouldn't be run from outside of `org-blocker-hook'."
  (org-edna-run change-plist
    (if-let ((form (org-entry-get pos "BLOCKER" org-edna-use-inheritance)))
        (org-edna-process-form form 'condition)
      t)))

;;;###autoload
(defun org-edna-load ()
  "Setup the hooks necessary for Org Edna to run.

This means adding to `org-trigger-hook' and `org-blocker-hook'."
  (interactive)
  (add-hook 'org-trigger-hook 'org-edna-trigger-function)
  (add-hook 'org-blocker-hook 'org-edna-blocker-function))

;;;###autoload
(defun org-edna-unload ()
  "Unload Org Edna.

Remove Edna's workers from `org-trigger-hook' and
`org-blocker-hook'."
  (interactive)
  (remove-hook 'org-trigger-hook 'org-edna-trigger-function)
  (remove-hook 'org-blocker-hook 'org-edna-blocker-function))



;; Tag Finder
(defun org-edna-finder/match (match-spec &optional scope skip)
  "Find entries using Org matching.

MATCH-SPEC may be any valid match string; it is passed straight
into `org-map-entries'.

SCOPE and SKIP are their counterparts in `org-map-entries'.
SCOPE defaults to agenda, and SKIP defaults to nil.

* TODO Test
  :PROPERTIES:
  :BLOCKER:  match(\"test&mine\" agenda)
  :END:

\"Test\" will block until all entries tagged \"test\" and
\"mine\" in the agenda files are marked DONE."
  (setq scope (or scope 'agenda))
  (org-map-entries
   ;; Find all entries in the agenda files that match the given tag.
   (lambda nil (point-marker))
   match-spec scope skip))

;; ID finder
(defun org-edna-finder/ids (&rest ids)
  "Find a list of headlines with given IDs.

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
     (unless (equal (point) self)
       (push (point-marker) markers))
     (while (org-get-next-sibling)
       (unless (equal (point) self)
         (push (point-marker) markers)))
     (nreverse markers))))

(defun org-edna-finder/siblings-wrap ()
  (org-with-wide-buffer
   (let ((self (and (ignore-errors (org-back-to-heading t)) (point)))
         (markers))
     ;; Go from this heading to the end
     (save-excursion
       (while (org-get-next-sibling)
         (unless (equal (point) self)
           (push (point-marker) markers))))
     ;; Go to the first heading
     (org-up-heading-safe)
     (org-goto-first-child)
     (save-excursion
       (while (not (equal (point) self))
         (push (point-marker) markers)
         (org-get-next-sibling)))
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
  "Find a list of ancestors.

Example:

* TODO Heading 1
** TODO Heading 2
*** TODO Heading 3
**** TODO Heading 4
***** TODO Heading 5
      :PROPERTIES:
      :BLOCKER:  ancestors
      :END:

In the above example, Heading 5 will be blocked until Heading 1,
Heading 3, and Heading 4 are marked DONE, while Heading 2 is
ignored."
  (org-with-wide-buffer
   (let ((markers))
     (while (org-up-heading-safe)
       (push (point-marker) markers))
     (nreverse markers))))

(defun org-edna-finder/olp (file olp)
  "Find a headline by its outline path.

Finds the heading given by OLP in FILE.  Both arguments are strings.

* TODO Test
  :PROPERTIES:
  :BLOCKER:  olp(\"test.org\" \"path/to/heading\")
  :END:

Test will block if the heading \"path/to/heading\" in
\"test.org\" is not DONE."
  (let ((marker (org-find-olp (cons file (split-string-and-unquote olp "/")))))
    (when (markerp marker)
      (list marker))))

;; TODO: Clean up the buffer when it's finished

(defun org-edna-finder/file (file)
  "Find a file by name.

The `file' finder finds a single file, specified as a string.
The returned target will be the minimum point in the file.

* TODO Test
  :PROPERTIES:
  :BLOCKER:  file(\"~/myfile.org\") headings?
  :END:

Here, \"Test\" will block until myfile.org is clear of headlines.

Note that with the default condition, `file' won't work."
  ;; If there isn't a buffer visiting file, then there's no point in having a
  ;; marker to the start of the file, so use `find-file-noselect'.
  (with-current-buffer (find-file-noselect file)
    (list (point-min-marker))))

(defun org-edna-finder/org-file (file)
  "Find a file in `org-directory'.

A special form of `file', `org-file' will find FILE (a string) in
`org-directory'.

* TODO Test
  :PROPERTIES:
  :BLOCKER:  org-file(\"test.org\")
  :END:

Note that the file still requires an extension."
  (with-current-buffer (find-file-noselect (expand-file-name file org-directory))
    (list (point-min-marker))))

(defun org-edna-finder/chain-find (&rest options)
  "Find a target as org-depend does.

Identical to the chain argument in org-depend, chain-find selects its single
target using the following method:

1. Creates a list of possible targets
2. Filters the targets from Step 1
3. Sorts the targets from Step 2

After this is finished, chain-find selects the first target in the list and
returns it.

One option from each of the following three categories may be used; if more than
one is specified, the last will be used.

*Selection*

- from-top:     Select siblings of the current headline, starting at the top
- from-bottom:  As above, but from the bottom
- from-current: Selects siblings, starting from the headline (wraps)
- no-wrap:      As above, but without wrapping

*Filtering*

- todo-only:          Select only targets with TODO state set that isn't a DONE state
- todo-and-done-only: Select all targets with a TODO state set

*Sorting*

- priority-up:   Sort by priority, highest first
- priority-down: Same, but lowest first
- effort-up:     Sort by effort, highest first
- effort-down:   Sort by effort, lowest first"
  ;; sortfun - function to use to sort elements
  ;; filterfun - Function to use to filter elements
  ;; Both should handle positioning point
  (let (targets
        sortfun
        ;; From org-depend.el:
        ;; (and (not todo-and-done-only)
        ;;      (member (second item) org-done-keywords))
        (filterfun (lambda (target)
                     (member (org-entry-get target "TODO") org-done-keywords))))
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
         ;; Remove any entry without a TODO keyword, or with a DONE keyword
         (setq filterfun
               (lambda (target)
                 (let ((kwd (org-entry-get target "TODO")))
                   (or (not kwd)
                       (member kwd org-done-keywords))))))
        ('todo-and-done-only
         ;; Remove any entry without a TODO keyword
         (setq filterfun
               (lambda (target)
                 (not (org-entry-get target "TODO")))))
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
      (setq targets (seq-remove filterfun targets)))
    (when targets
      (list (seq-elt 0 targets)))))



;; Set TODO state
(defun org-edna-action/todo! (last-entry new-state)
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
     ((member arg '(rm remove "rm" "remove"))
      (org-add-planning-info nil nil type))
     ((member arg '(cp copy "cp" "copy"))
      (unless last-ts
        (error "Tried to copy but last entry doesn't have a timestamp"))
      ;; Copy old time verbatim
      (org-add-planning-info type last-ts))
     ((string-match-p "\\`[+-]" arg)
      ;; Starts with a + or -, so assume we're incrementing a timestamp
      ;; We support hours and minutes, so this must be supported separately,
      ;; since org-read-date-analyze doesn't
      (pcase-let* ((`(,n ,what-string ,def) (org-read-date-get-relative arg this-time current))
                   (ts (if def current-ts this-ts))
                   (what (cdr (assoc-string what-string type-map))))
        (org--deadline-or-schedule nil type (org-edna--mod-timestamp ts n what))))
     (t
      ;; For everything else, assume `org-read-date-analyze' can handle it

      ;; The third argument to `org-read-date-analyze' specifies the defaults to
      ;; use if that time component isn't specified.  Since there's no way to
      ;; tell if a time was specified, tell `org-read-date-analyze' to use nil
      ;; if no time is found.
      (let* ((parsed-time (org-read-date-analyze arg this-time '(nil nil nil nil nil nil)))
             (have-time (nth 2 parsed-time))
             (final-time (apply 'encode-time (mapcar (lambda (e) (or e 0)) parsed-time)))
             (new-ts (format-time-string (if have-time "%F %R" "%F") final-time)))
        (org--deadline-or-schedule nil type new-ts))))))

(defun org-edna-action/scheduled! (last-entry &rest args)
  (org-edna--handle-planning 'scheduled last-entry args))

(defun org-edna-action/deadline! (last-entry &rest args)
  (org-edna--handle-planning 'deadline last-entry args))

(defun org-edna-action/tag! (last-entry tags)
  (ignore last-entry)
  (org-set-tags-to tags))

(defun org-edna-action/set-property! (last-entry property value)
  (ignore last-entry)
  (org-entry-put nil property value))

(defun org-edna-action/delete-property! (last-entry property)
  (ignore last-entry)
  (org-entry-delete nil property))

(defun org-edna-action/clock-in! (last-entry)
  (ignore last-entry)
  (org-clock-in))

(defun org-edna-action/clock-out! (last-entry)
  (ignore last-entry)
  (org-clock-out))

(defun org-edna-action/set-priority! (last-entry priority-action)
  "PRIORITY-ACTION is passed straight to `org-priority'."
  (ignore last-entry)
  (org-priority (if (stringp priority-action)
                    (string-to-char priority-action)
                  priority-action)))

(defun org-edna-action/set-effort! (last-entry value)
  (ignore last-entry)
  (if (eq value 'increment)
      (org-set-effort nil value)
    (org-set-effort value nil)))

(defun org-edna-action/archive! (last-entry)
  (ignore last-entry)
  (if org-edna-prompt-for-archive
      (org-archive-subtree-default-with-confirmation)
    (org-archive-subtree-default)))

(defun org-edna-action/chain! (last-entry property)
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

(defun org-edna-condition/done? (neg)
  (when-let ((condition
              (if neg
                  (member (org-entry-get nil "TODO") org-not-done-keywords)
                (member (org-entry-get nil "TODO") org-done-keywords))))
    (org-get-heading)))

(defun org-edna-condition/todo-state? (neg state)
  (let ((condition (string-equal (org-entry-get nil "TODO") state)))
    (when (org-xor condition neg)
      (org-get-heading))))

;; Block if there are headings
(defun org-edna-condition/headings? (neg)
  (let ((condition (not (seq-empty-p (org-map-entries (lambda nil t))))))
    (when (org-xor condition neg)
      (buffer-name))))

(defun org-edna-condition/variable-set? (neg var val)
  (let ((condition (equal (symbol-value var) val)))
    (when (org-xor condition neg)
      (format "%s %s= %s" var (if neg "!" "=") val))))

(defun org-edna-condition/has-property? (neg prop val)
  (let ((condition (string-equal (org-entry-get nil prop) val)))
    (when (org-xor condition neg)
      (org-get-heading))))

(defun org-edna-condition/re-search? (neg match)
  (let ((condition (re-search-forward match nil t)))
    (when (org-xor condition neg)
      (format "%s %s in %s" (if neg "Did Not Find" "Found") match (buffer-name)))))



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



(declare-function lm-report-bug "lisp-mnt" (topic))

(defun org-edna-submit-bug-report (topic)
  (interactive "sTopic: ")
  (require 'lisp-mnt)
  (let* ((src-file (locate-library "org-edna.el" t))
         (src-buf-live (find-buffer-visiting src-file))
         (src-buf (find-file-noselect src-file)))
    (with-current-buffer src-buf
      (lm-report-bug topic))
    ;; Kill the buffer if it wasn't live
    (unless src-buf-live
      (kill-buffer src-buf))))

(provide 'org-edna)

;;; org-edna.el ends here
