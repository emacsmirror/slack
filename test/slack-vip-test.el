;;; slack-vip-test.el --- tests for VIP/priority user support -*- lexical-binding: t; -*-

(require 'ert)
(require 'slack-team)
(require 'slack-user)
(require 'slack-vip)

(defun slack-vip-test--user (id &optional name priority)
  "Build a user plist with ID, NAME and optional VIP field value."
  (append (list :id id
                :name (or name id)
                :profile (list :display_name_normalized (or name id)
                               :real_name_normalized (or name id)))
          (when priority (list slack-user-vip-field priority))))

(defmacro slack-vip-test-setup (&rest body)
  "Run BODY with a team and a few cached users."
  (declare (indent 0) (debug t))
  `(let* ((u1 (slack-vip-test--user "U11111" "Alice" t))
          (u2 (slack-vip-test--user "U22222" "Bob" nil))
          (u3 (slack-vip-test--user "U33333" "Carol"))
          (users (let ((h (make-hash-table :test 'equal)))
                   (puthash "U11111" u1 h)
                   (puthash "U22222" u2 h)
                   (puthash "U33333" u3 h)
                   h))
          (team (make-instance 'slack-team
                               :self-id "U00000"
                               :id "T99999"
                               :users users)))
     ;; `slack-team-set-users' would sync the VIP field into the set; here we
     ;; install the users directly so the set starts empty and tests can
     ;; exercise both the field path and the set path independently.
     ,@body))

(ert-deftest slack-test-vip-p-via-field ()
  "A user whose plist VIP field is truthy is VIP even without set membership."
  (slack-vip-test-setup
    (should (slack-user-vip-p (slack-vip-test--user "U9" "Zoe" t) team))
    (should-not (slack-user-vip-p (slack-vip-test--user "U9" "Zoe" nil) team))
    (should-not (slack-user-vip-p (slack-vip-test--user "U9" "Zoe") team))))

(ert-deftest slack-test-vip-p-via-set ()
  "A user id in the priority set is VIP."
  (slack-vip-test-setup
    (slack-vip--set team "U22222" t)
    (should (slack-user-vip-p-id "U22222" team))
    (should (slack-user-vip-p "U22222" team))
    (slack-vip--set team "U22222" nil)
    (should-not (slack-user-vip-p-id "U22222" team))))

(ert-deftest slack-test-vip-sync-users ()
  "`slack-vip-sync-users' adds truthy-field users and removes nil-field ones,
leaving users without the field untouched."
  (slack-vip-test-setup
    (slack-vip-sync-users
     team
     (list (slack-vip-test--user "UA" "A" t)
           (slack-vip-test--user "UB" "B" nil)
           (slack-vip-test--user "UC" "C")))
    (should (gethash "UA" (slack-team-priority-users team)))
    (should-not (gethash "UB" (slack-team-priority-users team)))
    (should-not (gethash "UC" (slack-team-priority-users team)))))

(ert-deftest slack-test-vip-team-set-users-syncs ()
  "`slack-team-set-users' syncs the VIP field into the priority set."
  (slack-vip-test-setup
    (slack-team-set-users
     team
     (list (slack-vip-test--user "UX" "Xavier" t)
           (slack-vip-test--user "UY" "Yvonne")))
    (should (gethash "UX" (slack-team-priority-users team)))
    (should-not (gethash "UY" (slack-team-priority-users team)))))

(ert-deftest slack-test-vip-names-filter ()
  "`slack-user-vip-names' returns only VIP users."
  (slack-vip-test-setup
    ;; U11111 has the field set truthy in its cached plist.
    (let ((names (slack-user-vip-names team)))
      (should (= 1 (length names)))
      (should (equal "Alice" (caar names)))
      (should (equal "U11111" (plist-get (cdar names) :id))))))

(ert-deftest slack-test-vip-propertize-name ()
  "VIP names keep their case and get a ` [VIP]' badge with the VIP face;
non-VIP names are returned plain."
  (slack-vip-test-setup
    (slack-vip--set team "U22222" t)
    (let ((vip (slack-user-vip-propertize-name "Bob" "U22222" team))
          (plain (slack-user-vip-propertize-name "Carol" "U33333" team)))
      ;; Name keeps its original case; badge is appended.
      (should (equal "Bob [VIP]" vip))
      ;; The name part has no face; the badge has the VIP face.
      (should-not (get-text-property 0 'face vip))
      (should (eq 'slack-user-vip-face
                  (get-text-property (string-match "\\[VIP\\]" vip) 'face vip)))
      (should (equal "Carol" plain))
      (should-not (get-text-property 0 'face plain)))))

(ert-deftest slack-test-vip-add-remove ()
  "`slack-vip-add' / `slack-vip-remove' update the set (request stubbed)."
  (slack-vip-test-setup
    (cl-letf (((symbol-function #'slack-request)
              (lambda (req &rest _)
                (funcall (oref req success) :data (list :ok t)))))
      ;; Carol is not VIP initially.
      (should-not (slack-user-vip-p-id "U33333" team))
      (slack-vip-add "U33333" team)
      (should (slack-user-vip-p-id "U33333" team))
      ;; the cached user plist is annotated too.
      (should (eq t (plist-get (gethash "U33333" (oref team users))
                               slack-user-vip-field)))
      (slack-vip-remove "U33333" team)
      (should-not (slack-user-vip-p-id "U33333" team))
      (should-not (plist-get (gethash "U33333" (oref team users))
                             slack-user-vip-field)))))

(ert-deftest slack-test-vip-display-name ()
  "`slack-user-vip-display-name' appends the `[VIP]' badge to VIP users."
  (slack-vip-test-setup
    (let ((styled (slack-user-vip-display-name u1 team))
          (plain (slack-user-vip-display-name u3 team)))
      ;; u1 has :is_priority t in its plist.
      (should (equal "Alice [VIP]" styled))
      (should (eq 'slack-user-vip-face
                  (get-text-property (string-match "\\[VIP\\]" styled) 'face styled)))
      (should (equal "Carol" plain))
      (should-not (get-text-property 0 'face plain)))))

(ert-deftest slack-test-vip-remove-restricts-to-vip ()
  "`slack-user-vip-remove' builds its completion candidates from
`slack-user-vip-names', which contains only VIP users."
  (slack-vip-test-setup
    (slack-vip--set team "U22222" t)        ; Bob is VIP
    ;; Alice (U11111) is VIP via its :is_priority plist field.
    ;; Carol (U33333) is not VIP and must not be offered.
    (let ((candidates (mapcar #'car (slack-user-vip-names team))))
      (should (member "Bob" candidates))
      (should (member "Alice" candidates))
      (should-not (member "Carol" candidates)))))

;;; slack-vip-test.el ends here
