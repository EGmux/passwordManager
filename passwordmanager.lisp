;; (load  "quicklisp/setup.lisp")
;; (pushnew (uiop:getcwd) ql:*local-project-directories*)
;; (ql:register-local-projects)
                                        ;
;; (ql:quickload :clog)
;; (ql:quickload :lparallel)
;; (ql:quickload :ironclad)
;; (ql:quickload '#:com.inuoe.jzon)
;; (ql:quickload :str)

(defpackage :passwordmanager
            (:use :cl  :ironclad :com.inuoe.jzon :str)
            (:export
             #:keychain
             #:keychain-init
             #:keychain-remove
             #:keychain-get
             #:keychain-set
             #:keychain-dump
             #:keychain-load))

(in-package :passwordmanager)

(defclass keychain ()
  ((#:entries
    :initform nil
    :accessor entries
    :documentation "The keychain object, has the entries as alists, each entry is a domain followed by password, TODO:improve these. "
    :allocation :instance)
   (#:derived-keys
    :initform nil
    :accessor derived-keys
    :documentation "The derived password from the user provide password"
    :allocation :instance)
   (#:loaded
    :initform nil
    :accessor loaded
    :documentation "Is the keychain loaded?"
    :allocation :instance)))
;; (declaim (optimize (speed 0) (space 0) (debug 3)))

(defparameter *keychain* nil)

(defmethod keychain-init ((k keychain) password)
           (format t "initializing the keychain...~%")
           (if (not (loaded k))
               (progn
                 (setf (derived-keys k) (let* ((passwordvec (ascii-string-to-byte-array  password))
                                               (returnvalues (multiple-value-list (ironclad:pbkdf2-hash-password passwordvec  :iterations (expt 2 20))))
                                               (pbkdf2 (nth 0 returnvalues))
                                               (salt (nth 1 returnvalues))
                                               (hmac (make-mac :hmac pbkdf2 :sha256))
                                               (mac (progn (update-mac hmac (make-random-salt)) (produce-mac hmac) (random-data 16))))
                                          (list pbkdf2 hmac (list salt mac)  (pbkdf2-hash-password-to-combined-string passwordvec  :salt salt :iterations (expt 2 20)))))
                 (setf (entries k) (make-hash-table :test 'EQUALP))
                 (setf (loaded k) t) 
                 (format t "Your master key is: ~a~%" (derived-keys k)))
             (format t "Password Store already initialized!~%")))

(defmethod _keychain-load ((k keychain) password representation trustedDataCheck)
           "Given a serialized representation of a Keychain load the Keychain in memory"
           (let ((unloaded (not (loaded k))))
             (if unloaded 
                 (if (pbkdf2-check-password (ascii-string-to-byte-array password) (nth 3( derived-keys k)))
                     (let* ((master-password (nth 0 (derived-keys k)))
                            (hmac (nth 1 (derived-keys k)))
                            (dumped-hash-table (nth 0 (entries k)))
                            (dumped-hash-table-cur-digest
                             (if trusteddatacheck
                                 (progn
                                   (reinitialize-instance hmac :key master-password)
                                   (update-mac hmac  (flexi-streams:string-to-octets dumped-hash-table))
                                   (flexi-streams:octets-to-string (produce-mac hmac)))
                               nil))
                            (validatep (if trustedDataCheck
                                           (equalp (nth 1 (entries k)) dumped-hash-table-cur-digest)
                                         nil)))
                       (progn
                         (let ((reason (format nil "success")))
                           (cond ((and trusteddatacheck validatep) (progn
                                                                     (format nil "Password store has not been tampered.~% ")
                                                                     (setq reason "safe")))
                                 ((and trusteddatacheck (not validatep))
                                  (progn
                                    (format nil "Password store has been tampered, expected digest was ~a~% but got ~a~%"
                                            (nth 1 (entries k)) dumped-hash-table-cur-digest
                                            (setq reason "tampered")))))
                           (let* ((parsed-hash-table (com.inuoe.jzon:parse dumped-hash-table))
                                  (hash-table (make-hash-table :test 'EQUALP))
                                  (- (maphash #'(lambda (key value) 
                                                  (setf (gethash key hash-table) (coerce value 'sb-kernel::simple-array-unsigned-byte-8)) ) parsed-hash-table)))
                             (setf (entries k) hash-table)
                             (progn
                               (format t "Password store loaded!~%")
                               (setf (loaded k) t)
                               reason)))))
                   (format nil "failed"))
               (progn (format nil"Already loaded!~%")
                      (format t "logged")))))

(defmethod _keychain-dump ((k keychain))
           "Returns an encoding of the hashtable as an encrypted association list serialized in JSON and a SHA-256 digest" 
           (format t "Dumping the password manager entries...~%")
           (if (loaded k)
               (let* ((hash-table (entries k))
                      (master-password (nth 0 (derived-keys k)))
                      (hmac (nth 1 (derived-keys k)))
                      (dumped-hash-table (com.inuoe.jzon:stringify hash-table)) 
                      (digest-hash-table (progn
                                           (reinitialize-instance hmac :key master-password)
                                           (update-mac hmac (ascii-string-to-byte-array dumped-hash-table))
                                           (flexi-streams:octets-to-string (produce-mac hmac)))))
                 (progn
                   (setf (entries k) (list dumped-hash-table digest-hash-table))
                   (format t "Password store dumped!~%")
                   (setf (loaded k) nil)))
             (format t "Already Dumped!~%")))

(defmethod _keychain-set ((k keychain) name value)
           "Insert or update an entry in the Kechain, must be called only after the keychain has being loaded"
           (let* ((hmac (nth 1 (derived-keys k)))
                  (master-password (nth 0 (derived-keys k)))
                  (build-gcm (nth 2 (derived-keys k)))
                  (digest (progn
                            (reinitialize-instance hmac :key master-password)
                            (update-mac hmac (ascii-string-to-byte-array name))
                            (flexi-streams:octets-to-string (produce-mac hmac))))
                  (hash-table (entries k))
                  (gcm (make-authenticated-encryption-mode :gcm
                                                           :cipher-name :aes
                                                           :key (nth 1 build-gcm)
                                                           :initialization-vector (nth 0 build-gcm)))
                  (foundp (gethash digest hash-table))
                  (plaintext-encoded-octet (flexi-streams:string-to-octets (str:unwords (list  (concatenate 'string  name value)  value)))))
             (progn (if foundp
                        (format nil "Entry found: ~a, updating the value...~%" name)
                      (format nil "Entry not found, adding the new value...~%"))
                    (setf (gethash digest hash-table)
                          (encrypt-message gcm plaintext-encoded-octet))
                    
                    (setf (entries k) hash-table)
                    
                    (format nil "Password store updated~%"))))

(defmethod _keychain-get ((k keychain) name)
           "Returns the password for an entry if found, otherwise nil, check for swap attacks, note that we assume the password to never have any space characters"
           (let* ((hmac (nth 1 (derived-keys k)))
                  (master-password (nth 0 (derived-keys k)))
                  (build-gcm (nth 2 (derived-keys k)))
                  (digest (progn
                            (reinitialize-instance hmac :key master-password)
                            (update-mac hmac (ascii-string-to-byte-array name))
                            (flexi-streams:octets-to-string (produce-mac hmac))))
                  (hash-table (entries k))
                  (gcm (make-authenticated-encryption-mode :gcm
                                                           :cipher-name :aes
                                                           :key (nth 1 build-gcm)
                                                           :initialization-vector(nth 0 build-gcm)))
                  (ciphertext (gethash digest hash-table)))
             (if ciphertext
                 (let*   ((plaintext (flexi-streams:octets-to-string (decrypt-message gcm ciphertext)))
                          (parsed-plaintext (str:split " "  plaintext))
                          (expected (nth 0 parsed-plaintext))
                          (password (nth 1 parsed-plaintext))
                          (got (concatenate 'string name password))
                          (tamperedp (if (not (equal expected got )) t nil)))
                   (if tamperedp
                       (format nil "Swap attack happened, expected ~a, got ~a~%" expected got)
                     (format nil "Entry found: ~a~%" password)))
               (format nil "Entry not found~%"))))

(defmethod _keychain-remove ((k keychain) name)
           "Remove an entry in the Keychain"
           (if (loaded k)
               (let* ((hmac (nth 1 (derived-keys k)))
                      (master-password (nth 0 (derived-keys k)))
                      (digest (progn
                                (reinitialize-instance hmac :key master-password)
                                (update-mac hmac (ascii-string-to-byte-array name))
                                (flexi-streams:octets-to-string (produce-mac hmac))))
                      (hash-table (entries k))
                      (value (gethash digest (entries k))))
                 (if value
                     (progn
                       (format t "Entry found: ~a, removing it~%" (remhash digest hash-table))
                       (setf (entries k) hash-table)
                       (format nil "Password store updated~%"))
                   (format nil "Entry not found~%")))
             (format nil "Password Manager not loaded!")))

(defun keychain-init (password)
  (_keychain-init (setf *keychain* (make-instance 'keychain)) password))

(defun keychain-load (password representation trusteddatacheck)
  (_keychain-load *keychain* password (entries *keychain*) trusteddatacheck))

(defun keychain-dump ()
  (_keychain-dump *keychain*))

(defun keychain-set (name value)
  (_keychain-set *keychain* name  value))

(defun keychain-get(name)
  (_keychain-get *keychain* name))

(defun keychain-remove (name)
  (_keychain-remove *keychain* name))


