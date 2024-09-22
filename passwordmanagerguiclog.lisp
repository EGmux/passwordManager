;; graphical user interface for password manager powered by CLOG

(load "quicklisp/setup.lisp")
;; (pushnew (uiop:getcwd) ql:*local-project-directories*)


(defpackage :passwordmanagergui
  (:use :cl :clog :clog-user :passwordmanager))

(in-package :passwordmanagergui)
;; (ql:register-local-projects)
;; (ql:quickload :clog)
;; (ql:quickload :passwordmanager)
;; (clog:clog-repl)

;; (in-package clog-user)j

(defun on-new-window (body)
  (set-html-on-close body "Connection Lost")
  (load-css (html-document body) "/css/w3.css")
  (setf (title (html-document body)) "CLOG Chat")

  (let* ((backdrop   (create-div body :class "w3-container w3-cyan"))
         (form-box   (create-div backdrop :class "w3-container w3-white"))
         (dump-form (create-form form-box))
         (load-form (create-form form-box))
         (add-form (create-form form-box))
         (remove-form (create-form form-box))
         (get-form (create-form form-box))
         (init-form (create-form form-box))
         
         (caption-load    (create-section load-form :h1 :content "Password Manager"))
         (caption-message-loadentry    (create-section load-form :h3 :content "Master Password:"))
         (form-loadentry    (create-form-element caption-message-loadentry :input :label (create-label load-form)))
         (caption-load-answer    (create-section load-form :h3 :content " "))
         ;; (ok-button-back-load (create-button caption-load  :content "Go Back" :class "w3-button w3-white"))
         (ok-button-load-send (create-button caption-load :content "Send" :class "w3-button w3-white"))
         (caption-message-trustentry    (create-section load-form :h3 :content "Trusted Data Check:"))
         (form-trust-entry    (create-form-element caption-message-trustentry :input :label (create-label load-form)))
         
         (caption-dump    (create-section dump-form :h1 :content "Password Manager"))
         (caption-message-dump    (create-section caption-dump :h3 :content ""))
         (ok-button-dump (create-button caption-message-dump :content "Loaded, Click here to Dump" :class "w3-button w3-white"))
         (caption-message-remove    (create-section caption-message-dump :h3 :content ""))
         (caption-message-set    (create-section caption-message-dump :h3 :content ""))
         (caption-message-get    (create-section caption-message-dump :h3 :content ""))
         (ok-button-add (create-button caption-message-set :content "Add/Update an Entry" :class "w3-button w3-white"))
         (ok-button-remove (create-button caption-message-remove :content "Remove an Entry" :class "w3-button w3-white"))
         (ok-button-get (create-button caption-message-get :content "Get an Entry" :class "w3-button w3-white"))
         
         (caption-init    (create-section init-form :h1 :content "Password Manager"))
         (caption-message-addmasterpass    (create-section init-form :h3 :content "Password"))
         (form-addmasterpassword    (create-form-element caption-message-addmasterpass :input :label (create-label init-form)))
         (caption-init-answer    (create-section init-form :h3 :content " "))
         (ok-button-init-send (create-button caption-init :content "Send" :class "w3-button w3-white"))

         ;; add entry screen
         (caption-add    (create-section add-form :h1 :content "Password Manager"))
         (caption-message-adddomain    (create-section add-form :h3 :content "Domain Name for Entry"))
         (form-adddomain    (create-form-element caption-message-adddomain :input :label (create-label add-form)))
         (caption-message-addpassword  (create-section add-form :h3 :content "Password Name for Entry:"))
         (form-addpassword    (create-form-element caption-message-addpassword :input :label (create-label add-form)))
         (caption-add-answer    (create-section add-form :h3 :content " "))
         (ok-button-back-add (create-button caption-add  :content "Go Back" :class "w3-button w3-white"))
         (ok-button-add-send (create-button caption-add :content "Send" :class "w3-button w3-white"))

         ;; remove entry screen
         (caption-remove    (create-section remove-form :h1 :content "Password Manager"))
         (caption-message-removeentry    (create-section remove-form :h3 :content "Domain Name for Entry:"))
         (form-removeentry    (create-form-element caption-message-removeentry :input :label (create-label remove-form)))
         (caption-remove-answer    (create-section remove-form :h3 :content " "))
         (ok-button-back-remove (create-button caption-remove  :content "Go Back" :class "w3-button w3-white"))
         (ok-button-remove-send (create-button caption-remove :content "Send" :class "w3-button w3-white"))

         ;; get entry screen
         (caption-get    (create-section get-form :h1 :content "Password Manager"))
         (caption-message-getentry    (create-section get-form :h3 :content "Domain Name for Entry:"))
         (form-getentry    (create-form-element caption-message-getentry :input :label (create-label get-form)))
         (caption-get-answer    (create-section get-form :h3 :content " "))
         (ok-button-back-get (create-button caption-get  :content "Go Back" :class "w3-button w3-white"))
         (ok-button-get-send (create-button caption-get :content "Send" :class "w3-button w3-white"))
         
         (user-name))
    (declare (ignore p1 p2 caption br1 br2))
    ;; (setf (hiddenp chat-box) t)
    (setf (hiddenp dump-form) t)
    (setf (hiddenp load-form) t)
    (setf (hiddenp add-form) t)
    (setf (hiddenp remove-form) t)
    (setf (hiddenp get-form) t)
    ;; (setf (class-name form-box) "w3-container w3-blue")
    (setf (height backdrop) "100vh")
    (setf (display backdrop) :flex)
    (setf (justify-content backdrop) :center)
    (setf (align-items backdrop) :center)
    (setf (background-color form-box) :red)
    (setf (display backdrop) :flex)
    (setf (justify-content backdrop) :center)
    (setf (width form-box) "80vh")
    (set-on-click ok-button-dump
                  (lambda (obj)
                    (declare (ignore obj))
                    (setf (hiddenp load-form) nil)
                    (setf (hiddenp dump-form) t)
                    (setf (css-class-name backdrop) "w3-container w3-red")
                    (passwordmanager:keychain-dump)))
    (set-on-click ok-button-remove
                  (lambda (obj)
                    (declare (ignore obj))
                    (setf (hiddenp remove-form) nil)
                    (setf (hiddenp dump-form) t)))
    (set-on-click ok-button-add
                  (lambda (obj)
                    (declare (ignore obj))
                    (setf (hiddenp add-form) nil)
                    (setf (hiddenp dump-form) t)))
    (set-on-click ok-button-get
                  (lambda (obj)
                    (declare (ignore obj))
                    (setf (hiddenp get-form) nil)
                    (setf (hiddenp dump-form) t)))
    (set-on-click ok-button-back-add
                  (lambda (obj)
                    (declare (ignore obj))
                    (setf (hiddenp add-form) t)
                    (setf (hiddenp dump-form) nil)
                    (setf (text-value form-adddomain) "")
                    (setf (text-value form-addpassword) "")
                    (setf (text-value caption-add-answer) " ")))
    (set-on-click ok-button-back-get
                  (lambda (obj)
                    (declare (ignore obj))
                    (setf (hiddenp get-form) t)
                    (setf (hiddenp dump-form) nil)
                    (setf (text-value form-getentry) "")
                    (setf (text-value caption-get-answer) " ")))
    (set-on-click ok-button-back-remove
                  (lambda (obj)
                    (declare (ignore obj))
                    (setf (hiddenp remove-form) t)
                    (setf (hiddenp dump-form) nil)
                    (setf (text-value form-removeentry) "")
                    (setf (text-value caption-remove-answer) " ")))
    
    (set-on-click ok-button-load-send
                  (lambda (obj)
                    (declare (ignore obj))
                    (let ((masterpassword (text-value form-loadentry)))
                      (if (and (not (equal masterpassword  "")))
                          (progn
                            (setf (text-value ok-button-load-send) "Sent")
                            (setf (text-value caption-load-answer) "Please wait...")
                            (setf (text-value caption-load-answer)
                                  (let ((answer (passwordmanager:keychain-load masterpassword nil (not (equal (text-value form-trust-entry) " ")))))
                                    (cond ((or (equal answer "success") (equal answer "logged") (equal answer"safe"))
                                           (progn
                                             (setf (hiddenp load-form) t)
                                             (setf (hiddenp dump-form) nil)
                                             (setf (css-class-name backdrop) "w3-container w3-green")
                                             (setf (text-value form-loadentry) "")
                                             (setf (text-value form-trust-entry) "")
                                             " "))
                                          ((equal answer "failed")
                                           "Wrong Master Password")
                                          ((equal answer "tampered")
                                           "Store has been tampered!"))))
                            (setf (text-value ok-button-load-send) "Send"))
                          (setf (text-value caption-load-answer) "Missing arguments")))))
    (set-on-click ok-button-init-send
                  (lambda (obj)
                    (declare (ignore obj))
                    (let ((masterpassword (text-value form-addmasterpassword)))
                      (if (and (not (equal masterpassword  "")))
                          (progn
                            (setf (text-value ok-button-get-send) "Sent")
                            (setf (text-value caption-init-answer) "Please wait...")
                            (setf (text-value caption-get-answer) (passwordmanager:keychain-init masterpassword))
                            (setf (hiddenp init-form) t)
                            (setf (hiddenp dump-form) nil)
                            (setf (css-class-name backdrop) "w3-container w3-green")
                            (setf (text-value ok-button-get-send) "Send"))
                          (setf (text-value caption-add-answer) "Missing arguments")))))
    (set-on-click ok-button-get-send
                  (lambda (obj)
                    (declare (ignore obj))
                    (let ((domain (text-value form-getentry)))
                      (if (and (not (equal domain  "")))
                          (progn
                            (setf (text-value ok-button-get-send) "Sent")
                            (setf (text-value caption-get-answer) (passwordmanager:keychain-get domain))
                            (setf (text-value ok-button-get-send) "Send"))
                          (setf (text-value caption-get-answer) "Missing arguments")))))
    (set-on-click ok-button-add-send
                  (lambda (obj)
                    (declare (ignore obj))
                    (let ((domain (text-value form-adddomain))
                          (password (text-value form-addpassword)))
                      (if (and (not (equal domain  "")) (not (equal password "")))
                          (progn
                            (setf (text-value ok-button-add-send) "Sent")
                            (setf (text-value caption-add-answer) (passwordmanager:keychain-set domain password))
                            (setf (text-value ok-button-add-send) "Send"))
                          (setf (text-value caption-add-answer) "Missing arguments")))))
    (set-on-click ok-button-remove-send
                  (lambda (obj)
                    (declare (ignore obj))
                    (let ((domain (text-value form-removeentry)))
                      (if (not (equal domain  ""))
                          (progn
                            (setf (text-value ok-button-remove-send) "Sent")
                            (setf (text-value caption-remove-answer) (passwordmanager:keychain-remove domain))
                            (setf (text-value ok-button-remove-send) "Send"))
                          (setf (text-value caption-remove-answer) "Missing arguments"))))))
  (run body))




(defun main ()
  "Starts the application"
  (initialize #'on-new-window :port 4040)
  (sleep most-positive-fixnum))
