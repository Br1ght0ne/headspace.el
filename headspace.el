;;; headspace.el --- Emacs server for Headspace - Hardspace: Shipbreaker dynamic music service -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Oleksii Filonenko
;;
;; Author: Oleksii Filonenko <brightone@protonmail.com>
;; Maintainer: Oleksii Filonenko <brightone@protonmail.com>
;; Created: May 15, 2021
;; Modified: May 15, 2021
;; Version: 0.0.1
;; Keywords: multimedia
;; Homepage: https://github.com/Br1ght0ne/headspace.el
;; Package-Requires: ((emacs "24.3") flycheck (web-server "0.1.2"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Emacs server for Headspace - Hardspace: Shipbreaker dynamic music service
;;
;;; Code:

(require 'flycheck)
(require 'web-server)

(defgroup headspace ()
  "Group for Headspace server settings."
  :group 'headspace
  :prefix "headspace-")

(defcustom headspace-port 44100
  "Port to run Headspace server on."
  :type 'integer)

(defcustom headspace-measurement-time 5
  "Time to report in measurements."
  :type 'integer)

(defcustom headspace-hazard-levels
  '((error . 1.0) (warning . 0.5) (hint . 0.25))
  "A mapping of Flycheck error levels to hazard levels."
  :type '(alist :key-type symbol :value-type float))

(defcustom headspace-events
  '(;;(text after-change-functions)
    (file find-file-hook)
    (save after-save-hook))
  "Events to count on specified hooks."
  :type '(alist :key-type symbol :value-type (list hook)))

(defcustom headspace-actions-decay
  '((text . 0.7) (file . 0.95) (save . 0.9))
  "Decay multipliers for events."
  :type '(alist :key-type symbol :value-type float))

(defvar headspace-actions (make-hash-table))

(defun headspace-create-event-handler (event actions)
  "Create an event handler for EVENT from ACTIONS."
  (let ((handler-name (intern (format "headspace-%s-handler" event))))
     (defalias handler-name (lambda ()
         (puthash event
                  (+ 1 (or (gethash event actions) 0))
                  actions)))))

(defun headspace-add-hooks ()
  "Add hooks to editing events and killing Emacs."
  (add-hook 'kill-emacs-hook #'headspace-stop-server)
  (dolist (event headspace-events)
    (let ((handler (headspace-create-event-handler (car event) headspace-actions)))
      (dolist (hook (cdr event))
        (add-hook hook handler)))))

(defvar headspace-listeners '())

(defvar headspace-server nil)

(defvar headspace-timer nil)

(defun headspace-start-server ()
  "Start the server."
  (interactive)
  (unless headspace-server
    (message "Starting Headspace server.")
    (headspace-add-hooks)
    (setq headspace-timer (run-with-timer 2 2 #'headspace-notify))
    (setq headspace-server (headspace-make-server))))

(defun headspace-make-server ()
  "Create a server to serve HTTP requests."
  (setq headspace-listeners '())
  (ws-start
   '(((:GET . ".*") .
      (lambda (request)
        (with-slots (process) request
          (ws-response-header
           process 200
           '("Access-Control-Allow-Origin" . "*")
           '("Content-Type" . "text/event-stream")
           '("Connection" . "keep-alive")
           '("Cache-Control" . "no-cache")
           '("Transfer-Encoding" . "chunked"))
          (pushnew! headspace-listeners process)
          :keep-alive))))
   headspace-port))

(defun headspace-stop-server ()
  "Shutdown the server."
  (interactive)
  (when headspace-server
    (message "Shutting down Headspace server.")
    (ws-stop headspace-server)
    (setq headspace-server nil)
    (when (timerp headspace-timer)
      (setq headspace-timer (cancel-timer headspace-timer)))))

(defun headspace-restart-server ()
  "Restart the server."
  (interactive)
  (headspace-stop-server)
  (headspace-start-server))

(defun headspace-get-flycheck-errors ()
  "Get Flycheck errors as a list of flycheck-error structs."
  (unless (get-buffer flycheck-error-list-buffer)
    (with-current-buffer (get-buffer-create flycheck-error-list-buffer)
      (flycheck-error-list-mode)))
  ;; Reset the error filter
  (flycheck-error-list-reset-filter)
  (let ((source (current-buffer)))
    ;; Adjust the source, causing a refresh
    (flycheck-error-list-set-source source)
    (flycheck-error-list-with-buffer (flycheck-error-list-current-errors))))

(defun headspace-get-activity-level ()
  "Get current activity level."
  (min 1 (+
          (- 1 (/ (float (+ 1 (/ (or (gethash 'save headspace-actions) 0) 2)))))
          (- 1 (/ (float (+ 1 (/ (or (gethash 'file headspace-actions) 0) 2)))))
          (- 1 (/ (float (+ 1 (/ (or (gethash 'text headspace-actions) 0) 16))))))))

(defun headspace-get-hazard-level ()
  "Get current hazard level."
  (cdr (cl-find-if
        (lambda (level)
          (member (car level)
                  (mapcar (lambda (err)
                            (flycheck-error-level err))
                          (headspace-get-flycheck-errors))))
        headspace-hazard-levels)))

(defun headspace-decay-actions ()
  "Decay actions over time."
  (dolist (pair headspace-actions-decay)
    (puthash (car pair)
             (max 0 (* (or (gethash (car pair) headspace-actions) 0) (cdr pair)))
             headspace-actions)))

(defun headspace-measure-buffer ()
  "Get diagnostics of the current buffer."
  (prog1
      `((activity . ,(or (headspace-get-activity-level) 0))
        (hazard . ,(or (headspace-get-hazard-level) 0))
        (time . ,headspace-measurement-time))
    (headspace-decay-actions)))

(defun headspace-notify ()
  "Notify listeners."
  (let ((parameters (headspace-measure-buffer)))
    (dolist (listener headspace-listeners)
      (if (process-live-p listener)
          (ws-send listener (format "data: %s\n\n" (json-serialize parameters)))
        (setq headspace-listeners(remove listener headspace-listeners))))))

(provide 'headspace)
;;; headspace.el ends here
