;;; stopwatch.el --- stopwatch in mode-line -*- lexical-binding: t; -*-

;;; Commentary:

;; stopwatch.el provides showing stopwatch in mode-line.
;;
;; Start stopwatch
;;   M-x stopwatch-start
;;
;; Stop stopwatch
;;   M-x stopwatch-stop
;;
;; Pause stopwatch
;;   M-x stopwatch-pause
;;
;; Restart stopwatch
;;   M-x stopwatch-restart

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup stopwatch nil
  "Simple timer"
  :prefix "stopwatch-"
  :group 'timer)

(defvar notification-center-title "Emacs")
(defun notification-center (msg)
  (let ((tmpfile (make-temp-file "notification-center")))
   (with-temp-file tmpfile
     (insert
      (format "display notification \"%s\" with title \"%s\""
	      msg notification-center-title)))
   (unless (zerop (call-process "osascript" tmpfile))
     (message "Failed: Call AppleScript"))
   (delete-file tmpfile)))

(defcustom stopwatch-mode-line-sign "●"
  "Sign of timer"
  :type 'string)

(defface stopwatch-timer
  '((t (:weight bold)))
  "mode-line-face")

(defface working-face
  '((t (:foreground "cyan")))
  "mode-line-face")

(defface pausing-face
  '((t (:foreground "green")))
  "mode-line-face")

(defvar stopwatch--timer nil)
(defvar stopwatch--remainder-seconds 0)
(defvar stopwatch--mode-line "")

(defvar current-state 'working
  "Stopwatch statement flag, working or pausing")

(defvar notification-current-state 0) ;; 0 or 1
(defvar notification-time nil)

(defsubst stopwatch--time-to-string (seconds)
  (format "%02d:%02d " (/ seconds 60) (mod seconds 60))
  )

(defun propertize-sign ()
  (cond ((eq current-state 'working)
	 (propertize stopwatch-mode-line-sign 'face 'working-face))
	((eq current-state 'pausing)
	 (propertize stopwatch-mode-line-sign 'face 'pausing-face))
	(t nil)))

(defun stopwatch--propertize-mode-line ()
  (unless (string-empty-p stopwatch--mode-line)
    (concat (propertize-sign)
	    (propertize stopwatch--mode-line 'face 'stopwatch-timer))))

(defun stopwatch--set-mode-line ()
  (setq stopwatch--mode-line
	(stopwatch--time-to-string stopwatch--remainder-seconds)))

(defun stopwatch-timer--tick ()
  (if (and (> stopwatch--remainder-seconds (* notification-time 60))
	   (> notification-current-state 0))
      (progn
	(setq notification-current-state 0)
	(notification-center "Stopwatch Notification"))
    (cl-incf stopwatch--remainder-seconds)
    (stopwatch--set-mode-line)
    (stopwatch--propertize-mode-line)
    (force-mode-line-update)))

(defsubst stopwatch--set-remainder-second (minutes)
  (setq stopwatch--remainder-seconds (* 60 minutes)))

;;;###autoload

(defun stopwatch-notification-time (minutes)
  (interactive "nNotification Minutes:")
  (setq notification-current-state 1)
  (setq notification-time minutes))

(defun stopwatch-start (&optional minutes)
  (interactive)
  (when stopwatch--timer
    (error "Already start stopwatch!!"))
  (unless minutes
    (setq minutes 0))
  (setq notification-current-state 0)
  (setq current-state 'working)
  (stopwatch--set-remainder-second minutes)
  (setq stopwatch--timer (run-with-timer 0 1 'stopwatch-timer--tick)))

(defun stopwatch-stop ()
  (interactive)
  (cancel-timer stopwatch--timer)
  (setq stopwatch--timer nil
	stopwatch--mode-line "")
  (force-mode-line-update))

(defun stopwatch-restart ()
  (interactive)
  (setq current-state 'working)
  (setq stopwatch--timer (run-with-timer 0 1 'stopwatch-timer--tick)))

(defun stopwatch-pause ()
  (interactive)
  (setq current-state 'pausing)
  (cancel-timer stopwatch--timer))

(unless (member '(:eval (stopwatch--propertize-mode-line)) mode-line-format)
  (setq-default mode-line-format
		(cons '(:eval (stopwatch--propertize-mode-line))
		      mode-line-format)))

(provide 'stopwatch)

;;; stopwatch.el ends here
