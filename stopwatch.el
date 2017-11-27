;;; stopwatch.el --- stopwatch in mode-line -*- lexical-binding: t; -*-

;;; Commentary:

;; stopwatch.el provides showing stopwatch in mode-line.
;;
;; Start stopwatch
;;   M-x stopwatch-start
;;
;; Stop stopwatch
;;   M-x stopwatch-stop

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup stopwatch nil
  "Simple timer"
  :prefix "stopwatch-"
  :group 'timer)

(defcustom stopwatch-mode-line-sign "●"
  "Sign of timer"
  :type 'string)

(defface stopwatch-sign
  '((((class color) (min-colors 88) (background light))
     :foreground "blue")
    (((class color) (background dark))
     (:foreground "cyan"))
    (t nil))
  "mode-line-face")

(defface stopwatch-timer
  '((t (:weight bold)))
  "mode-line-face")

(defvar stopwatch--timer nil)
(defvar stopwatch--remainder-seconds 0)
(defvar stopwatch--mode-line "")

(defsubst stopwatch--time-to-string (seconds)
  (format "%02d:%02d " (/ seconds 60) (mod seconds 60))
  )

(defun stopwatch--propertize-mode-line ()
  (unless (string-empty-p stopwatch--mode-line)
    (concat (propertize stopwatch-mode-line-sign 'face 'stopwatch-sign)
	    (propertize stopwatch--mode-line 'face 'stopwatch-timer))))

(defun stopwatch--set-mode-line ()
  (setq stopwatch--mode-line
	(stopwatch--time-to-string stopwatch--remainder-seconds)))

(defun stopwatch-timer--tick ()
  (cl-incf stopwatch--remainder-seconds)
  (stopwatch--set-mode-line)
  (stopwatch--propertize-mode-line)
  (force-mode-line-update))

(defsubst stopwatch--set-remainder-second (minutes)
  (setq stopwatch--remainder-seconds (* 60 minutes)))

;;;###autoload

(defun stopwatch-start (&optional minutes)
  (interactive)
  (when stopwatch--timer
    (error "Already start stopwatch!!"))
  (unless minutes
    (setq minutes 0))
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
    (setq stopwatch--timer (run-with-timer 0 1 'stopwatch-timer--tick)))

(defun stopwatch-pause ()
  (interactive)
  (cancel-timer stopwatch--timer))

(unless (member '(:eval (stopwatch--propertize-mode-line)) mode-line-format)
  (setq-default mode-line-format
		(cons '(:eval (stopwatch--propertize-mode-line))
		      mode-line-format)))

(provide 'stopwatch)

;;; stopwatch.el ends here
