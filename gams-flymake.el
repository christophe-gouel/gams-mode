;;; gams-flymake.el --- A GAMS Flymake backend -*- lexical-binding: t -*-

;; Author: Christophe Gouel <christophe.gouel@inrae.fr>
;; Copyright (C) 2024 Christophe Gouel
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (flymake "1.0.0"))
;; Keywords: languages, tools, GAMS

;; This file is not part of any Emacs.

;;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author or from the Free Software Foundation,
;; Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Flymake is the built-in Emacs package that supports on-the-fly
;; syntax checking.  This file adds support for this in gams-mode.

;;; Code:

(require 'gams-mode)

(defun gams-flymake--parse-gams-lst-errors (lst-file)
  "Parse LST-FILE and return a sorted list of errors with location details."
  (with-temp-buffer
    (insert-file-contents lst-file)
    (goto-char (point-min))
    (let (errors)
      (while (re-search-forward "^\\*\\*\\*\\*\\(.*\\)$" nil t)
        (let ((error-line-content (match-string 1))
              error-codes
              error-line-number
              error-messages
              current-error-code)
          ;; Find positions of dollar signs and error codes in error-line-content
          (let ((pos 0))
            (while (string-match "\\(\\$\\)\\([0-9]+\\)" error-line-content pos)
              (let ((match-start (match-beginning 1))
                    (error-code (match-string 2 error-line-content)))
                ;; Column position is (position of '$' in error line) - 2
                (let ((column (- match-start 2))) ; Adjust for "****"
                  (push (list :error-code error-code
                              :column column)
                        error-codes))
                ;; Initialize messages for this error code
                (push (cons error-code '()) error-messages)
                ;; Move past the current match
                (setq pos (match-end 0)))))
          ;; Reverse error-messages to maintain order
          (setq error-messages (nreverse error-messages))
          ;; Collect error message lines
          (forward-line 1)
          (while (and (not (eobp))
                      (looking-at "^\\*\\*\\*\\*\\(.*\\)$"))
            (let ((line-content (match-string 1)))
              ;; Check for 'LINE' information to get the line number
              (cond
               ((string-match "^ +LINE +\\([0-9]+\\)" line-content)
                (setq error-line-number (string-to-number (match-string 1 line-content))))
               ;; Check if line starts with an error code
               ((string-match "^ *\\([0-9]+\\)\\s-+\\(.*\\)$" line-content)
                (setq current-error-code (match-string 1 line-content))
                (let ((msg (string-trim (match-string 2 line-content))))
                  (setcdr (assoc current-error-code error-messages)
                          (append (cdr (assoc current-error-code error-messages)) (list msg)))))
               ;; Continuation lines
               (t
                (let ((msg (string-trim line-content)))
                  (if current-error-code
                      (setcdr (assoc current-error-code error-messages)
                              (append (cdr (assoc current-error-code error-messages)) (list msg)))
                    ;; If current-error-code is nil, append to all error codes
                    (dolist (err-code error-messages)
                      (setcdr err-code (append (cdr err-code) (list msg)))))))))
            (forward-line 1))
          ;; Create an error entry for each error code
          (dolist (code-entry error-codes)
            (let* ((error-code (plist-get code-entry :error-code))
                   (column (plist-get code-entry :column))
                   (msgs (cdr (assoc error-code error-messages)))
                   (error-msg (string-join msgs " ")))
              ;; Remove error codes from the error message
              (setq error-msg (replace-regexp-in-string "^\\([0-9]+\\)\\s-*" "" error-msg))
              ;; Create the error entry
              (push (list :error-code error-code
                          :column column
                          :line-number error-line-number
                          :error-message error-msg)
                    errors)))))
      ;; Sort errors by line number and column
      (setq errors (sort errors
                         (lambda (a b)
                           (or (< (plist-get a :line-number) (plist-get b :line-number))
                               (and (= (plist-get a :line-number) (plist-get b :line-number))
                                    (< (plist-get a :column) (plist-get b :column)))))))
      errors)))

(defun gams-flymake-backend (report-fn &rest _args)
  "Flymake backend for GAMS files.

REPORT-FN is the Flymake callback function."
  (let* ((source-file (buffer-file-name))
         (lst-file (concat (file-name-sans-extension source-file) ".lst"))
         (source-buffer (current-buffer)))
    ;; Save the current buffer to ensure the file is up to date
    (save-buffer)
    ;; Run GAMS on the source file
    (let ((command (list gams-process-command-name source-file "action=c" "lo=0")))
      (make-process
       :name "gams-flymake"
       :buffer (generate-new-buffer "*gams-flymake*")
       :command command
       :noquery t
       :sentinel
       (lambda (proc _event)
         (when (eq (process-status proc) 'exit)
           (if (file-exists-p lst-file)
               (let ((errors (gams-flymake--parse-gams-lst-errors lst-file))
                     diagnostics)
                 ;; Convert errors to Flymake diagnostics
                 (dolist (err errors)
                   (let* ((line (plist-get err :line-number))
                          (col (plist-get err :column))
                          (msg (plist-get err :error-message)))
                     (with-current-buffer source-buffer
                       (save-excursion
                         (goto-char (point-min))
                         (forward-line (1- line))
                         (let* ((line-start (line-beginning-position))
                                (line-end (line-end-position))
                                (error-pos (+ line-start col)))
                           ;; Ensure error-pos is within line bounds
                           (when (> error-pos line-end)
                             (setq error-pos line-end))
                           ;; Create Flymake diagnostic
                           (let ((diag (flymake-make-diagnostic
                                        source-buffer
                                        error-pos
                                        (1+ error-pos)
                                        :error
                                        msg)))
                             (push diag diagnostics)))))))
                 ;; Report diagnostics to Flymake
                 (funcall report-fn diagnostics)
                 ;; Clean up temporary files
                 (when (file-exists-p lst-file)
                   (delete-file lst-file)))
             ;; If LST file doesn't exist, report no diagnostics
             (funcall report-fn nil))
           ;; Clean up process buffer
           (kill-buffer (process-buffer proc))))))))

(defun gams-flymake-setup ()
  "Set up Flymake for GAMS files."
  (when (and (buffer-file-name)
             (string-equal (file-name-extension (buffer-file-name)) "gms"))
    (add-hook 'flymake-diagnostic-functions 'gams-flymake-backend nil t)))

(add-hook 'gams-mode-hook 'gams-flymake-setup)

(provide 'gams-flymake)

;;; gams-flymake.el ends here
