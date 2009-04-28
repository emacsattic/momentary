;;; momentary --- momentarily display a buffer, then restore

;; Copyright (C) 2000 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>
;; Created:  3 Mar 2000
;; Version: 1.0
;; Keywords: convenience
;; X-URL: http://www.emacs.org/~johnw/emacs.html

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The main interface function is `with-output-to-momentary-buffer',
;; which has the same syntax and behavior as
;; `with-output-to-temp-buffer' -- except that windows disappear after
;; a timeout, if no relevant events occur immediately afterward.

;; The question of "relevant" is handled by the event function, which
;; is the third, optional argument to
;; `with-output-to-momentary-buffer'.  The default is to provide a
;; simple paging interface for that buffer, using SPACE and BACKSPACE
;; to page through the information.

;; But the user could easily define their own event function:

;; (with-output-to-momentary-buffer "*sample*"
;;     (print "hello there, Mr. Ed")
;;   (function
;;      (lambda (event)
;;         (if (event-matches-key-specifier-p event ?q)
;;             (message "Happy days are here again!")))))

;; With the above happy bit of code, pretty `q' after `*sample*' has
;; been displayed will result in a message.  But any other keystrokes
;; will terminate the event-reader, and cause the window to disappear
;; after `momentary-delay' seconds.

;;; History:

;;; Code:

(defconst momentary-version "1.0"
  "This version of momentary.")

(defgroup momentary nil
  "momentarily display a buffer, the restore."
  :group 'windows)

;;; User Variables:

(defcustom momentary-load-hook nil
  "*A hook that gets run after \"momentary.el\" has been loaded."
  :type 'hook
  :group 'momentary)

(defcustom momentary-display-delay 1
  "*The number of seconds to wait before restoring completion windows.
Once the completion window has been displayed, if the user then goes
on to type something else, that completion window will be removed from
the display (actually, the original window configuration before it was
displayed will be restored), after this many seconds of idle time.  If
set to nil, completion windows will be left on second until the user
removes them manually.  If set to 0, they will disappear immediately
after the user enters a key other than TAB."
  :type '(choice integer (const :tag "Never restore" nil))
  :group 'pcomplete)

;;; Internal Variables:

(defvar momentary-last-config nil)
(make-variable-buffer-local 'momentary-last-config)

(defvar momentary-buffer-name nil)
(make-variable-buffer-local 'momentary-buffer-name)

(defvar momentary-restore-timer nil)
(make-variable-buffer-local 'momentary-restore-timer)

;;; User Functions:

(defun momentary-restore ()
  "If the only window change was due to Completions, restore things."
  (if momentary-last-config
      (let* ((cbuf (get-buffer momentary-buffer-name))
             (cwin (and cbuf (get-buffer-window cbuf))))
        (when (and cwin (window-live-p cwin))
          (bury-buffer cbuf)
          (set-window-configuration momentary-last-config))))
  (setq momentary-last-config nil
        momentary-restore-timer nil))

;; Abstractions so that the code below will work for both Emacs 20 and
;; XEmacs 21

(unless (fboundp 'event-matches-key-specifier-p)
  (defalias 'event-matches-key-specifier-p 'eq))

(unless (fboundp 'read-event)
  (defsubst read-event (&optional prompt)
    (aref (read-key-sequence prompt) 0)))

(unless (fboundp 'event-basic-type)
  (defalias 'event-basic-type 'event-key))

;;;###autoload
(defun momentize-buffer (bufname window-config &optional event-func)
  "Make buffer BUFNAME disappear after a timeout.
WINDOW-CONFIG is the configuration to restore to.
EVENT-FUNC captures initial events, with the default behavior acting
as a pager."
  (let* ((curbuf (current-buffer)))
    (when momentary-restore-timer
      (cancel-timer momentary-restore-timer)
      (setq momentary-restore-timer nil))
    (unless momentary-last-config
      (setq momentary-last-config window-config))
    (setq momentary-buffer-name bufname)
    (unless event-func
      (message "Hit q to delete buffer `%s', SPC and DEL to scroll"
               bufname))
    (let (event)
      (prog1
          (catch 'done
            (while (with-current-buffer (get-buffer bufname)
                     (setq event (read-event)))
              (unless
                  (funcall (or event-func
                               'momentary-default-pager) event)
                (setq unread-command-events (list event))
                (throw 'done nil))))
        (if (and momentary-last-config momentary-display-delay)
            (setq momentary-restore-timer
                  (run-with-timer momentary-display-delay nil
                                  'momentary-restore)))))))

(defun momentary-buffer (bufname form &optional event-func)
  "Display buffer BUFNAME, populating with output from FORM.
FORM should output to `standard-output' (i.e., using `print', etc),
the information it wants the user to see.

If EVENT-FUNC if non-nil, it will be called for each event after the
window is displayed, until it returns nil."
  (let ((config (current-window-configuration)))
    (with-output-to-temp-buffer bufname
      (eval form))
    (momentize-buffer bufname event-func config)))

;;;###autoload
(defmacro with-output-to-momentary-buffer (bufname body &optional event-func)
  "Bind `standard-output' to buffer BUFNAME, eval BODY, then show that buffer.
The buffer is cleared out initially, and marked as unmodified when done.
All output done by BODY is inserted in that buffer by default.
The buffer is displayed in another window, but not selected.
The value of the last form in BODY is returned.
If BODY does not finish normally, the buffer BUFNAME is not displayed.

The hook `temp-buffer-setup-hook' is run before BODY,
with the buffer BUFNAME temporarily current.
The hook `temp-buffer-show-hook' is run after the buffer is displayed,
with the buffer temporarily current, and the window that was used
to display it temporarily selected.

If variable `temp-buffer-show-function' is non-nil, call it at the end
to get the buffer displayed instead of just displaying the non-selected
buffer and calling the hook.  It gets one argument, the buffer to display.

If the third argument EVENT-FUNC is a function, it will be called for
each event received immediately after displaying the buffer.  The
default is to act as a display pager, with SPACE and BACKSPACE moving
forward and backward, and `q' for quitting.

If any events are received outside of the events handled by EVENT-FUNC
\(that is, when EVENT-FUNC returns nil), the window will disappear
after `momentary-display-delay' seconds."
  `(momentary-buffer ,bufname (quote ,body) ,event-func))

(put 'with-output-to-momentary-buffer 'lisp-indent-function 2)

(defun momentary-default-pager (event)
  "The default display pager uses SPC/DEL to scroll, and `q' to quit.
EVENT contains the event triggered by the user.  If it's q, SPC or
DEL, we handle it here; otherwise, it gets passed back to Emacs, and
we return nil to indicate that no further events will be processed
here."
  (cond ((event-matches-key-specifier-p event ?q)
         (set-window-configuration momentary-last-config)
         (setq momentary-last-config nil))
        ((event-matches-key-specifier-p event ? )
         (save-selected-window
           (select-window
            (get-buffer-window momentary-buffer-name))
           (if (pos-visible-in-window-p (point-max))
               (goto-char (point-min))
             (scroll-up)))
         (message ""))
        ((or (event-matches-key-specifier-p event 'backspace)
             (event-matches-key-specifier-p event 'delete))
         (save-selected-window
           (select-window
            (get-buffer-window momentary-buffer-name))
           (if (pos-visible-in-window-p (point-min))
               (goto-char (point-max))
             (scroll-down)))
         (message ""))))

;;; Internal Functions:

(provide 'momentary)

(run-hooks 'momentary-load-hook)

;;; momentary.el ends here
