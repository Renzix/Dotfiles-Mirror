;;; makefile-executor-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "makefile-executor" "makefile-executor.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from makefile-executor.el

(autoload 'makefile-executor-execute-target "makefile-executor" "\
Execute a Makefile target from FILENAME.

FILENAME defaults to current buffer.

\(fn FILENAME &optional TARGET)" t nil)

(autoload 'makefile-executor-execute-project-target "makefile-executor" "\
Choose a Makefile target from all of the Makefiles in the project.

If there are several Makefiles, a prompt to select one of them is shown.
If so, the parent directory of the closest Makefile is added
as initial input for convenience in executing the most relevant Makefile.

\(fn)" t nil)

(autoload 'makefile-executor-execute-dedicated-buffer "makefile-executor" "\
Runs a makefile target in a dedicated compile buffer.

The dedicated buffer will be named \"*<target>*\".  If
`projectile' is installed and the makefile is in a project the
project name will be prepended to the dedicated buffer name.

\(fn FILENAME &optional TARGET)" t nil)

(autoload 'makefile-executor-execute-last "makefile-executor" "\
Execute the most recently executed Makefile target.

If none is set, prompt for it using
`makefile-executor-execute-project-target'.  If the universal
argument is given, always prompt.

\(fn ARG)" t nil)

(autoload 'makefile-executor-goto-makefile "makefile-executor" "\
Interactively choose a Makefile to visit.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "makefile-executor" '("makefile-executor-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; makefile-executor-autoloads.el ends here
