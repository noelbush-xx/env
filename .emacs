(load-file "~/.emacs.d/elisp/emacs-for-python/epy-init.el")
(add-to-list 'load-path "~/.emacs.d/elisp/")
(let ((default-directory  "~/.emacs.d/elisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'feature-mode)

(setq inhibit-startup-message t)
(setq transient-mark-mode t)

(defvar user-temporary-file-directory
  "~/.emacs-autosaves/")
(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        (tramp-file-name-regexp nil)))
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

(setq-default fill-column 80)
(require 'fill-column-indicator)
(setq fci-rule-color "gray10")
(add-hook 'python-mode-hook (lambda () (interactive) (fci-mode)))

(set-cursor-color "white")
(set-face-background 'region "gray20")

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(global-set-key [f4] 'goto-line)
(defun really-refresh-file ()
  (interactive)
  (revert-buffer t t t)
  )
(global-set-key [f5] 'really-refresh-file)
(global-set-key (read-kbd-macro "C-c C-c") 'comment-or-uncomment-region)
(global-set-key (read-kbd-macro "<insert>") 'nil)
(global-unset-key (kbd "C-M-l"))

(global-set-key [f6] 'revert-all-buffers)
(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (not (buffer-modified-p)))
	(revert-buffer t t t) )))
  (message "Refreshed open files."))

(global-set-key
 (read-kbd-macro "C-x p") "import pdb; pdb.set_trace() # --nsh DEBUG")

(custom-set-faces  ;;  only one 'custom-set-faces' entry may exist in .emacs!!
 '(default ((t (:foreground "white" :background "black" :bold t))) t)
 '(isearch ((t (:foreground "black" :background "yellow"))) t)
)

;; Do not ask about following symlinks.
(setq vc-follow-symlinks t)

(show-paren-mode t)

(setq default-frame-alist
 '(
    (width             . 75)
    (height            . 50)
  )
)

(set-face-attribute 'default nil :height 105)

;; turn off the toolbar, menubar or scrollbar
;; (this method will render the setting of frame size unreliable, this
;; could be resolved by using the .Xresources file instead)
(tool-bar-mode 0)
(menu-bar-mode 0)
(set-scroll-bar-mode nil)

;; Activate python highlighting for PYX and PPL files
(add-to-list 'auto-mode-alist '(".pyx'" . python-mode))
(add-to-list 'auto-mode-alist '(".ppl'" . python-mode))

;; font locking (i.e. syntax highlighting) spanning more than one line
(setq font-latex-do-multi-line t)

;; enable/disable highlighting of subscript and superscript via raised
;; or lowered text
(setq font-latex-fontify-script nil)

;; *********************
;; *** miscellaneous ***
;; *********************

;; Activate font-lock mode (syntax coloring).
(global-font-lock-mode t)

;; Line numbers are good.  Getting column numbering as well is better.
(column-number-mode t)

;; Always end files in a newline.
(setq require-final-newline 't)
;; ...or ask to end in newline if needed
; (setq require-final-newline 'query)

;; Frame title bar formatting to show full path of file
(setq-default
 frame-title-format
 (list '((buffer-file-name " %f" (dired-directory 
	 			  dired-directory
				  (revert-buffer-function " %b"
				  ("%b - Dir:  " default-directory)))))))
(server-start)
