;; -----------------------------------------------------------------------------
;; General config
;;
(add-to-list 'load-path "~/.emacs.d/elisp/")
(let ((default-directory  "~/.emacs.d/elisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(setq inhibit-startup-message t)
(setq transient-mark-mode t)

(setq vc-follow-symlinks t)
(show-paren-mode t)

; No toolbar, could hide menus too, but they're nice sometimes
(tool-bar-mode 0)
;(menu-bar-mode 0)
(set-scroll-bar-mode nil)

;; font locking (i.e. syntax highlighting) spanning more than one line
(setq font-latex-do-multi-line t)

;; enable/disable highlighting of subscript and superscript via raised or lowered text
(setq font-latex-fontify-script nil)

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

; autosave behavior
(defvar user-temporary-file-directory "~/.emacs-autosaves/")
(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        (tramp-file-name-regexp nil)))
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

; show column 80
(require 'fill-column-indicator)
(setq-default fill-column 80)
(setq fci-rule-color "gray20")
(add-hook 'after-change-major-mode-hook (lambda () (fci-mode 1)))

; make buffer names more understandable
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

; Let us grep our buffers, brothers.
(require 'grep-buffers)

; Change comint to allow cycling through input history using arrow keys.
(require 'comint)
(define-key comint-mode-map (kbd "M-") 'comint-next-input)
(define-key comint-mode-map (kbd "M-") 'comint-previous-input)
(define-key comint-mode-map [down] 'comint-next-matching-input-from-input)
(define-key comint-mode-map [up] 'comint-previous-matching-input-from-input)

;; -----------------------------------------------------------------------------
;; Keybindings
;;

; F4: Goto line
(global-set-key [f4] 'goto-line)

; F5: Refresh file
(global-set-key [f5] 'really-refresh-file)
(defun really-refresh-file ()
  (interactive)
  (revert-buffer t t t)
  )

; F6: Revert all buffers
(global-set-key [f6] 'revert-all-buffers)
(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (not (buffer-modified-p)))
	(revert-buffer t t t) )))
  (message "Refreshed open files."))

; C-M-c: Comment/uncomment region
(global-set-key (read-kbd-macro "C-M-c") 'comment-or-uncomment-region)

; misc
(global-set-key (read-kbd-macro "<insert>") 'nil)
;(global-unset-key (kbd "C-M-l"))


;; -----------------------------------------------------------------------------
;; Appearance
;;
(global-font-lock-mode t)

(set-cursor-color "white")
(set-face-background 'region "gray20")

(custom-set-faces  ;;  only one 'custom-set-faces' entry may exist in .emacs!!
 '(default ((t (:foreground "white" :background "black" :bold t))) t)
 '(isearch ((t (:foreground "black" :background "yellow"))) t)
)

(set-face-attribute 'default nil :height 105)


;; -----------------------------------------------------------------------------
;; Python-specific
;;
;(load-file "~/.emacs.d/elisp/emacs-for-python/epy-init.el")

(global-set-key
 (read-kbd-macro "C-x p") "import pdb; pdb.set_trace() # --nsh DEBUG")

;; Activate python highlighting for .py, .pyx and .ppl files
(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.p\(pl\|y\(x\|\)\\'" . python-mode))

(require 'ipython)

; Make cool-looking lambdas.
(require 'lambda-mode)
(add-hook 'python-mode-hook #'lambda-mode 1)
(setq lambda-symbol (string (make-char 'greek-iso8859-7 107)))

; Use Anything for code completion.
(require 'anything)
(require 'anything-ipython)
(when (require 'anything-show-completion nil t)
   (use-anything-show-completion 'anything-ipython-complete
                                 '(length initial-pattern)))

;; -----------------------------------------------------------------------------
;; Other mode specific
;;
(require 'feature-mode)

(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist))

(require 'haml-mode)

(add-hook 'haml-mode-hook
	  '(lambda ()
	     (setq indent-tabs-mode nil)
	     (define-key haml-mode-map "\C-m" 'newline-and-indent)))

;;
;; Last things last....
;;
(server-start)
