;;Don't use default.el file.
(setq-default inhibit-default-init t)

;;Disable backups
(setq make-backup-files nil)

;;Truncate lines instead of wrapping to the nextline
(setq-default truncate-lines t)

(setq-default require-final-newline nil)
(setq-default next-line-add-newlines nil)

;;Enable font-lock-mode.
(global-font-lock-mode t)

;; Match parent prentheses & braces
(show-paren-mode 1)

(add-hook 'lisp-mode-hook '(lambda ()
                             ;; Cada vez que se pulsa ENTER se indenta y luego se salta de línea.
			     (local-set-key (kbd "RET") 'newline-and-indent)))

(add-hook 'shell-mode-hook '(lambda ()
                              (visual-line-mode t)))

(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)

(add-hook 'c-mode-common-hook '(lambda ()
                                 ;; Cada vez que se pulsa ENTER se indenta y luego se salta de línea.
                                 (local-set-key (kbd "RET") 'newline-and-indent)
                                 ;; Hacer que variasPalabrasQueEsténJuntasDeEstaManera, sean
                                 ;; consideradas varias para el editor en modo C/C++.
                                 (subword-mode t)
                                 ;; Realiza alguna acción con ciertos caracteres, como el ";".
                                 (c-toggle-electric-state t)
                                 ;; Borrar una cadena de espacios con un solo DELETE o BACKDELETE
                                 (c-toggle-auto-hungry-state t)))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(add-hook 'java-mode-hook '(lambda ()
                             (c-set-style "java")))

(add-hook 'shell-script-mode '(lambda ()
                                (electric-indent-mode t)))

(add-hook 'python-mode (subword-mode t))

;; Quitar buffer de inicio con las AYUDAS.
(setq inhibit-startup-message t)

;; Display line and column numbers
(setq line-number-mode    t)
(setq column-number-mode  t)

;; I hate tabs!
(setq-default indent-tabs-mode nil)

(setq highlight-tabs t)
(setq highlight-trailing-whitespace t)

(tool-bar-mode 0)

;; Ido Mode settings.
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

(setq-default indent-tabs-mode nil)

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(setq color-theme-load-all-themes nil)

;; Org-mode settings
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-c.." 'org-time-stamp)
(global-font-lock-mode 1)
(setq org-directory "~/ownCloud/org")
(setq org-agenda-files (quote ("~/ownCloud/org/todo.org")))


;; MobileOrg
(setq org-mobile-directory "~/ownCloud/MobileOrg")
(setq org-mobile-inbox-for-pull (concat org-directory "/index.org"))

;; Evitar la aparición de eventos ya hechos (DONE).
(setq org-agenda-todo-ignore-scheduled (quote future))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   (quote
    ("f32dd8e7b3a508874eded03d5be43d2bdfffe81c199eea72de06ce3e653db720" default)))
 '(package-selected-packages
   (quote
    (magit async auto-highlight-symbol dash elisp-slime-nav epl eproject erlang f find-file-in-project gh git-commit helm helm-core idle-highlight-mode ido-completing-read+ ido-ubiquitous json-reformat json-snatcher logito magit-popup paredit pcache pkg-info popup pos-tip s smex with-editor clang-format edts flycheck-clangcheck flycheck-google-cpplint flycheck-pyflakes flylisp flymake-cppcheck flymake-google-cpplint flymake-jslint flymake-json flymake-python-pyflakes flymake-shell flyparens google-c-style json-mode latex-pretty-symbols latex-preview-pane ac-c-headers ac-clang ac-dabbrev ac-etags ac-slime auctex auto-complete-auctex auto-complete-c-headers auto-complete-clang auto-complete-clang-async auto-complete-exuberant-ctags magit-annex magit-filenotify magit-find-file magit-gerrit magit-gh-pulls magit-gitflow magit-tramp nyan-mode slime starter-kit-bindings starter-kit-js starter-kit-lisp)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(put 'dired-find-alternate-file 'disabled nil)

;; (defun toggle-fullscreen ()
;;   (interactive)
;;   (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
;;                           '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
;;   (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
;;                           '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
;; )
;; (toggle-fullscreen)

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 100 :width normal)))))

(load-theme 'tango-dark t)
(put 'upcase-region 'disabled nil)
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)
(setq desktop-restore-frames nil)
(desktop-save-mode 1)

(global-set-key "\C-w" 'backward-kill-word) ;; Backspace => C-w
(global-set-key "\C-x\C-k" 'kill-region) ;; Kill-Region(C-w) => C-x C-k
(global-set-key "\C-cx" 'smex)
(global-set-key [f6] '(lambda ()
                        (interactive)
                        (let ((mtg_path "$HOME/projects/mtg"))
                          (shell-command
                           (format (concat "find "
                                           mtg_path
                                           " -name \"*.cpp\" -or -name \"*.h\" | xargs etags --append -o "
                                           mtg_path
                                           "/TAGS"))))))

(global-set-key "\M-_" 'pop-tag-mark)

;; (require 'sublimity)
;; (require 'sublimity-scroll)

;; (require 'sublimity-map)

(eval-after-load 'flycheck
  '(progn
     (require 'flycheck-google-cpplint)
     ;; Add Google C++ Style checker.
     ;; In default, syntax checked by Clang and Cppcheck.
     (flycheck-add-next-checker 'c/c++-clang
                                'c/c++-googlelint 'append)))

(global-set-key [C-tab] 'clang-format-region)
(setq-default clang-format-style "Google")

(require 'iso-transl)

(add-to-list 'auto-mode-alist '("\\.jets\\'" . json-mode))

;; (set-face-attribute 'default nil :height 120)

(nyan-mode)

(setq inferior-lisp-program "/usr/bin/sbcl")
(require 'slime)
(slime-setup '(slime-fancy))

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
;(setq TeX-PDF-mode t)

