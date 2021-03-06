;(setq server-use-tcp t)

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t))

(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))

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
                             ;; Cada vez que se pulsa ENTER se indenta
                             ;; y luego se salta de línea.
			     (local-set-key (kbd "RET") 'newline-and-indent)))

(add-hook 'shell-mode-hook '(lambda ()
                              (visual-line-mode t)))

(let ((default-directory "~/.emacs.d/elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

;; (require 'google-c-style)
;; (add-hook 'c-mode-common-hook 'google-set-c-style)

(add-hook 'c-mode-common-hook '(lambda ()
                                 ;; Cada vez que se pulsa ENTER se
                                 ;; indenta y luego se salta de línea.
                                 (local-set-key (kbd "RET") 'newline-and-indent)
                                 ;; Hacer que
                                 ;; variasPalabrasQueEsténJuntasDeEstaManera,
                                 ;; sean consideradas varias para el
                                 ;; editor en modo C/C++.
                                 (subword-mode t)
                                 ;; Realiza alguna acción con ciertos
                                 ;; caracteres, como el ";".
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
(menu-bar-mode 0)
(scroll-bar-mode 0)

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
(setq org-log-done t)
(global-font-lock-mode 1)
(setq org-directory "~/ownCloud/org")
(setq org-agenda-files (list "~/ownCloud/org/tfm.org"
                             "~/ownCloud/org/todo.org"))


;; MobileOrg
;; (setq org-mobile-directory "~/ownCloud/MobileOrg")
;; (setq org-mobile-inbox-for-pull (concat org-directory "/index.org"))

;; Evitar que los eventos finalizados (DONE) sean mostrados .
(setq org-agenda-todo-ignore-scheduled (quote future))
;; (setq org-directory "~/ownCloud/org")
;; (setq org-agenda-files (concat org-directory "/todo.org"))

;; MobileOrg
;; (setq org-mobile-files org-agenda-files)
;; (setq org-mobile-directory "~/ownCloud/MobileOrg")
;; (setq org-mobile-inbox-for-pull (concat org-directory "/todo.org"))

(put 'dired-find-alternate-file 'disabled nil)

(load-theme 'sanityinc-solarized-dark t)
(put 'upcase-region 'disabled nil)
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)

(global-set-key (kbd "C-x m") 'term)
;; Start a new term even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (term t)))
(global-set-key "\M-%" 'query-replace-regexp)
(global-set-key "\C-w" 'backward-kill-word) ;; Backspace => C-w
(global-set-key "\C-x\C-k" 'kill-region) ;; Kill-Region(C-w) => C-x C-k
(global-set-key "\C-cx" 'smex)
(global-set-key [f6] '(lambda ()
                        (interactive)
                        (let ((mtg_path "$HOME/projects/mtg")) ;
                          (shell-command
                           (format (concat "find "
                                           mtg_path
                                           " -name \"*.cpp\" -or -name \"*.h\" | xargs etags --append -o "
                                           mtg_path
                                           "/TAGS"))))))

(global-set-key "\M-_" 'pop-tag-mark)

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

(nyan-mode)

(setq inferior-lisp-program "/usr/bin/sbcl")
(require 'slime)
;; (slime-setup '(slime-fancy))
(slime-setup '(slime))

;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;; (ac-config-default)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-save-query nil)
;(setq TeX-PDF-mode t)

(put 'downcase-region 'disabled nil)

(setq reftex-plug-into-AUCTeX t)

(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

(setq calendar-week-start-day 1
          calendar-day-name-array ["Domingo" "Lunes" "Martes" "Miércoles"
                                   "Jueves" "Viernes" "Sábado"]
          calendar-month-name-array ["Enero" "Febrero" "Marzo" "Abril" "Mayo"
                                     "Junio" "Julio" "Agosto" "Septiembre"
                                     "Octubre" "Noviembre" "Diciembre"])



(setq case-replace t)
(setq case-fold-search t)

(setq python-shell-interpreter "/home/zaka/anaconda2/envs/py34/bin/python")

;; (setq python-shell-interpreter "~/anaconda2/envs/py27/bin/python")

;; (setq python-shell-interpreter "~/anaconda2/envs/py27/bin/python")

; For Anaconda machine
; (setq python-shell-interpreter "~/anaconda3/envs/py35/bin/python")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#839496" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"))
 '(fci-rule-color "#073642")
 '(package-selected-packages
   (quote
    (ein ein-mumamo auto-complete-auctex company-auctex auctex company-math math-symbol-lists starter-kit starter-kit-bindings starter-kit-eshell starter-kit-js starter-kit-lisp smex slime nyan-mode magit color-theme-sanityinc-solarized)))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#cb4b16")
     (60 . "#b58900")
     (80 . "#859900")
     (100 . "#2aa198")
     (120 . "#268bd2")
     (140 . "#d33682")
     (160 . "#6c71c4")
     (180 . "#dc322f")
     (200 . "#cb4b16")
     (220 . "#b58900")
     (240 . "#859900")
     (260 . "#2aa198")
     (280 . "#268bd2")
     (300 . "#d33682")
     (320 . "#6c71c4")
     (340 . "#dc322f")
     (360 . "#cb4b16"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(set-face-attribute 'default nil :height 150)
