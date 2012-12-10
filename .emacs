;; Don't use default.el file.
(setq-default inhibit-default-init t)

;; Start an emacs server.
(server-start)

;;Deshabilitar back-ups
(setq make-backup-files nil)

;; Truncate lines instead of wrapping to the next line
(setq-default truncate-lines t)


(setq-default require-final-newline nil)
(setq-default next-line-add-newlines nil)

;; Activar font-lock-mode.
(global-font-lock-mode t)

;; Ver la pareja de un paréntesis o corchete.
(show-paren-mode 1)

(add-hook 'lisp-mode-hook '(lambda ()
                             ;; Cada vez que se pulsa ENTER se indenta y luego se salta de línea.
			     (local-set-key (kbd "RET") 'newline-and-indent)))

;; (add-hook 'find-file-hook (lambda () (flymake-find-file-hook 1)))

(add-hook 'c-mode-common-hook '(lambda ()
                                 ;; Cada vez que se pulsa ENTER se indenta y luego se salta de línea.
                                 (local-set-key (kbd "RET") 'newline-and-indent)
                                 ;; Hacer que variasPalabrasQueEsténJuntasDeEstaManera, sean
                                 ;; consideradas varias para el editor en modo C/C++.
                                 (subword-mode t)
                                 ;; Realiza alguna acción con ciertos caracteres, como el ";".
                                 (c-toggle-electric-state t)
                                 ;; Borrar una cadena de espacios con un solo DELETE o BACKDELETE
                                 (c-toggle-auto-hungry-state t)
                                 ;; Estilo de indentación Ellemtel
                                 (c-set-style "linux")))

(add-hook 'java-mode-hook '(lambda ()
                             (c-set-style "java")))

(add-hook 'shell-script-mode '(lambda ()
                                (electric-indent-mode t)))

;; Quitar buffer de inicio con las AYUDAS.
(setq inhibit-startup-message t)

;; Display line and column numbers
(setq line-number-mode    t)
(setq column-number-mode  t)

;; I hate tabs!
(setq-default indent-tabs-mode nil)

(setq highlight-tabs t)
(setq highlight-trailing-whitespace t)

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))

(tool-bar-mode 0)

;; Ido Mode settings.
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

(setq-default indent-tabs-mode nil)

;; M-x => C-x C-m
(global-set-key "\C-x\C-m" 'execute-extended-command)

;; Backspace => C-w
(global-set-key "\C-w" 'backward-kill-word)

;; Kill-Region(C-w) => C-x C-k
(global-set-key "\C-x\C-k" 'kill-region)

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; (add-to-list 'load-path "~/tech/emacs/color-theme/")
(require 'color-theme)

(setq color-theme-load-all-themes nil)

;; (require 'color-theme-tangotango)

;; (color-theme-tangotango)


;; Org-mode settings
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-c.." 'org-time-stamp)
(global-font-lock-mode 1)

;; Evitar la aparición de eventos ya hechos (DONE).
(setq org-agenda-todo-ignore-scheduled (quote future))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(put 'dired-find-alternate-file 'disabled nil)

(defun toggle-fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
)
(toggle-fullscreen)