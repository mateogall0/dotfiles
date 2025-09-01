(xterm-mouse-mode 1)

;; Scroll with mouse wheel in terminal
(global-set-key [mouse-4] (lambda () (interactive) (scroll-down	1)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))


;; Better default settings
(setq inhibit-startup-screen t)       ;; Skip startup screen
(setq ring-bell-function 'ignore)     ;; No beeping
(setq make-backup-files nil)          ;; No ~backup files
(setq auto-save-default nil)          ;; No autosave files
(show-paren-mode 1)                    ;; Highlight matching parentheses
(global-display-line-numbers-mode 1)   ;; Show line numbers

;; Enable clipboard integration (share kill-ring with OS clipboard)
(setq select-enable-clipboard t)
(setq select-enable-primary t) ;; for PRIMARY selection in X11

(when (not (display-graphic-p))
  (defun copy-to-x-clipboard (text &optional push)
    (with-temp-buffer
      (insert text)
      (call-process-region (point-min) (point-max) "xclip" nil 0 nil "-selection" "clipboard")))

  (setq interprogram-cut-function 'copy-to-x-clipboard))

;; Enable line numbers
(global-display-line-numbers-mode 1)

;; Show current line number normally, other lines with relative numbers
(setq display-line-numbers-type 'relative)
;; To show absolute line number for current line and relative for others
;; If using Emacs 26 or newer, this works:
(setq display-line-numbers-current-absolute t)

;; Shorter y/n prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Save open buffers
(global-set-key (kbd "<f5>") 'save-some-buffers)


(defun move-line-up ()
  "Move the current line up by one."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  "Move the current line down by one."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down )


(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-sexp 1))
        ((looking-back "\\s)" 1) (backward-sexp 1))
        (t (self-insert-command (or arg 1)))))

(global-set-key (kbd "C-c %") 'match-paren)

;; Delete selected when pasting
(delete-selection-mode 1)

;; Make Ctrl+w always delete previous word (kill word backward)
(global-set-key (kbd "C-w") 'backward-kill-word)


(defun my-increment-number-at-point (arg)
  "Increment number at point by ARG (default 1)."
  (interactive "p")
  (let ((num-start (point))
        num-end num)
    (skip-chars-backward "0123456789")
    (setq num-start (point))
    (skip-chars-forward "0123456789")
    (setq num-end (point))
    (setq num (string-to-number (buffer-substring-no-properties num-start num-end)))
    (delete-region num-start num-end)
    (insert (number-to-string (+ num arg)))))

(defun my-decrement-number-at-point (arg)
  "Decrement number at point by ARG (default 1)."
  (interactive "p")
  (let ((num-start (point))
        num-end num)
    (skip-chars-backward "0123456789")
    (setq num-start (point))
    (skip-chars-forward "0123456789")
    (setq num-end (point))
    (setq num (string-to-number (buffer-substring-no-properties num-start num-end)))
    (delete-region num-start num-end)
    (insert (number-to-string (- num arg)))))

(global-set-key (kbd "C-c i") 'my-increment-number-at-point)
(global-set-key (kbd "C-c d") 'my-decrement-number-at-point)


;; Basic package setup and bootstrapping use-package
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package company
  :ensure t
  :hook (after-init . global-company-mode))

(which-function-mode 1)
t
(semantic-mode 1)

(use-package imenu-list
  :ensure t
  :config
  (setq imenu-list-focus-after-iactivation t)
  (setq imenu-list-auto-resize t)
  (setq imenu-list-smart-toggle t)
  :bind ("C-' i" . imenu-list-smart-toggle))
(setq company-backends '((company-capf company-dabbrev-code company-files)))


;; hl
(global-hl-line-mode 1)


;; use system terminal colors
(unless (display-graphic-p)
  (setq term-termcap-color-level 24)
  (set-terminal-parameter nil 'background-mode 'dark))


;; move through panes
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <down>") 'windmove-down)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <right>") 'windmove-right)


(require 'ansi-color)

(defun my/ansi-color-shell-filter ()
  "Apply ANSI color codes in shell buffers."
  (ansi-color-apply-on-region comint-last-output-start (point)))

(add-hook 'shell-mode-hook
          (lambda ()
            (add-hook 'comint-output-filter-functions 'my/ansi-color-shell-filter nil)
            (ansi-color-for-comint-mode-on)))


;; Full colors
;; Face customizations
(set-face-foreground 'default "#FFFFFF")              ;; normal text white
(set-face-background 'default "#1F1F1F")

(set-face-background 'region "#444400")                ;; dark yellow-green selection background
(set-face-foreground 'region "#FFFFAA")                ;; pale yellow selection text

(set-face-foreground 'cursor "#FFFF00")                ;; bright yellow cursor

;; Mode line
(set-face-background 'mode-line "#005500")             ;; dark green mode line bg
(set-face-foreground 'mode-line "#AAFFAA")             ;; light green mode line fg
(set-face-background 'mode-line-inactive "#003300")    ;; darker green inactive mode line bg
(set-face-foreground 'mode-line-inactive "#557755")    ;; muted green inactive fg

(set-face-foreground 'font-lock-comment-face "#55AA55")  ;; green comments
(set-face-foreground 'font-lock-string-face "#BD6111")
(set-face-foreground 'font-lock-keyword-face "#FFFF55")  ;; yellow keywords
(set-face-foreground 'font-lock-function-name-face "#5599FF") ;; blue function names
(set-face-foreground 'font-lock-variable-name-face "#66CCCC")
(set-face-foreground 'font-lock-type-face "#88FF88")    ;; soft green types
(set-face-foreground 'font-lock-constant-face "#FFAA00") ;; orange constants
(set-face-background 'hl-line "#303030")

;; Customize faces
(set-face-attribute 'line-number nil
                    :foreground "#777777"  ;; relative line numbers
                    :background nil)

(set-face-attribute 'line-number-current-line nil
                    :foreground "#B3CC00"     ;; current line number
                    :background nil
                    :weight 'bold)


;; dired auto-update
(add-hook 'dired-mode-hook #'auto-revert-mode)

;; set dired flags
(setq dired-listing-switches "-alh --group-directories-first")

(defun reload-init-file ()
  "Reload Emacs init file."
  (interactive)
  (load-file user-init-file))


(global-set-key (kbd "C-c r") 'reload-init-file)

(global-set-key (kbd "<backtab>") 'indent-rigidly-left-to-tab-stop)
(global-set-key (kbd "TAB") 'indent-rigidly-right-to-tab-stop)


(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; hide upper bar
(menu-bar-mode -1)


(unless (package-installed-p 'which-key)
  (package-install 'which-key))

;; Enable
(require 'which-key)
(which-key-mode)
(setq which-key-idle-delay 0.3)  ;; Show quickly


(unless (package-installed-p 'goto-last-change)
  (package-install 'goto-last-change))

(require 'goto-last-change)
(global-set-key (kbd "C-x C-/") 'goto-last-change)
(global-set-key (kbd "C-x C-_") 'goto-last-change-back)


(unless (package-installed-p 'orderless)
  (package-install 'orderless))

(require 'orderless)
(setq completion-styles '(orderless)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))


;; git
(unless (package-installed-p 'magit)
  (package-refresh-contents)
  (package-install 'magit))
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(unless (package-installed-p 'diff-hl)
  (package-refresh-contents)
  (package-install 'diff-hl))
(require 'diff-hl)
(global-diff-hl-mode)
(diff-hl-flydiff-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)


;; Eglot as LSP client
(use-package eglot
  :ensure t
  :hook ((python-mode
          c-mode
          c++-mode
          rust-mode
          go-mode
          typescript-mode
          js-mode
          dart-mode) . eglot-ensure))

(defun my-python-indentation ()
  "Set Python indentation to 4 spaces."
  (setq-local python-indent-offset 4
              indent-tabs-mode nil))

(defun my-js-indentation ()
  "Set JavaScript indentation to 2 spaces."
  (setq-local js-indent-level 2
              tab-width 2
              indent-tabs-mode nil))

(defun my-c-indentation ()
  "Set C/C++ indentation"
  (setq-local c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil))

(add-hook 'dart-mode-hook 'my-js-indentation)
(add-hook 'js-mode-hook 'my-js-indentation)
(add-hook 'js2-mode-hook 'my-js-indentation)
(add-hook 'typescript-mode-hook 'my-ts-indentation)
(add-hook 'python-mode-hook 'my-python-indentation)
(add-hook 'c-mode-hook 'my-c-indentation)
(add-hook 'cpp-mode-hook 'my-c-indentation)

;; Corfu for VSCode-style completions
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode))

(use-package cape
  :ensure t)

;; Icons in completion popup
(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default))

;; Yasnippet for snippet expansions
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

;; Optional company-box for LSP popups (classic style)
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

;; Enable whitespace-mode with tab highlighting
(setq whitespace-style '(face tabs))
(global-whitespace-mode 1)

(custom-set-faces
 '(whitespace-tab ((t (:background "#3E402E")))))
