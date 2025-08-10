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

;; lsp-mode for language server support
(use-package lsp-mode
  :ensure t
  :hook ((python-mode . lsp)
         (c-mode . lsp)
         (c++-mode . lsp)
         (rust-mode . lsp)
         (dart-mode . lsp)
         (js-mode . lsp)
         (typescript-mode . lsp))
  :commands lsp)

;; company-mode for autocompletion
(use-package company
  :ensure t
  :hook (after-init . global-company-mode))

;; Indentation and tab settings per language
(add-hook 'python-mode-hook
          (lambda () (setq tab-width 4 indent-tabs-mode nil)))

(add-hook 'c-mode-hook
          (lambda () (setq c-basic-offset 2 indent-tabs-mode t)))

(add-hook 'c++-mode-hook
          (lambda () (setq c-basic-offset 2 indent-tabs-mode t)))

(add-hook 'rust-mode-hook
          (lambda () (setq tab-width 4 indent-tabs-mode nil)))

(add-hook 'dart-mode-hook
          (lambda () (setq tab-width 2 indent-tabs-mode nil)))

(add-hook 'js-mode-hook
          (lambda () (setq tab-width 2 indent-tabs-mode nil)))

(add-hook 'typescript-mode-hook
          (lambda () (setq tab-width 2 indent-tabs-mode nil)))


;; line and tag indications
(which-function-mode 1)
(add-hook 'prog-mode-hook #'lsp-headerline-breadcrumb-mode)
t
(semantic-mode 1)

(use-package imenu-list
  :ensure t
  :config
  (setq imenu-list-focus-after-iactivation t)
  (setq imenu-list-auto-resize t)
  (setq imenu-list-smart-toggle t)
  :bind ("C-' i" . imenu-list-smart-toggle))

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

;; tabs
(tab-bar-mode 1)

(require 'ansi-color)

(defun my/ansi-color-shell-filter ()
  "Apply ANSI color codes in shell buffers."
  (ansi-color-apply-on-region comint-last-output-start (point)))

(add-hook 'shell-mode-hook
          (lambda ()
            (add-hook 'comint-output-filter-functions 'my/ansi-color-shell-filter nil)
            (ansi-color-for-comint-mode-on)))


;; Full colors
;; Frame colors (default for new frames)
(add-to-list 'default-frame-alist '(foreground-color . "#FFFFFF")) ;; white text
(add-to-list 'default-frame-alist '(background-color . "#000000")) ;; black background
(add-to-list 'default-frame-alist '(cursor-color . "#FFFF00"))     ;; bright yellow cursor
(add-to-list 'default-frame-alist '(mouse-color . "#00FF00"))      ;; bright green mouse pointer
(add-to-list 'default-frame-alist '(border-color . "#0044AA"))     ;; blueish border

;; Face customizations
(set-face-foreground 'default "#FFFFFF")              ;; normal text white
(set-face-background 'default "#000000")              ;; black background

(set-face-background 'region "#444400")                ;; dark yellow-green selection background
(set-face-foreground 'region "#FFFFAA")                ;; pale yellow selection text

(set-face-foreground 'cursor "#FFFF00")                ;; bright yellow cursor

;; Mode line
(set-face-background 'mode-line "#005500")             ;; dark green mode line bg
(set-face-foreground 'mode-line "#AAFFAA")             ;; light green mode line fg
(set-face-background 'mode-line-inactive "#003300")    ;; darker green inactive mode line bg
(set-face-foreground 'mode-line-inactive "#557755")    ;; muted green inactive fg

;; Syntax highlighting examples
(set-face-foreground 'font-lock-comment-face "#55AA55")  ;; green comments
(set-face-foreground 'font-lock-string-face "#FF5555")   ;; red strings
(set-face-foreground 'font-lock-keyword-face "#FFFF55")  ;; yellow keywords
(set-face-foreground 'font-lock-function-name-face "#5599FF") ;; blue function names
(set-face-foreground 'font-lock-variable-name-face "#AAFFAA") ;; light green vars
(set-face-foreground 'font-lock-type-face "#88FF88")    ;; soft green types
(set-face-foreground 'font-lock-constant-face "#FFAA00") ;; orange constants
(set-face-background 'hl-line "#222200") 

(add-hook 'dired-mode-hook #'auto-revert-mode)
