(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'org)
(straight-use-package 'use-package)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode 1)
(global-visual-line-mode t)

(use-package pdf-tools
:straight t
)

(use-package visual-fill-column
  :straight t
  :commands visual-fill-column-mode
  :custom
  (fill-column-enable-sensible-window-split t)
  :bind
  (("C-x p" . 'visual-fill-column-mode)))


(straight-use-package
 '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))
(require 'nano)
(require 'nano-layout)
(require 'nano-colors)
(require 'nano-theme)
(nano-theme)
(require 'nano-defaults)
(require 'nano-session)
(require 'nano-modeline)
(require 'nano-bindings)
(let ((inhibit-message t))
  (message "Welcome to GNU Emacs / N Λ N O edition")
  (message (format "Initialization time: %s" (emacs-init-time))))

(require 'nano-splash)
(require 'nano-help)
(use-package evil-commentary 
:straight t
)
(evil-commentary-mode)
(use-package evil
  :straight t
    :init      ;; tweak evil's configuration before loading it
    (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
    (setq evil-want-keybinding nil)
    (setq evil-vsplit-window-right t)
    (setq evil-split-window-below t)
    (evil-mode))
  (use-package evil-collection
    :straight t
    :after evil
    :config
    (setq evil-collection-mode-list '(dashboard dired ibuffer))
    (evil-collection-init))

(use-package company
  :straight t
  :defer 2
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t))
;; Or if you use use-package
(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook))
(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
(setq dashboard-center-content t)
(setq dashboard-startup-banner "/home/maanav/.config/emacs/358760.jpg")

(use-package company-box
  :straight t
  :after company
  :diminish
  :hook (company-mode . company-box-mode))

(set-face-attribute 'default nil
  :font "JetBrainsMono NF"
  :height 100
  :weight 'medium)
(set-face-attribute 'variable-pitch nil
  :font "JetBrainsMono NF"
  :height 100
  :weight 'medium)
(set-face-attribute 'fixed-pitch nil
  :font "JetBrainsMono NF"
  :height 100
  :weight 'medium)
;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

;; This sets the default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; are not right unless I also add this method of setting the default font.
(add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font-10"))

;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.5)

(use-package general
  :straight t
  :config
  (general-evil-setup)

  ;; set up 'SPC' as the global leader key
  (general-create-definer dt/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode

  (dt/leader-keys
    "." '(find-file :wk "Find file")
    "b" '(:ignore t :wk "buffer")
    "bb" '(switch-to-buffer :wk "Switch buffer")
    "bk" '(kill-this-buffer :wk "Kill this buffer")
    "bn" '(next-buffer :wk "Next buffer")
    "bp" '(previous-buffer :wk "Previous buffer")
    "br" '(revert-buffer :wk "Reload buffer"))
)
  (dt/leader-keys
    "gc" '(:ignore t :wk "Evil commentary")
    "c" '(evil-commentary-open-line :wk "Org journal current")
)


(dt/leader-keys
    "m" '(:ignore t :wk "Org")
    "m j" '(org-journal-open-current-journal-file :wk "Org journal current")
)

  (dt/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "t e" '(eshell-toggle :wk "Toggle eshell")
    "t f" '(flycheck-mode :wk "Toggle flycheck")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "t n" '(neotree-toggle :wk "Toggle neotree file viewer")
    "t r" '(rainbow-mode :wk "Toggle rainbow mode")
    "t t" '(visual-line-mode :wk "Toggle truncated lines")
    "t v" '(vterm :wk "Toggle vterm"))

(use-package toc-org
  :straight t
    :commands toc-org-enable
    :init (add-hook 'org-mode-hook 'toc-org-enable))


(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets
  :straight t
)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(use-package org-journal
  :straight t
)
(setq org-journal-dir "~/notes/journal/")
(setq org-journal-date-format "%A, %d %B %Y")


(use-package which-key
  :straight t
  :init
    (which-key-mode 1)
  :config
  (setq which-key-side-window-location 'bottom
	  which-key-sort-order #'which-key-key-order-alpha
	  which-key-sort-uppercase-first nil
	  which-key-add-column-padding 1
	  which-key-max-display-columns nil
	  which-key-min-display-lines 6
	  which-key-side-window-slot -10
	  which-key-side-window-max-height 0.25
	  which-key-idle-delay 0.8
	  which-key-max-description-length 25
	  which-key-allow-imprecise-window-fit t
	  which-key-separator " → " ))

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1))

(setq doom-modeline-height 40)

(electric-indent-mode -1)
(require 'org-tempo)
(defun reload-init-file ()
  (interactive)
  (load-file user-init-file)
  (load-file user-init-file))
(setq custom-file (expand-file-name "customs.el" user-emacs-directory))
(add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))

(use-package counsel
  :straight t
  :after ivy
  :diminish
  :config (counsel-mode))

(use-package ivy
  :straight t
  :bind
  ;; ivy-resume resumes the last Ivy-based completion.
  (("C-c C-r" . ivy-resume)
   ("C-x B" . ivy-switch-buffer-other-window))
  :diminish
  :custom
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode))

(use-package all-the-icons-ivy-rich
  :straight t
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :straight t
  :after ivy
  :ensure t
  :init (ivy-rich-mode 1) ;; this gets us descriptions in M-x.
  :custom
  (ivy-virtual-abbreviate 'full
   ivy-rich-switch-buffer-align-virtual-buffer t
   ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))

(add-to-list 'default-frame-alist '(alpha-background . 100)) ; For all new frames henceforth

(use-package vterm
  :straight t
:config
(setq shell-file-name "/bin/sh"
      vterm-max-scrollback 5000))

(use-package rainbow-mode
  :straight t
  :diminish
  :hook org-mode prog-mode)

(straight-use-package 'catppuccin-theme)
;;(load-theme 'catppuccin :no-confirm)
;;(setq catppuccin-flavor 'macchiato)
;;(catppuccin-reload)

(use-package autothemer :straight t :ensure t)

(straight-use-package
 '(pinerose-emacs
   :host github
   :repo "konrad1977/pinerose-emacs"
   :branch "main"))
;;(load-theme 'rose-pine t)

(use-package treemacs
  :straight t
)

(setq calendar-week-start-day 1)
(add-to-list 'org-agenda-files "~/notes/school.org")

(use-package dired-open
  :straight t
  :config
  (setq dired-open-extensions '(("gif" . "sxiv")
                                ("jpg" . "sxiv")
                                ("png" . "sxiv")
                                ("mkv" . "mpv")
                                ("mp4" . "mpv"))))

(use-package peep-dired
  :straight t
  :after dired
  :hook (evil-normalize-keymaps . peep-dired-hook)
  :config
    (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
    (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file) ; use dired-find-file instead if not using dired-open package
    (evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file)
    (evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file)
)

(defun my-org-faces ()
    (set-face-attribute 'org-todo nil :height 0.8)
    (set-face-attribute 'org-level-1 nil :height 1.8)
    (set-face-attribute 'org-level-2 nil :height 1.6)
    (set-face-attribute 'org-level-3 nil :height 1.4)
)

(add-hook 'org-mode-hook #'my-org-faces)
(setq org-default-notes-file (concat org-directory "/notes/inbox.org"))

(use-package autothemer 
:straight t
)
