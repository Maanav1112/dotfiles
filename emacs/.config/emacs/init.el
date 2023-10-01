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

(use-package company-box
  :straight t
  :after company
  :diminish
  :hook (company-mode . company-box-mode))

(set-face-attribute 'default nil
  :font "scientifica"
  :height 120
  :weight 'medium)
(set-face-attribute 'variable-pitch nil
  :font "scientifica"
  :height 120
  :weight 'medium)
(set-face-attribute 'fixed-pitch nil
  :font "scientifica"
  :height 120
  :weight 'medium)
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)
(add-to-list 'default-frame-alist '(font . "scientifica-12"))
(setq-default line-spacing 1.50)

(use-package dashboard
  :straight t
  :init      ;; tweak dashboard config before loading it
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")
  (setq dashboard-center-content t) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 3)
                          (registers . 3)))
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text")
			      (bookmarks . "book"))))
(setq dashboard-startup-banner nil)
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
(use-package general
  :straight t
  :config
  (general-evil-setup)
  (general-create-definer leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode
  (leader-keys
    "." '(find-file :wk "Find file")
    "b" '(:ignore t :wk "buffer")
    "bb" '(switch-to-buffer :wk "Switch buffer")
    "bk" '(kill-this-buffer :wk "Kill this buffer")
    "bn" '(next-buffer :wk "Next buffer")
    "bp" '(previous-buffer :wk "Previous buffer")
    "br" '(revert-buffer :wk "Reload buffer"))
)
(leader-keys
        "n" '(:ignore t :wk "org roam")
        "n b" '(org-roam-buffer-toggle :wk "Org roam buffer toggle")
        "n f" '(org-roam-node-find :wk "Org roam buffer toggle")
        "n g" '(org-roam-graph :wk "Org roam graph")
        "n i" '(org-roam-node-insert :wk "Org roam insert")
        "n c" '(org-roam-capture :wk "Org roam capture")
        "n j" '(org-roam-dailies-capture-today :wk "Org roam dailies")
)
(leader-keys
    "m" '(:ignore t :wk "Org journal")
    "m j" '(org-journal-open-current-journal-file :wk "Org journal current")
)

(leader-keys
    "o" '(:ignore t :wk "Org" )
    "o j" '(helm-org-rifle :wk "Org rifle")
)
  (leader-keys
    "t" '(:ignore t :wk "Toggle")
    "t e" '(eshell-toggle :wk "Toggle eshell")
    "t f" '(flycheck-mode :wk "Toggle flycheck")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "t n" '(neotree-toggle :wk "Toggle neotree file viewer")
    "t r" '(rainbow-mode :wk "Toggle rainbow mode")
    "t t" '(visual-line-mode :wk "Toggle truncated lines")
    "t v" '(vterm :wk "Toggle vterm"))

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

(use-package doom-themes
  :straight t
  :config
  (setq doom-themes-enable-bold t    
        doom-themes-enable-italic t) 
  (load-theme 'doom-gruvbox t)
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

(use-package autothemer :straight t :ensure t)
(use-package treemacs
  :straight t
)

(setq calendar-week-start-day 1)
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
;; Org mode
(use-package org-roam
  :straight t
 )
(setq org-roam-directory (file-truename "~/notes/"))
(org-roam-db-autosync-mode)
(setq find-file-visit-truename t)
(setq org-roam-dailies-directory "~/notes/journal/")
(use-package org-bullets 
:straight t
)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(use-package helm-org-rifle
:straight t
)
(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.7))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.6))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.5))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.4))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.3))))
 '(org-level-6 ((t (:inherit outline-5 :height 1.2))))
 '(org-level-7 ((t (:inherit outline-5 :height 1.1)))))
(setq backup-directory-alist '((".*" . "~/.local/share/Trash/files")))
(add-to-list 'org-agenda-files "~/NEET.org")

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
