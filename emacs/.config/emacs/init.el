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

(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 2.0))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.8))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.6))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.4))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
  )

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)
(global-visual-line-mode t)

(straight-use-package
 '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))

 ;;(use-package doom-themes
;;:straight t
;;:config
;;(setq doom-themes-enable-bold t)    
;;(setq doom-themes-enable-italic t) 
;;(load-theme 'doom-gruvbox t)
;;(doom-themes-visual-bell-config)
;;(doom-themes-org-config))

(use-package doom-modeline
:straight t
:init (doom-modeline-mode 1))
(setq doom-modeline-height 40)

(use-package annalist
:straight t
)

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
(setq-default line-spacing 0.5)
(add-to-list 'default-frame-alist '(font . "scientifica-12"))
(setq global-prettify-symbols-mode t)

(use-package visual-fill-column
  :straight t
  :commands visual-fill-column-mode
  :custom
  (fill-column-enable-sensible-window-split t)
  :bind
  (("C-x p" . 'visual-fill-column-mode)))
(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)


(use-package evil
  :straight t
    :init      ;; tweak evil's configuration before loading it
    (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
    (setq evil-want-keybinding nil)
    (setq evil-vsplit-window-right t)
    (setq evil-split-window-below t)
    (evil-mode))

(use-package evil-nerd-commenter
  :straight t
)

(use-package nerd-icons
  :straight t
  )

(use-package dashboard
  :straight t 
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-startup-banner "~/Downloads/365030(1).jpg")  ;; use custom image as banner
  (setq dashboard-center-content nil) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 3)
                          (projects . 3)
                          (registers . 3)))
  :custom 
  (dashboard-modify-heading-icons '((recents . "file-text")
				      (bookmarks . "book")))
  :config
  (dashboard-setup-startup-hook))


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
    "bb" '(ibuffer :wk "ibuffer")
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

(use-package rainbow-mode
  :straight t
  :diminish
  :hook org-mode prog-mode)

(use-package org-modern
:straight t
)
(with-eval-after-load 'org (global-org-modern-mode))

;; Add frame borders and window dividers
(modify-all-frames-parameters
 '((right-divider-width . 40)
   (internal-border-width . 40)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-ellipsis "…"

 ;; Agenda styling
 org-agenda-tags-column 0
 org-agenda-block-separator ?─
 org-agenda-time-grid
 '((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
 org-agenda-current-time-string
 "⭠ now ─────────────────────────────────────────────────")

(global-org-modern-mode)

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
(setq org-roam-file-extensions '("org" "md")) ; enable Org-roam for a markdown extension
(org-roam-db-autosync-mode 1) ; autosync-mode triggers db-sync. md-roam-mode must be already active
(setq find-file-visit-truename t)
(setq org-roam-dailies-directory "~/notes/journal/")

(use-package helm-org-rifle
:straight t
)
(setq backup-directory-alist '((".*" . "~/.local/share/Trash/files")))
(add-to-list 'org-agenda-files "~/NEET.org")

(use-package toc-org
  :straight t
    :commands toc-org-enable
    :init (add-hook 'org-mode-hook 'toc-org-enable))

(use-package org-journal
  :straight t
)
(setq org-journal-dir "~/notes/journal/")
(setq org-journal-date-format "%A, %d %B %Y")

(use-package markdown-mode
:straight t
:init (setq markdown-command "multimarkdown")
)

(use-package org-download
  :after org
  :straight t
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "~/notes/Images")
  (org-download-heading-lvl 0)
  (org-download-timestamp "org_%Y%m%d-%H%M%S_")
  (org-image-actual-width 600)
  (org-download-screenshot-method "xclip -selection clipboard -t image/png -o > '%s'")
  :bind
  ("C-M-y" . org-download-screenshot)
  :config
  (require 'org-download))

(use-package nerd-icons-ibuffer
  :straight t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package highlight-indent-guides
:straight t
)
(setq highlight-indent-guides-method 'bitmap)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

(use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(straight-use-package 'catppuccin-theme)
(load-theme 'catppuccin :no-confirm)
(setq catppuccin-flavor 'mocha) ;; or 'latte, 'macchiato, or 'mocha
