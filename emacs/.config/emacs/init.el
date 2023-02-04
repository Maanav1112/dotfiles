;; Bootstrap straight.el
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
(straight-use-package 'use-package)
(setq package-enable-at-startup nil)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(straight-use-package 'org)

;; UI Improvements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)


(use-package amx
  :straight t
 )

(use-package general
  :straight t
  :config
  (general-create-definer leader-keys
    :keymaps '(normal)
    :prefix "SPC"
    :global-prefix "C-SPC"))

(leader-keys
  "."  'find-file
 )
;; Centaur tabs
(use-package dashboard
  :straight t
  :ensure t
   :init      ;; tweak dashboard config before loading it
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")
  (setq dashboard-startup-banner "~/.config/emacs/Marshmallow.png")
  (setq dashboard-center-content t) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 5)
                          (projects . 5)
                          (registers . 5)))
  :config
  (dashboard-setup-startup-hook))

(use-package evil-lion
  :straight t
  :ensure t
  :config
  (evil-lion-mode))
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
(use-package evil-tutor
  :straight t
  )
(use-package minimap
  :straight t
  :ensure t)
(use-package evil-commentary
  :straight t
  )
(evil-commentary-mode)
(use-package evil-escape
  :straight t
  :init
 )
(setq-default evil-escape-key-sequence "jk")
(evil-escape-mode 1)

(set-face-attribute 'default nil
  :font "TerminessTTF Nerd Font"
  :height 95
  :weight 'medium)
(set-face-attribute 'variable-pitch nil
  :font "TerminessTTF Nerd Font"
  :height 95
  :weight 'medium)
(set-face-attribute 'fixed-pitch nil
  :font "TerminessTTF Nerd Font"
  :height 90
  :weight 'medium)
;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)
;; Zooming in and out
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)


;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 1.7)

(use-package all-the-icons
  :straight t
  )

;; Doom modeline
(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)
  :config 
  (setq doom-modeline-time t)
  (display-time-mode 1)
  (setq doom-modeline-battery t)
  (display-battery-mode 1)
  )
(setq doom-modeline-height 39)

(use-package counsel
  :straight t
  :after ivy
  :config (counsel-mode))
(use-package ivy
  :straight t
  :defer 0.1
  :diminish
  :bind
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :custom
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode))
(use-package ivy-rich
  :straight t
  :init
  (ivy-rich-mode 1))
(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
(use-package all-the-icons-ivy
  :straight t
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))
(setq all-the-icons-ivy-buffer-commands '())
(use-package swiper
  :straight t
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

;; Org mode
(add-to-list 'load-path "/home/maanav/.config/emacs/lisp")
(setq org-agenda-files '("~/notes"))
(require 'org-superstar)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
(setq org-hide-emphasis-markers t)
(font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
(require 'ox-latex)
(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))
(add-to-list 'org-latex-classes
             '("book"
               "\\documentclass{book}"
               ("\\part{%s}" . "\\part*{%s}")
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
             )
(use-package org-roam
    :ensure t
)
(setq org-roam-directory (file-truename "~/notes"))
(setq org-roam-mode-sections
      (list #'org-roam-backlinks-section
            #'org-roam-reflinks-section
            ;; #'org-roam-unlinked-references-section
            ))
(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))
(org-roam-setup) 
(setq org-roam-completion-everywhere t)
(org-roam-db-autosync-mode)
(use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-modeorg-roam-ui)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))
(add-to-list  'load-path "~/.config/emacs/lisp/")
(use-package markdown-mode
  :straight t
)
(require 'md-roam)
(setq org-roam-file-extensions '("org" "md")) ; enable Org-roam for a markdown extension
(md-roam-mode 1) ; md-roam-mode needs to be active before org-roam-db-sync
(setq md-roam-file-extension "md") ; Default is "md". Specify an extension such as "markdown"



(use-package org-fancy-priorities
  :straight t
  :ensure t
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))
;; Org mode kotkeys
(leader-keys
  "o e" 'org-export-dispatch
  "o d t" 'org-display-inline-images
)
(use-package org-journal
  :ensure t
  :defer t
  :init
  ;; Change default prefix key; needs to be set before loading org-journal
  (setq org-journal-prefix-key "SPC J")
  :config
  (setq org-journal-dir "~/notes/journal/"
        org-journal-date-format "%A, %d %B %Y"))

;; Themes
;; (add-to-list 'custom-theme-load-path "~/.config/emacs/themes")
(use-package nord-theme
  :straight t
)
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes")
;; (load-theme 'nord t)
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-nord t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-nord") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Projectile
(use-package projectile
  :straight t
  :config
  (projectile-global-mode 1))

(column-number-mode)
(global-display-line-numbers-mode 'relative)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package elisp-refs
  :straight t
 )
(require 'helpful)

;; Solaire mode
(use-package solaire-mode
  :straight t
)
(solaire-global-mode +1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" "e3b2bad7b781a968692759ad12cb6552bc39d7057762eefaf168dbe604ce3a4b" "7ea883b13485f175d3075c72fceab701b5bf76b2076f024da50dff4107d0db25" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "bc0d2f14cbf4514e5ff1de67bbe93ac6c6c182ffdbaf8beb87aed5e0edb31683" "467dc6fdebcf92f4d3e2a2016145ba15841987c71fbe675dcfe34ac47ffb9195" default))
 '(warning-suppress-log-types '(((evil-collection)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
