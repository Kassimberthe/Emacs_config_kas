;;;;;;;;;;;;;;;;;;;;;;; Packages ;;;;;;;;;;;;;;;;;;;;;
(require 'package)

(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("melpa"  . "https://melpa.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Installer et charger general avant toute utilisation
(unless (package-installed-p 'general)
  (package-refresh-contents)
  (package-install 'general))

(require 'general)

;; Charger ton org principal
(org-babel-load-file (concat user-emacs-directory "config_kas.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(dtrt-indent vterm-toggle vterm eshell-syntax-highlighting eshell-toggle git-gutter mu4e-alert org-roam-ui consult-org-roam org-roam org-super-agenda winum visual-fill-column markdown-mode ox-pandoc ess company-irony irony company-c-headers ggtags cmake-mode pyvenv web-mode git-timemachine magit crontab-mode sudo-edit command-log-mode async dired-open dired-hide-dotfiles marginalia consult orderless vertico which-key all-the-icons-dired all-the-icons beacon pulsar toc-org org-superstar catppuccin-theme diminish doom-modeline drag-stuff company-box openwith pdf-tools cdlatex company-auctex auctex diff-hl flycheck rainbow-mode rainbow-delimiters general mu4e undo-tree evil-collection auto-compile)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :height 1.7))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.6))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.5))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.4))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.3))))
 '(org-level-6 ((t (:inherit outline-5 :height 1.2))))
 '(org-level-7 ((t (:inherit outline-5 :height 1.1)))))
