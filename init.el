
;;;;;;;;;;;;;;;;;;;;;;; Packages ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configurer les archives de packages pour MELPA, GNU et Nongnu
(require 'package)

(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")   ;; Packages officiels GNU
        ("melpa"  . "https://melpa.org/packages/")      ;; Dernières versions des packages
        ("nongnu" . "https://elpa.nongnu.org/nongnu/"))) ;; Packages Nongnu

;; Initialiser les archives de packages
(package-initialize)

;; Mettre à jour les archives si elles ne le sont pas déjà
(unless package-archive-contents
  (package-refresh-contents))

;; Installer use-package si ce n'est pas encore fait
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)  ;; Toujours installer les packages automatiquement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun package--save-selected-packages (&rest opt) nil)
(org-babel-load-file (concat user-emacs-directory "config_kas.org"))

