(setq user-full-name "Kassim Berthé")
(setq user-mail-address "berthekassime@gmail.com")

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package auto-compile
  :demand t
  :config (auto-compile-on-load-mode))

(setq load-prefer-newer t)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs #'y-or-n-p)

(use-package undo-tree
  :demand t
  :init
  (global-undo-tree-mode))  ;; Active `undo-tree` globalement

(use-package evil
  :demand t
  :after undo-tree  ;; S'assure que `undo-tree` est chargé avant
  :init
  (setq evil-respect-visual-line-mode t   ;; Respecte le mode `visual-line-mode`
        evil-undo-system 'undo-tree      ;; Utilise `undo-tree` pour l'historique
        evil-want-abbrev-expand-on-insert-exit nil
        evil-want-keybinding nil)        ;; Prépare pour des extensions comme `evil-collection`
  
  :config
  (evil-mode 1)  ;; Active `evil-mode`

  ;; Exemple de mappage global
  (evil-define-key '(normal insert) 'global (kbd "C-p") 'project-find-file)

  ;; Assurez-vous que les mappages pour `org-mode` fonctionnent seulement si `org` est chargé
  (with-eval-after-load 'org
    (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)
    (evil-define-key 'insert org-mode-map (kbd "S-<right>") 'org-shiftright)
    (evil-define-key 'insert org-mode-map (kbd "S-<left>") 'org-shiftleft))

  ;; Empêche `evil` de modifier la sélection visuelle
  (fset 'evil-visual-update-x-selection 'ignore))

(use-package evil-collection
  :after evil
  :demand t

  :config
  (setq evil-collection-mode-list
        '(comint
          deadgrep
          dired
          ediff
          elfeed
          eww
          ibuffer
          info
          magit
          mu4e
          package-menu
;          pdf-view
          proced
          replace
          vterm
          which-key))

  (evil-collection-init))

(add-hook 'org-mode-hook #'org-indent-mode)

(electric-pair-mode 1)

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (clojure-mode . rainbow-delimiters-mode)))

(use-package rainbow-mode
  :diminish
  :hook org-mode prog-mode)

(add-hook 'text-mode-hook #'hl-line-mode)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'org-agenda-finalize-hook #'hl-line-mode)

(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(add-hook 'elfeed-show-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'eshell-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'shell-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'eww-mode-hook (lambda () (display-line-numbers-mode -1)))

(show-paren-mode 1)

(use-package flycheck
  :diminish 'flycheck-mode
  :config (setq-default flycheck-emacs-lisp-load-path 'inherit)
  :init (global-flycheck-mode))

(setq-default ispell-program-name "aspell")
(setq ispell-list-command "--list")

(use-package diff-hl
  :config
  (global-diff-hl-mode t)
  :hook
  (magit-post-refresh-hook . diff-hl-magit-post-refresh))

(setq diff-hl-change-color "green")   ;; Couleur pour les modifications (maintenant vert)
(setq diff-hl-delete-color "red")     ;; Couleur pour les suppressions
(setq diff-hl-insert-color "magenta")    ;; Couleur pour les ajouts (maintenant magenta)

(use-package auctex
  :defer t
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-engine 'luatex)
  (setq-default TeX-master nil))

(use-package company-auctex
  :after (auctex company)         ;; Charger après AUCTeX et company
  :ensure t                       ;; S’assurer que le paquet est installé
  :config
  (company-auctex-init))         ;; Initialiser company-auctex pour compléter les macros, environnements, etc.

(use-package cdlatex
  :ensure t                                  ;; S'assurer que cdlatex est installé
  :diminish org-cdlatex-mode                 ;; Ne pas afficher le mode mineur dans la barre de mode
  :hook ((LaTeX-mode . turn-on-cdlatex)      ;; Activer cdlatex en LaTeX-mode
         (org-mode . turn-on-org-cdlatex))   ;; Activer cdlatex en org-mode
  :config
  (setq cdlatex-use-dollar-to-ensure-math nil)) ;; Option : éviter les conflits avec les dollars dans org

(use-package ox-latex
  :ensure nil                                        ;; ox-latex fait déjà partie de Org-mode
  :after org
  :commands (org-export-dispatch)
  :ensure-system-package latexmk                    ;; Vérifie que latexmk est installé

  :custom
  ;; Utiliser minted pour les blocs de code (avec syntax highlighting)
  (org-latex-src-block-backend 'minted)

  ;; Commande de compilation avec latexmk, en xelatex, avec shell-escape (requis pour minted)
  (org-latex-pdf-process
   '("latexmk -xelatex -shell-escape -quiet -f %f"))

  ;; Ajouter une commande LaTeX dans le préambule pour définir une couleur (ex : fond transparent gris clair)
  (org-latex-header "\\definecolor{lightgraytransparent}{rgb}{0.9, 0.9, 0.9}\n")

  ;; Ajouter des packages utiles au document exporté depuis Org vers LaTeX
  (org-latex-packages-alist
   '(("" "minted")                                     ;; Code coloré avec minted
     ("" "booktabs")                                   ;; Tableaux professionnels
     ("AUTO" "polyglossia" t ("xelatex" "lualatex"))   ;; Multilingue, alternatif à babel
     ("" "grffile")                                    ;; Noms de fichiers complexes dans \includegraphics
     ;; ("" "unicode-math")                            ;; Décommente si tu veux de meilleures fontes mathématiques
     ("" "xcolor")))                                   ;; Gestion des couleurs

  :config
  ;; Ajouter .tex à la liste des fichiers à supprimer après export
  (add-to-list 'org-latex-logfiles-extensions "tex"))

(use-package auctex
  :ensure t
  :defer t
  :after tex
  :config
  (add-to-list 'TeX-command-list
               '("Latexmk with shell-escape"
                 "latexmk -xelatex -shell-escape -interaction=nonstopmode -f %s"
                 TeX-run-TeX nil t))
  (setq TeX-command-default "Latexmk with shell-escape"))

(use-package ox-beamer
  :ensure nil                  ;; ox-beamer est inclus avec org-mode, donc inutile de l'installer
  :after ox-latex              ;; Charger après ox-latex (export LaTeX)
  :config
  ;; Tu peux ici personnaliser la classe Beamer si nécessaire
  ;; (par exemple, ajouter une classe personnalisée)
  ;; (add-to-list 'org-latex-classes
  ;;              '("beamer"
  ;;                "\\documentclass[presentation]{beamer}"
  ;;                ("\\section{%s}" . "\\section*{%s}")))
  )

(use-package pdf-tools
  :ensure t
  :defer t
  :config
  (pdf-tools-install))

;; Sélection du viewer selon le type d'affichage
(add-hook 'after-init-hook
          (lambda ()
            (with-eval-after-load 'tex
              (if (display-graphic-p)
                  ;; mode graphique : pdf-tools
                  (progn
                    (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
                    (setq TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
                    (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))
                ;; mode terminal / non-graphique : evince
                (setq TeX-view-program-selection '((output-pdf "Evince")))
                (setq TeX-view-program-list '(("Evince" "evince %o")))))))

;; Liste des extensions de fichiers générés par LaTeX à supprimer automatiquement après export Org->PDF
(setq org-latex-logfiles-extensions
      '("lof"          ;; List of Figures
        "lot"          ;; List of Tables
        "tex~"         ;; Fichier tex sauvegardé temporairement
        "aux"          ;; Fichier auxiliaire
        "idx"          ;; Index
        "log"          ;; Journal de compilation
        "out"          ;; Fichier de sortie auxiliaire
        "toc"          ;; Table des matières
        "nav"          ;; Navigation pour beamer
        "snm"          ;; Slideshow notes for beamer
        "vrb"          ;; Verbose log
        "dvi"          ;; Fichier DVI
        "fdb_latexmk"  ;; Fichier de suivi latexmk
        "blg"          ;; Bibliographie bibtex log
        "brf"          ;; Bibliographie
        "fls"          ;; Fichier de dépendances latex
        "entoc"        ;; ?
        "ps"           ;; Postscript
        "spl"          ;; ?
        "bbl"))        ;; Bibliographie bbl

(use-package pdf-tools
  :if (not (eq system-type 'windows-nt))
  :config
  (pdf-loader-install)
  ;; Ouvrir les PDF ajustés pour tenir sur la page
  (setq-default pdf-view-display-size 'fit-page)
  ;; Zoom plus précis avec un facteur de 1.1
  (setq pdf-view-resize-factor 1.1)
  ;; Utiliser la recherche standard d'Emacs
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (define-key pdf-view-mode-map (kbd "C-r") 'isearch-backward)
  ;; Raccourcis clavier pour les annotations
  (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation) ;; Ajouter une surbrillance
  (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)             ;; Ajouter une annotation texte
  (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete))                         ;; Supprimer une annotation

(use-package company
  :ensure t
  :config
  (setq company-tooltip-align-annotations t)
  (add-hook 'after-init-hook 'global-company-mode) ;; actif partout

  (add-to-list 'company-frontends 'company-tng-frontend)

  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map [tab] 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)

  (define-key company-mode-map (kbd "C-<SPC>") 'company-complete))

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode)
  :diminish)

(setq org-hide-emphasis-markers t)

(use-package drag-stuff
  :ensure t
  :config
  (drag-stuff-global-mode 1)

  ;; Raccourcis personnalisés pour evil-mode
  (with-eval-after-load 'evil
    (evil-define-key 'normal drag-stuff-mode-map
      (kbd "M-j") 'drag-stuff-down
      (kbd "M-k") 'drag-stuff-up)
    (evil-define-key 'visual drag-stuff-mode-map
      (kbd "M-j") 'drag-stuff-down
      (kbd "M-k") 'drag-stuff-up))
  )

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 35        ;; Hauteur de la barre de mode
        doom-modeline-bar-width 5      ;; Largeur de la barre droite
        doom-modeline-persp-name t     ;; Affiche le nom de la perspective
        doom-modeline-persp-icon t))   ;; Affiche une icône de dossier près du nom

(use-package diminish
  :init
  (diminish 'abbrev-mode)
  (diminish 'buffer-face-mode)
  (diminish 'flyspell-mode)
  (diminish 'org-indent-mode)
  (diminish 'org-cdlatex-mode)
  (diminish 'visual-line-mode)
  (diminish 'buffer-face-mode)
  (diminish 'highlight-indent-guides-mode)
  (diminish 'eldoc-mode)
  (diminish 'subword-mode))
"Diminish configuration applied successfully"

;; Définir le répertoire pour les thèmes personnalisés
(setq custom-theme-directory
      (concat user-emacs-directory "themes"))

;; Charger un thème personnalisé si nécessaire
;; (load-theme 'witchhazel t)

;; Utiliser le thème Catppuccin avec la saveur 'macchiato'
(use-package catppuccin-theme
  :demand t
  :custom
  (catppuccin-flavor 'macchiato)  ;; Options disponibles : 'latte, 'frappe, 'macchiato, 'mocha

  :config
  (catppuccin-reload))  ;; Recharge la configuration du thème

(eval-after-load 'org-indent '(diminish 'org-indent-mode))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-headline-bullets-list '("✸" "✿" "◆" "◉" "✯"))  ; Puces pour les titres
  (org-superstar-item-bullet-alist '((?* . "•")      ; Puce pour * list
                                     (?+ . "➤")      ; Puce pour + list
                                     (?1 . "➀")      ; Puce pour 1 list
                                     (?2 . "❖")      ; Puce pour 2 list
                                     (?3 . "☀")      ; Puce pour 3 list
                                     (?4 . "◆"))))    ; Puce pour 4 list

(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.7))))  ; Niveau 1 : Taille 1.7
  '(org-level-2 ((t (:inherit outline-2 :height 1.6))))  ; Niveau 2 : Taille 1.6
  '(org-level-3 ((t (:inherit outline-3 :height 1.5))))  ; Niveau 3 : Taille 1.5
  '(org-level-4 ((t (:inherit outline-4 :height 1.4))))  ; Niveau 4 : Taille 1.4
  '(org-level-5 ((t (:inherit outline-5 :height 1.3))))  ; Niveau 5 : Taille 1.3
  '(org-level-6 ((t (:inherit outline-5 :height 1.2))))  ; Niveau 6 : Taille 1.2
  '(org-level-7 ((t (:inherit outline-5 :height 1.1))))) ; Niveau 7 : Taille 1.1

(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(setq org-ellipsis "⤵")  ; Flèche pointant vers le bas pour symboliser le dépliage
(setq org-startup-folded 'content)  ; Plier le contenu par défaut

(setq save-place-forget-unreadable-files nil)  ; Conserver la position même pour les fichiers illisibles
(save-place-mode 1)  ; Activer le mode de sauvegarde de la position

;; Chargement et configuration du package 'pulsar'
(use-package pulsar
  :ensure t ;; Assure que le package est installé s'il ne l'est pas
  :bind ("<f8>" . pulsar-pulse-line)) ;; Associe la touche F8 à la commande 'pulsar-pulse-line'

(use-package all-the-icons
  :if (display-graphic-p)
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package which-key
  :demand t ;; Charge immédiatement `which-key` au démarrage d'Emacs
  :config
  ;; Active le mode `which-key` pour afficher les raccourcis clavier disponibles
  (which-key-mode))

(use-package vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter) ;; Confirmation dans un répertoire
              ("DEL" . vertico-directory-delete-char) ;; Suppression d'un caractère
              ("M-DEL" . vertico-directory-delete-word)) ;; Suppression d'un mot entier

  :init
  (vertico-mode))

(use-package savehist
  :demand t
  :init
  (savehist-mode))

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :bind
  (("M-i" . consult-imenu) ;; Accède à la liste des fonctions dans le buffer
   ("C-x b" . consult-buffer) ;; Liste des buffers ouverts
   ("C-x r b" . consult-bookmark) ;; Recherche parmi les signets
   ("C-s" . consult-line)) ;; Recherche dans la ligne actuelle
  :config
  (setq completion-in-region-function #'consult-completion-in-region))

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)) ;; Permet de changer le type d'affichage dans la mini-buffer
  :init
  (marginalia-mode))

;; Chargement du paquet `dired`, qui est un mode natif d'Emacs pour naviguer dans les répertoires.
  (use-package dired
    :demand t  ;; Force l'initialisation immédiate du paquet (utile si dired n'est pas déjà activé)
    :ensure nil  ;; Indique que `dired` est intégré à Emacs, donc pas besoin de le télécharger.
  
    ;; Activation du mode `undo-tree` automatiquement lors de l'ouverture de Dired.
    :hook (dired-mode . (lambda () (undo-tree-mode 1)))  ;; Lance `undo-tree-mode` dans Dired pour la gestion de l'historique.

    :config
    ;; Fonction pour démarrer un diaporama dans le répertoire courant de Dired avec la commande `s`.
    (defun +dired-slideshow ()
      "Démarre un diaporama dans le répertoire courant de Dired en utilisant la commande `s`."
      (interactive)
      (let ((dir (dired-current-directory)))  ;; Récupère le répertoire actuel de Dired
        (if dir
            (start-process "dired-slideshow" nil "s" dir)  ;; Lance un processus pour démarrer le diaporama.
          (message "No directory found")))  ;; Affiche un message d'erreur si aucun répertoire n'est trouvé.

    ;; Définition des raccourcis clavier pour Dired en mode normal (avec Evil).
    (evil-define-key 'normal dired-mode-map (kbd "o") 'dired-find-file-other-window)  ;; Ouvre le fichier dans une autre fenêtre.
    (evil-define-key 'normal dired-mode-map (kbd "p") 'transient-extras-lp-menu)  ;; Lien vers un menu (modifiez cette commande si nécessaire).
    (evil-define-key 'normal dired-mode-map (kbd "v") '+dired-slideshow)  ;; Lance le diaporama défini plus haut.

    ;; Configuration des options d'affichage de Dired pour `ls`.
    (setq-default dired-listing-switches
                  (combine-and-quote-strings '("-l"  ;; Affiche les informations détaillées sur les fichiers.
                                               "-v"  ;; Trie les fichiers par version.
                                               "-g"  ;; N'affiche pas les colonnes de propriétaire et de groupe.
                                               "--no-group"  ;; Ne montre pas les groupes.
                                               "--human-readable"  ;; Affiche les tailles des fichiers dans un format lisible par l'homme.
                                               "--time-style=+%Y-%m-%d"  ;; Formate les dates d'une manière spécifique.
                                               "--almost-all")))  ;; Affiche tous les fichiers sauf `.` et `..`.

    ;; Autres options de personnalisation pour le comportement de Dired.
    (setq dired-clean-up-buffers-too t  ;; Nettoie les buffers de Dired après l'édition.
          dired-dwim-target t  ;; Active la fonctionnalité "do what I mean" pour les cibles dans Dired.
          dired-recursive-copies 'always  ;; Permet les copies récursives sans confirmation.
          dired-recursive-deletes 'top  ;; Supprime les répertoires récursivement, mais demande une confirmation pour les sous-répertoires.
          global-auto-revert-non-file-buffers t  ;; Réactive les buffers non liés à des fichiers (ex. les répertoires).
          auto-revert-verbose nil))  ;; Désactive les messages verbaux lors de l'auto-revert.
)

;; Chargement du paquet `dired-hide-dotfiles` pour cacher les fichiers et répertoires commençant par un point (.) dans Dired.
(use-package dired-hide-dotfiles
  :demand t  ;; Force le chargement immédiat du paquet.
  :config
  ;; Active `dired-hide-dotfiles-mode` pour cacher les fichiers et répertoires dont le nom commence par un point.
  (dired-hide-dotfiles-mode 1)
  
  ;; Définition d'un raccourci clavier dans Dired pour activer/désactiver le mode `dired-hide-dotfiles-mode`.
  ;; Le raccourci `.` dans le mode normal d'Evil permet de basculer entre cacher ou afficher les fichiers cachés.
  (evil-define-key 'normal dired-mode-map "." 'dired-hide-dotfiles-mode))

;; Chargement du paquet `dired-open` pour ouvrir des fichiers avec des applications externes depuis Dired.
(use-package dired-open
  :demand t  ;; Force le chargement immédiat du paquet.
  
  :config
  ;; Définition des extensions de fichiers et des programmes à utiliser pour ouvrir ces fichiers.
  (setq dired-open-extensions
        `(("avi" . "mpv")  ;; Les fichiers `.avi` sont ouverts avec `mpv`.
          ("cbr" . "zathura")  ;; Les fichiers `.cbr` (bandes dessinées) sont ouverts avec `zathura`.
          ("cbz" . "zathura")  ;; Les fichiers `.cbz` (bandes dessinées) sont ouverts avec `zathura`.
          ("doc" . "abiword")  ;; Les fichiers `.doc` sont ouverts avec `abiword`.
          ("docx" . "abiword")  ;; Les fichiers `.docx` sont ouverts avec `abiword`.
          ("epub" . "foliate")  ;; Les fichiers `.epub` (ebooks) sont ouverts avec `foliate`.
          ("flac" . "mpv")  ;; Les fichiers `.flac` sont ouverts avec `mpv` (lecture audio).
          ("gif" . "ffplay")  ;; Les fichiers `.gif` sont ouverts avec `ffplay`.
          ("gnumeric" . "gnumeric")  ;; Les fichiers `.gnumeric` sont ouverts avec `gnumeric` (tableur).
          ("jpeg" . ,(executable-find "feh"))  ;; Les fichiers `.jpeg` sont ouverts avec `feh`.
          ("jpg" . ,(executable-find "feh"))  ;; Les fichiers `.jpg` sont ouverts avec `feh`.
          ("m3u8" . "mpv")  ;; Les fichiers de playlist `.m3u8` sont ouverts avec `mpv`.
          ("m4a" . "mpv")  ;; Les fichiers `.m4a` (audio) sont ouverts avec `mpv`.
          ("mkv" . "mpv")  ;; Les fichiers `.mkv` sont ouverts avec `mpv` (vidéo).
          ("mobi" . "foliate")  ;; Les fichiers `.mobi` (ebooks) sont ouverts avec `foliate`.
          ("mov" . "mpv")  ;; Les fichiers `.mov` (vidéo) sont ouverts avec `mpv`.
          ("mp3" . "mpv")  ;; Les fichiers `.mp3` (audio) sont ouverts avec `mpv`.
          ("mp4" . "mpv")  ;; Les fichiers `.mp4` (vidéo) sont ouverts avec `mpv`.
          ("mpg" . "mpv")  ;; Les fichiers `.mpg` (vidéo) sont ouverts avec `mpv`.
          ("pdf" . "zathura")  ;; Les fichiers `.pdf` sont ouverts avec `zathura`.
          ("png" . ,(executable-find "feh"))  ;; Les fichiers `.png` sont ouverts avec `feh`.
          ("webm" . "mpv")  ;; Les fichiers `.webm` sont ouverts avec `mpv`.
          ("webp" . ,(executable-find "feh"))  ;; Les fichiers `.webp` sont ouverts avec `feh`.
          ("wmv" . "mpv")  ;; Les fichiers `.wmv` (vidéo) sont ouverts avec `mpv`.
          ("xcf" . "gimp")  ;; Les fichiers `.xcf` (format de GIMP) sont ouverts avec `gimp`.
          ("xls" . "gnumeric")  ;; Les fichiers `.xls` (tableurs Excel) sont ouverts avec `gnumeric`.
          ("xlsx" . "gnumeric")))  ;; Les fichiers `.xlsx` (tableurs Excel) sont ouverts avec `gnumeric`.

  ;; Installation des paquets système nécessaires si non installés via une commande shell.
  (unless (executable-find "mpv")
    (shell-command "sudo apt-get install mpv"))
  
  (unless (executable-find "gnumeric")
    (shell-command "sudo apt-get install gnumeric"))
  
  (unless (executable-find "feh")
    (shell-command "sudo apt-get install feh"))
  
  (unless (executable-find "zathura")
    (shell-command "sudo apt-get install zathura"))
  
  (unless (executable-find "abiword")
    (shell-command "sudo apt-get install abiword"))
  
  (unless (executable-find "gimp")
    (shell-command "sudo apt-get install gimp"))
  
  (unless (executable-find "foliate")
    (shell-command "sudo apt-get install foliate"))
)

(use-package async
  :demand t  ;; Assure que le paquet est chargé immédiatement.
  
  :config
  ;; Active `dired-async-mode` pour effectuer les opérations Dired de manière asynchrone.
  (dired-async-mode 1))

(defun +image-dimensions (filename)
  "Given an image file `filename' readable by `identify', return a cons pair of integers denoting the width and height of the image, respectively."
  (->> (shell-command-to-string (format "identify %s" filename))
       (s-split " ")
       (nth 2)
       (s-split "x")
       (mapcar #'string-to-number)))

(defun +dired-convert-image (source-file target-width target-height target-file)
  "Resize an image file specified by `source-file` to `target-width` and `target-height`, and save the resized image as `target-file`."
  (interactive
   (let* ((source-file (dired-file-name-at-point))  ;; Récupère le nom du fichier sélectionné dans Dired
          (source-dimensions (+image-dimensions source-file))  ;; Récupère les dimensions de l'image source
          (source-width (nth 0 source-dimensions))  ;; Largeur de l'image source
          (source-height (nth 1 source-dimensions))  ;; Hauteur de l'image source
          (target-width (read-number "Width: " source-width))  ;; Demande à l'utilisateur la largeur cible
          (target-height (read-number "Height: "  ;; Demande à l'utilisateur la hauteur cible
                                      (if (= source-width target-width)
                                          source-height
                                        (round (* source-height
                                                  (/ (float target-width)
                                                     source-width))))))  ;; Conserve les proportions
          (target-file (read-file-name "Target: " nil nil nil
                                       (file-name-nondirectory source-file))))  ;; Demande le chemin du fichier cible
     (list source-file target-width target-height target-file)))  ;; Retourne les arguments nécessaires pour la conversion

  ;; Appelle ImageMagick pour redimensionner l'image
  (call-process "convert" nil nil nil
                (expand-file-name source-file)  ;; Fichier source avec son chemin complet
                "-resize" (format "%sx%s"
                                  target-width  ;; Largeur cible
                                  target-height)  ;; Hauteur cible
                (expand-file-name target-file)))  ;; Fichier cible où l'image redimensionnée sera sauvegardée

(use-package ediff
  :ensure nil  ;; Indique que le paquet 'ediff' est intégré dans Emacs et n'a pas besoin d'être installé séparément

  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)  ;; Définit la fonction de configuration des fenêtres pour l'affichage d'Ediff
  (setq ediff-split-window-function 'split-window-horizontally))  ;; Définit la méthode de découpage de fenêtre (ici, horizontalement)

;; Activer M-x command-log-mode
  (use-package command-log-mode)

(use-package sudo-edit
  :commands (sudo-edit))

(use-package crontab-mode)

(use-package calc
  :ensure nil

  :config
  (add-hook 'calc-trail-mode-hook 'evil-insert-state))

(use-package magit
  :ensure-system-package git  ;; Vérifie que Git est installé sur le système
  :hook (with-editor-mode . evil-insert-state)  ;; Mettre `evil-insert-state` quand `with-editor-mode` est activé
  :bind ("C-x g" . magit-status)  ;; Lier la commande Magit à la combinaison de touches C-x g

  :config
  (use-package magit-section)  ;; Charge la section de Magit
  (use-package with-editor)    ;; Charge le package with-editor pour la gestion des éditeurs dans Magit

  (require 'git-rebase)  ;; Charge la fonctionnalité git-rebase

  ;; Fonction pour parser l'auteur d'un commit en récupérant le nom et l'email
  (defun +get-author-parse-line (key value domain)
    (let* ((values (mapcar #'s-trim (s-split ";" value)))  ;; Sépare la chaîne en parties
           (name (car values))  ;; Le premier élément de la liste est le nom
           (email (or (cadr values) key)))  ;; Si pas d'email, utilise la clé comme email
      (format "%s <%s@%s>" name email domain)))  ;; Retourne une chaîne formatée pour l'auteur

  ;; Fonction pour obtenir les auteurs à partir d'un fichier YAML de configuration
  (defun +git-authors ()
    (let* ((config (yaml-parse-string (f-read-text "~/.git-authors")))  ;; Lit et parse le fichier YAML
           (domain (gethash 'domain (gethash 'email config)))  ;; Récupère le domaine
           (authors '()))  ;; Initialise la liste des auteurs
      (+maphash (lambda (k v) (+git-author-parse-line k v domain))  ;; Mappe les auteurs à partir du fichier YAML
                (gethash 'authors config))))  ;; Accède aux auteurs dans le fichier YAML

  ;; Fonction pour insérer un co-auteur dans un commit
  (defun +insert-git-coauthor ()
    "Prompt for co-author and insert a co-authored-by block."
    (interactive)
    (insert (format "Co-authored-by: %s\n"
                    (completing-read "Co-authored by:" (+git-authors)))))  ;; Demande un co-auteur et insère le bloc

  ;; Configuration de Magit
  (setq git-commit-summary-max-length 50  ;; Limite la longueur du résumé du commit
        magit-bury-buffer-function 'magit-restore-window-configuration  ;; Fonction pour gérer les buffers de Magit
        magit-display-buffer-function 'magit-display-buffer-fullframe-status-topleft-v1  ;; Fonction pour afficher Magit en mode plein écran
        magit-push-always-verify nil))  ;; Désactive la vérification avant chaque push

(use-package git-timemachine
  :defer t
  :bind ("C-c t" . git-timemachine-toggle))  ;; Lance la timemachine avec C-c t

(use-package web-mode
  :mode ("\\.erb$"
         "\\.html$"
         "\\.php$"
         "\\.rhtml$")

  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-indent-style 2))

(use-package rainbow-mode
  :hook web-mode)

(eval-and-compile
  (defun eww-browse-wikipedia-en ()
    (interactive)
    (let ((search (read-from-minibuffer "Recherche Wikipédia (EN) : ")))
      (eww-browse-url
       (concat "https://en.wikipedia.org/w/index.php?search=" search)))))

(eval-and-compile
  (defun eww-browser-english-dict ()
    (interactive)
    (let ((search (read-from-minibuffer "Recherche dans le dictionnaire (EN) : ")))
      (eww-browse-url
       (concat "https://www.merriam-webster.com/dictionary/" search)))))

(use-package eww
  :config
  (setq eww-search-prefix "https://startpage.com/search/?q=")
  :bind (("C-c w b" . 'eww)
         ("C-c w d" . 'eww-browser-english-dict)
         ("C-c w w" . 'eww-browse-wikipedia-en)))

(setq-default tab-width 2)

(use-package subword
  :config (global-subword-mode 1))

;; Charger les langages de programmation
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)  ;; Activer Emacs Lisp
   (shell . t)       ;; Activer Shell
   (python . t)      ;; Activer Python
   (ruby . t)        ;; Activer Ruby
   ;; (C++ . t)       ;; Activer C++ (désactivé pour l'instant)
   (latex . t)))     ;; Activer LaTeX

;; Utiliser org-tempo pour ajouter des raccourcis pour les blocs de code
(use-package org-tempo
  :ensure nil
  :demand t
  :config
  (dolist (item '(("sh" . "src sh")
                  ("el" . "src emacs-lisp")
                  ("li" . "src lisp")
                  ("sc" . "src scheme")
                  ("ts" . "src typescript")
                  ("py" . "src python")
                  ("yaml" . "src yaml")
                  ("json" . "src json")
                  ("c" . "src C")
                  ("r" . "src R")
    (add-to-list 'org-structure-template-alist item)))))

;; Utiliser Python 3 comme interpréteur pour Org-Babel
(setq org-babel-python-command "python3")

(add-hook 'c++-mode-hook (lambda () (c-set-style "stroustrup")))

(use-package cmake-mode
  :ensure t)

(use-package ggtags
  :ensure t
  :hook (c++-mode . ggtags-mode)
  :bind (:map ggtags-mode-map
         ("C-c g s" . ggtags-find-other-symbol)
         ("C-c g h" . ggtags-view-tag-history)
         ("C-c g r" . ggtags-find-reference)
         ("C-c g f" . ggtags-find-file)
         ("C-c g c" . ggtags-create-tags)
         ("C-c g u" . ggtags-update-tags)
         ("M-," . pop-tag-mark))
  :config
  (setq-local imenu-create-index-function #'ggtags-build-imenu-index))

(use-package company-c-headers
  :ensure t
  :config
  (add-to-list 'company-c-headers-path-system "/usr/include/c++/13")
  (add-to-list 'company-backends 'company-c-headers))

(require 'semantic)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)

(add-hook 'c++-mode-hook #'semantic-mode)

(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package company-irony
  :ensure t
  :config
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-irony)))

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt -i")

(add-hook 'python-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq indent-tabs-mode nil)))

(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  (add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1))))

(use-package sphinx-doc
  :ensure t
  :hook (python-mode . sphinx-doc-mode))

(use-package python-docstring
  :ensure t
  :config
  (setq python-docstring-sentence-end-double-space nil)
  :hook (python-mode . python-docstring-mode))

(use-package ein
  :ensure t
  :hook (ein:notebook-multilang-mode
         . (lambda () (ws-butler-mode -1) (visual-line-mode)))
  :custom-face
  (ein:cell-input-area ((t (:background "#f9f2d9")))))

(use-package org-tempo
  :ensure nil
  :demand t
  :config
  (dolist (item '(("sh"    . "src sh")               ;; Shell script
                  ("el"    . "src emacs-lisp")       ;; Emacs Lisp
                  ("li"    . "src lisp")             ;; Lisp
                  ("sc"    . "src scheme")           ;; Scheme
                  ("py"    . "src python")           ;; Python
                  ("pys"   . "src python :session my_session") ;; Python session
                  ("pyp"   . "src python :session my_session :results output") ;; Python with output
                  ("pyv"   . "src python :session my_session :results value") ;; Python with value
                  ("pyg"   . "src python :session my_session :results file :file graph.png") ;; Python with graph file
                  ("yaml"  . "src yaml")             ;; YAML
                  ("json"  . "src json")             ;; JSON
                  ("cpp"   . "src C++")              ;; C++
                  ("tex"   . "src latex")))          ;; LaTeX
    (add-to-list 'org-structure-template-alist item)))

(use-package ox-pandoc
  :ensure t)

(use-package markdown-mode
  :ensure t  ;; Installe automatiquement si non disponible
  :mode "\\.md\\'"  ;; Active markdown-mode pour les fichiers .md
  :config
  (setq markdown-command "pandoc") ;; Utilise Pandoc pour convertir Markdown
  (setq markdown-enable-math t) ;; Active le support des mathématiques
  (setq markdown-fontify-code-blocks-natively t) ;; Syntaxe des blocs de code colorée

  ;; Préférences pour une meilleure lisibilité
  (add-hook 'markdown-mode-hook #'visual-line-mode) ;; Active la coupure visuelle des lignes
  (add-hook 'markdown-mode-hook #'variable-pitch-mode) ;; Active une police proportionnelle
  (add-hook 'markdown-mode-hook #'visual-fill-column-mode) ;; Centre le texte

  ;; Désactiver les numéros de ligne dans markdown-mode
  (add-hook 'markdown-mode-hook (lambda () (display-line-numbers-mode -1))))

(use-package visual-fill-column
  :ensure t
  :config
  (setq visual-fill-column-width 80
        visual-fill-column-center-text t))

(with-eval-after-load 'org
  (require 'ox-md)) ;; Charge l'exportateur Markdown

;; Ajouter ox-ipynb au chemin de chargement
;;(add-to-list 'load-path "~/.emacs.d/ox-ipynb/")

;; Charger le paquet ox-ipynb
;;(require 'ox-ipynb)

;; Activer l'exportation vers Jupyter Notebook
;;(setq org-export-backends '(ipynb)) ;; Ajoute `ipynb` comme backend d'exportation

;; Assurez-vous d'avoir des blocs de code correctement formatés dans Org

;; Équilibrer automatiquement les fenêtres après suppression
(advice-add #'delete-window
            :after #'(lambda (&rest _)
                       (balance-windows))) ;; Appelle 'balance-windows' après 'delete-window'

;; Équilibrer les fenêtres et se déplacer après une division
(advice-add #'split-window
            :after #'(lambda (&rest _)
                       (balance-windows) ;; Équilibre les fenêtres
                       (other-window 1))) ;; Se déplace vers la nouvelle fenêtre

;; Load up doom-palenight for the System Crafters look
;(load-theme 'doom-palenight t)

(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))

(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'org-mobile)

;; TODO: Replace with C-c o a like <leader>oa (action open agenda)

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; windows home directory
;; NOTE: For some reason $HOME or ~ doesn't work on Windows even if we have wsl installed..
(if (eq system-type 'windows-nt)
	(setq home-directory (getenv "USERPROFILE"))
	(setq home-directory "~"))

;; (setq org-directory (concat home-directory "/Notes/org"))
(setq org-directory (concat home-directory "/Notes/orgfiles"))

(setopt org-startup-indented t
		org-ellipsis " ▼"
		org-hide-emphasis-markers t
		org-pretty-entities t
		org-src-fontify-natively t
		org-fontify-whole-heading-line t
		org-fontify-quote-and-verse-blocks t
		ord-edit-src-content-indentation 0
		org-hide-block-startup nil
		org-src-tab-acts-natively t
		org-src-preserve-indentation nil
		;; org-startup-folded t
		org-cycle-separator-lines 2
		org-hide-leading-stars t
		org-highlight-latex-and-related '(native)
		org-goto-auto-isearch nil)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-startup-folded t)

(setq org-agenda-files (list (concat org-directory "/personal.org")
							 (concat org-directory "/work.org")
							 (concat org-directory "/school.org")
							 (concat org-directory "/journal.org")))
(setq org-deadline-warning-days 7)
(setq org-agenda-include-diary nil) ; NOTE: We are including calendar holidays in diary.org
(setq org-agenda-span 7)
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-start-day "-1d")
(setq org-agenda-start-with-log-mode t)
;; TODO: org-agenda-custom-commands
(setq org-agenda-window-setup 'only-window)

(setq org-default-notes-file (concat org-directory "/brouillon.org"))
(setq org-capture-templates
    '(("b" "Brouillon" entry (file (lambda () (concat org-directory "/brouillon.org")))
     "* TODO %?\n %U\n" :empty-lines 1)
    ("p" "Personal" entry (file (lambda () (concat org-directory "/personal.org")))
     "* TODO %?\n %U\n" :empty-lines 1)
    ("w" "Work" entry (file (lambda () (concat org-directory "/work.org")))
     "* TODO %?\n %U\n" :empty-lines 1)
    ("s" "School" entry (file (lambda () (concat org-directory "/school.org")))
     "* TODO %?\n %U\n" :empty-lines 1)
     ("j" "Journal" entry (file+datetree (lambda () (concat org-directory "/journal.org")))
   "* %?\nEntered on %U\n" :empty-lines 1)))

(setq org-mobile-directory (concat home-directory "/Dropbox/Apps/MobileOrg"))
;; (setq org-mobile-directory (concat home-directory "/Dropbox/Applications/MobileOrg"))
(setq org-mobile-use-encryption nil)
;; (setq org-mobile-encryption-password "")
;; (setq org-mobile-files (list ()))
(setq org-mobile-force-id-on-agenda-items nil)
(setq org-mobile-inbox-for-pull (concat org-directory "/mobile-flagged.org"))

(use-package mu4e
  :load-path "/usr/share/emacs/site-lisp/elpa/mu4e-1.10.8"
  :demand t
  :bind (("C-c m" . mu4e))
  :hook (mu4e-compose-mode . flyspell-mode)
  :config
;  (require 'org-mu4e)
  (require 'shr)

  (setq mail-user-agent 'mu4e-user-agent
        mu4e-maildir "~/.maildir"
        mu4e-sent-folder "/fastmail/Sent"
        mu4e-drafts-folder "/fastmail/Drafts"
        mu4e-trash-folder "/fastmail/Trash"
        mu4e-refile-folder "/fastmail/Archive"
        mu4e-completing-read-function 'ivy-completing-read
        mu4e-confirm-quit nil
        mu4e-kill-buffer-on-exit t
        smtpmail-stream-type 'ssl
        smtpmail-smtp-server "smtp.fastmail.com"
        smtpmail-smtp-service 465
        send-mail-function 'smtpmail-send-it
        message-send-mail-function 'smtpmail-send-it
        mu4e-view-date-format "%a %e %b %Y %T"
        mu4e-headers-date-format "%d/%m/%Y"
        mu4e-headers-time-format "%T"
        mu4e-view-prefer-html t
        shr-use-colors nil
        shr-use-fonts nil
        shr-width 79)

  (setq mu4e-bookmarks
        '((:name "Unread messages"
                 :query "flag:unread AND NOT flag:trashed AND NOT maildir:/fastmail/Spam AND NOT maildir:/fastmail/Trash"
                 :key ?u)
          (:name "Today's messages"
                 :query "date:today..now"
                 :key ?t)
          (:name "Inbox"
                 :query "maildir:/fastmail/INBOX"
                 :key ?i))))

(setq auth-sources '("~/.password-store"))

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-auth-supported '(login)
      smtpmail-stream-type 'ssl
      smtpmail-smtp-server "smtp.fastmail.com"
      smtpmail-smtp-service 465
      smtpmail-smtp-user "berthekassime@fastmail.com")

;; Activer l'affichage des numéros de lignes relatifs globalement
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; Activer uniquement dans certains modes si souhaité
;; (add-hook 'prog-mode-hook (lambda ()
;;                             (setq display-line-numbers 'relative)
;;                             (display-line-numbers-mode t)))

;; Désactiver dans certains modes comme org, term, eshell
;(dolist (mode '(org-mode-hook term-mode-hook eshell-mode-hook))
;  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1)
  (setq git-gutter:modified-sign "≠")   ;; signe pour les lignes modifiées
  (setq git-gutter:added-sign    "+") ;; signe pour les lignes ajoutées
  (setq git-gutter:deleted-sign  "-") ;; signe pour les lignes supprimées
  (set-face-foreground 'git-gutter:modified "orange")
  (set-face-foreground 'git-gutter:added    "green")
  (set-face-foreground 'git-gutter:deleted  "red"))

(use-package general
  :config
  (general-evil-setup)
  
  ;; set up 'SPC' as the global leader key
  (general-create-definer dt/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode

  (dt/leader-keys
    "SPC" '(counsel-M-x :wk "Counsel M-x")
    "." '(find-file :wk "Find file")
    "=" '(perspective-map :wk "Perspective") ;; Lists all the perspective keybindings
    "TAB TAB" '(comment-line :wk "Comment lines")
    "u" '(universal-argument :wk "Universal argument"))

   (dt/leader-keys
    "a" '(:ignore t :wk "A.I.")
    "a a" '(ellama-ask-about :wk "Ask ellama about region")
    "a e" '(:ignore t :wk "Ellama enhance")
    "a e g" '(ellama-improve-grammar :wk "Ellama enhance wording")
    "a e w" '(ellama-improve-wording :wk "Ellama enhance grammar")
    "a i" '(ellama-chat :wk "Ask ellama")
    "a p" '(ellama-provider-select :wk "Ellama provider select")
    "a s" '(ellama-summarize :wk "Ellama summarize region")
    "a t" '(ellama-translate :wk "Ellama translate region"))
   
  (dt/leader-keys
    "b" '(:ignore t :wk "Bookmarks/Buffers")
    "b b" '(switch-to-buffer :wk "Switch to buffer")
    "b c" '(clone-indirect-buffer :wk "Create indirect buffer copy in a split")
    "b C" '(clone-indirect-buffer-other-window :wk "Clone indirect buffer in new window")
    "b d" '(bookmark-delete :wk "Delete bookmark")
    "b i" '(ibuffer :wk "Ibuffer")
    "b f" '(kill-current-buffer :wk "Kill current buffer")
    "b K" '(kill-some-buffers :wk "Kill multiple buffers")
    "b l" '(list-bookmarks :wk "List bookmarks")
    "b m" '(bookmark-set :wk "Set bookmark")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Reload buffer")
    "b R" '(rename-buffer :wk "Rename buffer")
    "b s" '(basic-save-buffer :wk "Save buffer")
    "b S" '(save-some-buffers :wk "Save multiple buffers")
    "b w" '(bookmark-save :wk "Save current bookmarks to bookmark file"))

  (dt/leader-keys
    "d" '(:ignore t :wk "Dired")
    "d d" '(dired :wk "Open dired")
    "d f" '(wdired-finish-edit :wk "Writable dired finish edit")
    "d j" '(dired-jump :wk "Dired jump to current")
    "d n" '(neotree-dir :wk "Open directory in neotree")
    "d p" '(peep-dired :wk "Peep-dired")
    "d w" '(wdired-change-to-wdired-mode :wk "Writable dired"))

  (dt/leader-keys
    "e" '(:ignore t :wk "Ediff/Eshell/Eval/EWW")    
    "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "e d" '(eval-defun :wk "Evaluate defun containing or after point")
    "e e" '(eval-expression :wk "Evaluate and elisp expression")
    "e f" '(ediff-files :wk "Run ediff on a pair of files")
    "e F" '(ediff-files3 :wk "Run ediff on three files")
    "e h" '(counsel-esh-history :which-key "Eshell history")
    "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
    "e n" '(eshell-new :wk "Create new eshell buffer")
    "e r" '(eval-region :wk "Evaluate elisp in region")
    "e R" '(eww-reload :which-key "Reload current page in EWW")
    "e s" '(eshell :which-key "Eshell")
    "e w" '(eww :which-key "EWW emacs web wowser"))

  (dt/leader-keys
    "f" '(:ignore t :wk "Files")    
    "f c" '((lambda () (interactive)
              (find-file "~/.emacs/config_kas.org")) 
            :wk "Open emacs config_kas.org")
    "f e" '((lambda () (interactive)
              (dired "~/.config/emacs/")) 
            :wk "Open user-emacs-directory in dired")
    "f d" '(find-grep-dired :wk "Search for string in files in DIR")
    "f g" '(counsel-grep-or-swiper :wk "Search for string current file")
    "f i" '((lambda () (interactive)
              (find-file "~/.emacs/init.el")) 
            :wk "Open emacs init.el")
    "f j" '(counsel-file-jump :wk "Jump to a file below current directory")
    "f l" '(counsel-locate :wk "Locate a file")
    "f r" '(counsel-recentf :wk "Find recent files")
    "f u" '(sudo-edit-find-file :wk "Sudo find file")
    "f U" '(sudo-edit :wk "Sudo edit file"))

  (dt/leader-keys
    "g" '(:ignore t :wk "Git")    
    "g /" '(magit-displatch :wk "Magit dispatch")
    "g ." '(magit-file-displatch :wk "Magit file dispatch")
    "g b" '(magit-branch-checkout :wk "Switch branch")
    "g c" '(:ignore t :wk "Create") 
    "g c b" '(magit-branch-and-checkout :wk "Create branch and checkout")
    "g c c" '(magit-commit-create :wk "Create commit")
    "g c f" '(magit-commit-fixup :wk "Create fixup commit")
    "g C" '(magit-clone :wk "Clone repo")
    "g f" '(:ignore t :wk "Find") 
    "g f c" '(magit-show-commit :wk "Show commit")
    "g f f" '(magit-find-file :wk "Magit find file")
    "g f g" '(magit-find-git-config-file :wk "Find gitconfig file")
    "g F" '(magit-fetch :wk "Git fetch")
    "g g" '(magit-status :wk "Magit status")
    "g i" '(magit-init :wk "Initialize git repo")
    "g l" '(magit-log-buffer-file :wk "Magit buffer log")
    "g r" '(vc-revert :wk "Git revert file")
    "g s" '(magit-stage-file :wk "Git stage file")
    "g t" '(git-timemachine :wk "Git time machine")
    "g u" '(magit-stage-file :wk "Git unstage file"))

 (dt/leader-keys
    "h" '(:ignore t :wk "Help")
    "h a" '(counsel-apropos :wk "Apropos")
    "h b" '(describe-bindings :wk "Describe bindings")
    "h c" '(describe-char :wk "Describe character under cursor")
    "h d" '(:ignore t :wk "Emacs documentation")
    "h d a" '(about-emacs :wk "About Emacs")
    "h d d" '(view-emacs-debugging :wk "View Emacs debugging")
    "h d f" '(view-emacs-FAQ :wk "View Emacs FAQ")
    "h d m" '(info-emacs-manual :wk "The Emacs manual")
    "h d n" '(view-emacs-news :wk "View Emacs news")
    "h d o" '(describe-distribution :wk "How to obtain Emacs")
    "h d p" '(view-emacs-problems :wk "View Emacs problems")
    "h d t" '(view-emacs-todo :wk "View Emacs todo")
    "h d w" '(describe-no-warranty :wk "Describe no warranty")
    "h e" '(view-echo-area-messages :wk "View echo area messages")
    "h f" '(describe-function :wk "Describe function")
    "h F" '(describe-face :wk "Describe face")
    "h g" '(describe-gnu-project :wk "Describe GNU Project")
    "h i" '(info :wk "Info")
    "h I" '(describe-input-method :wk "Describe input method")
    "h k" '(describe-key :wk "Describe key")
    "h l" '(view-lossage :wk "Display recent keystrokes and the commands run")
    "h L" '(describe-language-environment :wk "Describe language environment")
    "h m" '(describe-mode :wk "Describe mode")
    "h r" '(:ignore t :wk "Reload")
    "h r r" '((lambda () (interactive)
                (load-file "~/.emacs/init.el")
                (ignore (elpaca-process-queues)))
              :wk "Reload emacs config_kas")
    "h t" '(load-theme :wk "Load theme")
    "h v" '(describe-variable :wk "Describe variable")
    "h w" '(where-is :wk "Prints keybinding for command if set")
    "h x" '(describe-command :wk "Display full documentation for command"))

  (dt/leader-keys
    "m" '(:ignore t :wk "Org")
    "m a" '(org-agenda :wk "Org agenda")
    "m e" '(org-export-dispatch :wk "Org export dispatch")
    "m i" '(org-toggle-item :wk "Org toggle item")
    "m t" '(org-todo :wk "Org todo")
    "m B" '(org-babel-tangle :wk "Org babel tangle")
    "m T" '(org-todo-list :wk "Org todo list"))

  (dt/leader-keys
    "m b" '(:ignore t :wk "Tables")
    "m b -" '(org-table-insert-hline :wk "Insert hline in table"))

  (dt/leader-keys
    "m d" '(:ignore t :wk "Date/deadline")
    "m d t" '(org-time-stamp :wk "Org time stamp"))

  (dt/leader-keys
    "o" '(:ignore t :wk "Open")
    "o d" '(dashboard-open :wk "Dashboard")
    "o e" '(elfeed :wk "Elfeed RSS")
    "o f" '(make-frame :wk "Open buffer in new frame")
    "o F" '(select-frame-by-name :wk "Select frame by name"))

  ;; projectile-command-map already has a ton of bindings 
  ;; set for us, so no need to specify each individually.
  (dt/leader-keys
    "p" '(projectile-command-map :wk "Projectile"))
  
  (dt/leader-keys
    "r" '(:ignore t :wk "Radio")
    "r p" '(eradio-play :wk "Eradio play")
    "r s" '(eradio-stop :wk "Eradio stop")
    "r t" '(eradio-toggle :wk "Eradio toggle"))


  (dt/leader-keys
    "s" '(:ignore t :wk "Search")
    "s d" '(dictionary-search :wk "Search dictionary")
    "s m" '(man :wk "Man pages")
    "s o" '(pdf-occur :wk "Pdf search lines matching STRING")
    "s t" '(tldr :wk "Lookup TLDR docs for a command")
    "s w" '(woman :wk "Similar to man but doesn't require man"))

  (dt/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "t e" '(eshell-toggle :wk "Toggle eshell")
    "t f" '(flycheck-mode :wk "Toggle flycheck")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "t n" '(neotree-toggle :wk "Toggle neotree file viewer")
    "t o" '(org-mode :wk "Toggle org mode")
    "t r" '(rainbow-mode :wk "Toggle rainbow mode")
    "t t" '(visual-line-mode :wk "Toggle truncated lines")
    "t v" '(vterm-toggle :wk "Toggle vterm"))

  (dt/leader-keys
    "w" '(:ignore t :wk "Windows/Words")
    ;; Window splits
    "w f" '(evil-window-delete :wk "Close window")
    "w n" '(evil-window-new :wk "New window")
    "w -" '(evil-window-split :wk "Horizontal split window")
    "w v" '(evil-window-vsplit :wk "Vertical split window")
    ;; Window motions
    "w h" '(evil-window-left :wk "Window left")
    "w j" '(evil-window-down :wk "Window down")
    "w k" '(evil-window-up :wk "Window up")
    "w l" '(evil-window-right :wk "Window right")
    "w w" '(evil-window-next :wk "Goto next window")
    ;; Move Windows
    "w H" '(buf-move-left :wk "Buffer move left")
    "w J" '(buf-move-down :wk "Buffer move down")
    "w K" '(buf-move-up :wk "Buffer move up")
    "w L" '(buf-move-right :wk "Buffer move right")
    ;; Words
    "w d" '(downcase-word :wk "Downcase word")
    "w u" '(upcase-word :wk "Upcase word")
    "w =" '(count-words :wk "Count words/lines for buffer"))
)

;; Ajouter le dossier contenant ton thème personnalisé au chemin des thèmes
(add-to-list 'custom-theme-load-path "~/.emacs/themes/")

;; Charger ton thème personnalisé 'dtmacs'
(load-theme 'dtmacs t)

(add-hook 'org-mode-hook (lambda () (company-mode -1)))

(defun my/inhibit-angle-brackets-pairing (char)
  "Inhibit electric pairing for < only."
  (if (eq char ?<)
      t
    (funcall (default-value 'electric-pair-inhibit-predicate) char)))

(with-eval-after-load 'electric-pair
  (setq-default electric-pair-inhibit-predicate #'my/inhibit-angle-brackets-pairing))

;; A function for easily creating multiple buffers of 'eshell'.
;; NOTE: `C-u M-x eshell` would also create new 'eshell' buffers.
(defun eshell-new (name)
  "Create new eshell buffer named NAME."
  (interactive "sName: ")
  (setq name (concat "$" name))
  (eshell)
  (rename-buffer name))

(use-package eshell-toggle
  :ensure t
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil)
  (eshell-toggle-init-function #'eshell-toggle-init-ansi-term))

(use-package eshell-syntax-highlighting
  :ensure t
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

;; Eshell config
(setq eshell-rc-script (concat user-emacs-directory "eshell/profile")
      eshell-aliases-file (concat user-emacs-directory "eshell/aliases")
      eshell-history-size 5000
      eshell-buffer-maximum-lines 5000
      eshell-hist-ignoredups t
      eshell-scroll-to-bottom-on-input t
      eshell-destroy-buffer-when-process-dies t
      eshell-visual-commands '("bash" "fish" "htop" "ssh" "top" "zsh"))

(use-package vterm
  :ensure t
  :hook (vterm-mode . (lambda () (setq-local global-hl-line-mode nil))) ;; (optionnel) désactive le highlight dans vterm
  :config
  ;; Utilise /bin/sh comme shell (par défaut)
  (setq shell-file-name "/bin/sh"
        vterm-max-scrollback 5000))

(use-package vterm-toggle
  :ensure t
  :after vterm
  :config
  ;; En mode normal (evil), la touche ESC doit s'envoyer elle-même dans vterm,
  ;; ce qui permet de tuer les programmes comme dans un terminal classique.
  (evil-define-key 'normal vterm-mode-map (kbd "<escape>") 'vterm--self-insert)

  ;; Ne pas passer en fullscreen lors du toggle
  (setq vterm-toggle-fullscreen-p nil)

  ;; Scope sur le projet courant (autrement, c’est 'global)
  (setq vterm-toggle-scope 'project)

  ;; Gestion de l'affichage des buffers vterm
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (eq major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 ;; (display-buffer-reuse-window display-buffer-in-direction)
                 ;; display-buffer-in-direction/direction/dedicated est ajouté dans Emacs 27+
                 ;; (direction . bottom)
                 ;; (dedicated . t) ; dedicated supporté dans Emacs 27+
                 (reusable-frames . visible)
                 (window-height . 0.4))))
