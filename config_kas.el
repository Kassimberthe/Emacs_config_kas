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
  :config (global-diff-hl-mode t)
  :hook (magit-post-refresh-hook . diff-hl-magit-post-refresh))

(use-package auctex
  :defer t
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-engine 'luatex)
  (setq-default TeX-master nil))

(use-package company-auctex
  :after auctex
  :config
  (company-auctex-init))

(use-package cdlatex
  :diminish 'org-cdlatex-mode
  :hook ((LaTeX-mode . turn-on-cdlatex)
         (org-mode . turn-on-org-cdlatex)))

(use-package ox-latex
  :ensure-system-package latexmk
  :ensure nil
  :after org
  :commands (org-export-dispatch)

  :custom
  (org-latex-pdf-process '("latexmk -xelatex -shell-escape -quiet -f %f"))

  (org-latex-src-block-backend 'listings)
  (org-latex-listings-options
   '(("basicstyle" "\\ttfamily")
     ("showstringspaces" "false")
     ("keywordstyle" "\\color{blue}\\textbf")
     ("commentstyle" "\\color{gray}")
     ("stringstyle" "\\color{green!70!black}")
     ("stringstyle" "\\color{red}")
     ("frame" "single")
     ("numbers" "left")
     ("numberstyle" "\\ttfamily")
     ("columns" "fullflexible")))

  (org-latex-packages-alist '(("" "listings")
                              ("" "booktabs")
                              ("AUTO" "polyglossia" t ("xelatex" "lualatex"))
                              ("" "grffile")
                              ("" "unicode-math")
                              ("" "xcolor")))

  :config
  (add-to-list 'org-latex-logfiles-extensions "tex"))

(use-package ox-beamer
  :ensure nil
  :after ox-latex)

(use-package auctex
  :custom
  (TeX-parse-self t)

  :config
  (TeX-global-PDF-mode 1)

  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (LaTeX-math-mode)
              (setq TeX-master t))))

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

(use-package git-timemachine)

(use-package sudo-edit
  :commands (sudo-edit))

(use-package crontab-mode)

(use-package calc
  :ensure nil

  :config
  (add-hook 'calc-trail-mode-hook 'evil-insert-state))

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
