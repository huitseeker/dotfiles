(setq debug-on-error t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install use-package automatically
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)

(eval-when-compile
  (require 'use-package))
(use-package auto-compile
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)
(require 'bind-key)

;; Setup a few default packages
(setq url-http-attempt-keepalives nil)

(defvar prelude-packages '(ack-and-a-half auctex better-defaults
  clojure-mode coffee-mode deft diminish expand-region gist haml-mode
  haskell-mode helm helm-ag helm-projectile inf-ruby magit markdown-mode
  paredit projectile pymacs python sass-mode rainbow-mode
  scss-mode solarized-theme volatile-highlights yaml-mode yari
  yasnippet zenburn-theme)
  "A list of packages to ensure are installed at launch.")

(defun prelude-packages-installed-p ()
  (cl-loop for p in prelude-packages
        when (not (package-installed-p p)) do (cl-return nil)
        finally (cl-return t)))

(unless (prelude-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p prelude-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'prelude-packages)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; increase minimum prime bit size
(setq gnutls-min-prime-bits 4096)

;; modernize Emacs Lisp
(require 'cl)

(use-package dash
  :ensure t
  :config (eval-after-load "dash" '(dash-enable-font-lock)))

(use-package s
  :ensure t)

(use-package f
  :ensure t)

;; eval-depth doubled from default of 500
(setq max-lisp-eval-depth 10000)
(setq max-specpdl-size 10000)

;; bypass faces
(setq font-use-system-font t)

;; Server setup
(add-to-list
 'command-switch-alist
 '("server-start" . (lambda (&rest ignore)
                      (add-hook 'emacs-startup-hook 'server-start t))))


;; Package repo
(use-package exec-path-from-shell)

;; Personal info
(setq user-full-name "François Garillot"
      user-mail-address "francois@garillot.net")

;; Asciidoc
(use-package adoc-mode
  :ensure t
  :mode "\\.adoc?\\'")
(use-package artbollocks-mode
  :defer t
  :config
  (progn
    (setq artbollocks-weasel-words-regex
          (concat "\\b" (regexp-opt
                         '("one of the"
                           "should"
                           "just"
                           "sort of"
                           "a lot"
                           "probably"
                           "maybe"
                           "perhaps"
                           "I think"
                           "really"
                           "pretty"
                           "nice"
                           "action"
                           "utilize"
                           "leverage") t) "\\b"))
    ;; Don't show the art critic words, or at least until I figure
    ;; out my own jargon
    (setq artbollocks-jargon nil))
  :hook
  text-mode
  adoc-mode
  )

;; barebone Develock modes for Python and ProofGeneral
(eval-after-load "develock"
  '(progn
     (load "~/.emacs.d/packages/develock-py")
     (load "~/.emacs.d/packages/develock-proof")))

(defun tuareg-setup ()
  (progn
     (setq tuareg-in-indent 0) ; no extra indentation after `in'
     (global-set-key "\C-c\C-t" 'caml-types-show-type)
     (defun caml-types-explore-temp ()
       "types-explore at point"
       (interactive)
       (caml-types-explore mouse-movement))
     (local-set-key "\C-ct" 'caml-types-explore-temp)
     (local-set-key [f11] 'imenu)
     (setq tuareg-manual-url
           "http://caml.inria.fr/pub/docs/manual-ocaml/index.html")))

;;customization of a few major modes
(remove-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(remove-hook 'metapost-mode-hook #'turn-on-auto-fill)
(add-hook 'metapost-mode-hook 'turn-on-visual-line-mode)

(remove-hook 'bibtex-mode-hook #'turn-on-auto-fill)
(add-hook 'bibtex-mode-hook 'turn-on-visual-line-mode)

(add-hook 'tuareg-mode-hook 'tuareg-setup)


(defun list-fonts (pattern)
  (font-list pattern))
(defvar ocamldebug-history nil)
(defun my-camldebug (command-line)
  "Run camldebug on program FILE in buffer *camldebug-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for camldebug.  If you wish to change this, use
the camldebug commands `cd DIR' and `directory'."
  (interactive
   (list (read-from-minibuffer "Run ocamldebug (like this): "
                               (if (consp ocamldebug-history)
                                   (car ocamldebug-history)
                                 "ocamldebug")
                               nil
                               nil
                               '(ocamldebug-history . 1))))
  ; call something from camldebug.el to make sure it is loaded
  (camldebug-numeric-arg 1)

  ; We must override the camldebug-display-line
  (if (not (fboundp 'original-camldebug-display-line))
      (fset 'original-camldebug-display-line
            (symbol-function 'camldebug-display-line)))
  (defun camldebug-display-line (true-file character kind)
    (original-camldebug-display-line true-file character kind))


  (pop-to-buffer (concat "*camldebug*"))
  (setq words (split-string command-line))
  (message "Current directory is %s" default-directory)
  (apply 'make-comint (cons "camldebug"
                      (cons (car words)
                      (cons nil (cdr words)))))
  (set-process-filter (get-buffer-process (current-buffer))
                      'camldebug-filter)
  (set-process-sentinel (get-buffer-process (current-buffer))
                        'camldebug-sentinel)
  (camldebug-mode)
  (camldebug-set-buffer))

;; Less confirmations
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)

;;Simple bindings for delete and backspace, delete-selection and transient
;;modes
;; EMACS-24
;;(pc-selection-mode)

;;Window-splitting enhancement for WideScreen
(defun window-split-horizontally-twice ()
  "split the window in three horizontally"
  (interactive)
  (split-window-horizontally (* (/ (window-width) 3) 2))
  (set-window-buffer (next-window) (other-buffer))
  (split-window-horizontally)
  (set-window-buffer (next-window) (other-buffer)))

(global-set-key (kbd "C-x 4") 'window-split-horizontally-twice)

; It's annoying to have emacs suddenly suspend
(global-unset-key (kbd "C-z"))

;; for Kernighan style in braces arrangement
(fset 'braces-insertion
   [?{ return tab up ?\C-e return tab])

(global-set-key "\C-x\'" 'braces-insertion)

; Backup shortcut key for modes where tab is special (such as python)
(global-set-key "\M-/" 'hippie-expand)

;; IDO : better minibuffer completion
(use-package ido
  :ensure t
  :init  (setq ido-enable-flex-matching t
               ido-ignore-extensions t
               ido-use-virtual-buffers t
               ;; Don't be case sensitive
               ido-case-fold t
               ;; If the file at point exists, use that
               ;;ido-use-filename-at-point t
               ;; Or if it is an URL
               ;;ido-use-url-at-point t
               ;; Even if TAB completes uniquely,
               ;; still wait for RET
               ido-confirm-unique-completion t
               ;; If the input does not exist,
               ;; don't look in unnexpected places.
               ;; I probably want a new file.
               ido-auto-merge-work-directories-length -1
               ido-enable-flex-matching t
               ido-create-new-buffer 'always
               ido-everywhere t)
  :config
  (ido-mode 1)
  (ido-everywhere 1))

;; Flx
(use-package flx-ido
   :ensure t
   :init (setq ido-enable-flex-matching t
               ido-use-faces nil)
   :config (flx-ido-mode 1))

;; EXPERIMENTAL : execute-extended-command
(use-package smex
  :bind
  ("M-x" . smex)
  ("M-X" . smex-major-mode-commands)
  ;; This is the old M-x.
  ("C-c C-c M-x" . execute-extended-command)
  :init
  (smex-initialize);trigger use
  )

;; personal key-binding for comfort on my azerty keyboard (when I type french texts):
;; C-home deletes the indentation of the current line,
;; then merges it with the preceding line.
;; useful when programming
;; default key-binding is M-^
(define-key global-map [(control home)] 'delete-indentation)

;;load abbrevs for automatic completion of words (M-/)
(condition-case ()
  (quietly-read-abbrev-file)
  (file-error nil))

;;OCaml: automatically launch the tuareg mode when opening a file ending with .ml, .mli etc.
(setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Mode majeur pour éditer du code Caml" t)
(autoload 'camldebug "camldebug" "Exécuter le débogueur Caml" t)
(autoload 'caml-types "caml-types" "View the inferred types" t)

;; tuareg i-menu
(autoload 'tuareg-imenu-set-imenu "tuareg-imenu" "Configuration of imenu for tuareg" t)
(add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)

; Indiquer le nom de l'utilisateur@machine ur le cadre de la frame
(setq frame-title-format
      (list (user-real-login-name)"@"(system-name)":"'buffer-file-name))
(setq icon-title-format "%f")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; from the emacs manual
(setq font-lock-maximum-decoration t) ;as colored as possible
(setq visible-bell t) ;stop beeping

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; from http://www-rocq.inria.fr/~fleuret
(setq show-paren-delay 0);Show the matching immediately
(setq default-indicate-empty-lines t);show me empty lines at the end of the buffer

;automatic text
(defun start-latex ()
  "Adds all that stuff to start a new LaTeX document"
  (interactive)
  (goto-char (point-min))
  (insert "\\documentclass[a4paper,french]{article}
\\title{}
\\author{}
\\date{}

\\usepackage[french]{babel}    %pour la typographie et le texte automatique en français
\\usepackage{indentfirst}      %pour les paragraphes à la française
\\usepackage[utf8x]{inputenc} %pour les accents
\\usepackage[T1]{fontenc}      %pour la césure des mots accentués
\\usepackage{xspace}           %pour les commandes genre \cad -> c'est-à-dire



\\begin{document}

\\maketitle


")
  (goto-char (point-max))
  (insert "
\\end{document}
")
  (goto-char (point-min))
  (next-line 2)
  (backward-char 2)
  (LaTeX-mode)
)
;; Inserting licenses is now covered by yasnippet

;; The following should make Emacs ask when the buffer encoding is not
;; that with which emacs is set up to save the file. Emacs will then
;; prompt to chose one among "safe" encodings.
(setq select-safe-coding-system-accept-default-p
      '(lambda (coding)
         (string=
          (coding-system-base coding)
          (coding-system-base buffer-file-coding-system))))

; Gestion des backup (fichiers~)
; from http://mail.gnu.org/archive/html/help-gnu-emacs/2002-07/msg00117.html
(setq backup-directory-alist
      '((".+\\.tex$" . "~/backup/tex")
        ("/[^\\./]+\\..+$" . "~/backup/code")
        ("/[^\\.]+$" . "~/backup")
        ("/\\.." . "~/backup/dotfiles")))
(setq version-control t                ; Use version numbers for backups
      kept-new-versions 16             ; Number of newest versions to keep
      kept-old-versions 2              ; Number of oldest versions to keep
      delete-old-versions t            ; Ask to delete excess backup versions?
      backup-by-copying-when-linked t
      trim-versions-without-asking t ) ; Copy linked files, don't rename.

; Le curseur ne clignote pas.
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode -1))

; Bip visuel : l'écran flashe au lieu que la machine bippe.
(setq visible-bell nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; Configuration de divers modes ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'vc-mode-hook
          (if (string= (vc-backend (buffer-file-name)) "Git")
              (progn (local-unset-key "\C-x v a")
                     (local-set-key "\C-x v a" 'add-change-log-entry-other-window))
            ))

;; VC-mode is evolving disruptively in emacs-24,
;; which troubles auto-revert settings based on it.
;; we brutally activate auto-revert everywhere.
(global-auto-revert-mode t)

;; Check modeline for adds from vc
;; (defadvice vc-mode-line (after auto-revert-vc activate)
;;  "Make vc-mode turn on autorevert"
;;  (unless (boundp 'auto-revert-mode)
;;    (turn-on-auto-revert-mode)))
(setq auto-revert-check-vc-info t)

(add-hook 'text-mode-hook
          (lambda ()
            (auto-fill-mode 1)
            (if (fboundp 'comment-auto-fill-only-comments)
                (setq comment-auto-fill-only-comments nil))
            ))
;; EMACS-24
;;(add-hook 'text-mode-hook 'turn-on-filladapt-mode)

;; predefined generic modes for config files edition
(require 'generic-x) ; trigger use

; Les lignes sont coupées au bout de 74 caractères dans les modes où
; l'autofill est activé.
(setq-default fill-column 74)

(require 'tex-site)
(if (featurep 'tex-site)
    (add-hook 'LaTeX-mode-hook
              (lambda()
             ; On fournit le raccourci C-c f pour la commande de compilation.
                (local-set-key
                 [(control ?c) ?f]
                 (lambda () (interactive)
                   (TeX-command "LaTeX" 'TeX-master-file)))
             ; On fournit le raccourci C-c v pour la commande de visualisation.
                (local-set-key
                 [(control ?c) ?v]
                 (lambda () (interactive)
                   (TeX-command "View" 'TeX-master-file)))
                (auto-fill-mode 1)
                (setq comment-auto-fill-only-comments nil)
                ))
  ; Si AucTeX n'est pas chargé, on se contente du mode latex de base.
  (add-hook 'latex-mode-hook
            (lambda ()
              (setq tex-command
                    "latex \\\\nonstopmode\\\\input `basename * .tex`")
              (setq tex-dvi-view-command "xdvi")
              (setq comment-auto-fill-only-comments nil))))

; Se placer en mode latex et non tex lors de l'ouverture d'un fichier .tex.
(setq tex-default-mode 'LaTeX-mode)

; compilation avec Make en mode AucTeX
(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("Make" "make" TeX-run-compile nil t
      :help "Run make on directory")
    TeX-command-list)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-save-query nil)
  (setq TeX-PDF-mode t)))

; RefTeX
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" t)
(autoload 'reftex-citation "reftex-cite" "Make citation" t)
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase Mode" t)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex) ; with Emacs latex mode

; Commun aux modes c et c++.
(add-hook 'c-mode-common-hook
          (lambda ()
            ; On s'assure que delete fait vraiment delete et non backspace.
            ; Normalement, c'est pas la peine, mais on sait jamais...
            (setq c-delete-function 'delete-char)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Quelques bindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; « global-set-key » fonctionne comme « local-set-key », mais agit sur la
; keymap globale, qui a les deux particularités suivantes :
; - elle est utilisée dans tous les buffers
; - elle a plus faible priorité que les keymaps locales des buffers.

; « compile » permet de lancer une commande, recueille la sortie de la
; commande dans un buffer, puis, à chaque fois que vous tapez « C-x ` »,
; avance dans ce buffer jusqu'à tomber sur un message d'erreur indiquant un
; fichier et une position dans ce fichier (ce que font la plupart des
; compilateurs lorsqu'il y a une erreur dans un programme), et vous amène
; à l'endroit en question. C'est très pratique pour débuguer.
(global-set-key [f9] 'compile)
(global-set-key [f10] 'recompile)

(global-set-key [(meta ?g)] 'goto-line)

; « C-x u » appelle la fonction d'undo. Lorsque l'on doit le taper un grand
; nombre de fois, il arrive très souvent que l'on tape « C-x C-u », ce qui
; casse tout. On corrige :
(global-set-key [(control ?x) (control ?u)] 'advertised-undo)
(global-set-key [(control ?x) (meta ?u)] 'upcase-region)

; Il parait que :
;; Vous remercierez ces deux bindings quand vous serez en telnet depuis un Mac
; [.emacs conscrit selon Saint DOM, verset 159]
; Je ne sais pas quel est le problème, mais pourquoi pas...
(global-set-key [(control ?x) ?s] 'save-buffer)
(global-set-key [(control ?x) ?c] 'save-buffers-kill-emacs)

; Ces deux commandes sont bindées sur « M-{ » et « M-} », qui sont pénibles
; à taper sur un azerty.
(global-set-key [(meta ?p)] 'backward-paragraph)
(global-set-key [(meta ?n)] 'forward-paragraph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Divers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Lorsque l'on demande de compléter le nom d'un fichier, emacs ignorera les
; fichiers dont le nom se termine par ces suffixes. Vous pouvez y ajouter
; ou en enlever les extensions qui vous chantent.
(setq completion-ignored-extensions
      (append
       '(".zo" ".zi" ".cmo" ".cmi" ".cmx" ".aux" ".bbl" ".blg" ".dvi"
         ".pdf" ".ps" ".log" ".glob" ".annot")
       completion-ignored-extensions))

; Par défaut, on ouvre un fichier en mode texte.
(setq default-major-mode 'text-mode)

; Vous ne voulez pas utiliser rmail!
(put 'rmail 'disabled t)

; On réactive ces deux commandes.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

; On fait en sorte que tous les fichiers dont le nom contient « makefile »
; ou « Makefile » soient mis en mode make lorsqu'on les ouvre.

(setq auto-mode-alist
      (append (list
                '("[Mm]akefile" . makefile-mode))
              auto-mode-alist))

; Par défaut, les recherches/remplacements ne tiennent pas compte des
; différences minuscules/majuscules. Ça peut être pénible. Pour désactiver
; dans un buffer : « M-x toggle-case-fold-search ». Pour désactiver
; définitivement, décommentez la ligne suivante :
;; (setq-default case-fold-search nil)

; Par défaut, les fonctions d'indentations utilisent des tabulations, ce
; qui est très pénible. La ligne suivante fait en sorte que seuls des
; espaces soient insérés pour indenter.
(setq-default indent-tabs-mode nil)

;; Fond clair pour les Xterm
(unless window-system
  (setq frame-background-mode (quote light)))

;; preview-latex
(autoload 'TeX-preview-setup "preview")
(add-hook 'LaTeX-mode-hook 'LaTeX-preview-setup)
;; flyspell
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

;; flycheck
;; ==== flycheck ====

(use-package flycheck
  :bind
  (("M-n" . flycheck-next-error)
   ("M-p" . flycheck-previous-error))
  :init
  (setq flycheck-disabled-checkers '(python-flake8 python-pylint)))

;; ===== flycheck-pyflakes ======
(use-package flycheck-pyflakes)


;; Divers : prompts are bad
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)
;; If I say kill a buffer, kill its processes too !
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

; Pour naviguer entre les fenetres
;; undo-redo window configs
(when (fboundp 'winner-mode)
      (winner-mode 1))
(windmove-default-keybindings)

; extension to mode mapping : csharp-mode, among others
(setq auto-mode-alist
      (append '(
                ("\\.src$" . latex-mode)
                ("\\.md$" . emacs-lisp-mode)
                ("\\.txt$" . text-mode)
                ("\\.cs$" . csharp-mode)
                ("\\.xt$" . csharp-mode)
                ("\\.xti$" . csharp-mode)
                ("\\.aspx$" . html-mode)
                ("\\.v$" . proof-mode)
                ) auto-mode-alist ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              C# Mode support
;;;
(autoload 'csharp-mode "csharp-mode")

;; Scala-mode support
(setq
  ensime-sbt-command "/home/huitseeker/bin/sbt"
  sbt:program-name "/home/huitseeker/bin/sbt")

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

(use-package ensime
  :commands ensime ensime-mode)

;; (require 'ensime)
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(push "/home/huitseeker/Scala/scala/build/quick/bin/" exec-path)
(push "/usr/bin/" exec-path)


(require 'compile);; TOFIX
(setq compilation-error-regexp-alist
      (append
       '(;C# Compiler
         ;t.cs(6,18): error SC1006: Name of constructor must match name of class
         ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)[,]\\([0-9]+\\)): \\(error\\|warning\\) CS[0-9]+:" 1 3 4))
       compilation-error-regexp-alist))
;; compilation tries to find its buffer in other windows
(setq-default display-buffer-reuse-frames t)
(setq pop-up-windows nil)

;; Whizzy correction
(setq whizzytex-semantic-filtering 0)

;; Magit
;; == magit ==
(use-package magit
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status)
  :init
  (setq magit-diff-options (quote ("--word-diff")))
  (setq magit-diff-refine-hunk 'all)
  (setq magit-process-connection-type nil)
  ;; https://github.com/philjackson/magit/issues#issue/18
  ;; Infamous 'v' keypress suffices to revert by default !
  (setq magit-revert-item-confirm t)
  )

(use-package magithub
  :after magit
  :config (magithub-feature-autoinject t))



;; NXml-mode support
(autoload 'nxml-mode "nxml-mode" "XML editing mode." t)
(add-to-list 'auto-mode-alist
             (cons
              (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss") t) "\\'")
              'nxml-mode))
(fset 'xml-mode 'nxml-mode)

;; Coq mode
;; (setq auto-mode-alist (cons '("\\.v$" . coq-mode) auto-mode-alist))
;;   (autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)

;; Lose the UI
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Create a new scratch buffer
(defun create-scratch-buffer nil
  "create a new scratch buffer to work in. (could be *scratch* .. *scratchX*)"
  (interactive)
  (let ((n 0)
        bufname
        buf)
    (while (progn
             (setq bufname (concat "*scratch"
                                    (if (= n 0) "" (int-to-string n))
                                    "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    )
  )

(global-set-key (kbd "<f12>") 'create-scratch-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;
;;    Org-mode config ;;
;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-to-list 'load-path "~/.emacs.d/packages/org-mode/lisp")
;; (add-to-list 'load-path "~/.emacs.d/packages/org-mode/contrib/lisp")
(autoload 'org-mode "org" "Org mode" t)
;; (eval-after-load "org"
;;   '(setq org-log-done t))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(require 'org-install) ; trigger use
;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
;; (require 'ox-koma-letter)
;; (require 'ox-rst)
;; (require 'ox-beamer)
;; (require 'ob-core)
;; (require 'ob-exp)
;; (require 'ox-md)
;; (require 'ox-texinfo)
;; (require 'ox-odt)
;; (add-to-list 'load-path "~/.emacs/packages/org-asciidoc/")
;; (require 'ox-asciidoc)

(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name) (file-exists-p (buffer-file-name))
       (progn
     ;enable auto-revert-mode to update reftex when bibtex file changes on disk
     (global-auto-revert-mode t)
     (reftex-parse-all)
     ;add a custom reftex cite format to insert links
     (reftex-set-cite-format "** %l: %t \n")
     ))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
)

(add-hook 'org-mode-hook 'org-mode-reftex-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remember mode config ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-after-load "remember"
'(progn
   (setq remember-annotation-functions '(org-remember-annotation))
   (setq remember-handler-functions '(org-remember-handler))
   ))
(add-hook 'remember-mode-hook 'org-remember-apply-template)

;; ProofGeneral
;; (load "/opt/ProofGeneral/generic/proof-site.el") ; trigger use
;; (eval-after-load "proof-site"
;;   '(progn
;;      (load-file "~/coqfinitgroup/branches/release1/src/pg-ssr.el")
;;      (setq coq-prog-name "~/bin/ssrcoq")
;;      )
;; )

;; Autocompletion
(require 'hippie-exp) ; trigger use
(setq hippie-expand-try-functions-list
      '(try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-expand-dabbrev
        try-expand-dabbrev-from-kill
        try-expand-dabbrev-all-buffers
        try-complete-file-name-partially
        try-complete-file-name
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-expand-whole-kill))

;; DWIM [tab] key
(defun hippie-indent-or-expand (arg)
  "Either indent according to mode, or expand the word preceding
point using hippie-completion."
  (interactive "*P")
  (if (and
       (or (bobp) (= ?w (char-syntax (char-before))))
       (or (eobp) (not (= ?w (char-syntax (char-after))))))
      (hippie-expand arg)
    (indent-according-to-mode)))

(defun ac-indent-or-expand ()
  "Either indent according to mode, or expand the word preceding
point using autocomplete."
  (interactive "*")
  (if (and
       (or (bobp) (= ?w (char-syntax (char-before))))
       (or (eobp) (not (= ?w (char-syntax (char-after))))))
      (ac-start)
    (indent-according-to-mode)))

(global-set-key (quote [S-iso-lefttab]) (quote hippie-indent-or-expand))
(global-set-key (quote [S-tab]) (quote hippie-indent-or-expand))

(defun my-tab-fix ()
  (local-set-key [tab] 'ac-indent-or-expand))

(add-hook 'c-mode-hook          'my-tab-fix)
(add-hook 'sh-mode-hook         'my-tab-fix)
(add-hook 'tuareg-mode-hook         'my-tab-fix)
(add-hook 'emacs-lisp-mode-hook 'my-tab-fix)
(add-hook 'coq-mode-hook 'my-tab-fix)
(add-hook 'proof-mode-hook 'my-tab-fix)
(add-hook 'mail-mode-hook 'my-tab-fix)
(add-hook 'LaTeX-mode-hook 'my-tab-fix)
(add-hook 'python-mode-hook 'my-tab-fix)
(add-hook 'scala-mode-hook 'my-tab-fix)
(add-hook 'adoc-mode-hook 'my-tab-fix)
(add-hook 'text-mode-hook 'my-tab-fix)

;; Just as a failsafe
(setq tab-always-indent 'complete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-complete, without interference with the above ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'help-mode)
; help-mode necessary,see3
; http://github.com/m2ym/auto-complete/issues#issue/34
(use-package auto-complete) ; trigger use
; fuzzy completion/search
(use-package fuzzy
  :config
  (turn-on-fuzzy-isearch)
  )
; complete with tab, except in menus
(setq ac-use-menu-map t)
(define-key ac-menu-map [return] 'ac-complete)
(define-key ac-menu-map [tab] 'ac-next)
(define-key ac-menu-map [S-iso-lefttab] 'ac-previous)
(define-key ac-complete-mode-map [tab] 'ac-complete)
(define-key ac-complete-mode-map [return] 'ac-complete)
(define-key ac-complete-mode-map [C-g] 'ac-stop)
; (setq ac-trigger-key [tab])
;(when (require 'auto-complete-config nil 'noerror) ;; don't break if not installed
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (setq ac-comphist-file  "~/.emacs.d/ac-comphist.dat")
  (ac-config-default)
;)
(setq
     ac-delay 0.5
     ac-auto-start 2
     ac-use-quick-help nil
     ac-menu-height 15
     ac-show-menu-immediately-on-auto-complete nil
     ac-use-fuzzy t
     ac-candidate-limit 20)
(use-package ac-math)
; add LaTeX source
(add-hook 'LaTeX-mode-hook
                 (lambda ()
                   ; add them at the end of the list !
                   (add-to-list 'ac-sources 'ac-source-math-latex t)
                   (add-to-list 'ac-sources 'ac-source-latex-commands t)
                   ; don't only autofill comments
                   (if (fboundp 'comment-auto-fill-only-comments)
                       (setq comment-auto-fill-only-comments nil))
                   ))
; dabbrev by default
(require 'dabbrev)

(defun sanityinc/dabbrev-friend-buffer (other-buffer)
  (< (buffer-size other-buffer) (* 1 1024 1024)))
(setq dabbrev-friend-buffer-function 'sanityinc/dabbrev-friend-buffer)

(defun dabbrev--wrapper-ordered (prefix ignore-case &optional maxnum)
    "A wrapper for dabbrev that returns a list of expansions of
  PREFIX ordered in the same way dabbrev-expand find expansions.
  First, expansions from the current point and up to the beginning
  of the buffer is listed. Second, the expansions from the current
  point and down to the bottom of the buffer is listed. Last,
  expansions in other buffers are listed top-down. The returned
  list has at most MAXNUM elements."
    (dabbrev--reset-global-variables)
    (let ((all-expansions nil)
          (i 0)
          expansion)
      ;; Search backward until we hit another buffer or reach max num
      (save-excursion
        (while (and (or (null maxnum) (< i maxnum))
                    (setq expansion (dabbrev--find-expansion prefix 1 ignore-case))
                    (not dabbrev--last-buffer))
          (setq all-expansions (nconc all-expansions (list expansion)))
          (setq i (+ i 1))))
        ;; If last expansion was found in another buffer, remove of it from the
        ;; dabbrev-internal list of found expansions so we can find it when we
        ;; are supposed to search other buffers.
        (when (and expansion dabbrev--last-buffer)
          (setq dabbrev--last-table (delete expansion dabbrev--last-table)))
        ;; Reset to prepare for a new search
        (let ((table dabbrev--last-table))
          (dabbrev--reset-global-variables)
          (setq dabbrev--last-table table))
        ;; Search forward in current buffer and after that in other buffers
        (save-excursion
          (while
              (and (or (null maxnum) (< i maxnum))
                   (setq expansion (dabbrev--find-expansion prefix -1 ignore-case)))
            (setq all-expansions (nconc all-expansions (list expansion)))
            (setq i (+ i 1))))
        all-expansions))

; grab only the first 20 completions
(defun ac-source-dabbrev (abbrev)
  (interactive)
  (dabbrev--reset-global-variables)
  (let ((dabbrev-check-all-buffers t))
    (dabbrev--wrapper-ordered abbrev t 10)))

(defvar ac-source-dabbrev-words
  '((candidates
     . (lambda () (all-completions ac-target
                              (ac-source-dabbrev ac-target))))
    (cache))
  "Get all the completions using dabbrev")

(setq-default ac-sources
              '(ac-source-dabbrev-words ac-source-dictionary ac-source-words-in-buffer
              ac-source-words-in-same-mode-buffers
              ac-source-words-in-all-buffer))
; activate everywhere
(define-globalized-minor-mode real-global-auto-complete-mode
  auto-complete-mode (lambda ()
                       (if (not (minibufferp (current-buffer)))
                         (auto-complete-mode 1))
                       ))
(real-global-auto-complete-mode t)

; flyspell compatibility
(add-hook 'flyspell-mode-hook
          'ac-flyspell-workaround)
; automatic language detection for flyspell
(use-package auto-dictionary
  :hook flyspell-mode)

;;;;;;;;;;;;;;;
;; Yasnippet ;;
;;;;;;;;;;;;;;;
;; (require 'yasnippet);trigger use
;; (yas/initialize)
;; (yas/load-directory "~/elisp/snippets")
;; I don't like using partial words for completion
;; (setq yas/key-syntaxes '("w_" "w_." "^ "))
;; (setq hippie-expand-try-functions-list
;;       (cons
;;        'yas/hippie-try-expand
;;        hippie-expand-try-functions-list))

;; (defun yasnippet-config () (progn
;;              (make-variable-buffer-local 'yas/trigger-key)
;;              ; Invocated indirectly through ac-complete
;;              ; (setq yas/trigger-key [tab])
;;              (define-key yas/keymap [tab] 'yas/next-field-or-maybe-expand)
;;              (define-key yas/keymap [S-iso-lefttab] 'yas/prev-field)
;;              (yas/minor-mode-on)
;;              (local-unset-key "\M-n")
;;              (local-unset-key "\M-p")
;;              (add-to-list 'ac-sources 'ac-source-yasnippet)
;;              ))

;; (add-hook 'proof-mode-hook 'yasnippet-config)
;; (add-hook 'tuareg-mode-hook 'yasnippet-config)
;; (add-hook 'LaTeX-mode-hook 'yasnippet-config)

;;;;;;;;;;;;
;; Python ;;
;;;;;;;;;;;;
(use-package python
  :mode ("\\.py\\'" . python-mode)
        ("\\.wsgi$" . python-mode)
  :interpreter ("python" . python-mode)

  :init
  (setq-default indent-tabs-mode nil)

  :config
  (setq python-indent-offset 4)
  (add-hook 'python-mode-hook 'smartparens-mode)
  (add-hook 'python-mode-hook 'color-identifiers-mode))

(use-package color-identifiers-mode)

(require 'comint)
(define-key comint-mode-map [(meta p)]
   'comint-previous-matching-input-from-input)
(define-key comint-mode-map [(meta n)]
   'comint-next-matching-input-from-input)
(define-key comint-mode-map [(control meta n)]
    'comint-next-input)
(define-key comint-mode-map [(control meta p)]
    'comint-previous-input)

(use-package jedi
  :ensure t
  :init
  (jedi:ac-setup)
  )

(setq py-load-pymacs-p nil)
(require 'pymacs)
(eval-after-load "python-mode"
'(progn
   ;; Initialize Pymacs
   (autoload 'pymacs-apply "pymacs")
   (autoload 'pymacs-call "pymacs")
   (autoload 'pymacs-eval "pymacs" nil t)
   (autoload 'pymacs-exec "pymacs" nil t)
   (autoload 'pymacs-load "pymacs" nil t)
   (setq py-load-pymacs-p nil)
   ;; ;; Initialize Rope
   ;; (pymacs-load "ropemacs" "rope-")
   ;; (setq ropemacs-confirm-saving nil
   ;;       ropemacs-guess-project t
   ;;       ropemacs-enable-autoimport t
   ;;       )
   ;; (autoload 'anything-ipython-complete "anything-ipython" "" t)
   ;; (add-hook 'python-mode-hook '(lambda ()
   ;;                                 (define-key py-mode-map (kbd "M-<tab>") 'anything-ipython-complete)))
   ;; (add-hook 'ipython-shell-hook '(lambda ()
   ;;                                   (define-key py-mode-map (kbd "M-<tab>") 'anything-ipython-complete)))
   ;; ; ac features
   ;; (ac-ropemacs-initialize)
   ;; (add-hook 'python-mode-hook
   ;;           '(lambda ()
   ;;             (add-to-list 'ac-sources 'ac-source-ropemacs)))
   ))

;; py-autopep8
(use-package py-autopep8
  :ensure t
  :commands (py-autopep8-enable-on-save py-autopep8-buffer)
  :init
  (add-hook 'python-mode-hook 'py-autopep8-enable-on-save))

; Eldoc mode
; shows you the argument list of the function call you are currently writing
;; ensure:
;;; pip install jedi
;;  pip install flake8
;;  pip install importmagic
;;  pip install autopep8
;;  pip install yapf
(use-package elpy
  :ensure t
  :commands elpy-enable
  :init (with-eval-after-load 'python (elpy-enable))

  :config
  (electric-indent-local-mode -1)
  (delete 'elpy-module-highlight-indentation elpy-modules)
  (delete 'elpy-module-flymake elpy-modules))

;; AG
(use-package ag
  :ensure    t
  :commands  ag
  :init      (setq ag-highlight-search t)
  :config    (add-to-list 'ag-arguments "--word-regexp"))

;; Recentf
(use-package recentf
  :init
  (setq recentf-max-menu-items 25
        recentf-auto-cleanup 'never
        recentf-keep '(file-remote-p file-readable-p))
  (recentf-mode 1)
  (let ((last-ido "~/.emacs.d/ido.last"))
    (when (file-exists-p last-ido)
      (delete-file last-ido)))

  :bind ("C-c f r" . recentf-open-files))

;; Visual regex
(use-package visual-regexp
  :ensure t
  :init
  (use-package visual-regexp-steroids :ensure t)

  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace))

  ;; if you use multiple-cursors, this is for you:
  :config (use-package  multiple-cursors
            :bind ("C-c m" . vr/mc-mark)))

;; Helm
(require 'helm) ; immediate use
(require 'helm-config)
(defvar helm-source-emacs-commands
  (helm-build-sync-source "Emacs commands"
    :candidates (lambda ()
                  (let ((cmds))
                    (mapatoms
                     (lambda (elt) (when (commandp elt) (push elt cmds))))
                    cmds))
    :coerce #'intern-soft
    :action #'command-execute)
  "A simple helm source for Emacs commands.")

(setq helm-mini-default-sources '(helm-source-buffers-list
                                  helm-source-recentf
                                  helm-source-dired-recent-dirs
                                  helm-chrome-source
                                  hgs/helm-c-source-stars
                                  hgs/helm-c-source-repos
                                  hgs/helm-c-source-search
                                  helm-source-emacs-commands
                                  helm-source-buffer-not-found))

(global-set-key (kbd "\M-X") 'helm)

;; Redisplaying parts of the buffer as pretty symbols
(use-package pretty-mode
  :config
  (global-pretty-mode 1)
  )
; This fixes bugs of the global-pretty-mode preventing correct composition
(defadvice font-lock-fontify-syntactically-region (before prettify compile activate)
  "Make font-lock do pretty-things"
  (unless pretty-mode
      (pretty-mode)))

;; Point history
(use-package point-stack
  :bind
  ("<f5>" . "point-stack-push")
  ("<f6>" . "point-stack-pop")
  ("<f7>" . "point-stack-forward-stack-pop")
  )

;; Guess coding style
(use-package guess-language)

;; Auto pair delimiters
(use-package autopair
  :init
  (autopair-global-mode) ;; enable autopair in all buffers
  :config
  (setq autopair-autowrap t)
  ) ; immediate use

;; Create better buffer names. foo.c++:src, foo.c++:extra
;; instead of foo.c++ and foo.c++<2>
;; This should really be the default behaviour for Emacs!
  (require 'uniquify)
  (setq
   uniquify-buffer-name-style 'reverse
   uniquify-separator ":")

;; EXPERIMENTAL : improved newline
(defun newline-maybe-indent ()
  "Like newline-and-indent, but doesn't indent if the previous line is blank"
  (interactive "*")
  (if (= (line-beginning-position) (line-end-position))
      (newline)
    (newline-and-indent)))

(global-set-key "\r" 'newline-maybe-indent)
(global-set-key "\C-j" 'newline)

;; EXPERIMENTAL : enforced hardcore typing efficiency with emacs
(autoload 'tinyeat-delete-whole-word "tinyeat" t)
(autoload 'tinyeat-backward "tinyeat" t)
(global-set-key [C-backspace]
                (lambda ()
                  (interactive)
                  (re-search-backward "\\s-*" nil t)
                  (backward-char 1)
                  (tinyeat-delete-whole-word))) ;;

; we want [backspace] to delete a word, but not put it in the kill ring
(global-set-key [backspace] 'tinyeat-backward)
;; Interferes with the Win+R = xterm keybinding
;; add-hook 'server-visit-hook '(lambda ()
;;  (if (eq window-system 'x)
;;     (shell-command "xmodmap -e 'clear Lock' -e 'keycode 133 = F13'")
;;  )
;;  ))
(global-set-key [f13] 'execute-extended-command)

(global-unset-key (kbd "<left>") )
(global-unset-key (kbd "<right>") )
(global-unset-key (kbd "<up>") )
(global-unset-key (kbd "<down>") )

(global-unset-key (kbd "C-<left>") )
(global-unset-key (kbd "C-<right>") )
(global-unset-key (kbd "C-<up>") )
(global-unset-key (kbd "C-<down>") )

(global-unset-key (kbd "M-<left>") )
(global-unset-key (kbd "M-<right>") )
(global-unset-key (kbd "M-<up>") )
(global-unset-key (kbd "M-<down>") )

;; Kill ring optimization
(when (require 'browse-kill-ring nil 'noerror)
  (browse-kill-ring-default-keybindings)
  )

(global-set-key (kbd "\C-c \C-y")
                '(lambda ()
                   (interactive)
                   (popup-menu 'yank-menu))
                )

;; Cleanup
(defun clean-up-buffer-or-region ()
  "Untabifies, indents and deletes trailing whitespace from buffer or region."
  (interactive)
  (save-excursion
    (unless (region-active-p)
      (mark-whole-buffer))
    (untabify (region-beginning) (region-end))
    (indent-region (region-beginning) (region-end) nil)
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (delete-trailing-whitespace))))

;; (add-hook 'before-save-hook 'delete-trailing-whitespace)
;;(autoload 'whitespace-mode           "whitespace" "Toggle whitespace visualization."        t)
(global-set-key (kbd "C-c n") 'clean-up-buffer-or-region)

;; Show trailing spaces globally
(setq-default show-trailing-whitespace t)

;; nuke whitespaces when writing to a file
(add-hook 'before-save-hook (lambda ()
                              (unless (string= major-mode "makefile-mode")
                                (whitespace-cleanup))))
;; TOFIX : problems on makefiles

;; Common warnings
(add-hook 'prog-common-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIX\\|FIXME\\|TODO\\|BUG\\|HACK\\):" 1 font-lock-warning-face t)))))

(defun force-revert-buffer ()
  "revert-buffer, without confirmation"
  (interactive) (revert-buffer t t))

(global-set-key [f8] 'force-revert-buffer)
;; auto-revert for dropbox
(defun dropbox-auto-revert ()
  (interactive)
  (when (string-match "Dropbox" buffer-file-name)
    (auto-revert-mode 1)))

(add-hook 'find-file-hook 'dropbox-auto-revert)

;; saner mode-line
(line-number-mode 1)
(column-number-mode 1)
(let ((help-echo "mouse-1: select (drag to resize), mouse-2: delete others, mouse-3: delete"))
  (setq-default mode-line-position
    `((line-number-mode
       ((column-number-mode
         (,(propertize " (%l,%c)" 'help-echo help-echo))
         (,(propertize " L%l" 'help-echo help-echo))))
       ((column-number-mode
         (,(propertize " C%c" 'help-echo help-echo))))))))

;; Count untexed words
;; Bound to C-# in LaTeX-mode.
(defun wc-latex ()
  "Count words in a buffer disregarding LaTeX macro names and
environments etc. Counts the words in the region if the mark is
active, or in the whole buffer if not. Requires external programs
wc and untex."
  (interactive)
  (shell-command-on-region-or-buffer "untex  -e-o - | wc -w"))

(defun shell-command-on-region-or-buffer (cmd)
  "Run the command CMD on region if mark is active or whole buffer if not."
  (interactive)
  (let* ((begin (if mark-active (region-beginning) (point-min)))
    (end (if mark-active (region-end) (point-max))))
    (shell-command-on-region begin end cmd)))

;; EXPERIMENTAL : unicode codepoints
(use-package unipoint)

(setq tramp-default-method "ssh")
;; reopen unwritable files as root
(defun rename-tramp-buffer ()
  (when (file-remote-p (buffer-file-name))
    (rename-buffer
     (format "%s:%s"
             (file-remote-p (buffer-file-name) 'method)
             (buffer-name)))))

(add-hook 'find-file-hook
          'rename-tramp-buffer)

(defadvice find-file (around find-file activate)
  "Open FILENAME using tramp's sudo method if it's read-only."
  (if (and (not (file-writable-p (ad-get-arg 0)))
           (y-or-n-p (concat "File "
                             (ad-get-arg 0)
                             " is read-only.  Open it as root? ")))
      (find-file-sudo (ad-get-arg 0))
    ad-do-it))

(defun find-file-sudo (file)
  "Opens FILE with root privileges."
  (interactive "F")
  (set-buffer (find-file (concat "/sudo::" file))))

;; Side-by-side splitting rather than one-above-the-other
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; Undo trees !
;; (require 'undo-tree)
;; (global-undo-tree-mode)

;; correctly wrapped comments
(require 'newcomment)
(setq comment-auto-fill-only-comments 1)
(setq-default auto-fill-function 'do-auto-fill)

;; Experimental : ace-jump-mode
(use-package ace-jump-mode
  :bind
  ("C-c SPC" . ace-jump-mode)
  )

;; Experimental : darkroom-mode
(use-package darkroom)

;; edit server for chrome
(use-package edit-server
  :init
  (edit-server-start))

;; cycle through buffers with Ctrl-Tab (like Firefox)
(global-set-key (kbd "<C-tab>") 'bury-buffer)

;; Ad-hoc flymake for Java
(defun my-java-flymake-init ()
  (list "javac" (list (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-with-folder-structure))))

;; iedit
(use-package iedit
  :init
  (defun iedit-dwim (arg)
    "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
    (interactive "P")
    (if arg
        (iedit-mode)
      (save-excursion
        (save-restriction
          (widen)
          ;; this function determines the scope of `iedit-start'.
          (narrow-to-defun)
          (if iedit-mode
              (iedit-done)
            ;; `current-word' can of course be replaced by other
            ;; functions.
            (iedit-start (current-word)))))))
  :bind
  ("C-;" . iedit-dwim)
  )

(setq custom-theme-load-path
     (cons
        (expand-file-name "~/.emacs.d/packages/emacs-color-theme-solarized")
        load-path))
(load-theme 'solarized-dark t)

;; markdown-mode
(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . gfm-mode) auto-mode-alist))

;; org-mode in text
;; (add-hook 'text-mode-hook 'turn-on-orgstruct)
;; (add-hook 'text-mode-hook 'turn-on-orgstruct++)

;; VC-check-status
(use-package vc-check-status
  :ensure vc-check-status
  :init
  (progn
    (vc-check-status-activate 1))
  )

(defun check-grammar ()
  "Checks the current buffer with atdtool"
  (interactive)
  (compile (concat "atdtool -s localhost -P 1049 " (shell-quote-argument (buffer-file-name)))))

;; (add-to-list 'load-path "~/.emacs.d/jdc/")
;; (require 'jdc)

;; Centered-window-mode
(defun center-text ()
  "Center the text in the middle of the buffer. Works best in full screen"
  (interactive)
  (set-window-margins (car (get-buffer-window-list (current-buffer) nil t))
                        (/ (window-width) 4)
                        (/ (window-width) 4)))

(defun center-text-clear ()
  (interactive)
  (set-window-margins (car (get-buffer-window-list (current-buffer) nil t))
                        nil
                        nil))

(setq centered nil)

(defun center-text-mode ()
  (interactive)
  (if centered
    (progn (center-text-clear)
           (setq centered nil))
    (progn (center-text)
           (setq centered t))))

(define-key global-map (kbd "C-c M-t") 'center-text-mode)

;; Better Jumping
(use-package avy
  :ensure t
  :init (setq avy-background t))

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
    (defun unfill-paragraph (&optional region)
      "Takes a multi-line paragraph and makes it into a single line of text."
      (interactive (progn (barf-if-buffer-read-only) '(t)))
      (let ((fill-column (point-max))
            ;; This would override `fill-column' if it's an integer.
            (emacs-lisp-docstring-fill-column t))
        (fill-paragraph nil region)))

    (define-key global-map "\M-Q" 'unfill-paragraph)

;; which-key
(use-package which-key
  :ensure t
  :defer 10
  :diminish which-key-mode
  :config

  ;; Replacements for how KEY is replaced when which-key displays
  ;;   KEY → FUNCTION
  ;; Eg: After "C-c", display "right → winner-redo" as "▶ → winner-redo"
  (setq which-key-key-replacement-alist
        '(("<\\([[:alnum:]-]+\\)>" . "\\1")
          ("left"                  . "◀")
          ("right"                 . "▶")
          ("up"                    . "▲")
          ("down"                  . "▼")
          ("delete"                . "DEL") ; delete key
          ("\\`DEL\\'"             . "BS") ; backspace key
          ("next"                  . "PgDn")
          ("prior"                 . "PgUp"))

        ;; List of "special" keys for which a KEY is displayed as just
        ;; K but with "inverted video" face... not sure I like this.
        which-key-special-keys '("RET" "DEL" ; delete key
                                 "ESC" "BS" ; backspace key
                                 "SPC" "TAB")

        ;; Replacements for how part or whole of FUNCTION is replaced:
        which-key-description-replacement-alist
        '(("Prefix Command" . "prefix")
          ("\\`calc-"       . "") ; Hide "calc-" prefixes when listing M-x calc keys
          ("\\`projectile-" . "𝓟/")
          ("\\`org-babel-"  . "ob/"))

        ;; Underlines commands to emphasize some functions:
        which-key-highlighted-command-list
        '("\\(rectangle-\\)\\|\\(-rectangle\\)"
          "\\`org-"))

  ;; Change what string to display for a given *complete* key binding
  ;; Eg: After "C-x", display "8 → +unicode" instead of "8 → +prefix"
  (which-key-add-key-based-replacements
    "C-x 8"   "unicode"
    "C-c T"   "toggles-"
    "C-c p s" "projectile-search"
    "C-c p 4" "projectile-other-buffer-"
    "C-x a"   "abbrev/expand"
    "C-x r"   "rect/reg"
    "C-c /"   "engine-mode-map"
    "C-c C-v" "org-babel")

  (which-key-mode 1))

;; Projectile
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init (projectile-global-mode 1)
  :commands projectile-ag
  :config
  (setq projectile-switch-project-action 'projectile-commander
        projectile-completion-system 'ido
        projectile-create-missing-test-files t)
  (add-to-list 'projectile-globally-ignored-files ".DS_Store")

  (def-projectile-commander-method ?d
    "Open project root in dired."
    (projectile-dired))

  (def-projectile-commander-method ?s
    "Open a *shell* buffer for the project."
    (projectile-run-shell))

  (def-projectile-commander-method ?X
    "Open a Direx buffer on the side."
    (call-interactively #'ha/projectile-direx))

  (def-projectile-commander-method ?F
    "Git fetch."
    (magit-status)
    (call-interactively #'magit-fetch-current))

  (def-projectile-commander-method ?j
    "Jack-in with Cider."
    (let* ((opts (projectile-current-project-files))
           (file (ido-completing-read
                  "Find file: "
                  opts
                  nil nil nil nil
                  (car (cl-member-if
                        (lambda (f)
                          (string-match "core\\.clj\\'" f))
                        opts)))))
      (find-file (expand-file-name
                  file (projectile-project-root)))
      (run-hooks 'projectile-find-file-hook)
      (cider-jack-in))))

;; Direx

(use-package direx
  :ensure t
  :bind (("C-c p X" . ha/projectile-direx)
         :map direx:direx-mode-map
         ("q" . kill-buffer-and-window))
  :init
  (defun kill-buffer-and-window (&optional buffer)
    "Kills the buffer and closes the window it is in."
    (interactive)
    (kill-buffer buffer)
    (delete-window)))

;; rtags

(use-package rtags
  :config
  (setq rtags-path (expand-file-name "~/bin"))
  (setq rtags-autostart-diagnostics t)
  (rtags-enable-standard-keybindings c-mode-base-map "\C-t")
  (use-package helm-rtags
    :config
    (setq rtags-use-helm t)
    (setq rtags-display-result-backend 'helm))
  (use-package flycheck-rtags
    :config
    (add-hook 'c++-mode-hook
              (lambda()
                (flycheck-select-checker 'rtags))))
  )
