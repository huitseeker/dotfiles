(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(CadetBlue ((t (:foreground "CadetBlue"))))
 '(DarkGoldenRod ((t (:foreground "DarkOrange2"))))
 '(DarkOliveGreen ((t (:foreground "green4"))))
 '(Firebrick ((t (:background "gray95" :foreground "black"))))
 '(MidnightBlue ((t (:foreground "red2" :underline t))))
 '(RosyBrown ((t (:foreground "gray56"))))
 '(artbollocks-passive-voice-face ((t (:background "blue" :foreground "Gray"))))
 '(flyspell-duplicate ((((supports :underline (:style wave)) (class color) (min-colors 89)) (:underline (:style wave :color "#b58900") :inherit unspecified)) (((class color) (min-colors 89)) (:foreground "#b58900" :weight bold :underline t))))
 '(flyspell-duplicate-face ((t (:foreground "Blue" :underline t :weight bold))) t)
 '(font-lock-function-name-face ((((class color) (min-colors 89)) (:foreground "#268bd2"))))
 '(font-lock-string-face ((((class color) (min-colors 89)) (:foreground "#2aa198"))))
 '(font-lock-type-face ((((class color) (min-colors 89)) (:foreground "#b58900"))))
 '(font-lock-variable-name-face ((((class color) (min-colors 89)) (:foreground "#268bd2"))))
 '(gnus-signature ((((class color) (min-colors 89)) (:foreground "#b58900"))))
 '(gnus-signature-face ((t (:foreground "steel blue" :slant italic))) t)
 '(message-X-header-face ((t (:foreground "magenta" :weight bold))))
 '(mouse ((t (:background "black"))))
 '(proof-locked-face ((t (:background "PaleTurquoise1"))))
 '(proof-mouse-highlight-face ((t (:background "PaleTurquoise0"))))
 '(proof-queue-face ((t (:background "peach puff"))))
 '(proof-solve-tactics-face ((t (:foreground "red"))))
 '(proof-tacticals-name-face ((nil (:foreground "violet"))))
 '(proof-tactics-name-face ((t (:foreground "RoyalBlue3"))))
 '(region ((((class color) (min-colors 89)) (:foreground "#002b36" :background "#93a1a1"))))
 '(secondary-selection ((((class color) (min-colors 89)) (:background "#073642")))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-font-list
   (quote
    ((1 "" "" "\\mathcal{" "}")
     (2 "\\textbf{" "}" "\\mathbf{" "}")
     (3 "\\textsc{" "}")
     (5 "\\emph{" "}")
     (6 "\\textsf{" "}" "\\mathsf{" "}")
     (9 "\\textit{" "}" "\\mathit{" "}")
     (13 "\\textmd{" "}")
     (14 "\\textnormal{" "}" "\\mathnormal{" "}")
     (18 "\\textrm{" "}" "\\mathrm{" "}")
     (19 "\\textsl{" "}")
     (20 "\\texttt{" "}" "\\mathtt{" "}")
     (21 "\\textup{" "}")
     (4 "" "" t)
     (100 "\\date{" "}")
     (116 "\\titre{" "}")
     (115 "\\signature{" "}")
     (117 "\\url{" "}")
     (109 "\\mail{" "}"))))
 '(LaTeX-math-abbrev-prefix "!")
 '(LaTeX-math-list (quote ((113 "quad" nil))))
 '(TeX-force-default-mode t)
 '(background-color nil)
 '(background-mode dark)
 '(browse-url-browser-function (quote browse-url-firefox))
 '(canlock-password "cf79ea628b8e233a910a70b43cc471b81900ee88")
 '(case-fold-search t)
 '(column-number-mode t)
 '(compilation-message-face (quote default))
 '(confirm-kill-emacs nil)
 '(coq-compile-before-require t)
 '(coq-compiler (concat (getenv "HOME") "/.opam/system/bin/coqc"))
 '(coq-dependency-analyzer (concat (getenv "HOME") "/.opam/system/bin/coqdep"))
 '(coq-prog-name (concat (getenv "HOME") "/.opam/system/bin/coqtop"))
 '(coq-script-indent nil)
 '(coq-use-holes nil)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(current-language-environment "UTF-8")
 '(cursor-color nil)
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("d057f0430ba54f813a5d60c1d18f28cf97d271fd35a36be478e20924ea9451bd" "ec5f697561eaf87b1d3b087dd28e61a2fc9860e4c862ea8e6b0b77bd4967d0ba" "3f44e2d33b9deb2da947523e2169031d3707eec0426e78c7b8a646ef773a2077" "89dd0329d536d389753111378f2425bd4e4652f892ae8a170841c3396f5ba2dd" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "e11569fd7e31321a33358ee4b232c2d3cf05caccd90f896e1df6cab228191109" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(dabbrev-upcase-means-case-search t)
 '(default-input-method "ucs")
 '(enable-local-variables (quote other))
 '(ensime-startup-notification nil)
 '(fill-column 79)
 '(flymake-allowed-file-name-masks
   (quote
    (("\\.\\(?:c\\(?:pp\\|xx\\|\\+\\+\\)?\\|CC\\)\\'" flymake-simple-make-init)
     ("\\.xml\\'" flymake-xml-init)
     ("\\.html?\\'" flymake-xml-init)
     ("\\.cs\\'" flymake-simple-make-init)
     ("\\.p[ml]\\'" flymake-perl-init)
     ("\\.php[345]?\\'" flymake-php-init)
     ("\\.h\\'" flymake-master-make-header-init flymake-master-cleanup)
     ("\\.java\\'" flymake-simple-make-java-init flymake-simple-java-cleanup)
     ("[0-9]+\\.tex\\'" flymake-master-tex-init flymake-master-cleanup)
     ("[0-9]+\\.src\\'" flymake-master-tex-init flymake-master-cleanup)
     ("\\.tex\\'" flymake-simple-tex-init)
     ("\\.idl\\'" flymake-simple-make-init))))
 '(flymake-proc-allowed-file-name-masks
   (quote
    (("\\.\\(?:c\\(?:pp\\|xx\\|\\+\\+\\)?\\|CC\\)\\'" flymake-simple-make-init)
     ("\\.xml\\'" flymake-xml-init)
     ("\\.html?\\'" flymake-xml-init)
     ("\\.cs\\'" flymake-simple-make-init)
     ("\\.p[ml]\\'" flymake-perl-init)
     ("\\.php[345]?\\'" flymake-php-init)
     ("\\.h\\'" flymake-master-make-header-init flymake-master-cleanup)
     ("\\.java\\'" flymake-simple-make-java-init flymake-simple-java-cleanup)
     ("[0-9]+\\.tex\\'" flymake-master-tex-init flymake-master-cleanup)
     ("[0-9]+\\.src\\'" flymake-master-tex-init flymake-master-cleanup)
     ("\\.tex\\'" flymake-simple-tex-init)
     ("\\.idl\\'" flymake-simple-make-init))))
 '(font-lock-global-modes t)
 '(foreground-color nil)
 '(global-font-lock-mode t nil (font-lock))
 '(global-whitespace-mode nil)
 '(gnus-build-sparse-threads t)
 '(gnus-confirm-mail-reply-to-news t)
 '(gnus-fetch-old-ephemeral-headers t)
 '(gnus-fetch-old-headers t)
 '(gnus-gcc-mark-as-read t)
 '(gnus-header-face-alist
   (quote
    (("From" nil gnus-header-from-face)
     ("Subject" nil gnus-header-subject-face)
     ("Newsgroups\\|X-Conti" nil gnus-header-newsgroups-face)
     ("X-Newsreader" nil gnus-header-newsgroups-face)
     ("X-.*" nil message-X-header-face)
     ("" gnus-header-name-face gnus-header-content-face))))
 '(gnus-novice-user nil)
 '(gnus-options-not-subscribe "gmane.*")
 '(gnus-options-subscribe "ens\\.*|yaf\\.")
 '(gnus-refer-thread-limit t)
 '(gnus-subscribe-newsgroup-method (quote gnus-subscribe-hierarchically))
 '(gnus-summary-display-while-building t)
 '(gnus-summary-make-false-root (quote empty))
 '(gnus-summary-make-false-root-always nil)
 '(gnus-thread-hide-killed nil)
 '(gnus-thread-hide-subtree nil)
 '(gnus-thread-sort-functions
   (quote
    (gnus-thread-sort-by-most-recent-number gnus-thread-sort-by-number)))
 '(gnus-use-trees nil)
 '(gnus-view-pseudos (quote automatic))
 '(gnus-visible-headers nil)
 '(gud-gdb-command-name "gdb --annotate=1")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(hl-paren-colors (quote ("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))
 '(ispell-local-dictionary "american")
 '(ispell-program-name "/usr/local/bin/ispell")
 '(jdee-server-dir "/home/huitseeker/tmp/jdee-server/target/")
 '(kill-ring-max 20)
 '(kill-whole-line t)
 '(large-file-warning-threshold nil)
 '(magit-diff-use-overlays nil)
 '(mail-extr-ignore-single-names nil)
 '(mail-source-delete-incoming 3)
 '(mail-source-delete-old-incoming-confirm nil)
 '(max-lisp-eval-depth 642)
 '(max-specpdl-size 4000)
 '(menu-bar-mode nil)
 '(message-cross-post-note "nil")
 '(message-followup-to-note "nil")
 '(mm-text-html-renderer (quote w3m-standalone))
 '(mm-verify-option (quote always))
 '(mode-line-format
   (quote
    ("%e" mode-line-client mode-line-modified mode-line-auto-compile mode-line-remote mode-line-frame-identification mode-line-buffer-identification mode-line-position
     (vc-mode vc-mode)
     mode-line-modes
     (which-func-mode
      ("" which-func-format
       #("-" 0 1
         (help-echo "mouse-1: Select (drag to resize)
mouse-2: Make current window occupy the whole frame
mouse-3: Remove current window from display"))))
     (global-mode-string
      ("" global-mode-string
       #("-" 0 1
         (help-echo "mouse-1: Select (drag to resize)
mouse-2: Make current window occupy the whole frame
mouse-3: Remove current window from display")))))))
 '(mouse-wheel-mode t nil (mwheel))
 '(nnmail-expiry-wait (quote never))
 '(org-agenda-files (quote ("~/Dropbox/org/sched.org" "~/org/tadalist.org")))
 '(org-export-html-expand nil)
 '(org-time-stamp-custom-formats (quote ("{%m/%d/%y %a}" . "{%m/%d/%y %a %H:%M}")))
 '(package-selected-packages
   (quote
    (wgrep-ag mmm-auto mmm-mode k8s-mode rust-auto-use iedit edit-server point-stack pretty-mode multiple-cursors visual-regexp-steroids visual-regexp ag py-autopep8 jedi fuzzy flx-ido color-identifiers-mode flycheck-inline string-inflection cider ac-helm popwin cargo company-reftex ac-racer flycheck-rust rust-mode proof-general srefactor direx avy pyenv-mode-auto pyenv-mode wrap-region ace-window anzu darkroom ace-jump-mode unipoint autopair guess-language auto-dictionary adoc-mode flycheck-pyflakes smex bbdb writeroom-mode eclim groovy-mode magithub pcre2el wgrep counsel w3m json-mode gtags java-imports zenburn-theme yari yaml-mode which-key volatile-highlights vc-check-status use-package tuareg tidy solarized-theme scss-mode scala-mode sass-mode redpen-paragraph rainbow-mode python-mode pymacs pylint pycomplete paredit org-ac markdown-mode magit ipython inf-ruby helm-projectile haskell-mode gist fillcode expand-region exec-path-from-shell ess ensime elpy el-get deft coffee-mode clojure-mode better-defaults auto-compile auctex artbollocks-mode adaptive-wrap ack-and-a-half)))
 '(pop-up-frames nil)
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(proof-disappearing-proofs t)
 '(proof-follow-mode (quote follow))
 '(proof-imenu-enable t)
 '(proof-script-fly-past-comments nil)
 '(proof-shrink-windows-tofit t)
 '(proof-splash-enable nil)
 '(proof-strict-read-only t)
 '(proof-three-window-enable t)
 '(racer-rust-src-path
   "~/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src" t)
 '(reftex-default-bibliography (quote ("main.bib")))
 '(reftex-plug-into-AUCTeX (quote (t t t t t)))
 '(scroll-bar-mode nil)
 '(send-mail-function (quote smtpmail-send-it))
 '(show-paren-mode t)
 '(show-paren-style (quote mixed))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(standard-indent 2)
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(tool-bar-mode nil)
 '(transient-mark-mode t)
 '(truncate-partial-width-windows nil)
 '(ultex-use-auctex t)
 '(ultex-use-bib-cite t)
 '(ultex-use-color t)
 '(ultex-use-font-latex t)
 '(ultex-use-imenu t)
 '(vc-annotate-background-mode nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(whitespace-style
   (quote
    (face tabs trailing space-before-tab indentation empty space-after-tab tab-mark)))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
