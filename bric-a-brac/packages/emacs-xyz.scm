;;; emacs-xyz.scm --  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Arnaud Lechevallier <arnaud.lechevallier@free.fr>
;; Maintener: Arnaud Lechevallier <arnaud.lechevallier@free.fr>
;; Created: 2024/08/11

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;; A bunch of new or updated packages related to emacs.

(define-module (bric-a-brac packages emacs-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-build)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages gl)
  #:use-module (bric-a-brac packages gl)
  ;; #:export (emacs-substitute)
  ;; #:export (emacs-consult-denote)
  #:export (emacs-hasliberg-theme)
  #:export (emacs-miasma-theme)
  #:export (emacs-odin-mode)
  #:export (emacs-odin-ts-mode)
  #:export (emacs-flycheck-odin)
  #:export (emacs-ace-window-next)
  #:export (emacs-svg-lib-0.2.8)
  #:export (emacs-kind-icon-0.2.2)
  #:export (emacs-simple-httpd-1.4)
  #:export (emacs-copilot-0.4)
  #:export (emacs-copilot-master)
  #:export (emacs-ob-glsl)
  #:export (emacs-compat+info)
  #:export (emacs-denote-explore-fix)
  )

(define emacs-ob-glsl
  (let ((commit "65148e0596a17e342646e8d1bac465e0ca7f31d0") ; example commit
        (revision "0"))
    (package
      (name "emacs-ob-glsl")
      (version (git-version "0.0." revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/finalpatch/ob-glsl")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1mr02xyvfs3vl2gs4f6x4plq8q1ybrfqzk5s2qqs9916k8qcll23"))))
      (build-system cmake-build-system)
      (native-inputs
       (list pkg-config (@ (gnu packages build-tools) ninja) glbinding gcc-toolchain emacs))
      (inputs
       (list sdl2 sdl2-image))
      (arguments
       (list
        #:tests? #f
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'build 'build-elisp
              (lambda _
                (copy-file (string-append #$source "/ob-glsl.el") "ob-glsl.el")
                (invoke "emacs" "--batch"
                        "-L" "."
                        "-f" "batch-byte-compile"
                        "ob-glsl.el")))
            (add-after 'install 'install-elisp
              (lambda* (#:key outputs #:allow-other-keys)
                (let ((lisp-dir (string-append #$output "/share/emacs/site-lisp")))
                  (install-file "ob-glsl.el" lisp-dir)
                  (install-file "ob-glsl.elc" lisp-dir)
                  (install-file "ob-glsl-module.so" lisp-dir))
                #t)))))
      (synopsis "Org-Babel support for GLSL (OpenGL Shading Language)")
      (description
       "This package provides an Org-Babel backend for executing GLSL code blocks
in Emacs Org mode. It uses OpenGL 3.3 via SDL2 for rendering results.")
      (home-page "https://github.com/finalpatch/ob-glsl")
      (license license:expat))))

(define emacs-copilot-0.4
  (let ((commit "7904f13d52e8dcda3af67142c42fa7c8345ba6fe"))
    (package
      (name "emacs-copilot")
      (version "0.4.0")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/copilot-emacs/copilot.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1v197qvi8m074ln005r74i7036imlcpm7nmxpzcvin87nfk0agrs"))))
    (build-system emacs-build-system)
    (synopsis "Emacs copilot")
    (description "Emacs copilot")
    (home-page "https://github.com/copilot-emacs/copilot.el")
    (license license:expat))))

(define emacs-copilot-master
  (let ((commit "59a4a292236ac9bea8756c0a0613b750b14d91eb"))
    (package
     (inherit emacs-copilot-0.4)
     (name "emacs-copilot")
     (version "master")
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/copilot-emacs/copilot.el")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dhsc7yiqmspka6yjzp32z6ah5y575r6ybddbvqfldbxsp2cdz33")))))))

(define emacs-simple-httpd-1.4
  (package
   (inherit emacs-simple-httpd)
   (name "emacs-simple-httpd")
   (version "1.4.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/skeeto/emacs-web-server")
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "05z23nxjkd7wzp9h58ap9hxmgh6kqcsyx0fi7vlh8ny2rdd631r3"))))))

(define emacs-svg-lib-0.2.8
  (let ((commit "710803c3bea1a25d6d47475c6e1eee734e7144ae"))
    (package
      (inherit emacs-svg-lib)
      (name "emacs-svg-lib")
      (version "0.2.8")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/rougier/svg-lib")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0g8pl725pw4jjalbnjkp9j5iac37xn6q1w1djwdr34pjafc93l50")))))))

(define emacs-kind-icon-0.2.2
  (package
    (inherit emacs-kind-icon)
    (name "emacs-kind-icon")
    (version "0.2.2")
    (propagated-inputs (list emacs-svg-lib-0.2.8))))

(define emacs-odin-mode
  (let ((commit "65134ecf10ffc4893ca60432b979a23c5ac9a3f1")
        (revision "0"))
    (package
     (name "emacs-odin-mode")
     (version (git-version "0.0" revision commit))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mattt-b/odin-mode")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1bx0vaqq1va772gqgq7znicamqkz4ry0wai9rlvjwzc58axhml2b"))))
     (build-system emacs-build-system)
     (home-page "https://github.com/mattt-b/odin-mode.git")
     (synopsis "Emacs major mode for editing Odin code")
     (description "This package provides an Emacs major mode for highlighting
code written in the Odin programming language.")
     (license #f))))

(define emacs-hasliberg-theme
  (package
   (name "emacs-hasliberg-theme")
   (version "0.0")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/rytswd/hasliberg-theme.git")
                  (commit "2188dcc77aec78581164ced8608b5ba23eca8859")))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "19b7hi771v5qfbv3y4gqzhhsgc3bdzgh3jdvnimxyvhx3l3cgrwh"))))
   (build-system emacs-build-system)
   (arguments
    (list
     #:phases
     #~(modify-phases %standard-phases
                      (add-after 'unpack 'require-cl-lib
                                 (lambda _
                                   (substitute* "hasliberg-theme.el"
                                                ((";;; Code:") ";;; Code:\n(require 'cl-lib)")))))))
   (home-page "https://github.com/rytswd/hasliberg-theme")
   (synopsis "An Emacs dark theme inspired by Swiss Alps.")
   (description "Hasliberg theme for Emacs has been designed for readability based on the LCH color space, to achieve a more homogenous color gradient for the theme.")
   (license license:gpl3)))

(define emacs-miasma-theme
  (package
    (name "emacs-miasma-theme")
    (version "1.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/daut/miasma-theme.el")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "07fci5vli5d8m607v0v6q535gnmcf4aqyw7020pzq0d7ij4vwacj"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/daut/miasma-theme.el")
    (synopsis "Miasma theme for Emacs")
    (description
     "Miasma is a dark color theme for Emacs inspired by the woods. It is
mostly a direct port of Miasma theme for @samp{vim} editor.")
    (license license:gpl3)))

(define emacs-substitute
  (package
   (name "emacs-substitute")
   (version "0.1.8")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://git.sr.ht/~protesilaos/substitute")
                  (commit "b81bb7789847f3d1645a60422fa080c48b93dd47")))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0s1lgpwmdjz17rd4r6qp41agmpqwa9976v1pb55p648zjh0p2vlz"))))
   (build-system emacs-build-system)
   (home-page "https://git.sr.ht/~protesilaos/substitute")
   (synopsis "Efficiently replace targets in the buffer or context")
   (description
    "Substitute is a set of commands that perform text replacement (i) throughout
      the buffer, (ii) limited to the current definition (per narrow-to-defun),
      (iii) from point to the end of the buffer, and (iv) from point to the beginning
       of the buffer.

     These substitutions are meant to be as quick as possible and, as such, differ
      from the standard query-replace (which I still use).  The provided commands
       prompt for substitute text and perform the substitution outright.")
   (license license:gpl1+)))

(define emacs-consult-denote
  (package
   (name "emacs-consult-denote")
   (version "0.2.2")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/protesilaos/consult-denote")
		  (commit "9fbe0f6a2636f46928f02ddde9c454f36aa45f39")))
            (file-name (git-file-name name version))
            (sha256
             (base32
	      "0f4xjmapxwx7k4r1m0q3czjcrb5cgn5qh8ar5yn1fbmyygykwc6z"))))
   (build-system emacs-build-system)
   (inputs
    (list emacs-denote
	  emacs-consult))
   (home-page "https://github.com/protesilaos/consult-denote")
   (synopsis "Consult integration for Denote")
   (description
    "This package integrates the @code{emacs-denote} package with Daniel Mendler's @code{emacs-consult}.
     The idea is to enhance minibuffer interactions, such as by providing a preview of the file-to-linked/opened
     and by adding more sources to the @code{consult-buffer} command.")
   (license license:gpl3)))

(define emacs-odin-ts-mode
  (let ((commit "800134c4f104ab48b28ed33c8ebce1c8b8707add")
        (revision "0"))
    (package
     (name "emacs-odin-ts-mode")
     (version (git-version "0.1" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Sampie159/odin-ts-mode.git")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "147mfmdy660j7isy2cq0npfszzrxir7vnlp1b3wzrqh1cjklr8kq"))))
     (build-system emacs-build-system)
     (arguments
      (list
       #:phases
       #~(modify-phases %standard-phases
                        (add-after 'unpack 'patch-treesit
                                   (lambda _
                                     (substitute* "odin-ts-mode.el"
                                                  (("\\(when \\(treesit-ready-p.*)" all)
                                                   (string-append "(require 'treesit)\n"
                                                                  all))))))))
     (home-page "https://github.com/Sampie159/odin-ts-mode/tree/master")
     (synopsis "Tree-sitter major mode for editing Odin files")
     (description "This package provides a tree-sitter major mode
for editing Odin programming files. According to the author, this
is still a work-in-progress.")
     (license license:expat))))

(define emacs-ace-window-next
  (let ((commit "77115afc1b0b9f633084cf7479c767988106c196")
        (revision "0"))
    (package
      (inherit emacs-ace-window)
      (name "emacs-ace-window-next")
      (version (git-version "0.10" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/abo-abo/ace-window.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1l6rp92q4crahx9nq7s6zxqyw7ccrhkl95v70vxra7zndqpqwsbq")))))))

(define emacs-flycheck-odin
  (let ((commit "44147e3baccadf36d5403f470fab92ff433ba131")
        (revision "0"))
    (package
     (name "emacs-flycheck-odin")
     (version (git-version "0.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mattt-b/flycheck-odin.git")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02wisj5fwnm3020dii17iygswn2rhpjz0rc1529wnzll4vysrq27"))))
     (build-system emacs-build-system)
     (arguments
      (list
       #:phases
       #~(modify-phases %standard-phases
                        (add-after 'unpack 'patch-for-other-modes
                                   (lambda _
                                     (substitute* "flycheck-odin.el"
                                                  ((":modes \\((.*)\\)" all modes)
                                                   (string-append ":modes ("
                                                                  modes
                                                                  " odin-ts-mode)"))))))))
     (native-inputs
      (list emacs-flycheck))
     (home-page "https://github.com/mattt-b/flycheck-odin")
     (synopsis "Odin support for Flycheck")
     (description "This package provides a Flycheck checker for Odin.")
     (license license:expat))))

(define emacs-compat+info
  (package
    (inherit emacs-compat)
    (name "emacs-compat")
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'make-info
            (lambda _
              (invoke "make" "compat.info"))))))
    (native-inputs (list texinfo))))

(define emacs-denote-explore-fix
  (package
    (name "emacs-denote-explore")
    (version "4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pprevos/denote-explore")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04lc5fw11wixbjdkzbl63g03rdybv6q4mh1dc6c9y322g8qq3r0k"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-denote emacs-dash))
    (arguments
     (list
      #:tests? #f ;no tests
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'build-info-manual
            (lambda* (#:key outputs #:allow-other-keys)
              (invoke "emacs"
                      "--batch"
                      "--eval=(require 'ox-texinfo)"
                      "--eval=(find-file \"denote-explore.org\")"
                      "--eval=(org-texinfo-export-to-info)")))
          (add-after 'install 'copy-denote-html-template
            (lambda* (#:key outputs #:allow-other-keys)
              (with-directory-excursion (in-vicinity #$output
                                                     (string-append "/share/emacs/site-lisp/denote-explore-"
                                                                    #$version))
                (install-file (in-vicinity #$source "denote-explore-network.html") ".")))))))
    (native-inputs (list texinfo))
    (home-page "https://github.com/pprevos/denote-explore")
    (synopsis "Analyse and visualise a collection of Denote notes")
    (description
     "The Denote Explore package provides auxiliary functions to
maintain and explore your collection of Denote files.  Denote Explore provides
four groups of Emacs commands:
@enumerate
@item Summary statistics: Count and visualize notes, attachments and keywords.
@item Random walks: Generate new ideas using Serendipity.
@item Janitor: Manage your Denote collection.
@item Visualisations: Visualise your Denote network as a network
graph.  (Optional dependencies GraphViz, D3js, to be acquired separately!)
@end enumerate")
    (license license:gpl3+)))

;; Uncommnent to install with `guix package -f emacs-xyz.scm'
;; emacs-substitute
;; emacs-org-appear-0.3.1
;; emacs-hasliberg-theme
;; emacs-consult-denote
;; emacs-miasma-theme
;; emacs-odin-mode
;; emacs-odin-ts-mode
;; emacs-flycheck-odin
;; emacs-ace-window-next
;; emacs-svg-lib-0.2.8
;; emacs-kind-icon-0.2.2
;; emacs-simple-httpd-1.4
;; emacs-ob-glsl
;; emacs-copilot-master
;; emacs-copilot-0.4
;; emacs-compat+info
;; emacs-denote-explore-fix
