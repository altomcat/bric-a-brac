;;; emacs-xyz.scm --  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Arnaud Lechevallier <arnaud.lechevallier@free.fr>
;; Maintener: Arnaud Lechevallier <arnaud.lechevallier@free.fr>
;; Created: 2024/08/11
;; Version: 0.0.1

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
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:export (emacs-substitute)
  #:export (emacs-org-appear-0.3.1)
  )

(define-public emacs-substitute
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


(define-public emacs-org-appear-0.3.1
  (package
    (inherit emacs-org-appear)
    (name "emacs-org-appear")
    (version "0.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/awth13/org-appear")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hfhnzhmbxrw4kz977s48x4nbq86vda5dvj00s2ima2i22b8l2z4"))))
    ))


;; Uncommnent to install with `guix package -f emacs-substitute.scm'
;; emacs-substitute
;; emacs-org-appear-0.3.1
