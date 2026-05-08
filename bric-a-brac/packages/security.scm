;;; security.scm

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Arnaud Lechevallier <arnaud.lechevallier@free.fr>
;; Maintainer: Arnaud Lechevallier <arnaud.lechevallier@free.fr>
;; Created: 2026/03/08
;; Version:

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
;;; Added end-user applications related to security.

;;; Code

(define-module (bric-a-brac packages security)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages check)
  #:use-module (bric-a-brac packages python-xyz)
  #:export (peepdf)
  #:export (python-msoffcrypto-tool)
  #:export (python-pcodedmp)
  #:export (python-oletools)
  )


(define peepdf
  (let ((commit "28e8118a1ceb69d809eac5f5f88e1c6b49430e63"))
    (package
      (name "peepdf")
      (version "0")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/enzok/peepdf.git")
               (commit commit)
               ))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "19mlzi4gy9dqb6f1hmngw61qb8akac9r6yhkl41n0kzi5jb29j8j"))))
      (build-system python-build-system)
      (arguments
       (list #:tests? #f
             #:phases
             #~(modify-phases %standard-phases
                 (delete 'sanity-check))))
      (propagated-inputs
       (list python-pillow python-jsbeautifier python-colorama python-aespython python-libemu))
      (home-page "https://github.com/jesparza/peepdf")
      (synopsis "A Python tool for analyzing and exploring PDF files")
      (description
       "Peepdf is a Python-based tool designed to analyze and explore PDF files,
helping security researchers and forensic analysts understand the structure
and potential threats contained within them. It supports features such as
JavaScript beautification, object parsing, and decryption of embedded data.")
      (license license:gpl3+))))

(define python-msoffcrypto-tool
  (package
    (name "python-msoffcrypto-tool")
    (version "6.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nolze/msoffcrypto-tool")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05vkcdzlc9kqq0znbk355gwq3kglymjj91ax7vpcfy04p5fd06da"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-cryptography python-olefile))
    (native-inputs (list python-poetry-core))
    (home-page "https://github.com/nolze/msoffcrypto-tool")
    (synopsis
     "Python tool and library for decrypting and encrypting MS Office files using a password or other keys")
    (description
     "Python tool and library for decrypting and encrypting MS Office files using a
password or other keys.")
    (license license:expat)))

(define python-pcodedmp
  (package
    (name "python-pcodedmp")
    (version "1.2.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bontchev/pcodedmp")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sl1zs2qfagh390z3fn4sa3hqvskn3x0m6h0637i1kzbrcc8b0s9"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f                ; It needs python-oletools for tests
           #:phases                   ; which also needs pcodedmp itself
           #~(modify-phases %standard-phases
               (delete 'sanity-check))))
    ;;(propagated-inputs (list python-oletools))
    (native-inputs (list python-setuptools))
    (home-page "https://github.com/bontchev/pcodedmp")
    (synopsis "A VBA p-code disassembler")
    (description "This package provides a VBA p-code disassembler.")
    (license license:gpl3)))

(define python-oletools
  (package
    (name "python-oletools")
    (version "0.60.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/decalage2/oletools")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rmp0gjpl4sg4pw2v1hcnx1kg4m7zmssd3v0qccibdwjwmakayx2"))))
    (build-system pyproject-build-system)
    (inputs (list  python-msoffcrypto-tool))
    (propagated-inputs (list python-colorclass python-easygui python-olefile
                             python-pcodedmp python-pyparsing))
    (native-inputs (list python-pytest python-setuptools zip))
    (home-page "https://github.com/decalage2/oletools")
    (synopsis
     "Python tools to analyze security characteristics of MS Office and OLE files (also called Structured Storage, Compound File Binary Format or Compound Document File Format), for Malware Analysis and Incident Response #DFIR")
    (description
     "Python tools to analyze security characteristics of MS Office and OLE files
(also called Structured Storage, Compound File Binary Format or Compound
Document File Format), for Malware Analysis and Incident Response #DFIR.")
    (license license:expat)))

;; Uncomment to install with `guix package -f security.scm'
;; peepdf
;; python-pcodedmp
;; python-oletools
;; python-msoffcrypto-tool
