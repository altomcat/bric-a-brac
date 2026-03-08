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
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-build)
  #:use-module (bric-a-brac packages python-xyz)
  #:export (peepdf)
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

;; Uncomment to install with `guix package -f security.scm'
peepdf
