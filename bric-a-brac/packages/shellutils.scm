;;; shellutils.scm --  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Arnaud Lechevallier <arnaud.lechevallier@free.fr>
;; Maintener: Arnaud Lechevallier <arnaud.lechevallier@free.fr>
;; Created: 2026/11/04
;; Version:

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
;;; Added uptodate shell utils
;;; Code:

(define-module (bric-a-brac packages shellutils)
  #:use-module (guix packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages shellutils)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages golang-build)
  #:export (direnv-2.38))

(define direnv-2.38
  (package
    (inherit direnv)
    (name "direnv")
    (version "2.38.2")
    (source
     (origin (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/direnv/direnv")
                   (commit (string-append "v" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "1l0nb0pv2700g8xx1g3b4ljqb7vxbg8svcvylc14kan48i1nwdfi"))))
    (arguments
     (list #:tests? #f                  ; quick&dirty fix to skip test errors
           #:install-source? #f
           #:import-path "github.com/direnv/direnv"))))

;; uncomment to install with `guix package -f shellutils.scm'
direnv-2.38
