;;; antivirus.scm --  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Arnaud Lechevallier <arnaud.lechevallier@free.fr>
;; Maintener: Arnaud Lechevallier <arnaud.lechevallier@free.fr>
;; Created: 2025/08/27
;; Version: 0.0.1

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
;;; Code:


(define-module (bric-a-brac packages antivirus)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages antivirus)
  #:export (yara-4.5))

(define yara-4.5
  (package
    (inherit yara)
    (name "yara")
    (version "4.5.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/VirusTotal/yara")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01sgnh6m6bprmakagxqr2w960p81qwxgfn7c21isw2qv9hzj6b5x"))))))

;; Uncomment to install with `guix package -f antivirus.scm'
;; yara-4.5
