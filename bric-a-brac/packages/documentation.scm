;;; documentation.scm --  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Arnaud Lechevallier <arnaud.lechevallier@free.fr>
;; Maintener: Arnaud Lechevallier <arnaud.lechevallier@free.fr>
;; Created: 2024/08/10
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

;;; Commentary: Guix definition package for `doxygen@1.9.8'

(define-module (bric-a-brac packages documentation)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages)
  #:use-module (gnu packages documentation)
  #:export (doxygen-new))

(define-public doxygen-new
  (let ((version "1.9.8")
	(release "0"))
    (package
     (inherit doxygen)
     (version version)
     (home-page "https://www.doxygen.nl")
     (source (origin
	      (method url-fetch)
	      (uri (list (string-append home-page "files/doxygen-"
					version ".src.tar.gz")
			 (string-append "mirror://sourceforge/doxygen/rel-"
					version "/doxygen-" version
					".src.tar.gz")))
	      (sha256
	       (base32
		"0qjgw7bnx668hpi4r8m366vsq118s9365zf8z4x5yjrqx0ld5qq5"))))
     )))

;; Uncommnent to install with `guix package -f doxygen'
;; doxygen-new
