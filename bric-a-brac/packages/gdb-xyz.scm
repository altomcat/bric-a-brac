;;; gdb-xyz.scm --  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Arnaud Lechevallier <arnaud.lechevallier@free.fr>
;; Maintener: Arnaud Lechevallier <arnaud.lechevallier@free.fr>
;; Created: 2025/01/11
;; Version: 0.0.1
;; Keywords: gdb

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
;;; Add additional definition package around GDB

(define-module (bric-a-brac packages gdb-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages base)
  #:use-module (gnu packages fontutils)
  #:export (gf))

(define gf
  (let ((commit "ab47ef8a3f21cd6383b4f26f6b121754d448aaef")
        (version "0.0.0"))
    (package
      (name "gf")
      (version version)
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/nakst/gf.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0vb0j8spq47zymzd24szgpng7alv8j9nli8rn3ibv5746zh5fvv3"))))
      (build-system gnu-build-system)
      (arguments
       (list #:tests? #f
             #:phases
             #~(modify-phases %standard-phases
                 (delete 'configure)
                 (add-before 'build 'fix-builder
                   (lambda _
                     (substitute* "build.sh"
                       (("if \\[.*;")
                        (string-append
                         "extra_flags=\"$(pkg-config --cflags freetype2)"
                         " -DUI_FREETYPE"
                         " $(pkg-config --libs freetype2)\""))
                       (("else.*fi") ""))
                     #t))
                 (add-before 'build 'SSE2-ready
                   (lambda _
                     (when #$(string-prefix? "x86_64" (or (%current-system)
                                                          (%current-target-system)))
                           (substitute* "build.sh"
                             (("uname.*\"") "extra_flags=\"$extra_flags -DUI_SSE2\""))
                           (format #t (string-append
                                       "Will compile with extra flag -DUI_SSE2"
                                       " because architecture x86_64 has"
                                       " been detected.\n")))
                     #t))
                 (replace 'build
                   (lambda _
                     (invoke "./build.sh")
                     #t))
                 (replace 'install
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                     (let ((gf2 "gf2")
                           (bin (string-append (assoc-ref outputs "out")
                                               "/bin")))
                       (install-file gf2 bin))
                     #t)))))
      (native-inputs
       (list gdb
             libx11
             freetype
             pkg-config))
      (home-page "https://github.com/nakst/gf")
      (synopsis "A frontend for GDB.")
      (description
       "This package provides a frontend to the GNU debugger @code{GDB}.")
      (license license:expat))))

;; Uncommnent to install with `guix package -f gf2'
;; gf
