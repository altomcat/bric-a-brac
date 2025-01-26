;;; odin.scm --  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Arnaud Lechevallier <arnaud.lechevallier@free.fr>
;; Maintener: Arnaud Lechevallier <arnaud.lechevallier@free.fr>
;; Created: 2025/01/16
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
;;; My attempt to provide a definition package for ODIN compiler.

(define-module (bric-a-brac packages odin)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages python)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages commencement)
  #:export (odin))

(define odin
  (let ((commit "2aae4cfd461860bd10dcb922f867c98212a11449")
        (revision "dev-2025-01"))
    (package
     (name "odin")
     (version (git-version "0.0" revision commit))
     ;; (source (origin
     ;;          (method git-fetch)
     ;;          (uri (git-reference
     ;;                (url "https://github.com/odin-lang/Odin.git")
     ;;                (commit commit)))
     ;;          (file-name (git-file-name name version))
     ;;          (sha256
     ;;           (base32
     ;;            "17xgyr0xsg3bdfn472kniyld813vprm8g0x99qhj05w8wgirlxqr"))))
     ;; Useful to apply changes locally and confirm the right behavior
     ;; of the package we are building afterward
     (source (local-file "../../../projects/odin/Odin" "odin-checkout"
                         #:recursive? #t))
     (build-system gnu-build-system)
     (arguments
      (list #:tests? #f
            #:strip-binaries? #f
            #:phases
            #~(modify-phases %standard-phases
                             (delete 'configure)
                             (delete 'validate-runpath)
                             (delete 'strip)
                             (delete 'make-dynamic-linker-cache)
                             (replace 'build
                                      (lambda _
                                        (invoke "./build_odin.sh"
                                                "release-native")
                                        #t))
                             (replace 'install
                                      (lambda* (#:key inputs outputs #:allow-other-keys)
                                        (let* ((odin "odin")
                                               (src (assoc-ref inputs "source"))
                                               (src-base (string-append src "/base"))
                                               (src-core (string-append src "/core"))
                                               (src-vendor (string-append src "/vendor"))
                                               (out (assoc-ref outputs "out"))
                                               (out-base (string-append out "/base"))
                                               (out-core (string-append out "/core"))
                                               (out-vendor (string-append out "/vendor"))
                                               (bin (string-append out "/bin")))
                                          (install-file odin bin)
                                          (copy-recursively src-base out-base)
                                          (copy-recursively src-core out-core)
                                          (copy-recursively src-vendor out-vendor))
                                        #t))
                             (add-after 'install 'wrap-odin
                                        (lambda* (#:key inputs outputs #:allow-other-keys)
                                          (let* ((out (assoc-ref outputs "out"))
                                                 (bin (string-append out "/bin/odin"))
                                                 (llvm-lib (string-append (assoc-ref inputs "llvm")
                                                                          "/lib"))
                                                 (clang-lib (string-append (assoc-ref inputs "clang-toolchain")
                                                                           "/lib")))
                                            (wrap-program bin
                                                          `("ODIN_ROOT" = (,out))
                                                          `("LD_LIBRARY_PATH" = ( ,llvm-lib
                                                                                  ,clang-lib))))
                                          #t))
                             )))
     (native-inputs
      (list llvm-19
            clang-toolchain-19
            python-3
            which))
     (inputs
      (list clang-toolchain-19))
     (home-page "https://github.com/nakst/gf")
     (synopsis "A modern, fast, and simple systems programming language")
     (description
      "The Odin programming language is a modern systems language focused on simplicity,
performance, and productivity.  Designed as an alternative to C, it is suited for
high-performance development, including game engines, graphics programming, and systems
code.  Odin provides expressive syntax, support for data-oriented programming, a minimal
runtime, strong compile-time efficiency, and seamless C interoperability.  This package
includes the Odin compiler and standard library for building and running Odin programs.")
     (license license:expat))))

;; Uncomment to install with `guix package -f odin'
odin
