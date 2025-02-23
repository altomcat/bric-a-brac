;;; odin.scm --  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Arnaud Lechevallier <arnaud.lechevallier@free.fr>
;; Maintener: Arnaud Lechevallier <arnaud.lechevallier@free.fr>
;; Created: 2025/01/16
;; Version: 0.0.2

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
  #:use-module (bric-a-brac packages game-development)
  #:use-module (bric-a-brac packages gl)
  #:export (odin))

(define odin
  (let ((commit "584fdc0d4ab9dcc01e218e499fdaef9969223d65")
        (revision "dev-2025-02"))
    (package
     (name "odin")
     (version (git-version "0.0" revision commit))
     ;; works preferrably on a local directory, otherwise from the git repository
     (source
      (let ((local-source  "../../../projects/odin/Odin"))
        (if (file-exists? local-source)
            (local-file local-source "odin-checkout"
                        #:recursive? #t)
            (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/odin-lang/Odin.git")
                   (commit commit)))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "12y4rjssfryyyjxkkjnx32363xf6nmd6hnxgbl235z487jla9ag0"))
             (modules '((guix build utils)))
             (snippet
              '(begin
                 ;; (for-each delete-file-recursively
                 ;;           (find-files "vendor" "(darwin|macos|windows)" #:directories? #t))
                 (for-each delete-file
                           (find-files "vendor" "\\.(a|o|lib|dll|so(\\.[0-9]+)*)$"))))))))
     (build-system gnu-build-system)
     (arguments
      (list #:tests? #f
            #:strip-binaries? #f
            #:phases
            #~(modify-phases %standard-phases
                             (delete 'configure)
                             (replace 'build
                                      (lambda _
                                        ;; FIXME: how to avoid the usage of patchelf
                                        (substitute* "build_odin.sh"
                                                     (("\\./odin run examples.*" all)
                                                      (format #f "~a ~a    ~a~%"
                                                              "LD_PRELOAD=libgcc_s.so.1"
                                                              all
                                                              "patchelf --add-needed libgcc_s.so.1 ./demo")))
                                        (invoke "./build_odin.sh")
                                        #t))
                             (replace 'install
                                      (lambda* (#:key inputs outputs #:allow-other-keys)
                                        (let* ((odin "odin")
                                               (demo "demo")
                                               (demo-odin (string-append  demo "-" odin))
                                               (sources '("/base" "/core" "/vendor"))
                                               (bin (string-append #$output "/bin")))
                                          (install-file odin bin)
                                          (rename-file demo demo-odin)
                                          (install-file demo-odin bin)
                                          (for-each
                                           (lambda (folder)
                                             (copy-recursively (string-append #$source folder)
                                                               (string-append #$output folder)))
                                           sources))
                                        #t))
                             (add-after 'install 'build-stb-static-libraries
                                        (lambda* (#:key inputs outputs #:allow-other-keys)
                                          (with-directory-excursion (string-append (getcwd) "/vendor/stb/src")
                                                                    (setenv "CC" (which "gcc"))
                                                                    (setenv "AR" (which "gcc-ar"))
                                                                    (invoke (which "make") "unix"))
                                          #t))
                             (add-after 'build-stb-static-libraries 'replace-static-libraries
                                        (lambda* (#:key inputs outputs #:allow-other-keys)
                                          (let* ((target-system #$(or (%current-target-system)
                                                                      (%current-system)))
                                                 (box2d-lib-out (string-append #$output "/vendor/box2d/lib/"))
                                                 (raylib-lib-out (string-append #$output "/vendor/raylib/linux/"))
                                                 (glfw-lib-out (string-append #$output "/vendor/glfw/lib/"))
                                                 (stb-libs-out (string-append #$output "/vendor/stb/lib"))
                                                 (box2d-lib (string-append #$box2d+static "/lib/libbox2d.a"))
                                                 (raylib-lib (string-append #$raylib-with-extras+static "/lib/libraylib.a"))
                                                 (glfw-lib (string-append #$glfw+static "/lib/libglfw3.a"))
                                                 (stb-libs (string-append (getcwd) "/vendor/stb/lib")))
                                            (cond
                                             ((string-prefix? "x86_64-linux" target-system)
                                              (copy-file box2d-lib (string-append box2d-lib-out "box2d_other_amd64_avx2.a"))
                                              (for-each (lambda (file)
                                                          (install-file file stb-libs-out))
                                                        (find-files stb-libs "\\.a$"))
                                              (install-file raylib-lib raylib-lib-out)
                                              (install-file glfw-lib glfw-lib-out))
                                             (else
                                              '())))
                                          #t))
                             (add-after 'install 'wrap-odin
                                        (lambda* (#:key inputs outputs #:allow-other-keys)
                                          (let* ((bin (string-append #$output "/bin/odin"))
                                                 (llvm-lib (string-append #$llvm "/lib"))
                                                 (clang-lib (string-append #$clang-toolchain "/lib")))
                                            (wrap-program bin
                                                          `("ODIN_ROOT" = (,#$output))
                                                          `("LD_LIBRARY_PATH" = (,llvm-lib
                                                                                 ,clang-lib))))
                                          #t)))))
     (native-inputs
      (list llvm-18
            clang-toolchain-18
            ;;python-3
            which
            patchelf
            box2d+static
            glfw+static
            raylib-with-extras+static))
     (inputs
      (list clang-toolchain-18))
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
;; odin
