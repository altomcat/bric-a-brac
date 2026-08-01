;;; odin.scm --  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Arnaud Lechevallier <arnaud.lechevallier@free.fr>
;; Maintener: Arnaud Lechevallier <arnaud.lechevallier@free.fr>
;; Created: 2025/01/16

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
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages python)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages commencement)
  ;;
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages gl)
  ;;
  #:use-module (bric-a-brac packages game-development)
  #:use-module (bric-a-brac packages gl)
  #:export (odin)
  #:export (ols)
  #:export (ols-nightly))

;; Notes for myself:
;;
;; When building with Odin, one can append the flag -extra-linker-flags:"-Wl,-rpath,/path/one:/path/two
;; to the build command in order to avoid the later use of patchelf
;; It will link the executable with the needed libraries.

(define odin
  (package
      (name "odin")
      (version "dev-2026-07a")
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
                      (commit version)))
               (file-name (git-file-name name version))
               (sha256
                (base32
                 "1l8bfgfc2ljxb5k08nm18ykihgj74pwppax32srm0j7nwcs4q90l"))
               (modules '((guix build utils)))
               (snippet
                '(begin
                   ;; (for-each delete-file-recursively
                   ;;           (find-files "vendor"
                   ;;                       "(darwin|macos|windows)" #:directories? #t))
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
                     ;; Workaround to avoid the crash of the demo
                     (substitute* "build_odin.sh"
                       (("\\./odin run examples.*" all)
                        (format #f "~a ~a~%"
                                "LD_PRELOAD=libgcc_s.so.1"
                                all)))
                     (invoke "./build_odin.sh")
                     #t))
                 (replace 'install
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                     (let* ((odin "odin")
                            (sources '("/base" "/core" "/vendor"))
                            (bin (string-append #$output "/bin")))
                       (install-file odin bin)
                       (for-each
                        (lambda (folder)
                          (copy-recursively (string-append #$source folder)
                                            (string-append #$output folder)))
                        sources))
                     #t))
                 (add-after 'install 'build-stb-static-libraries
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                     (with-directory-excursion "./vendor/stb/src"
                       (setenv "CC" (which "gcc"))
                       (setenv "AR" (which "gcc-ar"))
                       (invoke (which "make") "unix"))
                     #t))
                 (add-after 'install 'build-cgltf-static-libraries
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                     (with-directory-excursion "./vendor/cgltf/src"
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
                            (cgltf-lib-out (string-append #$output "/vendor/cgltf/lib"))
                            (liblz4-lib-out (string-append #$output "/vendor/compress/lz4/lib"))
                            (liblua-lib-out (in-vicinity #$output "/vendor/lua/5.4/linux"))
                            (box2d-avx2-lib (string-append #$box2d-avx2+static
                                                           "/lib/libbox2d.a"))
                            (box2d-simd-lib (string-append #$box2d-simd+static
                                                           "/lib/libbox2d.a"))
                            (raylib-lib (string-append #$raylib-for-odin+static
                                                       "/lib/libraylib.a"))
                            (raylib-shared-lib (string-append #$raylib-for-odin "/lib"))
                            (glfw-lib (string-append #$glfw+static "/lib/libglfw3.a"))
                            (stb-libs (string-append (getcwd) "/vendor/stb/lib"))
                            (cgltf-lib (string-append (getcwd) "/vendor/cgltf/lib/cgltf.a"))
                            (liblz4-lib (string-append #$lz4:static "/lib/liblz4.a"))
                            (liblua-shared-lib (string-append #$lua-5.4 "/lib/liblua.so"))
                            (liblua-lib (string-append #$lua-5.4 "/lib/liblua.a")))
                       (cond
                        ((string-prefix? "x86_64-linux" target-system)
                         ;; box2d
                         (copy-file box2d-avx2-lib (string-append box2d-lib-out
                                                                  "box2d_other_amd64_avx2.a"))
                         (copy-file box2d-simd-lib (string-append box2d-lib-out
                                                                  "box2d_other_amd64_sse2.a"))
                         ;; stb
                         (for-each (lambda (file)
                                     (install-file file stb-libs-out))
                                   (find-files stb-libs "\\.a$"))
                         (install-file cgltf-lib cgltf-lib-out)
                         ;; raylib
                         (install-file raylib-lib raylib-lib-out)
                         (copy-recursively raylib-shared-lib raylib-lib-out)
                         ;; lua
                         (copy-file liblua-lib (in-vicinity liblua-lib-out "liblua54.a"))
                         (copy-file liblua-shared-lib (in-vicinity liblua-lib-out "liblua54.so"))
                         ;; glfw
                         (install-file glfw-lib glfw-lib-out)
                         ;; lz4
                         (install-file liblz4-lib liblz4-lib-out))
                        (else
                         '())))
                     #t))
                 (add-after 'install 'wrap-odin
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                     (let* ((bin (string-append #$output "/bin/odin"))
                            (raylib-lib (string-append #$output "/vendor/raylib/linux"))
                            (llvm-lib (string-append #$llvm "/lib"))
                            (clang-lib (string-append #$clang-toolchain "/lib")))
                       (wrap-program bin
                         `("ODIN_ROOT" = (,#$output))
                         `("LD_LIBRARY_PATH" = (,raylib-lib
                                                ,llvm-lib
                                                ,clang-lib))))
                     #t)))))
      (native-inputs
       (list llvm-18
             clang-toolchain-18
             which
             box2d-avx2+static
             box2d-simd+static
             glfw+static
             raylib-for-odin
             raylib-for-odin+static
             lua-5.4
             lz4))
      (inputs
       (list clang-toolchain-18
             wayland
             libxkbcommon
             glfw-3.4
             mesa
             bash-minimal))
      (home-page "https://odin-lang.org")
      (synopsis "A modern, fast, and simple systems programming language")
      (description
       "The Odin programming language is a modern systems language focused on simplicity,
performance, and productivity.  Designed as an alternative to C, it is suited for
high-performance development, including game engines, graphics programming, and systems
code.  Odin provides expressive syntax, support for data-oriented programming, a minimal
runtime, strong compile-time efficiency, and seamless C interoperability.  This package
includes the Odin compiler and standard library for building and running Odin programs.")
      (license license:expat)))

(define box2d-avx2+static
  (package
   (inherit box2d-3.1)
   (name "box2d-avx2+static")
   (arguments
    (substitute-keyword-arguments
     (package-arguments box2d)
     ((#:configure-flags original-flags)
      #~(cons* "-DBUILD_SHARED_LIBS=OFF"
               "-DBOX2D_AVX2=ON"
               "-DBOX2D_UNIT_TESTS=OFF"
               "-DBOX2D_SAMPLES=OFF"
               (filter (lambda (flag)
                         (not (member flag '("-DBOX2D_BUILD_TESTBED=OFF"
                                             "-DBOX2D_AVX2=OFF"
                                             "-DBUILD_SHARED_LIBS=ON"))))
                       #$original-flags)))
     ((#:phases phases)
      #~(modify-phases #$phases
                       (delete 'check)))))))

(define box2d-simd+static
  (package
    (inherit box2d-3.1)
    (name "box2d-simd+static")
    (arguments
     (substitute-keyword-arguments
         (package-arguments box2d)
       ((#:configure-flags original-flags)
        #~(cons* "-DBUILD_SHARED_LIBS=OFF"
                 "-DBOX2D_AVX2=OFF"
                 "-DBOX2D_UNIT_TESTS=OFF"
                 "-DBOX2D_SAMPLES=OFF"
                 (filter (lambda (flag)
                           (not (member flag '("-DBOX2D_BUILD_TESTBED=OFF"
                                               "-DBOX2D_AVX2=ON"
                                               "-DBUILD_SHARED_LIBS=ON"))))
                         #$original-flags)))
       ((#:phases phases)
        #~(modify-phases #$phases
            (delete 'check)))))))

;; When using USE_EXTERNAL_GLFW=OFF (default Odin compilation flag) that means
;; GLFW is embedded to Raylib, X11 becomes the default backend with my wayland
;; session. It's not the case with the raylib-5.5 build with an external
;; GLFW-3.4 shared library.

(define raylib-for-odin
  (let ((raylib raylib-6))
    (package
      (inherit raylib)
      (name "raylib-for-odin"))))

;; The previous explanation is true for static build too.
;; There is no work-around at the moment because GLFW needs to be embedded.
;; The final executable is linked against libraylib.a with GLFW embedded in it.
;; Add `mesa' package to be able to use X11 backend only.
;; EDIT for wayland : add `wayland', `libxkbcommon' and `glfw@3.4'

(define raylib-for-odin+static
  (let ((raylib raylib-6))
    (package
      (inherit raylib)
      (name "raylib-for-odin+static")
      (arguments
       (substitute-keyword-arguments (package-arguments raylib)
         ((#:configure-flags original-flags)
          #~(cons* "-DBUILD_SHARED_LIBS=OFF"
                   "-DWITH_PIC=ON"
                   "-DUSE_EXTERNAL_GLFW=OFF" ; glfw lib will be embedded with Raylib
                   "-DGLFW_BUILD_WAYLAND=ON" ; not working at the moment
                   (filter (lambda (flag)
                             (not (member flag '("-DBUILD_SHARED_LIBS=ON"
                                                 "-DUSE_EXTERNAL_GLFW=ON"))))
                           #$original-flags)))))
      (native-inputs
       (modify-inputs (package-native-inputs raylib)
         (append pkg-config
                 wayland
                 libxkbcommon))))))

(define ols
  (let* ((commit "16172aaca1e1fa3fea197b3f21016b332757b5ca")
         (version "dev-2026-05")
         (revision "1")
         (ols-version (git-version version revision commit)))
    (package
     (name "ols")
     (version version)
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/DanielGavin/ols")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0knsrxpb6p0pfffmm0p1jrjzjcl6h214j53l5bj68p5gmg4ibm7n"))))
     (build-system gnu-build-system)
     (arguments
      (list
       #:tests? #f
       #:phases
       #~(modify-phases %standard-phases
                        (delete 'configure)
                        (replace 'build
                                 (lambda* (#:key inputs #:allow-other-keys)
                                   (substitute* "build.sh"
                                                (("VERSION=\".*\"")
                                                 (format #f "VERSION=~s" #$ols-version)))
                                   (display "Building ols ...\n")
                                   (invoke "./build.sh")
                                   (display "Building odin formatter ...\n")
                                   (invoke "./odinfmt.sh")))
                        (replace 'install
                                 (lambda* (#:key inputs outputs #:allow-other-keys)
                                   (let ((bin (string-append #$output "/bin")))
                                     (for-each (lambda (app)
                                                 (install-file app bin))
                                               '("ols" "odinfmt"))))))))
    (native-inputs
     (list bash-minimal
           clang-toolchain
           odin))
    (home-page "https://github.com/DanielGavin/ols")
    (synopsis "Language server for the Odin programming language")
    (description
     "OLS is a language server implementation for the @code{Odin} programming
language. It provides completion, hover, references, semantic tokens,
document symbols, formatting support, and other LSP features.")
    (license license:expat))))

(define ols-nightly
  (let* ((commit "67ec8eae3cd6898a05ce1b1f6d632c2dafa197aa")
         (version "nightly")
         (revision "0")
         (ols-version (git-version version revision commit)))
  (package
    (inherit ols)
    (name "ols-nightly")
    (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/DanielGavin/ols")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1fa7xxf8lvn7ahp27yykhjx8zw11qgdd5vqsljdq9pxgiwh9nr0f"))))
     )))


;; Uncomment to install with `guix package -f odin'
;; raylib-for-odin
;; raylib-for-odin+static
;; box2d-simd+static
;; box2d-avx2+static
;; odin
;; ols
;; ols-nightly
