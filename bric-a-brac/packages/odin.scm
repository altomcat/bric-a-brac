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
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages python)
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
                            (liblz4-lib (string-append #$lz4:static "/lib/liblz4.a")))
                       (cond
                        ((string-prefix? "x86_64-linux" target-system)
                         (copy-file box2d-avx2-lib (string-append box2d-lib-out
                                                                  "box2d_other_amd64_avx2.a"))
                         (copy-file box2d-simd-lib (string-append box2d-lib-out
                                                                  "box2d_other_amd64_simd.a"))
                         (for-each (lambda (file)
                                     (install-file file stb-libs-out))
                                   (find-files stb-libs "\\.a$"))
                         (install-file cgltf-lib cgltf-lib-out)
                         (install-file raylib-lib raylib-lib-out)
                         (copy-recursively raylib-shared-lib raylib-lib-out)
                         (install-file glfw-lib glfw-lib-out)
                         (install-file liblz4-lib liblz4-lib-out))
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
             which
             patchelf
             box2d-avx2+static
             box2d-simd+static
             glfw+static
             raylib-for-odin
             raylib-for-odin+static
             lz4))
      (inputs
       (list clang-toolchain-18
             wayland
             libxkbcommon
             glfw-3.4
             mesa
             bash-minimal))
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

(define box2d-avx2+static
  (package
    (inherit box2d-3)
    (name "box2d-avx2+static")
    (arguments
     (substitute-keyword-arguments (package-arguments box2d)
       ((#:test-target f) "")
       ((#:configure-flags original-flags)
        `(cons* "-DBUILD_SHARED_LIBS=OFF"
                "-DBOX2D_AVX2=ON"
                "-DBOX2D_UNIT_TESTS=OFF"
                "-DBOX2D_SAMPLES=OFF"
                (filter (lambda (flag)
                          (not (member flag '("-DBOX2D_BUILD_TESTBED=OFF"
                                              "-DBOX2D_AVX2=OFF"
                                              "-DBUILD_SHARED_LIBS=ON"))))
                        ,original-flags)))))))

(define box2d-simd+static
  (package
    (inherit box2d-3)
    (name "box2d-simd+static")
    (arguments
     (substitute-keyword-arguments (package-arguments box2d)
       ((#:test-target f) "")
       ((#:configure-flags original-flags)
        `(cons* "-DBUILD_SHARED_LIBS=OFF"
                "-DBOX2D_AVX2=OFF"
                "-DBOX2D_UNIT_TESTS=OFF"
                "-DBOX2D_SAMPLES=OFF"
                (filter (lambda (flag)
                          (not (member flag '("-DBOX2D_BUILD_TESTBED=OFF"
                                              "-DBOX2D_AVX2=ON"
                                              "-DBUILD_SHARED_LIBS=ON"))))
                        ,original-flags)))))))

;; When using USE_EXTERNAL_GLFW=OFF (default Odin compilation flag) that means
;; GLFW is embedded to Raylib, X11 becomes the default backend with my wayland
;; session. It's not the case with the raylib-5.5 build with an external
;; GLFW-3.4 shared library.
(define raylib-for-odin
  (package
   (inherit raylib-5.5)
   (name "raylib-for-odin")))

;; (define raylib-for-odin
;;   (let ((inherit-from raylib-5.5))
;;     (package
;;       (inherit inherit-from-pkg)
;; (name "raylib-for-odin")
;;       (arguments
;;        (substitute-keyword-arguments (package-arguments inherit-from-pkg)
;;          ((#:configure-flags original-flags)
;;           ;; glfw library will be embedded with Raylib
;;           ;; (doesn´t work with wayland, glfw 3.4?)
;;           #~(cons* "-DUSE_EXTERNAL_GLFW=OFF"
;;                    "-DGLFW_BUILD_WAYLAND=ON"
;;                    (delete "-DUSE_EXTERNAL_GLFW=ON" #$original-flags)))))
;;       (native-inputs
;;        (modify-inputs (package-native-inputs inherit-from-pkg)
;;                       (append pkg-config
;;                               wayland
;;                               libxkbcommon))))))

;; The previous explanation is true for static build too.
;; There is no work-around at the moment because GLFW needs to be embedded.
;; The final executable is linked against libraylib.a with GLFW embedded in it.
;; Add `mesa' package to be able to use X11 backend only.
;; EDIT for wayland : add `wayland', `libxkbcommon' and `glfw@3.4'
(define raylib-for-odin+static
  (let ((inherit-from-pkg raylib-5.5))
    (package
      (inherit inherit-from-pkg)
      (name "raylib-for-odin+static")
      (arguments
       (substitute-keyword-arguments (package-arguments inherit-from-pkg)
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
       (modify-inputs (package-native-inputs inherit-from-pkg)
                      (append pkg-config
                              wayland
                              libxkbcommon))))))

;; Uncomment to install with `guix package -f odin'
;; raylib-for-odin+static
;; box2d-simd+static
;; box2d-avx2+static
;; odin
