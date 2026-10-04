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
      (version "dev-2026-09")
      ;; works preferrably on a local directory, otherwise from the git repository
      (source
       (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/odin-lang/Odin.git")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0xn1711afjl3haczpqh8n0s508xgi0c2r7d3ywirsznh6slhz4a1"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (for-each delete-file
                      (find-files "vendor" "\\.(a|o|lib|dll|so(\\.[0-9]+)*)$"))))))
      (build-system gnu-build-system)
      (arguments
       (list #:tests? #f
             #:strip-binaries? #f
             #:phases
             #~(modify-phases %standard-phases
                 (delete 'configure)
                 (replace 'build
                   (lambda _
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
                       (setenv "CC" (which "clang"))
                       (setenv "AR" (which "llvm-ar"))
                       (invoke "./build_stb.sh" "unix"))
                     #t))
                 (add-after 'install 'build-cgltf-static-libraries
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                     (with-directory-excursion "./vendor/cgltf/src"
                       (setenv "CC" (which "clang"))
                       (setenv "AR" (which "llvm-ar"))
                       (invoke "./build_cgltf.sh" "unix"))
                     #t))
                 (add-after 'build-stb-static-libraries 'replace-static-libraries
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                     (let* ((target-system #$(or (%current-target-system)
                                                 (%current-system)))
                            (box2d-avx2-lib (in-vicinity #$box2d-avx2+static
                                                           "/lib/libbox2d.a"))
                            (box2d-simd-lib (in-vicinity #$box2d-simd+static
                                                           "/lib/libbox2d.a"))
                            (raylib-lib (in-vicinity #$raylib-for-odin+static
                                                       "/lib/libraylib.a"))
                            (raylib-shared-lib (in-vicinity #$raylib-for-odin "/lib"))
                            (glfw-lib (in-vicinity #$glfw+static "/lib/libglfw3.a"))
                            (stb-libs (in-vicinity (getcwd) "/vendor/stb/lib"))
                            (cgltf-lib (in-vicinity (getcwd) "/vendor/cgltf/lib/cgltf.a"))
                            (liblz4-lib (in-vicinity #$lz4:static "/lib/liblz4.a"))
                            (liblua-shared-lib (in-vicinity #$lua-5.4 "/lib/liblua.so"))
                            (liblua-lib (in-vicinity #$lua-5.4 "/lib/liblua.a")))
                       (cond
                        ((string-prefix? "x86_64-linux" target-system)
                         ;; box2d
                         (with-directory-excursion
                          (in-vicinity #$output "/vendor/box2d/lib/")
                          (copy-file box2d-avx2-lib "box2d_other_amd64_avx2.a")
                          (copy-file box2d-simd-lib "box2d_other_amd64_sse2.a"))

                         ;; box3d
                         (install-file (in-vicinity #$box3d-simd+static "/lib/libbox3d.a")
                                    (in-vicinity #$output "/vendor/box3d/lib/linux-amd64"))

                         ;; stb
                         (for-each
                          (lambda (file)
                            (install-file file
                                          (in-vicinity #$output "/vendor/stb/lib")))
                          (find-files stb-libs "\\.a$"))

                         ;; cgltf
                         (install-file cgltf-lib
                                       (in-vicinity #$output "/vendor/cgltf/lib"))

                         ;; raylib
                         (with-directory-excursion
                          (in-vicinity #$output "/vendor/raylib/linux/")
                          (install-file raylib-lib ".")
                          (copy-recursively raylib-shared-lib "."))

                         ;; lua
                         (with-directory-excursion
                          (in-vicinity #$output "/vendor/lua/5.4/linux")
                          (copy-file liblua-lib "liblua54.a")
                          (copy-file liblua-shared-lib "liblua54.so")
                          (symlink "liblua54.so" "liblua.so.5.4"))

                         ;; glfw
                         (install-file glfw-lib
                                       (in-vicinity #$output "/vendor/glfw/lib/"))

                         ;; lz4
                         (install-file liblz4-lib
                                       (in-vicinity #$output "/vendor/compress/lz4/lib")))
                        (else
                         '())))
                     #t))
                 (add-after 'install 'wrap-odin
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                     (let* ((bin (string-append #$output "/bin/odin"))
                            (raylib-lib (string-append #$output "/vendor/raylib/linux"))
                            (lua-lib (in-vicinity #$output "/vendor/lua/5.4/linux"))
                            (llvm-lib (string-append #$llvm-18 "/lib"))
                            (clang-lib (string-append #$clang-toolchain-18 "/lib")))
                       (wrap-program bin
                         `("ODIN_ROOT" = (,#$output))
                         `("LD_LIBRARY_PATH" = (,raylib-lib
                                                ,lua-lib
                                                ,llvm-lib
                                                ,clang-lib))))
                     #t)))))
      (native-inputs
       (list llvm-18
             clang-toolchain-18
             which
             box2d-avx2+static
             box2d-simd+static
             box3d-simd+static
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
      #~(cons* "-DCMAKE_POSITION_INDEPENDENT_CODE=ON"
               "-DBUILD_SHARED_LIBS=OFF"
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
        #~(cons* "-DCMAKE_POSITION_INDEPENDENT_CODE=ON"
                 "-DBUILD_SHARED_LIBS=OFF"
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

(define box3d-simd+static
  (package
    (inherit box3d)
    (name "box3d-simd+static")
    (arguments
     (substitute-keyword-arguments
         (package-arguments box3d)
       ((#:configure-flags original-flags)
        #~(cons* "-DCMAKE_POSITION_INDEPENDENT_CODE=ON"
                 "-DBUILD_SHARED_LIBS=OFF"
                 "-DBOX3D_DISABLE_SIMD=ON"
                 "-DBOX3D_UNIT_TESTS=OFF"
                 "-DBOX3D_SAMPLES=OFF"
                 (filter (lambda (flag)
                           (not (member flag '("-DBOX3D_DISABLE_SIMD=OFF"
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
  (let* ((commit "4f4377078d9e6121d26d87a3342b7d1db3c25a4b")
         (version "dev-2026-08")
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
         "0hn12yp2lsabhsp5iak67mm1fh4y6mwhvpn276921lsfjiph64vm"))))
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
     (list odin-dev-2026-07))
    (home-page "https://github.com/DanielGavin/ols")
    (synopsis "Language server for the Odin programming language")
    (description
     "OLS is a language server implementation for the @code{Odin} programming
language. It provides completion, hover, references, semantic tokens,
document symbols, formatting support, and other LSP features.")
    (license license:expat))))

(define ols-nightly
  (let* ((commit "dd0f85d31c91e9d04202cc422288d15b52206474")
         (version "nightly")
         (revision "0")
         (ols-version "nightly-2026-10-04-939400ec")) ;(git-version version revision commit)
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
    (native-inputs
     (list odin)))))


;; uncomment to install with `guix package -f odin'
;; raylib-for-odin
;; raylib-for-odin+static
;; box2d-simd+static
;; box2d-avx2+static
odin
;; ols
;; ols-nightly
;; odin-dev-2026-04
;; odin-dev-2026-05
;; odin-dev-2026-06
;; odin-dev-2026-07a
