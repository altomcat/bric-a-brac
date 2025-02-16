;;; game-development.scm --  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Arnaud Lechevallier <arnaud.lechevallier@free.fr>
;; Maintener: Arnaud Lechevallier <arnaud.lechevallier@free.fr>
;; Created: 2024/08/10
;; Version: 0.0.7
;; Keywords: guile raylib

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
;;; My attempt to create a guix definition package for `guile-raylib', `box2d(v3)'.
;;;

(define-module (bric-a-brac packages game-development)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages base)
  #:export (raylib-with-extras)
  #:export (raylib-with-extras+static)
  #:export (box2d-3)
  #:export (box2d+static))

(define box2d-3
  (package
   (inherit box2d)
   (name "box2d")
   (version "3.0.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/erincatto/box2d")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0m01c23mxvg96zypqyi2fpkd1dsvgflafi3ncga6ihdvxbwaybk5"))))
   (build-system cmake-build-system)
   (arguments
    (substitute-keyword-arguments (package-arguments box2d)
                                  ((#:test-target f) "")
                                  ((#:configure-flags original-flags)
                                   `(append ,original-flags
                                            '("-DBUILD_SHARED_LIBS=ON"
                                              "-DBOX2D_UNIT_TESTS=OFF"
                                              "-DBOX2D_SAMPLES=OFF")))))))

(define box2d+static
  (package
   (inherit box2d-3)
   (name "box2d+static")
   (arguments
    (substitute-keyword-arguments (package-arguments box2d)
                                  ((#:test-target f) "")
                                  ((#:configure-flags original-flags)
                                   `(append ,original-flags
                                            '("-DBUILD_SHARED_LIBS=OFF"
                                              "-DBOX2D_UNIT_TESTS=OFF"
                                              "-DBOX2D_SAMPLES=OFF")))))))

(define-public raylib-with-extras
  (let ((commit "4f091f44a8d91d51019aa65c12da570435de450b")
        (revision "0"))
    (package
      (inherit raylib)
      (name "raylib")
      (version (git-version "5.5" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/raysan5/raylib/")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "08ywy0lrcmpjyahkap5i10wcx49cc145rhjb9n0yap0xqh3vizfg"))))
      (arguments
       (list #:tests? #f  ;no test
             #:configure-flags
             #~(list "-DBUILD_SHARED_LIBS=ON"
                     "-DUSE_EXTERNAL_GLFW=ON"
                     "-DWITH-PIC=ON"
                     "-DCMAKE_C_FLAGS=-lpulse")
             #:phases
             #~(modify-phases %standard-phases
                 (add-before 'configure 'configure-miniaudio
                   ;; Use PulseAudio as raudio backend.
                   (lambda _
                     (substitute* "src/raudio.c"
                       (("^#include \"external/miniaudio\\.h\"") "
   #define MA_NO_RUNTIME_LINKING
   #define MA_ENABLE_ONLY_SPECIFIC_BACKENDS
   #define MA_ENABLE_PULSEAUDIO
   #include \"external/miniaudio.h\"
   "))))
                 (add-before 'install 'install-parser
                   (lambda* (#:key inputs outputs #:allow-other-keys)
                     (let ((out (string-append (assoc-ref outputs "out")
                                               "/parser/output"))
                           (src (string-append (assoc-ref inputs "source")
                                               "/parser/output")))
                       (mkdir-p out)
                       (copy-recursively src out)))))))
      (inputs (list glfw-3.4 pulseaudio))
      (propagated-inputs (list glfw-3.4))
      (description
       "raylib is a high-level library for video game programming.  It aims to
  abstract away platform and graphics details, allowing you to focus on
  writing your game.  This package also provides the API in a variety of file
formats to create bindings for many programming languages."))))

(define raylib-with-extras+static
  (package
   (inherit raylib-with-extras)
   (name "raylib+static")
   (arguments
    (substitute-keyword-arguments (package-arguments raylib-with-extras)
                                  ((#:configure-flags original-flags)
                                   #~(list"-DBUILD_SHARED_LIBS=OFF"
                                          "-DWITH-PIC=ON"
                                          "-DUSE_EXTERNAL_GLFW=ON"
                                          "-DCMAKE_C_FLAGS=-lpulse"))))))

;; Uncommnent to install with `guix package -f raylib-with-extras'
;; raylib-with-extras
;; raylib-with-extras+static
;; box2d-3
;; box2d+static
