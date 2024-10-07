;;; game-development.scm --  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Arnaud Lechevallier <arnaud.lechevallier@free.fr>
;; Maintener: Arnaud Lechevallier <arnaud.lechevallier@free.fr>
;; Created: 2024/08/10
;; Version: 0.0.4
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
;;; My attempt to create a guix definition package for `guile-raylib'.
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
  #:export (raylib-with-extras))

(define-public raylib-with-extras
  (let ((commit "282d6478baa51a509bf0a4b1d761a0bd7fd8bbf7")
        (revision "0"))
    (package
      (inherit raylib)
      (name "raylib")
      (version (git-version "5.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/raysan5/raylib/")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "03ml6vhn6lsrcl9d5xg310hkkayxdccz660qs2zwfn9dwcsw2rl8"))))
      (arguments
       (list #:tests? #f  ;no test
             #:configure-flags
             #~(list "-DBUILD_SHARED_LIBS=ON"
                     "-DUSE_EXTERNAL_GLFW=ON"
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

;; Uncommnent to install with `guix package -f raylib-with-extras'
raylib-with-extras
