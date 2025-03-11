;;; game-development.scm --  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Arnaud Lechevallier <arnaud.lechevallier@free.fr>
;; Maintener: Arnaud Lechevallier <arnaud.lechevallier@free.fr>
;; Created: 2024/08/10
;; Version: 0.0.9
;; Keywords: raylib box2d

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
  #:export (box2d-3))

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
    (substitute-keyword-arguments
        (package-arguments box2d)
      ((#:test-target _) "")
      ((#:configure-flags original-flags)
       `(cons* "-DBOX2D_UNIT_TESTS=OFF"
               "-DBOX2D_SAMPLES=OFF"
               (delete "-DBOX2D_BUILD_TESTBED=OFF" ,original-flags)))))))

(define raylib-5.5
  (let ((commit "4f091f44a8d91d51019aa65c12da570435de450b")
        (tag "5.5")
        (revision "0"))
    (package
      (inherit raylib)
      (name "raylib")
      (version (git-version tag revision commit))
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
       (substitute-keyword-arguments (package-arguments raylib)
         ((#:phases current-phases)
          #~(modify-phases #$current-phases
              (add-after 'install 'install-parser
                (lambda _
                  (copy-recursively (string-append #$source "/parser/output")
                                    (string-append #$output "/parser/output"))))))))
      (inputs
       (modify-inputs (package-inputs raylib)
                      (replace "glfw" glfw-3.4))))))

;; Uncommnent to install with `guix package -f raylib-5.5'
;; raylib-5.5
;; box2d-3
