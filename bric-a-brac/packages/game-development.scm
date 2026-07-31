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
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages video)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ncurses)
  #:export (box2d-3.1)
  #:export (raylib-6)
  #:export (glslviewer))


(define glslviewer
  (package
    (name "glslviewer")
    (version "3.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/patriciogonzalezvivo/glslViewer.git")
             (commit version)
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1353cbl6g7lwvn68wwkksijahsg66yrjcfrmcmzawfcw425r7y5d"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f))
    (native-inputs
     (list pkg-config ncurses python coreutils))
    (inputs
     (list glfw mesa ffmpeg))
    (synopsis "Interactive GLSL sandbox and shader viewer")
    (description
     "glslViewer is an interactive sandbox for rendering GLSL shaders.
It provides a lightweight windowed environment for experimenting
with fragment shaders, similar to Shadertoy-style workflows but
running locally. The program supports real-time editing, automatic
reloading of shader files, and a minimal runtime for testing visual
effects without requiring a full graphics engine or application
framework.")
    (home-page "https://github.com/patriciogonzalezvivo/")
    (license (list license:expat))))

(define box2d-3.1
  (package
   (inherit box2d-3)
   (name "box2d")
   (version "3.1.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/erincatto/box2d")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0j4vf19idnimpf8niqiw9dmdm40mvvjrhky63yyv2n0z1zs35912"))))))

(define raylib-6
  (let ((commit "dbc56a87da87d973a9c5baa4e7438a9d20121d28")
        (tag "6.0")
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
                  "1scwmldxk5bvr4k99vi5cy4kjm77x5603pc9qxi2mv8w6c6qrvpk")))))))

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
                  "0m01c23mxvg96zypqyi2fpkd1dsvgflafi3ncga6ihdvxbwaybk5"))))
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
;; box2d-3.1
;; live-glsl
;; glslviewer
