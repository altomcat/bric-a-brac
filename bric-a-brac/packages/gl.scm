;;; gl.scm --  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Arnaud Lechevallier <arnaud.lechevallier@free.fr>
;; Maintener: Arnaud Lechevallier <arnaud.lechevallier@free.fr>
;; Created: 2024/08/10
;; Version: 0.0.1

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

;;; Commentary: Guix definition package for `glfw@3.4'

(define-module (bric-a-brac packages gl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages compression)
  #:use-module (bric-a-brac packages documentation)
  #:export (glfw+static)
  #:export (glbinding)
  )

(define glfw+static
  (package
    (inherit glfw-3.4)
    (name "glfw+static")
    (arguments
     (substitute-keyword-arguments (package-arguments glfw-3.4)
       ((#:out-of-source? _ #f) #f)
       ((#:configure-flags original-flags)
        #~(cons* "-DBUILD_SHARED_LIBS=OFF"
                 (delete "-DBUILD_SHARED_LIBS=ON"
                         #$original-flags)))))))

(define glbinding
  (package
    (name "glbinding")
    (version "3.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cginternals/glbinding.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0iskvrdhmvh1522v6444z6jdcc14gpwzdz4gmmygv5yz4nr4xfx0"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f))                ;no test
    (home-page "https://github.com/cginternals/glbinding")
    (synopsis "Modern C++11 OpenGL binding")
    (description
     "glbinding is a modern C++11 binding for the OpenGL API. It provides
 type-safe function wrappers, strongly typed enumerations, and automatic
 extension handling generated from the official OpenGL specification.
 It is suitable for graphics engines and other OpenGL-based applications.")
    (license license:expat)))



;; glfw+static
glbinding
