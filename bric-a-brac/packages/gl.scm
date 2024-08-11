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
  #:export (glfw-new)
  )


(define-public glfw-new
  (let ((version "3.4")
	(revision "0")
        (commit "7b6aead9fb88b3623e3b3725ebb42670cbe4c579"))
    (package
     (inherit glfw)
     (name "glfw")
     (version (git-version version revision commit))
     (source (origin
	      ;; The main goal here is to allow for '--with-branch'.
	      (method git-fetch)
	      (uri (git-reference
		    (url "https://github.com/glfw/glfw.git")
		    (commit commit)))
	      (file-name (git-file-name name version))
	      (sha256
	       (base32
                "1izxbb55hzi0b6jnfi11nvfsd3l85xzvb66jsb0ipkfxs95mdiqy"))))
     (native-inputs (list doxygen-new unzip pkg-config))
     )))

;; glfw-new
