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
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages freedesktop)
  #:export (raylib-with-extras)
  #:export (guile-raylib))


(define-public raylib-with-extras
  (package
    (inherit raylib)
    (name "raylib")
    (version "5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/raysan5/raylib/")
		    (commit "282d6478baa51a509bf0a4b1d761a0bd7fd8bbf7")))
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
		   (let ((output-directory (string-append (assoc-ref outputs "out")
							  "/parser/output"))
			 (source-directory (string-append (assoc-ref inputs "source")
							  "/parser/output")))
		     (mkdir-p output-directory)
		     (copy-recursively source-directory output-directory)
		     ))))))
    (inputs (list glfw-3.4 pulseaudio))
    (propagated-inputs (list glfw-3.4))
    ))

(define-public guile-raylib
  (package
    (name "guile-raylib")
    (version "0.0.0")
    (source (origin
	      (method git-fetch)
	      (uri (git-reference
		    (url "https://github.com/petelliott/raylib-guile.git")
                    (commit "e6b2ac8a21ac83c426ece993daa90ac04a90d0c2")))
	      (file-name (git-file-name name version))
	      (sha256
	       (base32 "114v2rcwqyczqw80hzm6ij8iqfr93x43kj8qkq8gk7w49wcq8c5c"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config glfw-3.4 wayland))
    (inputs (list guile-3.0
		  guile-lib))
    (propagated-inputs (list raylib-with-extras))
    (outputs '("out" "examples"))
    (arguments
     `( #:make-flags '("GUILE_AUTO_COMPILE=0")
       #:tests? #f
       #:modules
       ((guix build gnu-build-system)
	(guix build utils)
	(srfi srfi-1))
       #:imported-modules ((guix build guile-build-system)
			   ,@%default-gnu-imported-modules)
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
	 (delete 'strip) ;; remove useless warning on go files.
	 (delete 'install)
	 (add-before 'build 'retrieve-raylib-api
	   (lambda* (#:key inputs #:allow-other-keys)
	     (let ((raylib-version (last (string-split (assoc-ref inputs "raylib") #\-)))
		   (raylib-xml (string-append
				(assoc-ref inputs "raylib")
				"/parser/output/raylib_api.xml")))
	       (copy-file raylib-xml "raylib_api.xml")
	       #t)))

	 (add-after 'unpack 'fix-makefile
	   (lambda _
	     (use-modules (guix build guile-build-system))
	     (substitute* "Makefile"
	       (("--cflags guile-3.0") (format #f "--cflags guile-~a raylib"
					       (target-guile-effective-version))))
	     (substitute* "Makefile"
	       (("-lraylib") "`pkg-config --libs raylib`"))
	     (substitute* "Makefile"
	       (("install: all") "do-not-install:"))
	     ))

	 (add-after 'fix-makefile 'fix-guile-extensions
	   (lambda* (#:key inputs outputs #:allow-other-keys)
	     (use-modules (guix build guile-build-system))
	     (substitute* (find-files "." ".*\\.scm")
	       (("libraylib-guile") (format #f "~a/lib/guile/~a/extensions/libraylib-guile"
					    (assoc-ref outputs "out")
					    (target-guile-effective-version))))
	     #t))

	 (add-after 'build 'compile-and-install-guile-raylib
	   (lambda* (#:key inputs outputs #:allow-other-keys)
	     (use-modules (guix build guile-build-system))
	     (let* ((out (assoc-ref outputs "out"))
		    (examples (assoc-ref outputs "examples"))
		    (scm "raylib.scm")
		    (go "raylib.go")
		    (lib "libraylib-guile.so")
		    (effective-version (target-guile-effective-version))
		    (lib-out (string-append out "/lib/guile/" effective-version))
		    (share-out (string-append out "/share/guile/site/" effective-version)))
	       ;; get rid off warnings
	       (setenv "GUILE_AUTO_COMPILE" "0")
	       (invoke "guild" "compile" scm "-o" go)
	       (install-file scm (string-append share-out ))
	       (install-file go (string-append lib-out "/site-ccache"))
	       (install-file lib (string-append lib-out "/extensions"))
	       (copy-recursively "examples" examples)
	       #t)))
	 )))

    (home-page "https://github.com/petelliott/raylib-guile.git")
    (synopsis "Guile bindings for raylib library.")
    (description
     "This package provides GNU Guile bindings to the Raylib game development library.")
    (license license:zlib)))

;; Uncommnent to install with `guix package -f guile-raylib.scm'
;;raylib-with-extras
guile-raylib
