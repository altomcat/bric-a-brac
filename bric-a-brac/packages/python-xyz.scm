;;; python-xyz.scm

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Arnaud Lechevallier <arnaud.lechevallier@free.fr>
;; Maintainer: Arnaud Lechevallier <arnaud.lechevallier@free.fr>
;; Created: 2025/08/21
;; Version:

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
;;; Added python modules to be used for my convenience.

;;; Code

(define-module (bric-a-brac packages python-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages check)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages python-web)
  #:use-module (bric-a-brac packages antivirus)
  #:export (python-obsws-python)
  #:export (python-yara)
  )

(define python-pefile-2024.8.26
  (package
    (inherit python-pefile)
    (name "python-pefile")
    (version "2024.8.26")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/erocarrera/pefile")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "14bh23b7jipf2pxjivyn2khifal6fmk0pjsddrd3h1gd9n0wl9zd"))))))

(define python-yara
  (package
    (name "python-yara")
    (version "4.5.4")
    (source
     (origin (method git-fetch)
             (uri (git-reference
                    (url "https://github.com/VirusTotal/yara-python.git")
                    (commit (string-append "v" version))))
             (sha256
              (base32 "0w65c8ha05s9fzibbzijl90wgdwylg0vr9jw8d9s7s7q8jk0p76r"))))
    (build-system python-build-system)
    (arguments
     (list #:tests? #f
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'git-clone-yara
                 (lambda* _
                   (copy-recursively #$(origin
                                         (method git-fetch)
                                         (uri (git-reference
                                                (url "https://github.com/VirusTotal/yara")
                                                (commit (string-append "v" version))))
                                         (sha256
                                          (base32 "01sgnh6m6bprmakagxqr2w960p81qwxgfn7c21isw2qv9hzj6b5x")))
                                     "./yara")))
               (replace 'build
                 (lambda* (#:key python #:allow-other-keys)
                   (invoke (string-append #$python "/bin/python3")
                           "setup.py" "build" "--dynamic-link")))
               )))
    (inputs (list yara-4.5))
    (home-page "https://github.com/VirusTotal/yara-python")
    (synopsis "The Python interface for YARA")
    (description
     "This package provides a Python interface for YARA.")
    (license license:asl2.0)))

;; This definition package has been created with guix import
(define python-obsws-python
  (package
    (name "python-obsws-python")
    (version "1.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "obsws_python" version))
       (sha256
        (base32 "08gabra8fb6nda642si3jlqhizdsr2ylyazai2p50hn5hc1d15r2"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:tests? #f))
    (propagated-inputs (list python-tomli python-websocket-client))
    (native-inputs (list python-hatchling))
    (home-page "https://github.com/aatikturk/obsws-python")
    (synopsis "A Python SDK for OBS Studio WebSocket v5.0")
    (description
     "This package provides a Python SDK for OBS Studio @code{WebSocket} v5.0.")
    (license license:expat)))

(define-public python-ukkonen
  (package
    (name "python-ukkonen")
    (version "1.0.1")
    (source
     (origin
       ;; There are no tests in the PyPI tarball.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/asottile/ukkonen")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17gspl2dsykg000275svvyam4k7wz9ypi9xrfrmsgcgryczravlc"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (propagated-inputs (list python-cffi))
    (home-page "https://github.com/asottile/ukkonen")
    (synopsis "Implementation of bounded Levenshtein distance (Ukkonen)")
    (description "This package is an implementation of of bounded Levenshtein
distance (Ukkonen).")
    (license license:expat)))

(define-public python-libvirt
  (package
   (name "python-libvirt")
   (version "10.6.0")
   (source
    (origin
     (method url-fetch)
     (uri (string-append "https://libvirt.org/sources/python/libvirt-python-"
                         version ".tar.gz"))
     (sha256
      (base32 "1r3rvkgnc6j813mcdr7fdfnxx58imzl16azjkg54yy2gfayrq9g4"))))
   (build-system pyproject-build-system)
   (inputs
    (list libvirt))
   (propagated-inputs
    (list python-lxml))
   (native-inputs
    (list pkg-config python-pytest python-setuptools python-wheel))
   (home-page "https://libvirt.org")
   (synopsis "Python bindings to libvirt")
   (description "This package provides Python bindings to the libvirt
virtualization library.")
   (properties
    '((upstream-name . "libvirt-python")))
   (license license:lgpl2.1+)))

;; Uncomment to install with `guix package -f python-obsws-python'
;; python-libvirt
;; python-obsws-python
;; python-yara
;; python-pefile-2024.8.26
