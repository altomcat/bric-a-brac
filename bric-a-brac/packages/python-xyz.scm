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
  #:use-module (gnu packages perl)
  #:use-module (bric-a-brac packages antivirus)
  #:use-module (bric-a-brac packages reverse-engineering)
  #:export (python-obsws-python)
  #:export (python-yara)
  #:export (python-ukkonen)
  #:export (python-aespython)
  #:export (python-libemu)
  )

(define python-libemu
  (let ((commit "3e3f2022b38840d71061f194e5917e1ba831241e"))
    (package
      (name "python-libemu")
      (version "1.0")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/buffer/pylibemu.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1cqbgr7zn5xhrrq2plvn5xslxx86434fg5fj9b348nim1w4c3m8x"))))
      (build-system python-build-system)
      (arguments
       (list #:tests? #f))
      (native-inputs (list libemu))
      (home-page "https://github.com/buffer/pylibemu")
      (synopsis "A Libemu Cython wrapper")
      (description
       "pylibemu provides a Python-friendly interface to the libemu library via Cython bindings.
It enables the emulation of x86 shellcode under Python by leveraging libemu's engine, allowing analysis of shellcode, obtaining execution profiles, detecting dynamic behaviour (such as API calls e.g. LoadLibraryA, WSASocket), and capturing an emulation trace from within a Python environment.")
      (license license:gpl2))))

(define python-aespython
  (let ((commit "199906935b8336902b1f552d5570363dfa7ff110"))
    (package
      (name "python-aespython")
      (version "0")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/serprex/aespython")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "02vf2l297g5pa5lqzj9f6svnrhdlg1sd1s9q1rmgh7lr1ad7mw1w"))))
      (build-system python-build-system)
      (arguments
       (list #:tests? #f))
      (home-page "https://github.com/serprex/aespython")
      (synopsis "aespython is pure AES library for python3")
      (description
       "This package is a dependy of peepdf.")
      (license license:gpl3+))))

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
    (version "4.5.5")
    (source
     (origin (method git-fetch)
             (uri (git-reference
                    (url "https://github.com/VirusTotal/yara-python.git")
                    (commit (string-append "v" version))))
             (sha256
              (base32 "0mvx2k743vsb9vc9clbf2a2jcvwqg9m4vw8fa2r0jxqbj2ljbhfw"))))
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
                                          (base32 "16f8pkadyi6m8vp63hfs5634b6gix3ry9g5n7bkklx30ab2hpskb")))
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

(define python-ukkonen
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

(define python-libvirt
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
;; python-aespython
;; python-libemu
