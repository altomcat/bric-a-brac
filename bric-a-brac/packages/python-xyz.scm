(define-module (bric-a-brac packages python-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system python)
  #:use-module (guix packages)
  ;;#:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages check)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages virtualization)
  )

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

python-libvirt
