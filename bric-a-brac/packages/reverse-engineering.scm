(define-module (bric-a-brac packages reverse-engineering)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix licenses)
  #:use-module (guix gexp)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages glib)
  #:export (libemu))

(define libemu
  (package
    (name "libemu")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/buffer/libemu.git")
             (commit "master")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pjmr7285hdib3zqh6nbs1lzgb08byn5ayrgd6zvfw9bwsnf48fm"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (arguments
     (list #:tests? #t
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'build 'remove-install-data
                 (lambda _
                   (substitute* "Makefile"
                     (("install-data-am: install-pkgconfigDATA") ""))))
               )))
    (home-page "https://github.com/buffer/libemu")
    (synopsis "Library for x86 shellcode emulation")
    (description
     "libemu is a library that provides x86 instruction emulation and shellcode analysis functionality.")
    (license lgpl2.1+)))
