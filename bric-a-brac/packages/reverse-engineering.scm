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
  (let ((commit "bf443cb08546db92f572b45738a5bb20259c7cad"))
    (package
      (name "libemu")
      (version "1.0.5")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/buffer/libemu.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1hn0yjsc66wwdmbzg9yi4pf1zxycywmc3gmpwz2qjycla7hzb8sn"))))
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
      (license lgpl2.1+))))
