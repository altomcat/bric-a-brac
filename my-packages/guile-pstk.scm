(define-module (my-packages guile-pstk)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system guile)
  #:use-module (gnu packages)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages tcl)
  )

(define-public guile-pstk
  (package
   (name "guile-pstk")
   (version "0.0.0")
   (source (origin
	    (method git-fetch)
	    (uri (git-reference
		  (url "https://github.com/KikyTokamuro/guile-pstk")
		  ;;(url "https://codeberg.org/kakafarm/guile-pstk.git")
		  (commit "70396a6c49be6829b8fb6c3815bcb06dd17b2874")))
	    (file-name (git-file-name name version))
	    (sha256
	     (base32 "0az4dxfw33q01p6rrvs7l847m7qf1626299mk4gfavz37km5l99h"))))
   (build-system guile-build-system)
   ;; (arguments '())
   (inputs
    (list guile-3.0
	  tk
	  tcl))
   (home-page "https://github.com/KikyTokamuro/guile-pstk")
   ;;(home-page "https://codeberg.org/kakafarm/guile-pstk")
   (synopsis "Guile implementation of PS/Tk, a portable Scheme interface to Tk GUI toolkit.")
   (description
    "PS/TK version (http://mirror.informatimago.com/scheme/www.t3x.org/pstk/index.html) fixed to work fine on modern GNU Guile")
   (license license:bsd-3)))

;; Uncommnent to install with `guix package -f guile-pstk.scm'
;;guile-pstk
