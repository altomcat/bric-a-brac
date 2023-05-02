(define-module (my-packages font-iosevka-comfy)
  #:use-module (guix packages)
  #:use-module (gnu packages fonts)
  #:use-module (guix git-download)
  #:use-module (guix build-system font)
  #:use-module ((guix licenses) #:prefix license:))

(define-public font-iosevka-comfy
  (package
   (name "font-iosevka-comfy")
   (version "1.2.0")
   (source (origin
            (method git-fetch)
            (uri (git-reference
		  (url "https://github.com/protesilaos/iosevka-comfy.git")
		  (commit (string-append "" version))))
	    (file-name (git-file-name name version))
	    (sha256
	     (base32 "0qzqz0b4k0bm93l6k8bfbqmy04cfy8vbbv4xwf9iyrb305siwjjy"))))
   (build-system font-build-system)
   (home-page "https://github.com/protesilaos/iosevka-comfy")
   (synopsis "A TTF font derived from Iosevka")
   (description "Iosevka Comfy is a TTF font derived from Iosevka. It is a slightly tweaked version of the original, designed for enhanced readability.")
   (license license:silofl1.1)))
