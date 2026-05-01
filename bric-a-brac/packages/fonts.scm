(define-module (bric-a-brac packages fonts)
  #:use-module (guix packages)
  #:use-module (gnu packages fonts)
  #:use-module (guix git-download)
  #:use-module (guix build-system font)
  #:use-module ((guix licenses) #:prefix license:))

;; Replaced by font-aporetic
(define font-iosevka-comfy
  (package
   (name "font-iosevka-comfy")
   (version "2.1.0")
   (source (origin
            (method git-fetch)
            (uri (git-reference
		  (url "https://github.com/protesilaos/iosevka-comfy.git")
		  (commit (string-append "" version))))
	    (file-name (git-file-name name version))
	    (sha256
	     (base32 "1fh3ij3h1x6gwnhml5s9b947bxyy6b00lg2z4j7ppw7ww32sblyg"))))
   (build-system font-build-system)
   (home-page "https://github.com/protesilaos/iosevka-comfy")
   (synopsis "A TTF font derived from Iosevka")
   (description "Iosevka Comfy is a TTF font derived from Iosevka. It is a slightly tweaked version of the original, designed for enhanced readability.")
   (license license:silofl1.1)))
