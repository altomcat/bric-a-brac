(define-module (bric-a-brac packages doxygen-1.9.8)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages documentation)
  )

(define-public doxygen-1.9.8
  (let ((version "1.9.8")
	(release "0"))
    (package
     (inherit doxygen)
     (version version)
     (home-page "https://www.doxygen.nl")
     (source (origin
	      (method url-fetch)
	      (uri (list (string-append home-page "files/doxygen-"
					version ".src.tar.gz")
			 (string-append "mirror://sourceforge/doxygen/rel-"
					version "/doxygen-" version
					".src.tar.gz")))
	      (sha256
	       (base32
		"0qjgw7bnx668hpi4r8m366vsq118s9365zf8z4x5yjrqx0ld5qq5")))))))

;; Uncommnent to install with `guix package -f doxygen-1.9.8'
;;doxygen-1.9.8
