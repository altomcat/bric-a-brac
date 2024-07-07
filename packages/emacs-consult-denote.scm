(define-module (packages emacs-consult-denote)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  )

(define-public emacs-consult-denote
  (package
   (name "emacs-consult-denote")
   (version "0.1.1")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/protesilaos/consult-denote")
		  (commit "decdaa3935aa79b23f8ceab5768b248ee15e65fd")))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "1wggmam8fj9nxg8mkm28k5nch8h2hwaiqnld2wwpd1zlldrvr2f9"))))
   (build-system emacs-build-system)
   (inputs
    (list emacs-denote
	  emacs-consult))
   (home-page "https://github.com/protesilaos/consult-denote")
   (synopsis "`consult-denote' enhance the minibuffer interaction by previewing your notes in a `consult' way.")
   (description
    "`consult-denote' integrates the `denote' package with Daniel Mendler's `consult'.
     The idea is to enhance minibuffer interactions, such as by providing a
     preview of the file-to-linked/opened and by adding more sources to the
     `consult-buffer' command.")

   (license license:gpl3)))

;; Uncommnent to install with `guix package -f emacs-consult-denote'
;;emacs-consult-denote
