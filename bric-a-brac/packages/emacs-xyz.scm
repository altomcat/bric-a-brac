(define-module (bric-a-brac packages emacs-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:export (emacs-substitute)
  #:export (emacs-org-appear-0.3.1)
  )

(define-public emacs-substitute
  (package
    (name "emacs-substitute")
    (version "0.1.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.sr.ht/~protesilaos/substitute")
                    (commit "b81bb7789847f3d1645a60422fa080c48b93dd47")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0s1lgpwmdjz17rd4r6qp41agmpqwa9976v1pb55p648zjh0p2vlz"))))
    (build-system emacs-build-system)
    (home-page "https://git.sr.ht/~protesilaos/substitute")
    (synopsis "Efficiently replace targets in the buffer or context")
    (description
     "Substitute is a set of commands that perform text replacement (i) throughout
      the buffer, (ii) limited to the current definition (per narrow-to-defun),
      (iii) from point to the end of the buffer, and (iv) from point to the beginning
       of the buffer.

     These substitutions are meant to be as quick as possible and, as such, differ
      from the standard query-replace (which I still use).  The provided commands
       prompt for substitute text and perform the substitution outright.")

    (license license:gpl1+)))

emacs-substitute

(define-public emacs-org-appear-0.3.1
  (package
    (inherit emacs-org-appear)
    (name "emacs-org-appear")
    (version "0.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/awth13/org-appear")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hfhnzhmbxrw4kz977s48x4nbq86vda5dvj00s2ima2i22b8l2z4"))))
    ))

emacs-org-appear-0.3.1
