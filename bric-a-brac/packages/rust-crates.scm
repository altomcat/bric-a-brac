
(define-module (bric-a-brac packages rust-crates)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:export (lookup-cargo-inputs
            my-cargo-inputs))

;;;
;;; This file is managed by guix import.  DO NOT add definitions manually.
;;;

(define* (crate-name->package-name name)
  (downstream-package-name "rust-" name))

(define* (crate-source name version hash #:key (patches '()) (snippet #f))
  (origin
    (method url-fetch)
    (uri (crate-uri name version))
    (file-name
     (string-append (crate-name->package-name name) "-" version ".tar.gz"))
    (sha256 (base32 hash))
    (modules '((guix build utils)))
    (patches patches)
    (snippet snippet)))

(define-syntax define-cargo-inputs
  (syntax-rules (=>)
    ((_ lookup inputs ...)
     (define lookup
       (let ((table (make-hash-table)))
         (letrec-syntax ((record
                          (syntax-rules (=>)
                            ((_) #t)
                            ((_ (name => lst) rest (... ...))
                             (begin
                               (hashq-set! table 'name (filter identity lst))
                               (record rest (... ...)))))))
           (record inputs ...)
           (lambda (name)
             "Return the inputs for NAME."
             (hashq-ref table name))))))))

(define* (my-cargo-inputs name
                          ;; NOTE: Change to your own module.
                          #:key (module '(bric-a-brac packages rust-crates)))
  "Lookup Cargo inputs for NAME defined in MODULE, return an empty list if
unavailable."
  (let ((lookup (module-ref (resolve-interface module) 'lookup-cargo-inputs)))
    (or (lookup name)
        (begin
          (warning (G_ "no Cargo inputs available for '~a'~%") name)
          '()))))

;;;
;;; Rust dependencies fetched from crates.io and non-workspace development
;;; snapshots.
;;;

(define qqqq-separator 'begin-of-crates)


(define ssss-separator 'end-of-crates)

;;;
;;; Cargo inputs.
;;;

(define-cargo-inputs lookup-cargo-inputs)
