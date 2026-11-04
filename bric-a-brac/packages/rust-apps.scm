(define-module (bric-a-brac packages rust-apps)
  #:use-module (guix import crate)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rust-crates)
  #:use-module (bric-a-brac packages rust-crates)
  #:use-module (gnu packages tls)
  #:export (rust-script)
  #:export (pure-magic-rs)
  )

(define rust-script
  (package
    (name "rust-script")
    (version "0.36.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/fornwall/rust-script")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0w52rlfwxl29855mrhf10cybrnbrj7wixll9hchb50f428in8v78"))))
    (build-system cargo-build-system)
    (arguments (list #:install-source? #f))
    (inputs (my-cargo-inputs 'rust-script))
    (home-page "https://github.com/fornwall/rust-script")
    (synopsis "Run Rust script files")
    (description
     "comment")
    (license license:asl2.0)))

(define pure-magic-rs
  (package
    (name "pure-magic-rs")
    (version "0.3.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/qjerome/magic-rs")
              (commit (string-append name "-v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vg3srwph6alyl5ngs1bwjpyygla4fnw106sfvp0l5v3b4swlwaa"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-pyo3-version
            (lambda _
              ;; after a guix audit
              (substitute* "python/Cargo.toml"
                (("pyo3 = \"0\\.27.0\"")
                 "pyo3 = \"0.29\""))))
          (replace 'install
            (lambda _
              (let ((bin (in-vicinity #$output "bin"))
                    (lib (in-vicinity #$output "lib")))
                (with-directory-excursion "target/release"
                  ;; Install binaries
                  (install-file "wiza" bin)
                  (install-file "libmagic_embed.so" lib)
                  (install-file "libpure_magic_rs.so" lib))))))))
    (native-inputs (list python-3))
    (inputs (my-cargo-inputs 'pure-magic-rs))
    (home-page "https://github.com/qjerome/magic-rs")
    (synopsis "Safe Rust reimplementation of libmagic")
    (description
     "magic-rs is a high-performance, memory-safe Rust library for detecting file types,
similar to libmagic. It supports MIME type inference and custom magic rules.")
    (license license:gpl3+)))

;; Uncomment to install with `guix package -f pure-magic-rs'
;; pure-magic-rs
