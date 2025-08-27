;;; engineering.scm --  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Arnaud Lechevallier <arnaud.lechevallier@free.fr>
;; Maintener: Arnaud Lechevallier <arnaud.lechevallier@free.fr>
;; Created: 2025/08/21
;; Version:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;; My best effort to release an up-to-date package definition for Radare2


(define-module (bric-a-brac packages engineering)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages digest)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages file)
  #:use-module (gnu packages engineering)
  #:use-module (gnu packages python)
  #:use-module (gnu packages elf)
  #:export (radare2-5.2)
  #:export (radare2-5.9))

(define rizin-0.8
  (package
    (inherit rizin)
    (name "rizin")
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/rizinorg/rizin/releases/download/v"
                    version "/rizin-src-v" version ".tar.xz"))
              (sha256
               (base32
                ;;"0dvybjm447c28b5516fb8cya55345j0d21w0j2gjh6cp20kcg6ns"
                "1hjf180q4ba0cs5ys7vwy5xs1k6195kransj8fn3dp6p4mjiwazg"
                ))))
    (native-inputs
     (modify-inputs (package-native-inputs rizin)
                    (append cmake pkg-config)))
    (arguments
    (substitute-keyword-arguments (package-arguments rizin)
      ((#:phases phases)
       #~(modify-phases #$phases
           (delete 'skip-integration-tests)
           (add-before 'configure 'skip-integration-tests
             (lambda _
               ;; Skip integration tests, which require prebuilt binaries at:
               ;; <https://github.com/rizinorg/rizin-testbins>.
               (substitute* "test/meson.build"
                 (("subdir\\('integration'\\)") ""))
              ;;; Skip failing tests.
               (substitute* "test/unit/meson.build"
                 (("'tokens',\n") ""))))
           ))))))


(define radare2-5.2
  (package
    (name "radare2")
    (version "5.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/radareorg/radare2")
                    (commit version)))
              (sha256
               (base32
                "0n3k190qjhdlj10fjqijx6ismz0g7fk28i83j0480cxdqgmmlbxc"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ; tests require git and network access
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'mklibdir
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref outputs "out") "/lib"))
             #t)))
       #:configure-flags
       (list "--with-openssl"
             "--with-rpath"
             "--with-syscapstone"
             "--with-sysmagic"
             "--with-syszip"
             "--with-sysxxhash")
       #:make-flags
       (list "CC=gcc")))
    ;; TODO: Add gmp and libzip and make the build system actually find them.
    (native-inputs
     (list pkg-config capstone libuv openssl libzip xxhash))
    (inputs
     (list capstone libuv zip))
    (propagated-inputs
     ;; In the Libs: section of r_hash.pc.
     (list xxhash))
    (home-page "https://radare.org/")
    (synopsis "Reverse engineering framework")
    (description
     "Radare2 is a complete framework for reverse-engineering, debugging, and
analyzing binaries.  It is composed of a set of small utilities that can be
used together or independently from the command line.

Radare2 is built around a scriptable disassembler and hexadecimal editor that
support a variety of executable formats for different processors and operating
systems, through multiple back ends for local and remote files and disk
images.

It can also compare (@dfn{diff}) binaries with graphs and extract information
like relocation symbols.  It is able to deal with malformed binaries, making
it suitable for security research and analysis.")
    (license license:lgpl3)))

(define r2-vector35-arch-arm64
  (let ((commit "55d73c6bbb94448a5c615933179e73ac618cf876")
        (revision "0"))
    (package
      (name "vector35-arch-arm64")
      (version (git-version "0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/radareorg/vector35-arch-arm64.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1dfhnsin3gg9w805d050zhsb30jah6vfqcb1j2hck0s3kjkp3755"))))
      (build-system trivial-build-system)
      (arguments
       (list #:builder
             (with-imported-modules '((guix build utils))
               #~(let ((share (string-append #$output "/arch-arm64/")))
                   (use-modules (guix build utils))
                   (mkdir-p share)
                   (copy-recursively (assoc-ref %build-inputs "source")
                                     share)))))
      (home-page "https://github.com/radareorg/vector35-arch-arm64")
      (synopsis "ARM64 architecture plugin for Binary Ninja")
      (description "This package only provides the source checkout of vector35-arch-arm64.")
      (license license:expat))))

(define r2-vector35-arch-armv7
  (let ((commit "f270a6cc99644cb8e76055b6fa632b25abd26024")
        (revision "0"))
    (package
     (name "vector35-arch-armv7")
     (version (git-version "0.0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/radareorg/vector35-arch-armv7.git")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sdj311zc2zxrjwhjp91kdyb53fhwgn98p0xirvwxjiwncky05v2"))))
     (build-system trivial-build-system)
     (arguments
      (list #:builder
            (with-imported-modules
             '((guix build utils))
             #~(let ((share (string-append #$output "/arch-armv7/")))
                 (use-modules (guix build utils))
                 (mkdir-p share)
                 (copy-recursively (assoc-ref %build-inputs "source")
                                   share)))))
     (home-page "https://github.com/radareorg/vector35-arch-arm64")
     (synopsis "ARM64 architecture plugin for Binary Ninja")
     (description "This package only provides the source checkout of vector35-arch-arm64.")
     (license license:expat))))

(define radare2-5.9
  (package
    (inherit radare2-5.2)
    (name "radare2")
    (version "5.9.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/radareorg/radare2")
                    (commit version)
                    (recursive? #t)))
              (sha256
               (base32
                "1l1vblj3n7gdw688xlibz0d7f0yhp45xbpzqa33magl44p9yyaax"))
              (file-name (git-file-name name version))))
    (arguments
     (substitute-keyword-arguments (package-arguments radare2-5.2)
       ((#:configure-flags original-flags)
        #~(cons* "--with-syscapstone"
                 "--with-syslz4"
                 "--with-syszip"
                 "--with-compiler=gcc"
                 #$original-flags))
       ((#:phases original-phases)
        #~(modify-phases #$original-phases
            (add-before 'build 'arch-import
              (lambda* (#:key inputs #:allow-other-keys)
                (let* ((path "libr/arch/p/arm/v35/")
                       (makefile (string-append path "Makefile")))
                  ;; Remove reference to v35arm repositories
                  (substitute* makefile
                    (("git clone -q https://github.com/radareorg/vector35-arch-arm64 arch-arm64") "")
                    (("cd arch-arm64 && git checkout -q radare2 > /dev/null && git reset --hard \\$\\(ARCH_ARM64_COMMIT\\)") "")
                    (("git clone -q https://github.com/radareorg/vector35-arch-armv7 arch-armv7") "")
                    (("cd arch-armv7 && git checkout -q radare2 > /dev/null && git reset --hard \\$\\(ARCH_ARMV7_COMMIT\\)") ""))
                  ;; Replace with the repositories from inputs
                  (copy-recursively #$r2-vector35-arch-arm64 path)
                  (copy-recursively #$r2-vector35-arch-armv7 path))))
            (add-after 'install 'fix-plugin-rpath
              (lambda _
                (use-modules (guix build utils))
                (for-each (lambda (file)
                            (invoke "patchelf"
                                    "--add-rpath" (string-append %output "/lib")
                                    file))
                          (find-files (string-append %output
                                                     "/lib/radare2/"
                                                     #$version) "\\.so$"))
                #t))))))
    (native-inputs
     (modify-inputs (package-native-inputs radare2-5.2)
                    (append r2-vector35-arch-armv7 r2-vector35-arch-arm64
                            patchelf)))
    (inputs
     (modify-inputs (package-inputs radare2-5.2)
                    (append zlib libzip zip lz4 file)))))

;; Uncomment to install with `guix package -f engineering.scm'
;; radare2-5.2
;; r2-vector35-arch-arm64
;; r2-vector35-arch-armv7
radare2-5.9
;; rizin-0.8
;; cutter-2.4
