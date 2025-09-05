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
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xdisorg)
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
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages elf)
  #:export (radare2-5.2)
  #:export (radare2-5.9)
  #:export (rizin-0.8)
  #:export (cutter-2.4))
;; RIZIN

(define rzghidra
  (package
    (name "rzghidra")
    (version "0.8.0")
    (source
     (origin (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/rizinorg/rz-ghidra.git")
                   (commit (string-append "v" version))
                   (recursive? #t)))
             (sha256
              (base32
               "184yf5v30k6yvpb9kjbjzr1rrxc3acg8xqm0c3bjmfy0w6g093dq"))
             (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:parallel-build? #f  ; parallel building failed on my WSL2 system
           #:configure-flags
           #~(list "-DBUILD_CUTTER_PLUGIN=ON"
                   (string-append "-DCMAKE_INSTALL_PREFIX=" #$output)
                   (string-append "-DRIZIN_INSTALL_PLUGDIR=" (string-append #$output "/lib/rizin/plugins"))
                   (string-append "-DCUTTER_INSTALL_PLUGDIR=" (string-append #$output "/share/rizin/cutter/plugins/native")))))
    (native-inputs
     (list pkg-config rizin-0.8 openssl cutter-2.4))
    (inputs (list
             qtsvg
             qttools
             python
             qtbase
             qt5compat
             libxkbcommon))
    (home-page "https://radare.org/")
    (synopsis "Reverse engineering decompiler")
    (description
     "Ghidra decompiler for Rizin.")
    (license license:lgpl3)))

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
                "1hjf180q4ba0cs5ys7vwy5xs1k6195kransj8fn3dp6p4mjiwazg"))))
    (arguments
     (substitute-keyword-arguments (package-arguments rizin)
       ((#:configure-flags original-flags)
        #~(cons* "-Dportable=true" #$original-flags))
       ((#:phases phases)
        #~(modify-phases #$phases
            (delete 'skip-integration-tests)
            (add-before 'configure 'skip-integration-tests
              (lambda _
                ;; Skip integration tests, which require prebuilt binaries at:
                ;; <https://github.com/rizinorg/rizin-testbins>.
                (substitute* "test/meson.build"
                  (("subdir\\('integration'\\)") ""))
                ;; Skip failing tests.
                (substitute* "test/unit/meson.build"
                  (("'tokens',\n") ""))
                ;; Apply a fix similar to Nix's patch for managing plugins
                (substitute* "librz/util/path.c"
                  (("char \\*pid_to_path = rz_sys_pid_to_path\\(rz_sys_getpid\\(\\)\\);")
                   "return rz_sys_getenv(\"GUIX_RZ_PREFIX\");\nchar *pid_to_path = rz_sys_pid_to_path(rz_sys_getpid());"))))))))
    (native-search-paths
     (list (search-path-specification
            (variable "GUIX_RZ_PREFIX")
            (separator #f)
            (files '("")))))))

(define cutter-2.4
  (package
    (inherit cutter)
    (name "cutter")
    (version "2.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rizinorg/cutter")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "090gfg90k0fn3jiyssdigjgb7xn473hxfm7gpl1rwn3kl6fv7lvw"))))
    (arguments
     (list #:tests? #f
           #:configure-flags
           #~(list "-DCUTTER_USE_BUNDLED_RIZIN=OFF"
                   "-DCUTTER_ENABLE_PYTHON=ON"
                   "-DCUTTER_ENABLE_PYTHON_BINDINGS=ON")
           #:phases
           #~(modify-phases %standard-phases
             (add-after 'unpack 'patch-location
               (lambda _
                   (substitute*
                       "src/plugins/PluginManager.cpp"
                     (("QString location = QStandardPaths::writableLocation\\(QStandardPaths::AppDataLocation\\);")
                      (string-append "return QString::fromUtf8(std::getenv(\"GUIX_RZ_PREFIX\"))+\"/share/rizin/cutter/plugins/\";\n"
                                     "QString location = QStandardPaths::writableLocation(QStandardPaths::AppDataLocation);"))))))))
    (inputs
     (modify-inputs (package-inputs cutter)
       (replace "rizin" rizin-0.8) ;; use my definition package temporary
       (replace "qtsvg" qtsvg)
       (replace "qttools" qttools)
       (append python qtbase qt5compat libxkbcommon python-pyside-6 ksyntaxhighlighting python-pyside-6 graphviz)))))

;; RADARE2

(define r2ghidra
  (package
   (name "r2ghidra")
   (version "6.0.0")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/radareorg/r2ghidra.git")
                  (commit version)
                  (recursive? #t)))
            (sha256
             (base32
              "0kbzyd5jmxbcwv3zqzwakc46l17bpzlx2syh15hfi9vyncci9sms"))
            (file-name (git-file-name name version))))
   (build-system meson-build-system)
   (arguments
    (list #:phases
          #~(modify-phases %standard-phases
                           (add-before 'configure 'patch-meson-build
                                       (lambda* (#:key inputs #:allow-other-keys)
                                         (let* ((path "./subprojects")
                                                (meson-build-file (string-append path "/packagefiles/ghidra-native/meson.build"))
                                                (patches "./subprojects/ghidra-native"))
                                           ;; (substitute* "configure.acr"
                                           ;;   (("CHKPRG! GIT git") ""))
                                           ;; (substitute* "meson.build"
                                           ;;   (("pugixml_sources = \\[\n") "")
                                           ;;   (("'third-party/pugixml/src/pugixml\\.cpp'\n") "#")
                                           ;;   (("'third-party/pugixml/src/',\n") "")
                                           ;;   (("pugixml_sources,\n") "")
                                           ;;   (("ghidra = subproject\\('ghidra-native', default_options: \\['default_library=static', 'werror=false'\\]\\)") "ghidra = dependency('ghidra-native')"))
                                           (substitute* "subprojects/Makefile"
                                                        (("WRAP_wrap_git_url:=https://github.com/radareorg/ghidra-native\n") "")
                                                        (("WRAP_wrap_git_revision:=d2242691bf45a12e3ea483a4789f825af4651506\n") ""))
                                           (substitute* "subprojects/ghidra-native.mk"
                                                        (("git clone --no-checkout --depth=1 https://github.com/radareorg/ghidra-native ghidra-native") "")
                                                        (("cd ghidra-native && git fetch --depth=1 origin d2242691bf45a12e3ea483a4789f825af4651506") ""))
                                           (substitute* "subprojects/ghidra-native.wrap"
                                                        (("wrap-git") "wrap-file"))
                                           ;; (delete-file "subprojects/ghidra-native.wrap")
                                           ;; replace with the repository from inputs
                                           (copy-recursively #$ghidra-native path)
                                           (copy-file meson-build-file (string-append patches "/meson.build"))

                                           (display "========8<======== PATCH ==============\n")
                                           ;;(system* #$(file-append coreutils "/bin/ls") path)
                                           (with-directory-excursion patches
                                                                     (let ((patches
                                                                            (list "patches/0001-space-after-comma.patch"
                                                                                  "patches/0002-make-sleigharch-public.patch"
                                                                                  "patches/0004-public-fields.patch"
                                                                                  "patches/0006-readonly-warning.patch"
                                                                                  "patches/0010-null-subflow.patch"
                                                                                  "patches/0020-Fix-double-free-crash-when-deinitializing-multiple-X.patch"
                                                                                  "patches/0023-Undef-LoadImage-for-windows.patch"
                                                                                  "patches/0024-ignore-symbol-beyond-space.patch"
                                                                                  "patches/0044-bad-unicode-codepoint.patch"
                                                                                  "patches/0055-datatype-clone.patch"
                                                                                  "patches/0056-nullderef-workaround.patch"
                                                                                  "patches/0080-getparent-flow.patch"
                                                                                  "patches/0090-nocasts-warnings.patch"
                                                                                  "patches/0091-decompiler-xml-packer.patch"
                                                                                  "patches/0092-badvar-segfault.patch"
                                                                                  "patches/0093-no-virtual-destructor.patch")))
                                                                       (map (lambda (p)
                                                                              (invoke #$(file-append patch "/bin/patch") "-p1" "-i" p))
                                                                            patches)))
                                           (display "========8<======== DONE ==============\n")
                                           ))))))
   (native-inputs
    (list pkg-config cmake zlib ghidra-native radare2 pugixml openssl))
   (inputs
    (list pugixml openssl radare2))
   (home-page "https://radare.org/")
   (synopsis "Reverse engineering decompiler")
   (description
    "Ghidra decompiler for Radare2.")
   (license license:lgpl3)))

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
;; radare2-5.9
;; rizin-0.8
;; cutter-2.4
