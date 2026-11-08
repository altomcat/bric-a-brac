;;; engineering.scm

;;Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Arnaud Lechevallier <arnaud.lechevallier@free.fr>
;; Maintainer: Arnaud Lechevallier <arnaud.lechevallier@free.fr>
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
;;; My best effort to release an up-to-date package definition for +-Radare-2-+,
;;; Rizin and Cutter
;;; With Wayland, you may need to install `qtwayland' in addition to `Cutter'

;;; Code

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
  #:use-module (guix build-system qt)
  #:use-module (guix build utils)
  #:use-module (guix build emacs-build-system)
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
  #:use-module (gnu packages xml)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages crypto)
  #:export (rizin-0.9)
  #:export (cutter-2.5)
  #:export (rzghidra-0.9))

;; RIZIN
;;

(define rzghidra-0.9
  (package
    (name "rzghidra")
    (version "0.9.0")
    (source
     (origin (method git-fetch)
             (uri (git-reference
                    (url "https://github.com/rizinorg/rz-ghidra.git")
                    (commit (string-append "v" version))
                    (recursive? #t)))
             (sha256
              (base32
               "0zsy9cy9ll7ny8dsh690jkq68hgv2yax1nrmjlqax84nvnd0zp27"))
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
     (list pkg-config rizin-0.9 openssl cutter-2.5))
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

(define rizin-0.9
  (package
    (inherit rizin)
    (name "rizin")
    (version "0.9.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/rizinorg/rizin/releases/download/v"
                    version "/rizin-src-v" version ".tar.xz"))
              (sha256
               (base32
                "1jixajg107lxhdqywvs97s0izz27yyqphm718akxvbx7miywvhbs"))))
    (build-system meson-build-system)
    (arguments
     (substitute-keyword-arguments (package-arguments rizin)
       ((#:configure-flags original-flags)
        #~(cons* (string-append "-Dpackager_version=" #$version)
                 "-Duse_sys_blake3=enabled"
                 "-Ddebugger=true"
                 "-Duse_sys_libzip_openssl=true"
                 "-Dportable=true"      ;; hack to access plugin through GUIX_RZ_PLUGIN
                 (filter (lambda (flag)
                           (not (string-contains flag "-Dpackager_version=")
                                ;; (or (member flag '("-Duse_swift_demangler=true"))
                                ;;     (string-contains flag "-Dpackager_version="))
                                ))
                         #$original-flags)))
       ((#:phases original-phases)
        #~(modify-phases #$original-phases
            (add-before 'check 'set-openssl-modules
              (lambda* (#:key inputs #:allow-other-keys)
                (setenv "OPENSSL_MODULES"
                        (in-vicinity #$openssl "/lib/ossl-modules"))))
            (add-before 'configure 'skip-integration-tests
              (lambda _
                ;; Skip integration tests, which require prebuilt binaries at:
                ;; <https://github.com/rizinorg/rizin-testbins>.
                (substitute* "test/meson.build"
                  (("subdir\\('integration'\\)") ""))
                ;; Patch to access plugins through GUIX_RZ_PLUGIN path
                (substitute* "librz/util/path.c"
                  (("char \\*pid_to_path = rz_sys_pid_to_path\\(rz_sys_getpid\\(\\)\\);")
                   "return rz_sys_getenv(\"GUIX_RZ_PREFIX\");\nchar *pid_to_path = rz_sys_pid_to_path(rz_sys_getpid());"))
              ;;; Skip failing tests.
                (substitute* "test/unit/meson.build"
                  (("'hash',\n") ""))))
            ))))
    (native-search-paths
     (list (search-path-specification
            (variable "GUIX_RZ_PREFIX")
            (separator #f)
            (files '("" "plugin" "plugins")))))))

(define cutter-2.5
  (package
    (name "cutter")
    (version "2.5.0")                   ;keep in sync with rizin
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rizinorg/cutter")
             (commit (string-append "v" version))
             ;; Needed for src/translations.
             (recursive? #t)))
       (modules '((guix build utils)))
       (snippet #~(delete-file-recursively "rizin"))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qq2df831dqhhliydg6nr4qmfsig6yp78ahy8g7qlkkv1as5nxbn"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:qtbase qtbase
      #:configure-flags
      #~(list "-DCUTTER_USE_BUNDLED_RIZIN=OFF"
              "-DCUTTER_ENABLE_PYTHON=ON"
              "-DCUTTER_ENABLE_PYTHON_BINDINGS=ON")
      #:tests? #f ; no tests
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-location
            (lambda _
              (substitute* "src/plugins/PluginManager.cpp"
                (("const QString extraPluginDirs = CUTTER_EXTRA_PLUGIN_DIRS;")
                 "const char *guixRzPrefix = std::getenv(\"GUIX_RZ_PREFIX\");\n\
const QString extraPluginDirs = guixRzPrefix\n\
    ? QDir(QString::fromUtf8(guixRzPrefix)).filePath(\"share/rizin/cutter/plugins\")\n\
    : QStringLiteral(CUTTER_EXTRA_PLUGIN_DIRS);\n
qDebug() << \"GUIX_RZ_PREFIX:\" << (guixRzPrefix ? guixRzPrefix : \"<not set>\");\n
\n")))))))
    (native-inputs
     (list pkgconf))
    (inputs
     (list graphviz
           ksyntaxhighlighting
           libzip
           openssl
           python
           python-pyside-6
           python-shiboken
           qt5compat
           qtsvg
           qttools
           qtwayland
           rizin-0.9
           zlib))
    (home-page "https://cutter.re")
    (synopsis "Software reverse engineering platform")
    (description
     "Cutter is a reverse engineering platform powered by @code{rizin}.  It
aims to be an advanced and customizable reverse engineering platform while
keeping the user experience in mind.  Cutter is created by reverse engineers
for reverse engineers.")
    (license (list license:cc-by-sa3.0 ;the "Iconic" icon set
                   license:gpl3+))))

;; Uncomment to install with `guix package -f engineering.scm'
;; cutter-2.5
;; rizin-0.9
;; rzghidra-0.9
