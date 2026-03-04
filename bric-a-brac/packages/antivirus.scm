;;; antivirus.scm

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Arnaud Lechevallier <arnaud.lechevallier@free.fr>
;; Maintainer: Arnaud Lechevallier <arnaud.lechevallier@free.fr>
;; Created: 2025-08-27

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

;;; Commentary: Added stuff related to YARA

;;; Code:

(define-module (bric-a-brac packages antivirus)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix build utils)
  #:use-module (guix build-system copy)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (gnu packages antivirus)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (bric-a-brac packages engineering)
  #:use-module (bric-a-brac packages python-xyz)
  #:export (yara-4.5)
  #:export (cutter-hyara))

(define cutter-hyara
  (package
    (name "cutter-hyara")
    (version "2.3-patch")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/hyuunnn/Hyara.git")
              (commit version)))
       (snippet
        '(begin
           (use-modules (guix build utils))
           (substitute* "Hyara_Cutter.py"
             (("import PySide2\\.QtWidgets as QtWidgets")
              "from PySide6.QtWidgets import QWidget\n")
             (("QtWidgets\\.QWidget")
              "QWidget")
             (("self\\.startAddrAction = QtWidgets\\.QAction\\(\"Hyara - Select Start Address\"\\)")
              "")
             (("self\\.endAddrAction = QtWidgets\\.QAction\\(\"Hyara - Select End Address\"\\)")
              "")
             (("menu\\.addAction\\(self\\.startAddrAction\\)")
              "self.startAddrAction = menu.addAction(\"Hyara - Select Start Address\")")
             (("menu\\.addAction\\(self\\.endAddrAction\\)")
              "self.endAddrAction = menu.addAction(\"Hyara - Select End Address\")"))
           (substitute* "hyara_lib/integration/cutter_api.py"
             (("return cutter.cmdj\\(\"itj\"\\)")
              "return cutter.cmdj(\"iTj\")"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12kcxiqmxl3dmbklzjwbiq6b84fvv21yjwc72saqx3j7h7ki568p"))))
       (build-system copy-build-system)
    (arguments
     (list #:install-plan
           #~'(("__init__.py" "share/rizin/cutter/plugins/python/hyara/")
               ("Hyara_Cutter.py" "share/rizin/cutter/plugins/python/hyara/")
               ("hyara_lib" "share/rizin/cutter/plugins/python/hyara/"))))
    (propagated-inputs
      (list python python-yara python-pillow python-pyside-6 python-pefile))
     (search-paths
      (list (search-path-specification
              (variable "GUIX_RZ_PREFIX")
              (files '("share")))))
    (home-page "https://github.com/hyuunnn/Hyara")
    (synopsis "YARA rule authoring and testing plugin for disassemblers")
    (description "Hyara is a plugin that helps create and test @code{yara} rules within reverse-engineering tools such as IDA Pro, Binary Ninja, Cutter, and Ghidra. It lets users generate rules from code or data, validate them on the fly, and highlight matches directly in the disassembler view.This release is oriented for use with the @code{cutter} reverse-engineering framework.")
    (license license:expat)))

(define yara-4.5
  (package
    (inherit yara)
    (name "yara")
    (version "4.5.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/VirusTotal/yara")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16f8pkadyi6m8vp63hfs5634b6gix3ry9g5n7bkklx30ab2hpskb"))))))

;; Uncomment to install with `guix package -f antivirus.scm'
;; yara-4.5
;; cutter-hyara
