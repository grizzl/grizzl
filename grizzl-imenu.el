;;; grizzl-imenu.el --- Fuzzy search for imenu.
;;
;;
;; Copyright (C) 2014
;;
;; Author: boyw165
;; Version: 0.0.1
;; Compatibility: GNU Emacs 24.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Usage:
;; ------
;; M-x grizzl-imenu
;;
;; Customization:
;; Check `imenu-create-index-function' and `imenu--index-alist' for details.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2014-11-15
;;    Initial release.
;;
;;; Code:

;; GNU library.
(require 'imenu)
(eval-when-compile (require 'cl))

;; 3rd party library.
(require 'grizzl)

(defcustom grizzl-imenu-before-goto-hook nil
  ""
  :type '(repeat function)
  :group 'grizzl)

(defcustom grizzl-imenu-after-goto-hook nil
  ""
  :type '(repeat function)
  :group 'grizzl)

(defun grizzl-imenu-goto (selection)
  )

(defun grizzl-parse-imenu (database)
  (when database
    (let (ret)
      (dolist (entry database)
        (setq ret
              (append ret
                      (if (imenu--subalist-p entry)
                          (grizzl-parse-imenu (cdr entry))
                        (list entry)))))
      ret)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (grizzl-parse-imenu (save-excursion (funcall imenu-create-index-function)))
;;;###autoload
(defun grizzl-imenu ()
  (interactive)
  (lexical-let*
      ((database (save-excursion
                   (imenu--cleanup)
                   (funcall imenu-create-index-function)))
       (entries (grizzl-parse-imenu database)))
    (grizzl-completing-read "find local symbol"
                            (mapcar (lambda (entry)
                                      (car entry))
                                    entries))
    (goto-char (marker-position
                (cdr (elt entries
                          (grizzl-selection)))))
    (recenter 3)))

(provide 'grizzl-imenu)
;;; grizzl-imenu.el ends here
