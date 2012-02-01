 ;;; lettuce-mode.el --- Lettuce major mode

 ;; Copyright (C) 2001  Noel Bush

 ;; Author: Noel Bush
 ;; Keywords: extensions

 ;; This file is free software; you can redistribute it and/or modify
 ;; it under the terms of the GNU General Public License as published by
 ;; the Free Software Foundation; either version 2, or (at your option)
 ;; any later version.

 ;; This file is distributed in the hope that it will be useful,
 ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
 ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 ;; GNU General Public License for more details.

 ;; You should have received a copy of the GNU General Public License
 ;; along with GNU Emacs; see the file COPYING.  If not, write to
 ;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 ;; Boston, MA 02111-1307, USA.

 ;;; Code:

 (defvar lettuce-mode-syntax-table
   (let ((st (make-syntax-table)))
     (modify-syntax-entry ?# "<" st)
     (modify-syntax-entry ?\n ">" st)
     st)
   "Syntax table for `lettuce-mode'.")

 (defvar lettuce-font-lock-keywords
   '(("function \\(\\sw+\\)" (1 font-lock-function-name-face)))
   "Keyword highlighting specification for `lettuce-mode'.")

 (defvar lettuce-imenu-generic-expression
   ...)

 (defvar lettuce-outline-regexp
   ...)

 ;;;###autoload
 (define-derived-mode lettuce-mode fundamental-mode "Lettuce"
   "A major mode for editing Lettuce test specifications."
   :syntax-table lettuce-mode-syntax-table
   (set (make-local-variable 'comment-start) "# ")
   (set (make-local-variable 'comment-start-skip) "#+\\s-*")
   (set (make-local-variable 'font-lock-defaults)
	'(lettuce-font-lock-keywords))
   (set (make-local-variable 'indent-line-function) 'lettuce-indent-line)
   (set (make-local-variable 'imenu-generic-expression)
	lettuce-imenu-generic-expression)
   (set (make-local-variable 'outline-regexp) lettuce-outline-regexp)
   ...)

 ;;; Indentation

 (defun lettuce-indent-line ()
   "Indent current line of Lettuce."
   (interactive)
   (let ((savep (> (current-column) (current-indentation)))
	 (indent (condition-case nil (max (lettuce-calculate-indentation) 0)
		   (error 0))))
     (if savep
	 (save-excursion (indent-line-to indent))
       (indent-line-to indent))))

 (defun lettuce-calculate-indentation ()
   "Return the column to which the current line should be indented."
   ...)


 (provide 'lettuce)
 ;;; lettuce-mode.el ends here
