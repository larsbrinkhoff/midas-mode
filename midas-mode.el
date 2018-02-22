;;; midas-mode.el --- Programming language mode for MIDAS assembly language
;;; copyright 2018 Lars Brinkhoff

;; You can redistribute it and/or modify it under the terms of the
;; GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; Author: Lars Brinkhoff <lars@nocrew.org>
;; Keywords: languages assembler midas
;; URL: http://github.com/larsbrinkhoff/midas-mode
;; Version: 0.1

;;; Commentary:
;; Programming language mode for MIDAS assembly language

;;; Code:

(eval-when-compile (byte-compile-disable-warning 'cl-functions))
(require 'cl)

(defvar midas-mode-hook)

(unless (fboundp 'asm-mode)
  (defalias 'prog-mode 'fundamental-mode))

(unless (fboundp 'setq-local)
  (defmacro setq-local (var val)
    `(set (make-local-variable ',var) ,val)))

(defvar midas-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\; "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\. "_" table)
    (modify-syntax-entry ?\% "_" table)
    (modify-syntax-entry ?\{ "_" table)
    (modify-syntax-entry ?\} "_" table)
    (modify-syntax-entry ?\[ "(" table)
    (modify-syntax-entry ?\] ")" table)
    (modify-syntax-entry ?\< "(" table)
    (modify-syntax-entry ?\> ")" table)
    (modify-syntax-entry ?\' "/" table)
    (modify-syntax-entry ?\" "/" table)
    (modify-syntax-entry ?^ "/" table)
    (modify-syntax-entry ?\/ "\"" table)
    table))

;;;###autoload
(define-derived-mode midas-mode asm-mode "MIDAS"
		     "Major mode for editing MIDAS files."
		     :syntax-table midas-mode-syntax-table
  (setq font-lock-defaults '(nil))
  (setq-local comment-start-skip ";")
  (setq-local comment-start ";")
  (setq-local comment-end "$"))
