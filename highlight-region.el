;;; highlight-region.el --- Automatically highlight text matching active region  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Nicolas Richard

;; Author: Nicolas Richard <youngfrog@members.fsf.org>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Automatically highlight text matching region when mark is active.

;; see http://emacs.stackexchange.com/questions/22041/automatic-search-and-replace-of-the-marked-region

;; TODO: check usefulness in relation with selected.el


;;; Code:

(defvar-local highlight-region--highlighted-text "")

(define-minor-mode highlight-region-mode
  "Automatically highlight current region when mark is active."
  nil nil nil
  (highlight-region--unhighlight-region)
  (remove-hook 'activate-mark-hook #'highlight-region--highlight-region t)
  ;; as for GNU Emacs 25.0.93.1 (i686-pc-linux-gnu, X toolkit, Xaw scroll bars) of 2016-05-03
  ;; it seems that the following behaviour of activate-mark-hook described in its dosctring :
  ;; >> It is also run at the end of a command, if the mark is active and
  ;; >> it is possible that the region may have changed.
  ;; actually doesn't work. Thus let's add ourselves to post-command-hook...
  (remove-hook 'post-command-hook #'highlight-region--highlight-region t)
  (remove-hook 'deactivate-mark-hook #'highlight-region--unhighlight-region t)
  (when highlight-region-mode
    (add-hook 'activate-mark-hook #'highlight-region--highlight-region t)
    (add-hook 'post-command-hook #'highlight-region--highlight-region t)
    (add-hook 'deactivate-mark-hook #'highlight-region--unhighlight-region t)))

(defun highlight-region--highlight-region ()
  "Highlight currently active region"
  (when (use-region-p)
    (let ((str (buffer-substring-no-properties (region-beginning) (region-end))))
      (unless (or (string-empty-p str)
                  (string= str highlight-region--highlighted-text))
        (highlight-region--unhighlight-region)
        (setq highlight-region--highlighted-text str)
        ;; TODO: add a face `highlight-region' and use it.
        (highlight-regexp (regexp-quote str))))))

(defun highlight-region--unhighlight-region nil
  (unhighlight-regexp (regexp-quote highlight-region--highlighted-text))
  (setq highlight-region--highlighted-text ""))

(provide 'highlight-region)
;;; highlight-region.el ends here