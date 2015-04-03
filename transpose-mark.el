;;; transpose-mark.el --- Transpose data using the Emacs mark -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2015 Kevin W. van Rooijen <kevin.van.rooijen@attichacker.com>
;; This file is part of transpose-mark.

;; transpose-mark is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; transpose-mark is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with transpose-mark.  If not, see <http://www.gnu.org/licenses/>.

(defface transpose-mark-region-set-face
  '((t :background "#7700ff" :foreground "#ffffff"))
  "Transpose Marked region face" :group 'transpose-mark)

(defvar transpose-mark-region-overlay 'nil "Overlay for Transpose Mark Region")

(defun transpose-mark ()
  (interactive)
  (if (region-active-p) (transpose-mark-region) (transpose-mark-line)))

(defun transpose-mark-region ()
  "Transpose the current region with the previously marked region.
Once you've transposed one the region is reset."
  (interactive)
  (if (and transpose-mark-region-overlay (overlay-start transpose-mark-region-overlay))
      (let* ((current-region (buffer-substring-no-properties (mark) (point)))
             (target-start (overlay-start transpose-mark-region-overlay))
             (target-end (overlay-end transpose-mark-region-overlay))
             (target-region (buffer-substring-no-properties target-start target-end)))
        (if (> (mark) target-start)
            (progn
              (transpose-mark-region-set-current target-region)
              (transpose-mark-region-set-target target-start target-end current-region))
          (progn
            (transpose-mark-region-set-target target-start target-end current-region)
            (transpose-mark-region-set-current target-region)))
        (delete-overlay transpose-mark-region-overlay))
    (transpose-mark-save-point)))

(defun transpose-mark-line ()
  "Transpose the current line with the line which the current mark
is pointing to."
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (beginning-of-line)
      (kill-line)
      (pop-to-mark-command)
      (beginning-of-line)
      (yank)
      (kill-line))
    (yank)
    (pop-mark)
    (move-to-column col)))

(defun transpose-mark-region-set-target (target-start target-end current-region)
  (kill-region target-start target-end)
  (insert-at-point current-region (min target-start target-end)))

(defun transpose-mark-region-set-current (target-region)
  (kill-region (mark) (point))
  (insert target-region))

(defun transpose-mark-save-point ()
  (deactivate-mark nil)
  (setq transpose-mark-region-overlay (make-overlay (mark) (point)))
  (overlay-put transpose-mark-region-overlay 'face 'transpose-mark-region-set-face)
  (message "Transpose Mark Region set!"))

(defun insert-at-point (string point)
  "Inserts a string at a given point."
  (save-excursion
    (goto-char point)
    (insert string)))

(provide 'transpose-mark)

;;; transpose-mark.el ends here
