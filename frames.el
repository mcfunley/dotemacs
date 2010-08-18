;;;
;; frames.el 
;; Utilities for manipulating frames.
;; 
;; by Dan McKinley, 2008
;; http://mcfunley.com
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;; 

(eval-when-compile (require 'cl))

(defcustom frames:*main* nil "The first frame created.")
(defcustom frames:*initial-size* '(115 . 52)
  "The size of the first frame on startup. Set to (cols . rows).")
(defcustom frames:*initial-pos* '(10 . 35)
  "The position of the first frame. Set to (left . top).")

(defvar frames:*zoom-config* nil)


(defun* frames:autoposition ()
  "Lays out the frames on the available display area."
  (interactive)
  (let* ((total-width (frames:total-width))
	 (disp-width (x-display-pixel-width))
	 (framecount (length (frame-list)))
	 (top (cdr frames:*initial-pos*))
	 (spare-width (- disp-width total-width))
	 (spacer (/ spare-width (1+ framecount))))
    (when (< spare-width 0)
      (error "Total frame width is too wide."))
    (let ((x spacer))
      (dolist (f (reverse (frame-list)))
	(set-frame-position f x top)
	(setf x (+ x (+ spacer (frame-pixel-width f))))))))

(defun* frames:zoom (&optional (frame nil))
  "Centers and widens the active frame."
  (interactive)
  (let* ((f (or frame (selected-frame)))
	 (others (remove f (frame-list)))
	 (w (round (* (frames:display-column-width) 0.75))))
    (push (current-frame-configuration) frames:*zoom-config*)
    (when (< (frame-width f) w)
      (set-frame-width f w))
    (mapcar #'iconify-frame others)
    (frames:center f)))
	
(defun frames:unzoom ()
  "Restores the frame configuration from before the last call to `frames:zoom'."
  (interactive)
  (when frames:*zoom-config*
    (set-frame-configuration (pop frames:*zoom-config*))))

(defun frames:column-pixel-width ()
  (/ (frame-pixel-width) (frame-width)))

(defun frames:display-column-width ()
  (/ (x-display-pixel-width) (frames:column-pixel-width)))
  
(defun* frames:center (&optional (frame nil))
  (let* ((cx (/ (x-display-pixel-width) 2))
	 (x (- cx (/ (frame-pixel-width frame) 2))))
    (set-frame-parameter frame 'left x)))

(defun* frames:position-first-frame ()
  (let ((fr frames:*main*))
    (set-frame-position 
     fr (car frames:*initial-pos*) (cdr frames:*initial-pos*))
    (set-frame-size 
     fr (car frames:*initial-size*) (cdr frames:*initial-size*))))

(defun* frames:tag (frame tag)
  (let ((tags (cons tag (frames:get-tags frame))))
    (frames:set-tags frame tags)))

(defun* frames:set-tags (frame tags)
  (modify-frame-parameters frame (list (cons 'tags tags))))

(defun* frames:remove-tag (frame tag)
  (let ((tags (remove tag (frames:get-tags frame))))
    (frames:set-tags frame tags)))

(defun* frames:get-tags (frame) 
  (frame-parameter frame 'tags))

(defun* frames:has-tag (frame tag)
  (find tag (frames:get-tags frame)))

(defun* frames:find-by-tag (tag)
  (loop for f in (frame-list)
	when (frames:has-tag f tag)
	collect f))

(defun* frames:setup ()
  "Initializes the frames package - call from .emacs before creating new frames."
  (let ((fr (car (frame-list))))
    (setq frames:*main* fr)
    (frames:tag fr 'main)
    (frames:position-first-frame)))

(defun* frames:total-width ()
  (reduce #'+ (frame-list) :key #'frame-pixel-width))

(provide 'frames)