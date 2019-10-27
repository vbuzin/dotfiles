(defvar vb:cfg-geometry-file (expand-file-name "geometry" user-emacs-directory))

(defun save-framegeometry ()
  "Gets the current frame's geometry and saves to ~/.emacs.d/geometry"
  (let ((framegeometry-left (frame-parameter (selected-frame) 'left))
        (framegeometry-top (frame-parameter (selected-frame) 'top))
        (framegeometry-width (frame-parameter (selected-frame) 'width))
        (framegeometry-height (frame-parameter (selected-frame) 'height))
        (framegeometry-file vb:cfg-geometry-file))

    (with-temp-buffer
      (insert
       ";;; This is the previous emacs frame's geometry.\n"
       ";;; Last generated " (current-time-string) ".\n"
       "(setq initial-frame-alist\n"
       "      '(\n"
       (format "        (top . %d)\n" (max framegeometry-top 0))
       (format "        (left . %d)\n" (max framegeometry-left 0))
       (format "        (width . %d)\n" (max framegeometry-width 0))
       (format "        (height . %d)))\n" (max framegeometry-height 0)))
      (when (file-writable-p framegeometry-file)
        (write-file framegeometry-file))))
  )

(defun load-framegeometry ()
  "Loads ~/.emacs.d/geometry which should load the previous frame's geometry."
  (let ((framegeometry-file vb:cfg-geometry-file))
    (when (file-readable-p framegeometry-file)
      (load-file framegeometry-file)))
  )

;; Special work to do ONLY when there is a window system being used
(if window-system
    (progn
      (add-hook 'after-init-hook #'load-framegeometry)
      (add-hook 'kill-emacs-hook #'save-framegeometry)))

;; eof
