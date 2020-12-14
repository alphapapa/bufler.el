(defun bufler-tab-line-tab-face (tab _tabs face)
  "Return a FACE in which to display TAB according to its workspace."
  (cl-labels ((relative-luminance
	       ;; Copy of `modus-themes-wcag-formula', an elegant
	       ;; implementation by Protesilaos Stavrou.  Also see
	       ;; <https://en.wikipedia.org/wiki/Relative_luminance> and
	       ;; <https://www.w3.org/TR/WCAG20/#relativeluminancedef>.
	       (name) (cl-loop for k in '(0.2126 0.7152 0.0722)
			       for x in (color-name-to-rgb name)
			       sum (* k (if (<= x 0.03928)
					    (/ x 12.92)
					  (expt (/ (+ x 0.055) 1.055) 2.4)))))
	      (contrast-ratio
	       ;; Copy of `modus-themes-contrast'; see above.
	       (a b) (let ((ct (/ (+ (relative-luminance a) 0.05)
				  (+ (relative-luminance b) 0.05))))
		       (max ct (/ ct)))))
    (when (bufferp tab)
      (let* ((workspace-path (bufler-buffer-workspace-path tab))
             ;; (_ (message "PATH:%S  TAB:%S" workspace-path tab))
	     (hash (float (abs (sxhash workspace-path))))
	     ;; TODO: Wrap-around the value to get the color I want.
	     (ratio (/ hash (float most-positive-fixnum)))
	     (color-num (round (* (* 255 255 255) ratio)))
	     (color-rgb (list (/ (float (logand color-num 255)) 255)
			      (/ (float (lsh (logand color-num 65280) -8)) 255)
			      (/ (float (lsh (logand color-num 16711680) -16)) 255)))
	     (background-rgb (color-name-to-rgb (face-background 'tab-line nil t)))
             (foreground-name (face-foreground 'tab-line nil t))
             (new-color-name (if (< (contrast-ratio (apply #'color-rgb-to-hex color-rgb)
                                                    foreground-name)
                                    3)
				 (progn
				   ;; Contrast ratio too low: I don't know the best way to fix this,
				   ;; but using the complement seems to produce decent results.
				   ;; FIXME: Calculate and apply an adjustment instead.
				   (apply #'color-rgb-to-hex
					  (append (color-complement (apply #'color-rgb-to-hex
									   (append color-rgb (list 2))))
						  (list 2))))
			       (apply #'color-rgb-to-hex (append color-rgb (list 2))))))
	(setf face `(:inherit ,face :background ,new-color-name))))
    face))

(face-background '(:inherit (tab-line-tab-inactive-alternate tab-line-tab-inactive)) nil  t)
(face-background '((t :background "red")) nil  t)


(cl-defun bufler-workspace-buffers (&optional (frame (selected-frame)))
  "Return list of buffers for FRAME's workspace.
Works as `tab-line-tabs-function'."
  ;; This is specifically for `bufler-workspace-tabs-mode', but it
  ;; needn't be only for that, so it probably belongs here.
  (let (buffers)
    (--tree-map-nodes (bufferp it)
                      (push it buffers)
                      (bufler-buffers :path (frame-parameter frame 'bufler-workspace-path)))
    (cl-sort buffers #'< :key (lambda (buffer)
                                (sxhash (bufler-buffer-workspace-path buffer))))))
