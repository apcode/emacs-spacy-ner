;; spacy-mode.el -- Edit NER labels for spacy NER model
;;

(require 'json)
(require 'ov)


(defvar spacy--label-properties
  '(
    (label "ORG" bg "#7B241C" char \o)
    (label "LOCATION" bg "#943126" char \g)
    (label "PERSON" bg "#633974" char \r)
    (label "ACTION" bg "#1A5276" char \a)
    (label "TASK" bg "#1A5276" char \t)
    (label "TOOL" bg "#1A5276" char \h)
    (label "SYSTEM" bg "#117864" char \s)
    (label "PRODUCT" bg "#0E6655" char \p)
    (label "PART" bg "#196F3D" char \q)
    (label "COMPONENT" bg "#196F3D" char \c)
    (label "INTERNAL" bg "#196F3D" char \i)
    (label "FAULT" bg "#B9770E" char \f)
    (label "SYMPTOM" bg "#AF601A" char \y)
    (label "MODEL" bg "#AF601A" char \e)
    (label "MPN" bg "#AF601A" char \z)
    (label "MATERIAL" bg "#AF601A" char \x)
    (label "COLOR" bg "#212F3D" char \u)
    (label "MEASURE" bg "#212F3D" char \v)
    )
  "Stores list of NER labels and colors.")

(defun spacy-display-labels ()
  "Display overlayed labels in new buffer."
  (interactive)
  (progn
    (pop-to-buffer
     (get-buffer-create "*-ner-tags-*"))
    (dolist (label-prop spacy--label-properties)
      (let ((start (point)))
        (insert (format "%c %s" (plist-get label-prop 'char) (plist-get label-prop 'label)))
        (ov start (point) 'face (list :background (plist-get label-prop 'bg)))
        (newline))
      (setq i (1+ i)))))


(defvar spacy-ner-mode-hook nil
  "List of functions to call when entering spacy-ner-mode.")


(defun spacy-ner ()
  "Create a spacy ner editable buffer for labelling text.

Special commands:
\\{spacy-ner-mode-map}"
  (interactive)
  (let ((ner-file (read-file-name "Enter ner training file: "))
        (inhibit-read-only t))
    (switch-to-buffer (format "*-Spacy-NER-%s-*" (file-name-base ner-file)))
    ;;    (spacy-ner-mode)
    (erase-buffer)
    (setq major-mode 'spacy-ner-mode)
    (setq mode-name "Spacy-NER")
    (run-hooks 'spacy-ner-mode-hook)
    (use-local-map spacy-ner-mode-map)
    (read-only-mode)
    (make-local-variable 'orig-file)
    (setq orig-file ner-file)
    (spacy--load-dataset orig-file)))


(defun spacy--load-dataset (filename)
  "Load jsonlines from FILENAME into current buffer."
  (let ((lines (with-temp-buffer
                 (insert-file-contents filename)
                 (split-string (buffer-string) "\n" t)))
        (offset 1)
        (inhibit-read-only t))
    (dolist (line lines)
      (let* ((inhibit-read-only t)
             (json-object-type 'hash-table)
             (json-array-type 'list)
             (json-key-type 'string)
             (json (json-read-from-string line))
             (text (gethash "text" json))
             (spans (gethash "spans" json)))
        (insert text)
        (newline)
        (spacy--mark-line offset spans)
        (setq offset (point))))))


(defun spacy--mark-line (offset spans)
  "Markup line at OFFSET with NER label SPANS."
  (dolist (span spans)
    (let* ((start (+ offset (gethash "start" span)))
          (end (+  offset (gethash "end" span)))
          (label (gethash "label" span))
          (label-bg (cdr (assoc label spacy--label-properties))))
      (ov start end 'face (list :background label-bg) 'label label))))


(defun spacy--save-dataset (filename)
  "Save json dataset to FILENAME."
  (let ((overlays (sort (ov-all) (lambda (ov1 ov2)
                                   (< (ov-beg ov1) (ov-beg ov2)))))
        (offend 0)
        (lines (split-string (buffer-string) "\n" t)))
    (with-temp-file filename
      (dolist (line lines)
        (setq offbeg (1+ offend)
              offend (+ offbeg (length line)))
        (let* ((over (car overlays))
               (spans 0))
          (while (and over (< (ov-beg over) offend))
            (let ((span (list :start (- (ov-beg over) offbeg)
                              :end (- (ov-end over) offbeg)
                              :label (ov-val over 'label))))
              (if (listp spans)
                  (setq spans (cons span spans))
                (setq spans (cons span ())))
              (setq overlays (cdr overlays)
                    over (car overlays))))
          ;; write text and any spans
          ;; convert to vector for json
          (let ((vec (vector)))
            (if (listp spans)
                (setq vec (vconcat vec (reverse spans))))
            (insert (json-encode
                     (list :text line :spans vec)))
            (newline)))))))


(defalias 'forward-label 'ov-goto-next
  "Move to next label.")

(defalias 'backward-label 'ov-goto-prev
  "Move to previous label.")

(defun forward-label-at ()
  "Move to next label of same type as at point."
  (interactive)
  (let ((over (ov-at)))
    (if over
        (ov-goto-next 'label (ov-val over 'label)))))

(defun backward-label-at ()
  "Move to next label of same type as at point."
  (interactive)
  (let ((over (ov-at)))
    (if over
        (ov-goto-prev 'label (ov-val over 'label)))))


(defun spacy-clear ()
  "Clear all overlays."
  (interactive)
  (ov-clear))


(defun spacy-write ()
  "Write to new file."
  (interactive)
  (let ((saved-file-tmp (read-file-name "File to save to: ")))
    (make-local-variable 'saved-file)
    (setq saved-file saved-file-tmp)
    (spacy--save-dataset saved-file)))


(defun spacy-save ()
  "Save current NER buffer back to original file."
  (interactive)
  (if (boundp 'saved-file)
      (spacy--save-dataset saved-file)
    (spacy-write)))


(defun spacy-delete ()
  "Delete the label at point."
  (interactive)
  (let ((over (ov-at)))
    (if over
        (ov-reset over))))


(defun spacy-label (label)
  "Apply a LABEL to either existing or new region."
  (interactive "sLabel: ")
  (let* ((entry (plist-get spacy--label-properties 'label))
         (name (car entry))
         (bg (cdr entry))
         (over (ov-at)))
    (if over
        (ov-set over 'face
                (list :background bg
                'label name))
      (ov (mark) (point) 'face
          (list :background bg)
          'label name)
    (deactivate-mark))))


(defun spacy-label-n (num-label)
  "Label using NUM-LABEL entry in list."
  (interactive "nLabel Num: ")
  (spacy-label (plist-get (nth (- num-label 1) spacy--label-properties) 'label)))


(defun spacy-label-matches ()
  "Create labels for all matching text."
  (interactive)
  (let* ((over (ov-at))
         (text (buffer-substring (ov-beg over) (ov-end over))))
    (ov-set text 'face (ov-val over 'face) 'label (ov-val over 'label))
    (ov-clear over)))


(defvar spacy-ner-mode-map nil
  "Keymap for spacy-ner-mode.")


(if spacy-ner-mode-map
    nil
  (setq spacy-ner-mode-map (make-sparse-keymap))
  (define-key spacy-ner-mode-map (kbd "C-c d") 'spacy-delete)
  (define-key spacy-ner-mode-map (kbd "C-x C-s") 'spacy-save)
  (define-key spacy-ner-mode-map (kbd "C-x C-w") 'spacy-write)
  (define-key spacy-ner-mode-map (kbd "C-c l") 'spacy-label)
  (define-key spacy-ner-mode-map (kbd "C-c n") 'spacy-label-n)
  (define-key spacy-ner-mode-map (kbd "C-c m") 'spacy-label-matches)
  (define-key spacy-ner-mode-map (kbd "C-M-<right>") 'forward-label)
  (define-key spacy-ner-mode-map (kbd "C-M-<left>") 'backward-label)
  (define-key spacy-ner-mode-map (kbd "C-M-S-<right>") 'forward-label-at)
  (define-key spacy-ner-mode-map (kbd "C-M-S-<left>") 'backward-label-at)
  ;; label specific bindings
  (dolist (props spacy--label-properties)
    (define-key spacy-ner-mode-map (kbd (format "C-c %c" (plist-get props 'char)))
      (lambda ()
        (interactive)
        (spacy-label (plist-get props 'label))))
    )
  ;; (define-key spacy-ner-mode-map (kbd "C-c o") (lambda ()
  ;;                                                (interactive)
  ;;                                                (spacy-label "ORG")))
  ;; (define-key spacy-ner-mode-map (kbd "C-c g") (lambda ()
  ;;                                                (interactive)
  ;;                                                (spacy-label "LOCATION")))
  ;; (define-key spacy-ner-mode-map (kbd "C-c r") (lambda ()
  ;;                                                (interactive)
  ;;                                                (spacy-label "PERSON")))
  ;; (define-key spacy-ner-mode-map (kbd "C-c a") (lambda ()
  ;;                                                (interactive)
  ;;                                                (spacy-label "ACTION")))
  ;; (define-key spacy-ner-mode-map (kbd "C-c t") (lambda ()
  ;;                                                (interactive)
  ;;                                                (spacy-label "TASK")))
  ;; (define-key spacy-ner-mode-map (kbd "C-c t") (lambda ()
  ;;                                                (interactive)
  ;;                                                (spacy-label "TOOL")))
  ;; (define-key spacy-ner-mode-map (kbd "C-c s") (lambda ()
  ;;                                                (interactive)
  ;;                                                (spacy-label "SYSTEM")))
  ;; (define-key spacy-ner-mode-map (kbd "C-c p") (lambda ()
  ;;                                                (interactive)
  ;;                                                (spacy-label "PRODUCT")))
  ;; (define-key spacy-ner-mode-map (kbd "C-c r") (lambda ()
  ;;                                                (interactive)
  ;;                                                (spacy-label "PART")))
  ;; (define-key spacy-ner-mode-map (kbd "C-c c") (lambda ()
  ;;                                                (interactive)
  ;;                                                (spacy-label "COMPONENT")))
  ;; (define-key spacy-ner-mode-map (kbd "C-c i") (lambda ()
  ;;                                                (interactive)
  ;;                                                (spacy-label "INTERNAL")))
  ;; (define-key spacy-ner-mode-map (kbd "C-c f") (lambda ()
  ;;                                                (interactive)
  ;;                                                (spacy-label "FAULT")))
  ;; (define-key spacy-ner-mode-map (kbd "C-c y") (lambda ()
  ;;                                                (interactive)
  ;;                                                (spacy-label "SYMPTOM")))
  ;; (define-key spacy-ner-mode-map (kbd "C-c e") (lambda ()
  ;;                                                (interactive)
  ;;                                                (spacy-label "MODEL")))
  ;; (define-key spacy-ner-mode-map (kbd "C-c z") (lambda ()
  ;;                                                (interactive)
  ;;                                                (spacy-label "MPN")))
  ;; (define-key spacy-ner-mode-map (kbd "C-c x") (lambda ()
  ;;                                                (interactive)
  ;;                                                (spacy-label "MATERIAL")))
  ;; (define-key spacy-ner-mode-map (kbd "C-c u") (lambda ()
  ;;                                                (interactive)
  ;;                                                (spacy-label "COLOR")))
  ;; (define-key spacy-ner-mode-map (kbd "C-c v") (lambda ()
  ;;                                                (interactive)
  ;;                                                (spacy-label "MEASURE")))
  )


(provide 'spacy-ner)
