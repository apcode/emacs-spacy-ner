;; spacy-mode.el -- Edit NER labels for spacy NER model
;;
;; ("X" . "#FCF3CF")
;; ("X" . "#F6DDCC")
;; ("X" . "#CD6155")
;; ("X" . "#AF7AC5")
;; ("X" . "#5499C7")
;; ("X" . "#1ABC9C")
;; ("X" . "#F4D03F")
;; ("X" . "#D35400")
;; ("X" . "#7B241C")
;; ("X" . "#5B2C6F")

(require 'json)
(require 'ov)


(defvar spacy--label-backgrounds
  '(
    ("ORG" . "#117864")
    ("LOC" . "#9A7D0A")
    ("PERSON" . "#873600")
    )
  "Stores list of NER labels and colors.")


(defun spacy-display-labels ()
  "Display overlayed labels in new buffer."
  (interactive)
  (progn
    (pop-to-buffer
     (get-buffer-create "*-ner-tags-*"))
    (dolist (label spacy-label-backgrounds)
      (let ((start (point)))
        (insert (car label))
        (ov start (point) 'face (list :background (cdr label)))
        (newline)))))


(defun spacy-ner ()
  "Create a spacy ner editable buffer."
  (interactive)
  (let ((ner-file (read-file-name "Enter ner training file: ")))
    (switch-to-buffer (format "*-Spacy-NER-%s-*" (file-name-base ner-file)))
    (make-local-variable 'orig-file)
    (setq orig-file ner-file)
    (spacy--load-dataset orig-file)))


(defun spacy--load-dataset (filename)
  "Load jsonlines from FILENAME into current buffer."
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char 1)
    (let ((offset 1))
      (dolist (line (split-string (buffer-string) "\n" t))
        (let* ((json-object-type 'hash-table)
               (json-array-type 'list)
               (json-key-type 'string)
               (json (json-read-from-string line))
               (text (gethash "text" json))
               (spans (gethash "spans" json)))
          (switch-to-buffer edit-buffer)
          (insert text)
          (newline)
          (spacy--apply-overlays offset spans)
          (setq offset (point))))))


(defun spacy--apply-overlays (offset spans)
  "Apply overlays with line OFFSET from SPANS."
  (dolist (span spans)
    (let* ((start (+ offset (gethash "start" span)))
          (end (+  offset (gethash "end" span)))
          (label (gethash "label" span))
          (label-bg (cdr (assoc label spacy-label-backgrounds))))
      (ov start end 'face (list :background label-bg) 'label label))))


(defun spacy--save-dataset ()
  "Save json dataset back to BUFFER."
  (let ((overlays (reverse (ov-all)))
        (offend 0)
        (lines (split-string (buffer-string) "\n" t))
        (buffer (generate-new-buffer "*temp*)")))
    (switch-to-buffer buffer)
    ;; (insert (prin1-to-string spans) "\n")
    (dolist (line lines)
      (setq offbeg (+ offend 1)
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
          (newline))))))


(defun spacy-save ()
  "Save current NER buffer back to original file."
  (interactive)
  (spacy--save-dataset))


(defun spacy-delete ()
  "Delete the label at point."
  (interactive)
  (let ((ov (ov-at)))
    (if ov
        (ov-reset ov))))


(defun spacy-relabel (label)
  "Change existing label to new LABEL."
  (interactive "sLabel: ")
  (let ((ov (ov-at)))
    (if ov
        (ov-set ov 'face
                (list :background (cdr (assoc label spacy-label-backgrounds)))
                'label label))))


(defun spacy-label (label)
  "Add a new LABEL at current region."
  (interactive "sLabel: ")
  (ov (mark) (point) 'face
      (list :background (cdr (assoc label spacy-label-backgrounds)))
      'label label)
  (deactivate-mark))
