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
    (let ((i 1))
      (dolist (label spacy--label-backgrounds)
        (let ((start (point)))
          (insert (format "%d %s" i (car label)))
          (ov start (point) 'face (list :background (cdr label)))
          (newline))
        (setq i (+ i 1))))))


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
          (label-bg (cdr (assoc label spacy--label-backgrounds))))
      (ov start end 'face (list :background label-bg) 'label label))))


(defun spacy--save-dataset (filename)
  "Save json dataset to FILENAME."
  (let ((overlays (sort (ov-all) (lambda (ov1 ov2)
                                   (< (ov-beg ov1) (ov-beg ov2)))))
        (offend 0)
        (lines (split-string (buffer-string) "\n" t)))
    (with-temp-file filename
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
  (let* ((entry (assoc label spacy--label-backgrounds))
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
  (spacy-label (car (nth (- num-label 1) spacy--label-backgrounds))))


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
  (define-key spacy-ner-mode-map (kbd "C-c x") 'spacy-clear)
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
  (define-key spacy-ner-mode-map (kbd "C-c 1") (lambda ()
                                                 (interactive)
                                                 (spacy-label "ORG")))
  (define-key spacy-ner-mode-map (kbd "C-c 2") (lambda ()
                                                 (interactive)
                                                 (spacy-label "LOC")))
  (define-key spacy-ner-mode-map (kbd "C-c 3") (lambda ()
                                                 (interactive)
                                                 (spacy-label "PERSON")))
  )


(provide 'spacy-ner)
