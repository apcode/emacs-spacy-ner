# Emacs Mode for Spacy-NER

An emacs mode for editing json files used to [train spacy ner models](https://spacy.io/usage/training). The  mode allows you to quickly view and label text with NER labels. You can add, delete, alter and match labels across text. Typically you have one sentence or short sequence of text per line. You can define your own labels or use the predefined ones in the mode. See [Spacy](https://spacy.io).

Training files are jsonlines files with one json object per line of text. They have `text` and `spans` fields. The `spans` field is a list of span objects that contain a `start` `end` and `label`.

    {"text": "Google is based in Mountain View", "spans": [{"start": 0, "end": 6, "label": "ORG"}]}

The `spans` field can be missing if there are no spans defined.

## Using spacy-ner mode

To use the mode, add the following to your .emacs

    (require 'spacy-ner)

See [Customization](#customization).

Run the command `spacy-ner`. This will ask you for the input jsonlines file.

The mode opens a new buffer with the NER view of the text in the input file.

![spacy-ner](https://github.com/apcode/emacs-spacy-ner/blob/master/spacy-ner-view.png)

## Commands

### Navigation

`forward-label [C-M-<right>]`
: Skip to next label in text.

`backward-label [C-M-<left>]`
: Skip to previous label in text.

`forward-label-at [C-M-S-<right>]`
: Skip to next label of same kind as the current point.

`backward-label-at [C-M-S-<left>]`
: Skip to previous label of same kind as the current point.

`spacy-display-labels`
: Display the list of labels with number and colors. Opens a separate buffer.

![spacy-ner](https://github.com/apcode/emacs-spacy-ner/blob/master/spacy-labels.png)

### Editing

`spacy-label [C-c l]`
: Apply or change a label. Asks for the label string. If point is in an existing label, then that label is changed to the new label. Otherwise the new label is applied to the current marked region.

`spacy-label-n [C-c n]`
: Apply or change a label. Asks for the label number. The numbers are the orded number in the label list. This can be viewed with `spacy-display-labels`. If point is in an existing label, then that label is changed to the new label. Otherwise the new label is applied to the current marked region.

`spacy-label-matches [C-c m]`
: Apply the label the point is in to all text. All cases of the label's text have the current label applied, i.e. if the string "Google" is labelled as "ORG", then this command will apply the "ORG" label to all cases of the string "Google" in the buffer.

`[C-c <label-key>]`
: Each label has a key that with `C-c` calls `spacy-label "LABEL"`. So this provides an easy shortcut to apply any label to the current marked region.

`spacy-clear [C-c x]`
: Delete all labels in the buffer

`spacy-delete [C-c d]`
: Delete the label at point.

`spacy-save [C-x C-s]`
: Save the labels and text back to a jsonl file for spacy training. The first time you run this command it asks for the filename to save to. Subsequent calls save to that file.

`spacy-write [C-c C-w]`
: Save the labels and text to a new jsonl filefor spacy training.

## Customization

You can define your own label list to use. You can either do this in your .emacs before requiring `spacy-ner` or add a function to `spacy-ner-mode-hook`.

The label list should look like:

    (defvar spacy--label-list
        '(
          ("ORG" . "#117864")
          ("LOC" . "#9A7D0A")
          ("PERSON" . "#873600")
         )
        "Stores list of assoc pairs: (label . color).")

## TODO

- Train spacy model on current buffer. Display performance in temp buffer.
- Apply spacy model's labels to current buffer text.
- Iterative functionality to continually train and update labels from spacy model.
  - label -> train -> update labels from model.
