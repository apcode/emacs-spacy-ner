# Emacs Mode for Space-NER

An emacs mode for editing json files used to train spacy ner models. The  mode allows you to quickly view and label text with NER labels. You can add, delete, alter and match labels across text. Typically you have one sentence or short sequence of text per line. You can define your own labels or use the predefined ones in the mode.

Training files are jsonlines files with one json object per line of text. They have `text` and `spans` fields. The `spans` field has `start` `end` and `label`

    {"text": "Google is based in Mountain View", "spans": [{"start": 0, "end": 6, "label": "ORG"}]}

The `spans` field can be missing if there are no spans defined.

## Using spacy-ner mode

To use the mode, you run the command `spacy-ner`. This will ask you for the input jsonlines file.

The mode opens a new buffer with the NER view of the text in the input file.

![spacy-ner](http://github.com/apcode/spacy-ner/master/spacy-ner-view.png)

To use, add the following to your .emacs

    (require 'spacy-ner)

See [Customization](#customization).

## Commands

### Navigation

`forward-label [C-M-<right>]`
: Skip to next label in text.

`backward-label [C-M-<left>]`
: Skip to previous label in text.

`forward-label-at [C-M-S-<right>]`
: Skip to next label of same kind as the current point is in.

`backward-label-at [C-M-S-<left>]`
: Skip to previous label of same kind as the current point is in.

`spacy-display-labels`
: Display the list of labels with number and colors. Opens a separate buffer.

### Editing

`spacy-label [C-c l]`
: Apply or change a label. Asks for the label string. If point is in an existing label, then that label is changed to the new label. Otherwise the new label is applied to the current marked region.

`spacy-label-n [C-c n]`
: Apply or change a label. Asks for the label number. The numbers are the orded number in the label list. This can be viewed with `spacy-display-labels`. If point is in an existing label, then that label is changed to the new label. Otherwise the new label is applied to the current marked region.

`spacy-label-matches [C-c m]`
: Apply the label the point is in to all text. All cases of the label's text have the current label applied, i.e. if the string "Google" is labelled as "ORG", then this command will apply the "ORG" label to all cases of the string "Google" in the buffer.

`[C-c 1..n]`
: `C-c 1` applies label number 1, `C-c 2` label 2 etc. The number from 1 to n are the label numbers in the list.

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
)
