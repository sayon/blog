---
layout: post
title: "Taking notes in markdown: insert a part of a screenshot"
category: articles
tags: [markdown, emacs]
comments: true
---

I have recently switched to Spacemacs with `markdown-mode` to write everything
markdown-related, especially making notes for whatever things I am fiddling
with. While 'markdown-mode' is amazing, there is one feature I miss, that is:
'pasting' a part of a screenshot into a markdown file by saving it somewhere
and generating a correct link. It is a huge waste of time and concentration to
do it by hand. 

I have written a little helper to automatize it:

```elisp
  (defun my-markdown-screenshot ()
    "Prompt a name, grab an area of screen, save it in 'assets' folder and
    insert a link into markdown."

    (interactive)
    (setq filename
            (concat 
             (file-name-directory buffer-file-name)
             "assets/"
             (read-string "Image name: " )
             "_"
             (format-time-string "%Y%m%d_%H%M%S")  ".png"))
    (call-process "import" nil nil nil filename)
    (insert (concat "![](" filename ")"))
    (markdown-display-inline-images)
    )

```

It uses imagemagick to grab an area. You can switch to another desktop by 
using keyboard. I have tested it only on my Linux machine thought.

**Note** the `assets` directory should exist.
