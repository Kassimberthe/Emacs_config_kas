;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "ob-ipython" "20180224.953"
  "Org-babel functions for IPython evaluation."
  '((s               "1.9.0")
    (dash            "2.10.0")
    (dash-functional "1.2.0")
    (f               "0.17.2")
    (emacs           "24"))
  :url "http://www.gregsexton.org"
  :commit "7147455230841744fb5b95dcbe03320313a77124"
  :revdesc "714745523084"
  :keywords '("literate programming" "reproducible research")
  :authors '(("Greg Sexton" . "gregsexton@gmail.com"))
  :maintainers '(("Greg Sexton" . "gregsexton@gmail.com")))
