
(setf lips:*use-smart-quotes* t)

(defparameter *last-bref-book* nil)

(setf lips:*paragraph-begin* "<p>")
(setf lips:*paragraph-end* (lambda ()
                             (setf *last-bref-book* nil)
                             (format t "</p>")))

(defparameter *lips-files* nil)

(defun all-lips-files ()
  (if *lips-files*
      *lips-files*
      (let* ((thisdirname (namestring (car (directory "."))))
             (files (loop for f in (directory "**/*.lips") collect
                         (pathname (subseq (namestring f) (length thisdirname))))))
        (setf *lips-files* files))))

(defun lips-file-by-name (name)
  (loop for filename in (all-lips-files) do
       (when (string= (pathname-name filename) name)
         (return filename))))

(defun pwarn (str)
  (format *error-output* "*** Warning: ~a~%" str))

(defun html-footer ()
  (format nil "~%</article></body></html>"))

(add-finish-hook #'html-footer)
(macro page (type title)
  (%!
"<!DOCTYPE html>
<html>
<head>
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
<meta charset=\"UTF-8\">
<link rel=\"stylesheet\" href=\"/site.css\">
<title>~a</title>
</head>
<body class=\"~a\">
<article id=\"container\">
"
          title
          (cond
            ((string= type "draft")
             "draft")
            (t
             (when (string/= type "article")
               (pwarn (format nil "Unrecognized article type '~a', using 'article'" type)))
             "article"))))

(macro banner (text)
  (%! "<div class=\"banner\">~a</div>" text))

(defun list-body ()
  (loop
     for x = (read-macro-argument)
     while (string/= x ":end")
     do (%! "<li>~a</li>~%" x)))

(macro --- (x)
  (declare (ignore x))
  ($! "&#8288;")
  ($ "—")
  ($! "&#8288;"))

(macro ulist ()
  (%! "<ul>")
  (list-body)
  (%! "</ul>"))

(macro olist ()
  (%! "<ol>")
  (list-body)
  (%! "</ol>"))

(macro clist ()
  (%! "<div class=\"customlist\">")
  (loop
     for number = (read-macro-argument)
     while (string/= number ":end")
     do (%! "<div><span class=\"laligned\">~a&nbsp;</span>~a</div>~%"
            number
            (read-macro-argument)))
  (%! "</div>"))

(macro emptypara ()
  ($! "&nbsp;")
  ($ " " :fake))

(macro sup (text)
  ($! "<sup class=\"superscript\">")
  ($ text)
  ($! "</sup>"))

(macro comment (word)
  (declare (ignore word)))

(macro sc (text)
  ($! "<span class=\"smallcaps\">")
  ($ text)
  ($! "</span>"))

(macro multiverse (book chapter range translation)
  ($! "<div class=\"scripture-block-quote\">")
  (loop
     for verse-or-end = (read-macro-argument)
     while (string/= verse-or-end ":end")
     do
       ($! "<div>")
       (%! "<sup class=\"laligned superscript\">~a</sup>" verse-or-end)
       ($! (read-macro-argument))
       ($! "</div>"))
  (%! "<div class=\"quote-attrib\">")
  (%! "<a target=\"_blank\" href=\"~a\">~a ~a:~a</a>, ~a"
      (make-bverse-url book chapter range translation)
      book chapter range translation)
  ($! "</div></div>"))

(defun make-bverse-url (book chapter verse translation)
  (if (string= "kjva" (string-downcase translation))
      (format nil "https://studybible.info/AKJV/~a ~a:~a" book chapter verse)
      (format nil "https://www.biblegateway.com/passage/?search=~a+~a%3A~a&version=~a"
              book chapter verse translation)))

(defparameter *bref-no-abbreviation* nil)

(macro bref (book chapter verse translation)
  (%! "<a target=\"_blank\" href=\"~a\">"
      (make-bverse-url book chapter verse translation))
  (if (and (not *bref-no-abbreviation*) *last-bref-book* (string= *last-bref-book* book))
      (% "~a:~a" chapter verse)
      (% "~a ~a:~a" book chapter verse))
  (setf *last-bref-book* book)
  ($! "</a>"))

(macro lbref (book chapter verse translation)
  (let ((*bref-no-abbreviation* t))
    ($bref book chapter verse translation)))

(macro bverse (book chapter verse translation text)
  ($! "<div class=\"scripture-block-quote\">")
  (%! "<div>~a</div>" text)
  ;; Why is this div not showing up in firefox reader view?
  (%! "<div class=\"quote-attrib\"><a target=\"_blank\" href=\"~a\">"
      (make-bverse-url book chapter verse translation))
  (%! "~a ~a:~a</a>, ~a"
     book
     chapter
     verse
     translation)
  ($! "</div></div>"))

(macro poemsong ()
  ($! "<div class=\"poemsong\">")
  (loop
     for x = (read-macro-argument)
     while (string/= x ":end")
     do (%! "<div class=\"poemsong-line\">~a</div>~%"
            (if (= (length x) 0)
                "&nbsp;"
                x)))
  ($! "</div>"))

(macro header (num text)
  (%! "<h~a>" num)
  ($ text)
  (%! "</h~a>" num))

(macro b (word)
  ($! "<b>")
  ($ word)
  ($! "</b>"))

(macro i (word)
  ($! "<i>")
  ($ word)
  ($! "</i>"))

(macro link (text)
  (%! "<a target=\"_blank\" href=\"~a\">" text)
  ($ text)
  ($! "</a>"))

(macro hlink (text link)
  (%! "<a target=\"_blank\" href=\"~a\">" link)
  ($ text)
  ($! "</a>"))

(defv *footnotes* nil)

(macro footnote (text)
  (push text *footnotes*)
  (% "<a id=\"reference-~a\" href=\"#footnote-~a\">[~a]</a>"
     (length *footnotes*)
     (length *footnotes*)
     (length *footnotes*)))

(defun print-footnotes ()
  (when *footnotes*
    (%! "<div id=\"footnotes-wrapper\">~%")
    (%! "<h3 class=\"footnote-header\">Footnotes</h3>~%")
    (%! "<div id=\"footnotes\">")
    (loop for note in (reverse *footnotes*) for i from 1 to (length *footnotes*)
       do
         (%! "<div id=\"footnote-~a\" class=\"footnote\">~%" i)
         (%! "<span class=\"footnote-num\"><a href=\"#reference-~a\">~a</a>.&nbsp;</span><span class=\"footnote-text\">~a</span>~%"
             i
             i
             note)
         ($! "</div>"))
    ($! "</div></div>")))

(add-finish-hook #'print-footnotes)

(macro href (text name)
  (let ((lips-path (lips-file-by-name name)))
    (when (not lips-path)
      (error "Cannot find lips file for link ~a" name))
    (%! "<a href=\"/~a.html\">"
        (subseq (namestring lips-path) 0 (- (length (namestring lips-path)) 5)))
    ($ text)
    ($! "</a>")))

(macro article (type title)
  ($page type title)
  (%!
"<div id=\"homediv\">
<a href=\"/\">⌂</a>
</div>
<h1 class=\"article-title\">~a</h1>
"
          title))

(defun sanitize-link (text)
  (remove-if-not (lambda (c)
                   (or
                    (lower-case-p c)
                    (upper-case-p c)
                    (digit-char-p c)
                    (char= c #\-)
                    (char= c #\_)))
                 (substitute #\- #\Space (string-downcase text))))

(macro anchor (text)
  (let ((sanitized (sanitize-link text)))
    (%! "<span id=\"~a\"><a href=\"#~a\">" sanitized sanitized)
    ($ text)
    ($ "</a><span>")))

(macro img (href alt-text)
  (%! "<img src=\"/img/~a\" alt=\"~a\"></img>" href alt-text))
