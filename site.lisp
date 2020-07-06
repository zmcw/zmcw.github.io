
(setf lips:*use-smart-quotes* t)
(setf lips:*paragraph-begin* "<p>")
(setf lips:*paragraph-end* "</p>")

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

(macro emptypara ()
  ($! "&nbsp;")
  ($ " " :fake))

(macro sup (text)
  ($! "<sup class=\"superscript\">")
  ($ text)
  ($! "</sup>"))

(macro comment (word)
  (declare (ignore word)))

(macro multiverse (book range translation)
  ($! "<div class=\"scripture-block-quote\">")
  (loop
     for verse-or-end = (read-macro-argument)
     while (string/= verse-or-end ":end")
     do
       ($! "<div>")
       (%! "<sup class=\"aligned-superscript\">~a</sup>" verse-or-end)
       ($! (read-macro-argument))
       ($! "</div>"))
  (%! "<div class=\"quote-attrib\">")
  (%! "~a ~a, ~a" book range translation)
  ($! "</div></div>"))

(macro bverse (book chapter verse translation text)
  ($! "<div class=\"scripture-block-quote\">")
  (%! "<div>~a</div>" text)
  ;; Why is this div not showing up in firefox reader view?
  (%! "<div class=\"quote-attrib\"><a target=\"_blank\" href=\"https://biblehub.com/~a/~a-~a.htm\">"
      (substitute #\_ #\Space (string-downcase book))
      chapter
      verse)
  (%! "~a ~a:~a</a>, ~a"
     book
     chapter
     verse
     translation)
  ($! "</div></div>"))

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
