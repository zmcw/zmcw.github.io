
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
<div id=\"container\">
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

(macro sup (text)
  ($! "<span class=\"superscript\">")
  ($ text)
  ($! "</span>"))

(macro comment (word)
  (declare (ignore word)))

(macro multiverse (book range translation)
  ($! "<div class=\"scripture-block-quote\">")
  (loop
     for verse-or-end = (read-macro-argument)
     while (string/= verse-or-end ":end")
     do
       ($! "<div>")
       (%! "<span class=\"aligned-superscript\">~a</span>" verse-or-end)
       ($! (read-macro-argument))
       ($! "</div>"))
  (%! "<div class=\"quote-attrib\">")
  (%! "~a ~a, ~a" book range translation)
  ($! "</div></div>"))

(macro bverse (book chapter verse translation text)
  ($! "<div class=\"scripture-block-quote\">")
  ($! text)
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

(defun html-footer ()
  (format nil "~%</div></body></html>"))

(add-finish-hook #'html-footer)
