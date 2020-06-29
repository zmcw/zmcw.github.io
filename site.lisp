
(defparameter *lips-files* nil)

(setf lips:*use-smart-quotes* t)

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

(define-symbol-macro article (start-article (read)))
(define-symbol-macro h1 (header-1 (read)))
(define-symbol-macro page (start-page (read)))
(define-symbol-macro href (href (read) (read)))
(define-symbol-macro ul (ul))
(define-symbol-macro /ul (/ul))
(define-symbol-macro b (b (read)))

(defun b (word)
  (format nil "<b>~a</b>" word))

(defun ul ()
  (lips:reset-paragraph)
  (let ((lips:*paragraph-begin* "<li>")
        (lips:*paragraph-end* "</li>")
        (lips:*in-paragraph* nil))
    (format t "<ul>")
    (let ((result (lips:process-stream *standard-input*)))
      (when (not (eq result 'ul))
        (error "Unmatched UL tag, got ~a" result)))))

(defun /ul ()
  (values "</ul>" 'ul))

(defun header-1 (text)
  (format nil "<h1>~a</h1>" text))

(defun href (text name)
  (let ((lips-path (lips-file-by-name name)))
    (when (not lips-path)
      (error "Cannot find lips file for link ~a" name))
    (format nil "<a href=\"~a.html\">~a</a>"
            (subseq (namestring lips-path) 0 (- (length (namestring lips-path)) 5))
            text)))

(defun start-page (page-title)
  (setf lips:*paragraph-begin* "<p>")
  (setf lips:*paragraph-end* "</p>")
  (lips:reset-paragraph)
  (format nil
          "<!DOCTYPE html>
<html>
<head>
<meta charset=\"UTF-8\">
<link rel=\"stylesheet\" href=\"site.css\">
<title>~a</title>
</head>
<body>
<div id=\"container\">
"
          page-title))

(defun start-article (page-title)
  (lips:reset-paragraph)
  (format nil
          "~a
<div id=\"homediv\">
<a href=\"/\">⌂</a>
</div>
<h1>~a</h1>
"
          (start-page page-title)
          page-title))

(defun html-footer ()
  (format nil "~%</div></body></html>"))

(add-finish-hook #'html-footer)
