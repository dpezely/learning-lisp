;;; -*- Lisp -*-

;; Walk through the thought process of using the macro facility
;; within ANSI Common Lisp.

;; This is a step-by-step guide to various iterations (including
;; mistakes) along the way to creating powerful macros with
;; compile-time considerations that save run-time cycles.

;; But first, just get something working; optimizations come later.

;; Your mistakes might be different than mine.  The point is that
;; while you will make programming errors, some lead to inefficiencies
;; and others detract from correctness.  Here, we resolve correctness
;; first, push things to be handled at compile-time second and
;; efficiencies last.

;; The choice of project is HTML code generation.  Since mid-1990's,
;; many programmers already know HTML/XHTML (http://w3.org/TR/xhtml),
;; so you can hopefully focus on Lisp.

;; We create macros for a familiar HTML tag, but we want XHTML
;; compliance rather than early 1990's trivial use of HTML.
;; That is, we accommodate optional attributes for style sheets, etc.
;; (For brevity, however, we omit some attributes like "tabindex".)

;; For example, generate: <a href="link" class="foo">label</a>
;; from (a (:href "link" :class "foo") "label")

;; Note, however, that your code might use hard-coded strings or other
;; compile-time constants.  Lisp encourages you to make the compiler
;; resolve this, so the run-time never deals with this particular
;; substitution: (FORMAT nil "class=\"~A\"" "foo").

;; This will get you well on your way to creating your own
;; domain-specific language on top of Lisp.


;; License: Copyright (C) 2006, 2008 Daniel Joseph Pezely
;; Available under a Creative Commons License.
;; http://creativecommons.org/licenses/by/2.0/

;; Document history:
;; Fixed typos, compiles on MacOSX (x86, 10.5.5) under SBCL (1.0.20 & .23, December 2008)
;; Compiles on MacOSX (PPC, 10.4.7) under SBCL (0.9.14, August 2006)
;; Originally compiled on MacOSX (PPC, 10.4) under SBCL (0.9.11, June 2006)


(defpackage :html  (:use :cl))
(in-package :html)

(declaim (optimize (safety 3) (debug 3) (speed 0) (space 0)))


;; First attempt at creating a macro for HTML's <A> tag.

(defmacro anchor1 (value &key href id name style target title)
  `(concatenate 'string "<a "
		,(mapcan #'(lambda (x)
			     (if (second x)
				 `(format nil "~A=\"~A\""
					  ,(first x)
					  ,(second x))))
			 `(("href" ,href) ("id" ,id) ("name" ,name)
			   ("style" ,style) ("target" ,target) ("title" ,title)))
		(format nil ">~A</a>" ,value)))

;; Testing:
;; (macroexpand-1 '(anchor1 "label" :href "link"))
;; (anchor1 "label" :href "link")

;; Well... it works...  (ugly but gives the right final answer)

;; There are too many calls to FORMAT at run-time.

;; We can do better!



;; Resolve attribute names at compile-time because these are
;; (usually) constant strings since they come from keywords of our
;; parameter list.  Let's see if we might leverage the fact our
;; parameter names happen to be HTML attribute names as well...
;; (Hint: this version won't work and expect STYLE-WARNINGS.)

#+(or)
(defmacro anchor2-BROKEN (value &key href id name style target title)
  `(concatenate 'string "<a "
		,@(mapcar #'(lambda (x)
			      (if (symbol-value x)
				  (typecase x
				    (string
				     (format nil "~A=\"~A\"" (symbol-name x) x))
				    (otherwise
				     `(format nil "~A=\"~A\"" ,(symbol-name x) ,x)))))
			  (list 'href 'id 'name 'target 'title)) ;WRONG!
		(format nil ">~A</a>" ,value)))

;; Nice try, but the approach of (list 'href ...) isn't correct for Lisp.


;; Grr.... Do it the hard/ugly way-- just make it work for now, then
;; we'll clean it up in the next pass.
    
(defmacro anchor3 (content &key class href id name style target title)
  (let ((args (list (list 'class class)	; 'args is ugly, but just make it work!
		    (list 'href href)
		    (list 'id id)
		    (list 'name name)
		    (list 'style style)
		    (list 'target target)
		    (list 'title title))))
    `(format nil
	     ,(identity	;IDENTITY here helps maintain backquote/comma sanity
	       `(concatenate		; hint: that's a clue!
		 'string
		 "<a"
		 ,@(remove-if #'null ;REMOVE-IF needed due to MAPCAR and IF combo.
			      (mapcar #'(lambda (x)
					  (if (second x)
					      (concatenate 'string " "
							   (string-downcase
							    (symbol-name (first x)))
							   "=\"~A\"")))
				      args))
		 ">~A</a>"))
	     ,@(remove-if #'null
			  (mapcar #'(lambda (x) (if (second x)
						    (second x)))
				  args))
	     ,content)))

;; Testing:
;; (macroexpand-1 '(anchor3 "label" :href "link" :class "foo"))
;; (anchor3 "label" :href "link" :class "foo")



;; Now break-out what would be common with other HTML/XML tags.  This
;; gives us a general way to access the xml attributes, thereby doing
;; away with ugly definition of 'args of the previous example.
;; But fear not!  We'll account for Emacs/SLIME's parameter hints (see below).

(defmacro xml-element1 (tag (&rest key-value-pairs) &body content)
  ;; Note lack of WITH-GENSYMS because no variables are named within the expansion.
  (let ((tag-name (string-downcase (symbol-name tag)))
	;; this idiom of using #'cddr came from looking at someone else's code:
	(args (loop for (k v) on key-value-pairs by #'cddr
		 collect (list (string-downcase (symbol-name k)) v))))
    `(format nil
	     ,(identity	;again, IDENTITY used only for backquote/comma sanity below
	       `(concatenate
		 'string
		 "<" ,tag-name
		 ,@(remove-if #'null
			      (mapcar #'(lambda (x)
					  (if (second x)
					      (concatenate 'string " " (first x) "=\"~A\"")))
				      args))
		 ">~A"
		 "</" ,tag-name ">"))
	     ,@(remove-if #'null
			  (mapcar #'(lambda (x) (if (second x)
						    (second x)))
				  args))
	     ,@content)))

;; (macroexpand-1 '(xml-element1 :a (:href "link" :class "foo") "label"))
;; (xml-element1 :a (:href "link" :class "foo") "label")

;; Although we use 'args from &rest, we also specify &key to help the
;; two programmers who are still unfamiliar with HTML.
(defmacro anchor4 ((&rest args &key class href id name style target title) &body content)
  `(xml-element1 :a (,@args) ,@content))
;; Oops, "style warning" from compiler... 

;; Add IGNORE:
(defmacro anchor5 ((&rest args &key class href id name style target title) &body content)
  (declare (ignore class href id name style target title))
  `(xml-element1 :a (,@args) ,@content))

;; (macroexpand-1 '(anchor5 (:href "link" :class "foo") "label"))
;; (macroexpand-1 (macroexpand-1 '(anchor5 (:href "link" :class "foo") "label")))
;; (anchor5 (:href "link" :class "foo") "label")
;; invalid HTML, valid Lisp: (anchor5 () "label")
;; should fail with "link" unknown: (anchor5 ("link" "foo") "label")



;; We're on a roll!

;; Hmm... Seems easy enough to generate various HTML tags from a spec...
;; Yes, a spec such as the DTD for XHTML-Strict... Oh, the possibilities!



;; Note: no GENSYMs because no vars exposed in expansion

(defmacro xhtml-generate-BROKEN (tag-name (&rest attributes) &key (end-tag t))
  (let* ((tag tag-name)
	 (def `(defmacro ,tag))
	 (all-args `(&rest args &key ,@attributes))
	 (call ``(xml-element1 ,,tag (,args)))) ;not quite!
    (if end-tag
	`(,@def ((,@all-args) &body content)
	     (,@call content))
	`(,@def (,@all-args)
	     (,@call)))))

;; (macroexpand-1 '(xhtml-generate-BROKEN a (class href id name style target title)))
;; (macroexpand-1 '(xhtml-generate-BROKEN img (alt class src id name style) :end-tag nil))

;; warns: (xhtml-generate-BROKEN a (class href id name style target title))
;; fails: (macroexpand-1 '(a (:href "link" :class "foo") "label"))
;; fails: (macroexpand '(a (:href "link" :class "foo") "label"))
;; fails: (a (:href "link" :class "foo") "label")


;; Despite the error, we won't fix it now.  Let's finish the rest.

;; Yes, this was meant to be a teaser, so you don't get bored in the middle.

;; XHTML-GENERATE-BROKEN also shows, however, an inelegant use of
;; LET*, which many new to Lisp might use.

;; Because there are dependencies among local variables, it's
;; cleaner to use nested LETs instead.  This is done on further
;; examples below.

;; The Lisp idiom is to use nested LETs (rather than LET*) which
;; highlights dependencies, making code easier to read.  One LET would
;; capture evaluation of the macro's parameters and an immediate inner
;; LET would then make use of those.





;; Note: There is a subtle problem with the previous and next few
;; iterations of XML-ELEMENT.  In our rush to do yet more experiments
;; and premature optimization, we omitted additional test cases that
;; would have caught it.  Those familiar with HTML or XML may have
;; noticed.

;; The code above wrongfully assumes existence of a closing tag.
;; This was actually caught later with test cases but mentioned here
;; for those who found the bug.  Because this type of programming
;; error is common, it's preserved as part of the exercise/experience.








;; OPTIMIZATION:

;; If passed a constant string at compile-time, handle the
;; substitution immediately.  (Less to do at run-time!)

(defmacro xml-element-optimization1 (tag (&rest key-value-pairs) &body content)
  (let ((tag-name (string-downcase (symbol-name tag)))
	(args (loop for (k v) on key-value-pairs by #'cddr
		 collect (list (string-downcase (symbol-name k)) v))))
    `(format nil
	     ,(identity		   ;this isn't an idiom; it's a kludge
	       `(concatenate
		 'string
		 "<" ,tag-name
		 ,@(remove-if #'null ;see note about this #'remove-if further below
			      (mapcar #'(lambda (x)
 					  (if (second x)
					      (typecase (second x)
						(string
						 (concatenate 'string " "
							      (first x) "=\"" (second x) "\""))
						(otherwise
						 (concatenate 'string " "
							      (first x) "=\"~A\"")))))
				      args))
		 ">~A"
		 "</" ,tag-name ">")) ;wrongfully assumes content and closing tag
	     ,@(remove-if #'null
			  (mapcar #'(lambda (x)
				      (if (and (second x)
					       (not (subtypep (type-of (second x)) 'string)))
					  (second x)))
				  args))
	     ,@content)))

;; That's getting rather long, but we'll trim later.
;; (macroexpand-1 '(xml-element-optimization1 :a (:href "link" :class "foo") "label"))
;; (xml-element-optimization1 :a (:href "link" :class "foo") "label")


(defmacro anchor6 ((&rest args &key class href id name style target title) &body content)
  (declare (ignore class href id name style target title))
  `(xml-element-optimization1 :a (,@args) ,@content))

;; (macroexpand-1 '(anchor6 (:href "link" :class "foo") "label"))
;; (macroexpand-1 (macroexpand-1 '(anchor6 (:href "link" :class "foo") "label")))
;; (anchor6 (:href "link" :class "foo") "label")
;; incorrect HTML but valid Lisp: (anchor6 () "label")
;; should fail: (anchor6 ("link" "foo") "label")





;; OPTIMIZATION:

;; Also handle case of constant CDATA ("label") at compile-time.

(defmacro xml-element-WRONG (tag (&rest key-value-pairs) &body content)
  (let ((tag-name (string-downcase (symbol-name tag)))
	(args (loop for (k v) on key-value-pairs by #'cddr
		 collect (list (string-downcase (symbol-name k)) v)))
	(cdata content))
    `(format nil
	     ,(identity		    ;when you see something like this 
	       `(concatenate	   ;it's time to break-out a new macro
		 'string
		 "<" ,tag-name
		 ,@(remove-if #'null
			      (mapcar #'(lambda (pair)
					  (let ((a (first pair))
						(b (second pair)))
					    (if b
						(typecase b
						  (string
						   (concatenate 'string " " a "=\"" b "\""))
						  (otherwise
						   (concatenate 'string " " a "=\"~A\""))))))
				      args))
		 ;;was:		 ">~A"
		 ">"
		 ,(if (subtypep (type-of cdata) 'string) ;wrong!
		      cdata
		      "~A")
		 "</" ,tag-name ">"))
	     ,@(remove-if #'null
			  (mapcar #'(lambda (pair)
				      (let ((b (second pair)))
					(if (and b
						 (not (subtypep (type-of b) 'string)))
					    b)))
				  args))
	     ;;was:	     ,@content)))
	     ,@(if (not (subtypep (type-of cdata) 'string)) ;still wrong!
		   cdata))))

;; Way too long!
;; (macroexpand-1 '(xml-element-WRONG :a (:href "link" :class "foo") "label"))



;; Corrected: realizing that &body param is a CONS

(defmacro xml-element-KINDA-WORKS (tag (&rest key-value-pairs) &body content)
  "Works... but only when a closing tag is required; not for <IMG ... />"
  (let ((tag-name (string-downcase (symbol-name tag)))
	(args (loop for (k v) on key-value-pairs by #'cddr
		 collect (list (string-downcase (symbol-name k)) v)))
	(cdata-string (if (and (eql (type-of content) 'cons)
			       (= (length content) 1)
			       (subtypep (type-of (first content)) 'string))
			  (first content))))
    `(format nil
	     ,(identity		     ;identity hack is still a problem
	       `(concatenate
		 'string
		 "<" ,tag-name
		 ,@(remove-if #'null
			      (mapcar #'(lambda (pair)
					  (let ((a (first pair))
						(b (second pair)))
					    (if b
						(typecase b
						  (string
						   (concatenate 'string " " a "=\"" b "\""))
						  (otherwise
						   (concatenate 'string " " a "=\"~A\""))))))
				      args))
		 ">" 
		 ,(if cdata-string
		      cdata-string
		      "~A")
		 "</" ,tag-name ">"))
	     ,@(remove-if #'null
			  (mapcar #'(lambda (pair)
				      (let ((b (second pair)))
					(if (and b
						 (not (subtypep (type-of b) 'string)))
					    b)))
				  args))
	     ,@(if (not cdata-string)
		   content))))

;; (macroexpand-1 '(xml-element-KINDA-WORKS :a (:href "link" :class "foo") "label"))
;; (macroexpand-1 '(xml-element-KINDA-WORKS :a (:href "link" :class "foo") (concatenate 'string "label")))
;; (macroexpand-1 '(xml-element-KINDA-WORKS :a (:href "link" :class "foo") (lambda () "label")))
;; (xml-element-KINDA-WORKS :a (:href "link" :class "foo") "label")
;; (xml-element-KINDA-WORKS :a (:href "link" :class "foo") (concatenate 'string "label"))
;; (xml-element-KINDA-WORKS :a (:href "link" :class "foo") (lambda () "label"))


;; It works... mostly!  There's still a problem: for XHTML where a
;; closing tag is forbidden (such as with BASE, BR, HR, IMG and META),
;; but enough already!  The code above is long and bulky...

;; In pre-ANSI Lisp days, terminals had typically 24-25 lines.  While
;; we have more screen real estate now, it's still best to keep a
;; function or macro to fit well within a typical frame.

;; Time to break-out inner functionality as separate functions...

;; These are functions rather than macros because they get called at
;; compile-time.  Since there's nothing gained by making them macros,
;; we'll keep with simplicity of functions.

(defun extract-attribute-pairs (key-value-pairs)
  "Create XML attributes.
If key/value pair are both constant strings, resolve immediately.
If value is anything else (i.e., computed at run-time), create FORMAT pattern.
See also #'extract-nonconst-variables."
  (mapcar #'(lambda (pair)
	      (let ((key (first pair))
		    (value (second pair)))
		(if value
		    (typecase value ;using #'constantp would have been too broad
		      (number (format nil " ~A=\"~A\"" key value))
		      (string (concatenate 'string " " key "=\"" value "\""))
		      (otherwise (concatenate 'string " " key "=\"~A\""))))))
	  key-value-pairs))
;; (extract-attribute-pairs '(("A" 1) ("B" "b") ("C" (identity "c"))))
;; Note lack of nil in results in preceding line.

(defun extract-nonconst-values (key-value-pairs)
  "Create parameter list for FORMAT pattern generated by #'extract-attribute-pairs"
  (mapcar #'(lambda (pair)
	      ;; ignore first element of pair
	      (let ((value (second pair)))
		(if (and value
			 (not (subtypep (type-of value) 'number))
			 (not (subtypep (type-of value) 'string)))
		    value)))
	  key-value-pairs))
;; (extract-nonconst-values '(("A" 1) ("B" "b") ("C" (identity "c"))))
;; Same input as above, yet we preserve nil in results.

(defmacro xml-element-ALMOST (tag (&rest key-value-pairs) &body content)
  "Generate XHTML/XML element with attributes and CDATA.
Parameters containing constant strings receive compile-time
optimization and become part of the static string for run-time."
  (let ((tag-name (string-downcase (symbol-name tag)))
	(args (loop for (k v) on key-value-pairs by #'cddr
		 collect (list (string-downcase (symbol-name k)) v)))
	(cdata content))
    (let ((cdata-string (if (and (eql (type-of content) 'cons)
				 (= (length content) 1)
				 (subtypep (type-of (first content)) 'string))
			    (first content))))
      `(format nil
	       ,(identity	      ;tired of this IDENTITY bug yet?
		 `(concatenate 'string "<" ,tag-name
			       ;;Ah ha!  This #'remove-if does nothing for us:
			       ;;,@(remove-if #'null (extract-const-pairs args))
			       ,@(extract-attribute-pairs args)
			       ">"
			       ;; Still need to fix for forbidden closing tag!
			       ,(if cdata-string
				    cdata-string
				    "~A")
			       "</" ,tag-name ">")) 
	       ,@(remove-if #'null (extract-nonconst-values args))
	       ,@(if (not cdata-string)
		     cdata)))))

;; (macroexpand-1 '(xml-element-ALMOST :a (:href "link" :class "foo") "label"))
;; (macroexpand-1 '(xml-element-ALMOST :a (:href "link" :class "foo") (concatenate 'string "label")))
;; (macroexpand-1 '(xml-element-ALMOST :a (:href "link" :class "foo") (lambda () "label")))
;; (xml-element-ALMOST :a (:href "link" :class "foo") "label")
;; (xml-element-ALMOST :a (:href "link" :class "foo") (concatenate 'string "label"))
;; (xml-element-ALMOST :a (:href "link" :class "foo") (lambda () "label"))

;; Note nested LET to highlight interdependence among local variables.

;; The ,(IDENTITY `(CONCATENATE ...)) nonsense isn't working for us.

;; We still have a quoted CONCATENATE, which gets run at run-time
;; rather than compile time.  What to do?  The architecture is wrong.


(defmacro anchor ((&rest args &key class href id name style target title)
		  &body content)
  "Generate XHTML <A> tag"
  (declare (ignore class href id name style target title))
  `(xml-element-ALMOST :a (,@args) ,@content))

;; (macroexpand-1 '(anchor (:href "link" :class "foo") "label"))
;; (macroexpand-1 (macroexpand-1 '(anchor (:href "link" :class "foo") "label")))
;; (anchor (:href "link" :class "foo") "label")
;; invalid HTML but valid Lisp: (anchor () "label")
;; should fail: (anchor ("link" "foo") "label")







;; While thinking about how to fix the compile-time versus run-time
;; business, there's a feature that should be added in case we want to
;; use this as-is: a parameter for the stream.

;; When deciding the aesthetics of such an argument, consider
;; mimicking an existing Common Lisp function.  Since the semantics
;; of our macro are closest to FORMAT, use that.  (Suggestion: place
;; this arg before all other args for consistency with FORMAT.)

;; Passing nil, we get the same behavior as before.  Passing t, we
;; get standard output.  Passing a stream handle, we push the
;; complexity of allocation/gc down to the OS level, and we suddenly
;; have re-entrant code (useful for long-running processes).

;; (defmacro xml-element (STREAM tag (&rest key-value-pairs) &body content)
;;   ...                  ^^^^^^
;;    `(format ,STREAM
;;       ...   ^^^^^^^

;; Or we could use a dynamic/special (i.e., global) variable.

;; Using semantics consistent with FORMAT and PRINC, let's introduce
;; *stream* via DEFVAR.


(defvar *stream* t "semantics are consistent with FORMAT and PRINC")






;; Next, we need to break-out additional functions for the
;; compile-time concatenation of those strings.

;; We'll continue using the same versions of #'extract-attribute-pairs
;; and #'extract-nonconst-values defined above.

(defun build-attributes-string (attributes-list &optional (collector ""))
  "Converts list from #'extract-attribute-pairs into string for FORMAT"
  (if (null attributes-list)
      collector
      (let ((attribute (pop attributes-list)))
	(build-attributes-string attributes-list
				 (concatenate 'string collector attribute)))))

;; (build-attributes-string '(" A=\"1\"" " B=\"b\""))
;; This is similar to how we'll actually use it:
;; (build-attributes-string (extract-attribute-pairs '(("A" 1) ("B" "b") ("C" (identity "c")))))

(defmacro xml-element (tag (&rest key-value-pairs) &body content)
  "Generate XML element with attributes and CDATA.
Parameters containing constant strings receive compile-time
optimization and become part of the static string for run-time."
  (let ((tag-name (string-downcase (symbol-name tag)))
	(args (loop for (k v) on key-value-pairs by #'cddr
		 collect (list (string-downcase (symbol-name k)) v)))
	(cdata content))
    (let ((cdata-string (if (and cdata
				 (eql (type-of cdata) 'cons)
				 (= (length cdata) 1)
				 (subtypep (type-of (first cdata)) 'string))
			    (first cdata))))
      `(format *stream*
	       ,(concatenate 'string "<" tag-name
			     (build-attributes-string (extract-attribute-pairs args))
			     (if cdata
				 (if cdata-string
				     (concatenate 'string ">" cdata-string "</" tag-name ">")
				     (concatenate 'string ">~A</" tag-name ">"))
				 " />"))
	       ,@(remove-if #'null (extract-nonconst-values args))
	       ,@(if cdata
		     (if (not cdata-string)
			 cdata))))))

;; (macroexpand-1 '(xml-element :a (:href "link" :class "foo") "label"))
;; (macroexpand-1 '(xml-element :img (:src "x.png" :alt "X" :class "foo")))
;; (macroexpand-1 '(xml-element :a (:href "link" :class "foo") (concatenate 'string "label")))
;; (macroexpand-1 '(xml-element :a (:href "link" :class "foo") (identity "label")))
;; (xml-element :a (:href "link" :class "foo") "label")
;; (xml-element :img (:src "x.png" :alt "X" :class "foo"))
;; (xml-element :a (:href "link" :class "foo") (identity "label"))
;; (xml-element :a (:href "link" :class "foo") (concatenate 'string "label"))



(defmacro anchor ((&rest args &key class href id name style target title) &body content)
  (declare (ignore class href id name style target title))
  `(xml-element :a (,@args) ,@content))

;; (macroexpand-1 '(anchor (:href "link" :class "foo") "label"))
;; (macroexpand-1 (macroexpand-1 '(anchor (:href "link" :class "foo") "label")))
;; (anchor (:href "link" :class "foo") "label")
;; invalid HTML, valid Lisp: (anchor () "label")
;; should fail: (anchor ("link" "foo") "label")

#+(or) ;;note: #+(or) makes a block comment
(progn
  (defmacro a ((&rest args &key class href id name style target title) &body content)
    "anchor"
    (declare (ignore class href id name style target title))
    `(xml-element :a (,@args) ,@content))
  
  (defmacro b ((&rest args &key id name) &body content)
    "bold"
    (declare (ignore id name))
    `(xml-element :b (,@args) ,@content))

  (defmacro blockquote ((&rest args &key class id name style) &body content)
    "blockquote"
    (declare (ignore class id name name style))
    `(xml-element :blockquote (,@args) ,@content))

  (defmacro i ((&rest args &key id name) &body content)
    "italics"
    (declare (ignore id name))
    `(xml-element :i (,@args) ,@content))

  (defmacro em ((&rest args &key id name) &body content)
    "emphasis"
    (declare (ignore id name))
    `(xml-element :em (,@args) ,@content))

  (defmacro p ((&rest args &key class id name style) &body content)
    "paragraph"
    (declare (ignore class id name name style))
    `(xml-element :p (,@args) ,@content)))









;; Back to the macro-generation macro...

;; "hmm... seems easy enough to generate various HTML tags from a spec..."
;; apart from double-backquoted-nested-comma madness.
;; When you see this: ,',  think of the 1950-60's "fallout" shelter sign :-)
;; http://bc.tech.coop/blog/041205.html
;; http://google.com/search?q=lisp+macro+backquote+nested+comma

(defmacro xhtml-generate (tag-name (&rest attributes) &key (end-tag t))
  "Generate a macro that in turn creates one XHTML tag.
This might become the inner workings of a DTD-to-lisp code generation facility."
  (let ((tag tag-name)
	(attrs attributes))
    (let ((def `(defmacro ,tag))
	  (all-args `(&rest args &key ,@attrs))
	  (ignore `(declare (ignore ,@attrs)))
	  ;; the tricky bits: `` and , versus ,', and so on...
	  (call-sans-cdata ``(xml-element ,',tag (,@args)))
	  (call-with-cdata ``(xml-element ,',tag (,@args) ,@content)))
      (if end-tag
	  `(,@def (,all-args &body content)
	       ,ignore
	     (,@call-with-cdata))
	  ;;`(,@def ,all-args	;Aesthetic alternative but inconsistent API
	  `(,@def (,all-args)	 ;Extra parens redundant without &body
	       ,ignore
	     (,@call-sans-cdata))))))

;; (macroexpand-1 '(xhtml-generate a (class href id name style target title)))
;; (macroexpand-1 '(xhtml-generate img (alt class id name src style) :end-tag nil))
;; (macroexpand '(xhtml-generate a (class href id name style target title)))

;; (xhtml-generate a (class href id name style target title))
;; (macroexpand-1 '(a (:href "link" :class "foo") "label"))
;; (a (:href "link" :class "foo") "label")

;; (xhtml-generate img (alt class id name src style) :end-tag nil)
;; (macroexpand-1 '(img (:alt "X" :src "x.png" :class "foo")))
;; (img (:alt "X" :src "x.png" :class "foo"))

;;; Alternate example:
;;; Considering the code comment on "Aesthetic alternative" above such as
;;; for tags without &body, try applying the commented `(,@def ,all-args
;;; line [and commenting the line which follows it], then try these:
;; (xhtml-generate img (alt class id name src style) :end-tag nil)
;; (macroexpand-1 '(img :alt "X" :src "x.png" :class "foo"))
;; (img :alt "X" :src "x.png" :class "foo")

#+(or) (macroexpand-1 '(a (:href "link" :class "foo")
			(img (:alt "X" :src "x.png" :class "foo"))))
#+(or) (macroexpand '(a (:href "link" :class "foo")
		      (img (:alt "X" :src "x.png" :class "foo"))))
;; if *stream* was t, this would render out of sequence:
#+(or) (let ((*stream* nil))
	 (a (:href "link" :class "foo")
	   (img (:alt "X" :src "x.png" :class "foo"))))



;; Try some experiments of changing the internal double backquote
;; combinations, and read more on the topic someday.  In particular,
;; Paul Graham's _On Lisp_ is considered a must-read on the subject of
;; advanced macros!


(xhtml-generate a (class href id name style target title))
(xhtml-generate abbr (class id name style))
(xhtml-generate acronym (class id name style))
(xhtml-generate b (class id name style))
(xhtml-generate base (href target))
(xhtml-generate big (class id name style))
(xhtml-generate blockquote (class id name style))
(xhtml-generate br (class id name style) :end-tag nil)
(xhtml-generate cite (class id name style))
(xhtml-generate code (class id name style))
(xhtml-generate dd (class id name style))
(xhtml-generate dfn (class id name style))
(xhtml-generate div (class id name style))
(xhtml-generate dl (class id name style))
(xhtml-generate dt (class id name style))
(xhtml-generate em (class id name style))
(xhtml-generate hr (class id name style) :end-tag nil)
(xhtml-generate i (class id name style))
(xhtml-generate img (alt class height id name src style width title) :end-tag nil)
(xhtml-generate kbd (class id name style))
(xhtml-generate ol (class id name style))
(xhtml-generate ul (class id name style))
(xhtml-generate li (class id name style))
(xhtml-generate object (alt class height id name src style width title))
(xhtml-generate p (class id name style))
(xhtml-generate pre (class id name style))
(xhtml-generate q (class id name style))
(xhtml-generate samp (class id name style))
(xhtml-generate small (class id name style))
(xhtml-generate span (class id name style))
(xhtml-generate strong (class id name style))
(xhtml-generate sub (class id name style))
(xhtml-generate sup (class id name style))
(xhtml-generate tt (class id name style))
(xhtml-generate var (class id name style))

;; Next, consider using #'read and custom symbol macros to parse the
;; official W3.org DTD for XHTML.  One way might be to then capture
;; the Lisp code that gets generated and incorporate that resulting
;; .lisp file into your code repository.






;; Back to our library...

;; The above code assumes that you have a single s-expression for your
;; entire HTML file.  In some cases, that's fine.  In other
;; situations, you want to start shoving bits down the wire as soon as
;; possible.

;; For that immediate mode, you'll need explicit open and close
;; routines.

;; While we're here, let's make the close macro optionally close all
;; currently opened tags.  This extra feature might be an additional
;; iteration for some, but others might be comfortable by now with
;; taking-on these features in one step.

;; This stack of closing tags should be relative so that an inner
;; portion of your program can close all of its opened tags without
;; closing the entire document.

;; Be aware that this is the wrong implementation for the above
;; criteria!

(let (element-stack-to-be-closed)	; not quite!
  
  (defpackage :html-tag-namespace)	; for INTERNed tag names
  
  (defmacro xml-open-element (tag (&rest key-value-pairs))
    "Allow incomplete structure to begin being sent to *stream*.
That is, start sending data down the wire even without a full
sexp to support on-the-fly generation of content.
See also: #'xml-close-element."
    (let ((tag-name (string-downcase (symbol-name tag)))
	  (args (loop for (k v) on key-value-pairs by #'cddr
		   collect (list (string-downcase (symbol-name k)) v))))
      (push (intern tag-name :html-tag-namespace) element-stack-to-be-closed)
      `(format *stream*
	       ,(concatenate 'string "<" tag-name
			     (build-attributes-string (extract-attribute-pairs args))
			     ">")
	       ,@(remove-if #'null (extract-nonconst-values args)))))

  (defmacro xml-close-element (&optional (tag nil tag?))
    "Complements #'xml-open-element.  Omitting tag name closes all."
    (if tag?
	(let ((tag-name (string-downcase (symbol-name tag))))
	  (if (eq (intern tag-name :html-tag-namespace)
		  (first element-stack-to-be-closed))
	      (pop element-stack-to-be-closed)
	      (error "<~A> tag must be closed first" (first element-stack-to-be-closed)))
	  `(princ ,(concatenate 'string "</" tag-name ">")  *stream*))
	(labels ((build-string (stack &optional (collector ""))
		   (if (null stack)
		       collector
		       (let ((tag-name (string-downcase (pop stack))))
			 (build-string stack
				       (concatenate 'string collector "</" tag-name ">"))))))
	  `(princ ,(build-string element-stack-to-be-closed) *stream*))))

  (defun dump-tags-to-be-closed ()
    (dolist (tag element-stack-to-be-closed)
      (format t "to be closed: <~A>~%" tag))))

;; (macroexpand-1 '(xml-open-element :div (:id "tag" :class "foo")))
;; (xml-open-element :div (:id "tag" :class "foo"))
;; (xml-open-element :p (:id "para" :class "moo"))
;; (dump-tags-to-be-closed)
;; (macroexpand-1 '(xml-close-element :p))
;; (macroexpand-1 '(xml-close-element :div))
;; (macroexpand-1 '(xml-close-element))
;; (macroexpand-1 (macroexpand-1 '(xml-close-element)))
;; should complain: (xml-close-element :p)
;; (xml-close-element :div)
;; (xml-close-element)

;; The approach above is a common mistake when those new to Lisp have
;; read more pages of books on the subject than have written lines of
;; Lisp code.

;; It's close-- with those functions defined within a LET-- but not
;; quite what we need.

;; First, review what's there and what works.

;; Because no local closures are used within #'xml-close-element
;; (everything it needs gets passed as a parameter), we don't
;; necessarily have to define it via LABELS.  It could be a
;; stand-alone function, and by the time we get to the pretty-printer,
;; it may just be broken-out.  Keeping it there, however, helps
;; illustrate the thought process.

(defun test-page-LAME ()
  (xml-open-element :html
		    (:xmlns "http://www.w3.org/1999/xhtml" :lang "en"))
  (xml-open-element :body ())
  ;; this closure is lame because its use is hidden and compiler might complain:
  (let (element-stack-to-be-closed)
    (xml-open-element :div (:id "tag" :class "foo"))
    (xml-open-element :p (:id "para" :class "moo"))
    (format *stream* "text")
    (xml-close-element))
  (xml-close-element)
  ;; return nil indicating side-effects:
  nil)

;; (test-page-LAME)

;; Note that the <html> tag is incomplete!
;; it should look like this:
;; <html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
;; So we're missing an attribute called "xml:lang" with value "en".

;; We'll have to address that later!



;; Move the lexical variable to become dynamic/special, and you've fixed it.

(defvar *element-stack-to-be-closed* nil
  "For every tag we open with #'xml-open-element, make sure we close in correct order.")

(defpackage :html-tag-namespace)	; for INTERNed tag names
  
(defmacro xml-open-element (tag (&rest key-value-pairs))
  "Allow incomplete structure to begin being sent to *stream*.
That is, start sending data down the wire even without a full
sexp to support on-the-fly generation of content.
See also: #'xml-close-element."
  (let ((tag-name (string-downcase (symbol-name tag)))
	(args (loop for (k v) on key-value-pairs by #'cddr
		 collect (list (string-downcase (symbol-name k)) v))))
    (push (intern tag-name :html-tag-namespace) *element-stack-to-be-closed*)
    `(format *stream*
	     ,(concatenate 'string "<" tag-name
			   (build-attributes-string (extract-attribute-pairs args))
			   ">")
	     ,@(remove-if #'null (extract-nonconst-values args)))))

(defmacro xml-close-element (&optional (tag nil tag?))
  "Complements #'xml-open-element.  Omitting tag name closes all."
  (if tag?
      (let ((tag-name (string-downcase (symbol-name tag))))
	(if (eq (intern tag-name :html-tag-namespace)
		(first *element-stack-to-be-closed*))
	    (pop *element-stack-to-be-closed*)
	    (error "<~A> tag must be closed first" (first *element-stack-to-be-closed*)))
	`(format *stream* ,(concatenate 'string "</" tag-name ">")))
      (labels ((build-string (&optional (collector ""))
		 (if (null *element-stack-to-be-closed*)
		     collector
		     (let ((tag-name (string-downcase (pop *element-stack-to-be-closed*))))
		       (build-string (concatenate 'string collector "</" tag-name ">"))))))
	`(format *stream* ,(build-string)))))

(defun dump-tags-to-be-closed ()
  (dolist (tag *element-stack-to-be-closed*)
    (format t "to be closed: <~A>~%" tag)))

;; (macroexpand-1 '(xml-open-element :div (:id "tag" :class "foo")))
;; (xml-open-element :div (:id "tag" :class "foo"))
;; (xml-open-element :p (:id "para" :class "moo"))
;; (dump-tags-to-be-closed)
;; (macroexpand-1 '(xml-close-element :p))
;; (macroexpand-1 '(xml-close-element :div))
;; (macroexpand-1 '(xml-close-element))
;; (macroexpand-1 (macroexpand-1 '(xml-close-element)))
;; (dump-tags-to-be-closed)
;; should complain: (xml-close-element :p)
;; should complain: (xml-close-element :div)
;; (xml-close-element)
;; (dump-tags-to-be-closed)

(defun test-page ()
  (xml-open-element :html (:xmlns "http://www.w3.org/1999/xhtml" :lang "en"))
  (xml-open-element :body ())
  (let (*element-stack-to-be-closed*)
    (xml-open-element :div (:id "tag" :class "foo"))
    (xml-open-element :p (:id "para" :class "moo"))
    (format *stream* "text")
    (xml-close-element))
  (xml-close-element)
  ;; return nil indicating side-effects:
  nil)

;; (test-page)
;; (dump-tags-to-be-closed)

;; Experiment: comment-out one of the two #'xml-close-element calls
;; sequentially, and see what fails each time.

					;(xml-close-element :p)
					;(xml-close-element :div)
					;(xml-close-element :body)
					;(xml-close-element :html)


;; Note that #'test-page specifically DOESN'T create the following
;; structures.  (Otherwise, the entire s-expression would have to be
;; evaludated before sending a single byte to *stream*, which some
;; HTML packages actually require.)

#+(or) '(html
	 (head (title "..."))
	 (body
	  (div (attrs)
	   (p "...")
	   (p "..."))
	  (div (attrs)
	   (ul
	    (li "...")
	    (li "..."))
	   (p "..."))))

#+(or) '(html
	 (head (title "..."))
	 (body
	  (loop for x in list
	     collect x)))











;; Next, this is what Peter Seibel in PCL refers to as an HTML compiler...

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Take apart an s-expression describing the structure and content of
;; an HTML page, and convert the constant portions to continuous PRINC
;; statements.  That is, despite different levels of the structure,
;; reconcile the static text as one continuous block of characters to
;; work much like server-side includes dating back to the earliest
;; http servers of the early 1990's.  However, do this at
;; compile-time, so the programmer gets the benefits of thinking in
;; HTML while the run-time gains efficiencies of static text.

;; The trick, however, is using lazy evaluation and therefore a macro.

;; We don't want to evaluate the entire s-expression before converting
;; to a string, because that introduces unnecessary lag and consumes
;; an excess of run-time memory.  For long-running, multi-dispatched
;; process such as with FastCGI or mod_lisp, efficiency becomes
;; important.  This is a case of early optimization (but not premature
;; optimization).


;; Conceptual Design:
;; - outter macro captures params without eval then feeds to inner macro as quoted
;; - inner macro then takes apart quoted param list

;; Sometimes, you might find this idiom: macro calls function:
;;(defun inner-function (body)
;;  (dolist (line body)
;;     (pprint line)))
;;
;;(defmacro outer-macro (&body body)
;;  (inner-function `(,@body)))

(defmacro no-execute (&body body)
  "Demonstrating the Lisp idiom of PUSH with NREVERSE"
  (let (compiled)
    (dolist (line body)
      ;; do something here
      (push line compiled))
    (pprint (nreverse compiled))))

#+(or)
(macroexpand-1
 (no-execute
   (xml-open-element :html (:xmlns "http://www.w3.org/1999/xhtml" :lang "en"))
   (xml-open-element :body ())
   (let (*element-stack-to-be-closed*)
     (no-execute
       (xml-open-element :div (:id "tag" :class "foo"))
       (xml-open-element :p (:id "para" :class "moo"))
       (format *stream* "text")
       (xml-close-element)))
   (xml-close-element)))


;; The important thing to note is that at no time did either of these
;; two attempt to evaluate the body as code!  This frees us to now
;; take apart the body s-expression for compile-time behavior that
;; optimizes the run-time.

;; Next, of the FIRST elements of each nested s-expression, we need to
;; determine which are macros or functions defined by our HTML package
;; versus defined elsewhere.  That lets us then decide whether to
;; apply immediate versus lazy evaluation.


(defun compile-time-constant? (s-exp)
  (let ((whole s-exp))
    (typecase whole
      (null t)
      (number t)
      (string t)
      (keyword t)
      (list (dolist (part whole t)
	      (unless (compile-time-constant? part) (return nil)))))))

;; (compile-time-constant? nil)
;; (compile-time-constant? :foo)
;; (compile-time-constant? '(1 2 3))
;; (compile-time-constant? "xyz")
;; (compile-time-constant? '(:p "text"))
;; (compile-time-constant? '(:p (:class "foo") "text"))
;; (not (compile-time-constant? '(loop for x in (list 1 2 3) collect x)))

;; While #'compile-time-constant? isn't tail-recursive, it'll only be
;; executed at compile-time, so we can afford to be less concerned in
;; favor of readablity.

(defmacro render (&body body)
  (let ((source body)
	compiled)
    (dolist (line source)
      (if (stringp line)
	  (push `(print ,line *stream*) compiled)
	  (let ((command (first line)))
	    (if (and (fboundp command)
		     (eq (symbol-package command) *package*))

		(destructuring-bind (&optional attribute-list content) (cddr line)
		  (if (and (compile-time-constant? attribute-list)
			   (compile-time-constant? content))
		      (let ((*stream* nil))
			(push `(pprint ,line *stream*) compiled))
		      (push line compiled)))
		(push line compiled)))))
    `(progn ,@(nreverse compiled))))

#+(or)
(macroexpand-1
 '(render
   (xml-open-element html (:xmlns "http://www.w3.org/1999/xhtml" :lang "en"))
   (xml-open-element body ())
   (let (*element-stack-to-be-closed*)
     (render
       (xml-open-element div (:id "tag" :class "foo"))
       (loop for x in '(1 2 3 4 5)
	  collect (print (xml-element li () x) *stream*))
       (xml-open-element p (:id "para" :class "moo"))
       "text"
       (xml-close-element)))
   (xml-close-element)))

;; Use of PRINT within the LOOP isn't ideal, but overall, it works!








;; AFTERWORD:

;; Why write your own HTML library when so many exist?  This is an
;; exercise since you probably already know HTML and can immediately
;; validate results visually.  That lets you focus on learning Lisp.

;; Going all the way, we'd want further optimization such that FORMAT
;; only gets used when absolutely necessary (i.e., when an actual
;; substitution is performed); otherwise, use PRINC.  

;; It's been done already.  Browse http://CLiki.net/ projects or read
;; Peter Seibel's book, _Practical Common Lisp_.

;; Sometimes, however, it's useful as an exercise to do it yourself.
;; It's important to face that learning curve rather than merely using
;; someone else's library all the time.  That might just make the
;; difference between a coder and a hacker.
