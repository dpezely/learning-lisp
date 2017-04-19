Learning Lisp (2006)
====================

Follow one programmer while creating a new Lisp package.

Observe each iteration of the code as it evolves.

Experience mistakes, corrections, optimizations and all...

The most challenging aspect to learning the Lisp programming language is
gaining the right perspective.  Cliché phrases about "forgetting what you
know" and "getting out of your own way" of course apply, but this is a
practical approach to adopting the Lisp mindset.

It's one attempt to help you get from A to B, from an academic set of
principles to get beyond that tipping point of being comfortable with the
language enough for everyday use.

This assumes that you're already familiar with most of the introduction to
Lisp materials and have a a working Lisp system.

Walk through the thought process of using the macro facility
within ANSI Common Lisp.

This is a step-by-step guide to various iterations (including
mistakes) along the way to creating powerful macros with
compile-time considerations that save run-time cycles.

But first, just get something working; optimizations come later.

Your mistakes might be different than mine.  The point is that
while you will make programming errors, some lead to inefficiencies
and others detract from correctness.  Here, we resolve correctness
first, push things to be handled at compile-time second and
efficiencies last.

## Learning Lisp Macros by generating HTML

The choice of project is HTML code generation.  Many programmers already
know HTML/XHTML, so you can hopefully focus on Lisp.

We create macros for a familiar HTML tag, but we want XHTML.

That is, we accommodate optional attributes for style sheets, etc.
(For brevity, however, we omit some attributes like `tabindex`.)

For example, generate:

	<a href="link" class="foo">label</a>

from

	(a (:href "link" :class "foo") "label")

Note, however, that your code might use hard-coded strings or other
compile-time constants.  Lisp encourages you to make the compiler
resolve this, so the run-time never deals with this particular
substitution:

	(FORMAT nil "class=\"~A\"" "foo")

but instead gets substituted at compile time to:

	(FORMAT nil "class=\"foo\"").

This will get you well on your way to creating your own
domain-specific language on top of Lisp.


There are two versions of the same content:

1. Lisp source-- best viewed with Emacs-- [html.lisp](./html.lisp)
2. an HTML file with syntax highlighting baked-in [html.lisp.html](./html.lisp.html)


It's recommended that you view the source within Emacs and SLIME so you can
evaluate the Lisp forms every step of the way.  That's required because
displaying results are left as an exercise for you, the reader.


## But Why?

You might be asking, "Why write your own HTML library when so many exist?"

This is an exercise since you probably already know HTML and can
immediately validate results visually.  That lets you focus on learning
Lisp.

Going all the way, we'd want further optimization such that `FORMAT`
only gets used when absolutely necessary (i.e., when an actual
substitution is performed); otherwise, use `PRINC`.

Yes, it's been done already. Browse [Quicklisp](https://www.quicklisp.org/)
projects, and read Peter Seibel's 2005 book,
[Practical Common Lisp](http://gigamonkeys.com/book) as well as
Edi Weitz's 2016 [Common Lisp Recipes](http://weitz.de/cl-recipes/).

Sometimes, however, it's useful as an exercise to do it yourself.
It's important to face that learning curve rather than merely using
someone else's library all the time.  That might just make the
difference between a coder and a hacker.

## License

Available under a Creative Commons Attribution License as well as "The
Unlicense", so basically do as you wish.
