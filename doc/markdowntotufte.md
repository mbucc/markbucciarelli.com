October 28, 2022

How to Convert Markdown to Tufte CSS
======================================

How to keep both the simplicity of blogging in Markdown and
the beauty of the
[Tufte CSS](https://edwardtufte.github.io/tufte-css/)
styling?

Specifically, I want to use these features Tufte CSS:

  * margin notes
  * sections (vertical spacing)
  * quotations
  * small cap "new thoughts" to introduce each section
  * figure handling
  * code

The strategy is to either embed the HTML in the markdown
and use the `--unsafe` cmark option or to use Awk to preprocess
the source markdown before sending it through `cmark`.


    margin notes     awk.
                         -- mn: note-id
                         <a href="http://www.edwardtufte.com/tufte/books_be">
                           <em>Beautiful Evidence</em>
                         </a>
                         -- mn

    sections         awk.
                     wrap each markdown h2 in a section tag.
                     use ### for headings within a section
                     wrap entire document in <article>.

    quotations       HTML in markdown.
                     if you want a citation, use:
                         > line1 of quotation
                         > line2 of quotation
                         >
                         > <footer>citation goes here</footer>

    figures          awk.
                     Use awk to wrap img in <figure> tag.
                     For a `class=fullwidth` Tufte CSS image, use
                     the processing instruction `<?fullwidth?>` in
		     the markdown before the image.

    code             no change needed.
                     cmark wraps code in <pre><code>,
                     which is what Tufte CSS expects.

    new thoughts     HTML in markdown.
                     <span class=newthought>In the beginning,</span>


Steps
-------------------------------------

For each `*.markdown` file, mkws:

1. extracts title, date and tags

2. passes the filename, title, date, and tags to `pp share/tufte.upphtml`
template


The tufte.upphtml template:

1. templates html head section, including twitter card info.

2. templates article, h1, date (p class=subtitle)

3. processes markdown with `cmark` and `awk` scripts.  (Note that
`cmark` will not transform markdown formatting that is inside an
html tag; for example, `<figure>![](t.png)</figure>` will not be
changed into an image tag.)

4. templates section with tags

5. close article
