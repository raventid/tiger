# Building the companion as a book

The companion chapters live as Markdown files in `companion/`. This directory
collects them into a single printable PDF.

## Quick start

```sh
./companion/book/build.sh
# -> companion/book/companion.pdf
```

The script concatenates the chapters in reading order and runs them through
[pandoc](https://pandoc.org) using `metadata.yaml` for the book title, table of
contents, and code-block syntax highlighting.

## Markdown-to-PDF tools that handle code blocks well

- **pandoc + LaTeX** — what `build.sh` uses. The most flexible option: real book
  layout, automatic table of contents, numbered sections, and syntax
  highlighting for fenced code blocks (` ```ocaml `, ` ```sh `, ...). Pair it
  with [`tectonic`](https://tectonic-typesetting.github.io) for a zero-config
  LaTeX engine, or TeX Live / MacTeX for `xelatex`.
- **Quarto** (<https://quarto.org>) — a friendly layer on top of pandoc with
  nice book/website projects and good defaults; great if you want HTML and PDF
  from the same source.
- **mdBook** (<https://rust-lang.github.io/mdBook/>) — produces a searchable
  HTML book with excellent code rendering; print to PDF from the browser.
- **md-to-pdf** (<https://github.com/simonhaenisch/md-to-pdf>) — a small
  Node/Puppeteer tool, syntax highlighting via highlight.js; quick for a single
  file but less control over book structure.

For a print-quality book, pandoc + LaTeX (or Quarto) is the recommended path.
