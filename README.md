October 15, 2022

My blog website
====================


Today, I converted from using Hakyll to [mkws.sh](https://mkws.sh).

Steps:

1. Install per instructions at link above.
2. Move bin and share directories up one level.
3. sudo port install cmark (for Markdown processing)
4. Update the [site template](./share/l.upphtml) to always use cmake.
5. Update the [css file](./share/s.uppcss) to use Tufte's style
