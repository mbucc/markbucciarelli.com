#! /bin/sh -e
# Convert markdown to DOM that is TufteCSS-friendly.
# October 30, 2022

D="$(dirname $0)"
sed -n '8,$p' \
    | awk -f "$D/margin_notes.awk" \
    | cmark --unsafe --smart \
    | awk -f "$D/sections.awk" \
    | awk -f "$D/figures.awk"
