#!/usr/bin/env bash
# Build the companion guide into a single PDF using pandoc.
#
# Requirements:
#   - pandoc            (https://pandoc.org)
#   - a LaTeX engine    (tectonic, or TeX Live / MacTeX providing xelatex)
#
# Usage:
#   ./companion/book/build.sh            # -> companion/book/companion.pdf
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
companion="$(dirname "$here")"
out="$here/companion.pdf"

# Chapters are concatenated in reading order.
chapters=(
  "$companion/preface.md"
  "$companion/chapter1.introduction.md"
  "$companion/chapter2.lexical_analysis.md"
)

pandoc \
  --metadata-file="$here/metadata.yaml" \
  --pdf-engine=xelatex \
  --from=gfm \
  --output="$out" \
  "${chapters[@]}"

echo "Wrote $out"
