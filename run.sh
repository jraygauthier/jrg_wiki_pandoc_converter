#!/usr/bin/env bash

set -euf -o pipefail

SCRIPT_DIR=`cd $(dirname $0) > /dev/null;pwd`

DEFAULT_WIKI_IN_REPO_URL="https://my/url/to/jrg_wiki"

WIKI_IN_REPO_URL="${1:-$DEFAULT_WIKI_IN_REPO_URL}"
INCREMENTAL="${2:-}"
NO_GIT_SYNC="${3:-}"
ASSUME_PREBUILT_FILTERS="${4:-}"
OUT_TYPE="${5:-html}"
OUT_DIR="${6:-$SCRIPT_DIR/out_${OUT_TYPE}}"
WIKI_IN_DIR="${7:-$SCRIPT_DIR/wiki_in}"


if [ -z "$NO_GIT_SYNC" ]; then

  echo "Updating wiki repository from \`$WIKI_IN_REPO_URL\` to \`$WIKI_IN_DIR\`."
  echo "--------------------------------------------------------------------------"


    if [ -d "$WIKI_IN_DIR" ]; then
      git -C "$WIKI_IN_DIR" pull
    else
      git clone "$WIKI_IN_REPO_URL" "$WIKI_IN_DIR"
    fi

  echo ""
  echo ""

fi


if [ -z "$INCREMENTAL" ]; then

  echo "Removing existing output directory at \`$OUT_DIR\`."
  echo "--------------------------------------------------------------------------"

  # The following allows the output directory to be a pre-existing git directory.

  if [ -d "$OUT_DIR" ]; then
    # Remove all the non dot files from the output directory.
    find "$OUT_DIR" -mindepth 1 -maxdepth 1 ! -name "\.*" | xargs rm -rf

    # Remove the directory itself when found to be not empty.
    rmdir --ignore-fail-on-non-empty "$OUT_DIR"
  fi


  echo ""
  echo ""

fi


FILTERS_DIR="$SCRIPT_DIR/filters"
FILTERS_BIN_DIR="$FILTERS_DIR/bin"
export PATH="$PATH:$FILTERS_BIN_DIR:$FILTERS_DIR"



if [ -z "$ASSUME_PREBUILT_FILTERS" ]; then

  echo "Compiling all pandoc filters found under \`$FILTERS_DIR\` to \`$FILTERS_BIN_DIR\`."
  echo "--------------------------------------------------------------------------"
  echo ""

  getMinimalPandocTypeVersion() {
    echo "1.16"
  }

  getCurrentPandocTypeVersion() {
    if ! command -v ghc-pkg >/dev/null 2>&1 ; then
      return 1 # False
    fi

    if ! local ghcPkgPandocTypesVersionField="$(2>/dev/null ghc-pkg field pandoc-types version)"; then
      return 1 # False
    fi

    if [ -z "$ghcPkgPandocTypesVersionField" ]; then
      return 1 # False
    fi

    local pandocTypesV="$(echo "${ghcPkgPandocTypesVersionField}" | sed -r -e 's/version: ([0-9\.]+)/\1/')"
    echo "$pandocTypesV"
  }

  foundMinimalPandocTypeVersion() {

    if ! local pandocTypesV="$(getCurrentPandocTypeVersion)"; then
      return 1 # False
    fi

    if [ -z "$pandocTypesV" ]; then
      return 1 # False
    fi

    test "$(echo "$pandocTypesV" | sed -r -e 's/([0-9]+)\.[0-9]+\.[0-9]\.[0-9]+/\1/')" -eq '1' &&
    test "$(echo "$pandocTypesV" | sed -r -e 's/[0-9]+\.([0-9]+)\.[0-9]\.[0-9]+/\1/')" -ge '16'
  }



  if ! foundMinimalPandocTypeVersion; then
    >&2 echo "ERROR: Could not find minimal \`$(getMinimalPandocTypeVersion)\` \`pandoc-types\` "\
             "library version required to build \`pandoc\` filters. Current version "\
             "is \`$(getCurrentPandocTypeVersion)\`."
    exit 1
  fi

  # Compile pandoc filters
  mkdir -p "$FILTERS_BIN_DIR"

  find "$FILTERS_DIR" -type f -name "*.hs" | \
  while read filterF; do 

    filerName="`basename "$filterF" .hs`"

    if [ ! -e "$FILTERS_BIN_DIR/$filerName" ] || [ "$filterF" -nt "$FILTERS_BIN_DIR/$filerName" ]; then

      echo "$filerName"

      ghc -o "$FILTERS_BIN_DIR/$filerName" --make "$filterF"

    fi

  done


  echo ""
  echo ""

else

  echo "Validating that pandoc filters under \`$FILTERS_DIR\` are available in precompiled form."
  echo "---------------------------------------------------------------------------------------------"
  echo ""

  find "$FILTERS_DIR" -type f -name "*.hs" | \
  while read filterF; do 

    filerName="`basename "$filterF" .hs`"

    command -v $filerName >/dev/null 2>&1 || \
      { echo "Filter \"${filerName}\" could not be found in precompiled form. Please compile it first and make in available in \"PATH\""; exit 1; }
    
  done
fi


IN_PAGE_EXT="md"
OUT_PAGE_EXT="html"

IN_ROOT_DIR="$WIKI_IN_DIR"

pushd "$IN_ROOT_DIR" > /dev/null


# Copy images and other files which are not pages but where
# delivered alongside pages.
echo "Copying non page files recursively from \`$IN_ROOT_DIR\` to \`$OUT_DIR\`."
echo "--------------------------------------------------------------------------"
echo ""

find . -type f ! -name "*.md" ! -path "./.git/*" | \
while read inF; do

  outF="$inF"
  outD="`dirname "$OUT_DIR/$outF"`"

  if [ ! -e "$OUT_DIR/$outF" ] || [ "$inF" -nt "$OUT_DIR/$outF" ]; then

    echo "$inF"

    mkdir -p "$outD"
    cp -T "$inF" "$OUT_DIR/$inF"

  fi
done

echo ""
echo ""


# Transform pages.
echo "Transform pages from \`$IN_ROOT_DIR\` to \`$OUT_DIR\` using pandoc."
echo "--------------------------------------------------------------------------"
echo ""


#echo "./Tests/Maths.md" | \
find . -type f -name "*.md" ! -path "./.git/*" | \
while read inF; do 

  inDir=`dirname $inF`

  pathFromInDirToInRoot=`realpath --relative-to="$inDir" $IN_ROOT_DIR`

  outF="`echo "$inF" | sed -r -e 's#^(.+)\.'${IN_PAGE_EXT}'$#\1.'${OUT_PAGE_EXT}'#g'`"

  if [ ! -e "$OUT_DIR/$outF" ] || [ "$WIKI_IN_DIR/$inF" -nt "$OUT_DIR/$outF" ]; then
    echo "$inF"


    outD="`dirname "$OUT_DIR/$outF"`"
    mkdir -p "$outD"

    cat "$WIKI_IN_DIR/$inF" | \
    pandoc -f markdown -t json | \
    dot_filter "$inF" "$OUT_DIR/img" "/img" | \
    plantuml_filter "$inF" "$OUT_DIR/img" "/img" | \
    adapt_local_page_link_filter "$OUT_PAGE_EXT" "$pathFromInDirToInRoot" | \
    pandoc -s -f json -t html --mathjax --toc --toc-depth=4 > "$OUT_DIR/$outF" || \
      { echo "Error converting markdown input file \"$WIKI_IN_DIR/$inF\" to \"$OUT_DIR/$outF\" \"$OUT_TYPE\" output."; \
        rm -f "$OUT_DIR/$outF"; exit 1; }

  fi

done

echo ""
echo ""

popd > /dev/null

