#!/usr/bin/env bash

set -euf -o pipefail

DEFAULT_WIKI_IN_REPO_URL="https://my/url/to/jrg_wiki"
WIKI_IN_REPO_URL="${1:-$DEFAULT_WIKI_IN_REPO_URL}"

DEFAULT_WIKI_OUT_HTML_REPO_URL="https://my/url/to/jrg_wiki_html"
WIKI_OUT_HTML_REPO_URL="${2:-$DEFAULT_WIKI_OUT_HTML_REPO_URL}"

INCREMENTAL="${3:-}"
NO_IN_GIT_SYNC="${4:-}"
NO_OUT_GIT_SYNC="${5:-}"
ASSUME_PREBUILT_FILTERS="${6:-}"

SCRIPT_DIR=`cd $(dirname $0) > /dev/null;pwd`

CONVERTER_ROOT_DIR="$SCRIPT_DIR"
WIKI_OUT_HTML_DIR="$SCRIPT_DIR/wiki_out_html"



if [ -z "$NO_OUT_GIT_SYNC" ]; then

  if [ -d "$WIKI_OUT_HTML_DIR" ]; then
    git -C "$WIKI_OUT_HTML_DIR" pull
  else
    git clone "$WIKI_OUT_HTML_REPO_URL" "$WIKI_OUT_HTML_DIR"
  fi

fi

nix-shell --run \
  "$CONVERTER_ROOT_DIR/run.sh \"$WIKI_IN_REPO_URL\" \"$INCREMENTAL\" \"$NO_IN_GIT_SYNC\" \"$ASSUME_PREBUILT_FILTERS\" \"html\" \"$WIKI_OUT_HTML_DIR\"" \
  "$CONVERTER_ROOT_DIR/shell.nix"


