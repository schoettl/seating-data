#!/bin/bash
# Merge simulation output to one LOG_EVENT.csv file and save it to data/simulated/

printUsage() {
    cat <<EOF
usage: $PROGNAME [ <output-directory> ]
  If output directory is ommitted, the latest output in DEFAULT_OUTPUT_DIR is used.
  If output directory is not found, the DEFAULT_OUTPUT_DIR is searched for it.
EOF
}

readonly DEFAULT_OUTPUT_DIR=~/vadere/PersSchoettl/vadere_project/output

readonly PROGNAME=$(basename "$0")
readonly PROGDIR=$(dirname "$(readlink -m "$0")")

# $1: error message
exitWithError() {
    echo "$1" >&2
    exit 1
}

printLatestOutputDir() {
    find "$DEFAULT_OUTPUT_DIR" -maxdepth 1 -mindepth 1 -type d \
        | sed -r 's/.*_([^_]+_[^_]+)/\1 \0/' \
        | sort \
        | tail -1 \
        | sed 's/[^ ]* //'
}

main() {
    declare outputDir
    if (( $# == 0 )); then
        outputDir=$(printLatestOutputDir)
    elif (( $# == 1 )); then
        outputDir=$1
        [[ -d $outputDir ]] || outputDir=$DEFAULT_OUTPUT_DIR/$outputDir
    else
        exitWithError "$(printUsage)"
    fi

    echo "using output from: $outputDir"

    declare targetDir="$PROGDIR"/data/simulated
    mkdir -p "$targetDir"
    cat "$outputDir"/LOG_EVENT*.csv > "$targetDir"/LOG_EVENT.csv
}

main "$@"
