#!/bin/bash
# Merge simulation output to one LOG_EVENT.csv file and save it to data/simulated/

printUsage() {
    cat <<EOF
usage: $PROGNAME <output-directory>
EOF
}

readonly PROGNAME=$(basename "$0")
readonly PROGDIR=$(dirname "$(readlink -m "$0")")

# $1: error message
exitWithError() {
    echo "$1" >&2
    exit 1
}

main() {
    declare outputDir=$1
    [[ -n $outputDir ]] \
        || exitWithError "error: no simulation output directory specified."

    declare targetDir="$PROGDIR"/data/simulated
    mkdir -p "$targetDir"
    cat "$outputDir"/LOG_EVENT*.csv > "$targetDir"/LOG_EVENT.csv
}

main "$@"
