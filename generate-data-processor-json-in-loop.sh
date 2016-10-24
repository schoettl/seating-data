#!/bin/bash
# Generate JSON in a loop for multiple simulation runs.

printUsage() {
    cat <<EOF
usage: $PROGNAME <arg>
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
    (( $# == 0 )) || exitWithError "$(printUsage)"
    declare -gr COMPARTMENT_COUNT=11
    for (( i = 0; 1; i++ )); do
        ./generate-data-processor-json.sh -s "$((i*COMPARTMENT_COUNT))" -p "$((i*1000))" -e "$((i*1000))" "$COMPARTMENT_COUNT"
    done
}

# do not pass $@ when using $ARGS:
main "$@"
