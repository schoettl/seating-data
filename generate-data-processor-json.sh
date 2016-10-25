#!/bin/bash
# Generate data processor JSON configuration (output files and processors with attributes)

printUsage() {
    cat <<EOF
usage: $PROGNAME [-s <survey_id_offset>] [-p <person_id_offset>] [-e <event_id_offset>] <compartment_count>
EOF
}

readonly PROGNAME=$(basename "$0")
readonly PROGDIR=$(dirname "$(readlink -m "$0")")

OUTPUT_FILE_JSON='
  , {
    "type" : "org.vadere.simulator.models.seating.dataprocessing.LogEventOutputFile",
    "filename" : "LOG_EVENT<number>.csv",
    "processors" : [ <processor_id> ]
  }
'

PROCESSOR_JSON='
  , {
    "type" : "org.vadere.simulator.models.seating.dataprocessing.LogEventProcessor",
    "id" : <processor_id>,
    "attributesType" : "org.vadere.state.attributes.processor.AttributesLogEventProcessor",
    "attributes" : {
      "compartmentIndex" : <compartment_index>,
      "surveyId" : <survey_id>,
      "personIdOffset" : <person_id_offset>,
      "firstLogEventId" : <first_log_event_id>
    }
  }
'

PROCESSOR_ID_OFFSET=100

# $1: error message
exitWithError() {
    echo "$1" >&2
    exit 1
}

# $*: command line arguments = "$@"
parseCommandLine() {
    # declare options globally and readonly
    while getopts "s:p:e:" OPTION; do
         case $OPTION in
         s)
             declare surveyIdOffs=$OPTARG
             ;;
         p)
             declare personIdOffs=$OPTARG
             ;;
         e)
             declare eventIdOffs=$OPTARG
             ;;
         h)
             printUsage
             exit 0
             ;;
        esac
    done

    declare -gr SURVEY_ID_OFFS=$surveyIdOffs
    declare -gr PERSON_ID_OFFS=$personIdOffs
    declare -gr EVENT_ID_OFFS=$eventIdOffs

    shift $((OPTIND-1))

    (( $# == 1 )) || exitWithError "$(printUsage)"

    declare -gr COMPARTMENT_COUNT=$1

    return 0
}

main() {
    parseCommandLine "$@"

    declare i outputFilesFile processorsFile
    outputFilesFile=$(mktemp)
    processorsFile=$(mktemp)

    for (( i = 1 ; i <= COMPARTMENT_COUNT ; i++ )); do
        declare processorId=$((PROCESSOR_ID_OFFSET + i))
        declare surveyId=$((SURVEY_ID_OFFS + i))
        declare outputFileNumber
        outputFileNumber=$(printf "%02d" "$i")
        echo "$OUTPUT_FILE_JSON" | sed \
            -e "s/<number>/$outputFileNumber/" \
            -e "s/<processor_id>/$processorId/" \
            >> "$outputFilesFile"
        echo "$PROCESSOR_JSON" | sed \
            -e "s/<processor_id>/$processorId/" \
            -e "s/<compartment_index>/$i/" \
            -e "s/<survey_id>/$surveyId/" \
            -e "s/<person_id_offset>/$((PERSON_ID_OFFS + i * 20))/" \
            -e "s/<first_log_event_id>/$((EVENT_ID_OFFS + i * 20 + 1))/" \
            >> "$processorsFile"
    done

    xclip -selection c < "$outputFilesFile"
    echo "Output files JSON: $outputFilesFile"
    echo "Copied to clipboard. Press enter to continue."
    read -r _

    xclip -selection c < "$processorsFile"
    echo "Processors JSON: $processorsFile"
    echo "Copied to clipboard. Press enter to continue."
    read -r _

    echo "Finished."
}

main "$@"
