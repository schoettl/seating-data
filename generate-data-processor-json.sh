#!/bin/bash
# Generate data processor JSON configuration (output files and processors with attributes)

printUsage() {
    cat <<EOF
usage: $PROGNAME <compartment_count>
EOF
}

readonly PROGNAME=$(basename "$0")
readonly PROGDIR=$(dirname "$(readlink -m "$0")")

OUTPUT_FILE_JSON='
  , {
    "type" : "org.vadere.simulator.models.seating.LogEventOutputFile",
    "filename" : "LOG_EVENT<number>.csv",
    "processors" : [ <processor_id> ]
  }
'

PROCESSOR_JSON='
  , {
    "type" : "org.vadere.simulator.models.seating.LogEventProcessor",
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

main() {
    (( $# == 1 )) || exitWithError "$(printUsage)"
    declare compartmentCount=$1

    declare i outputFilesFile processorsFile
    outputFilesFile=$(mktemp)
    processorsFile=$(mktemp)

    for (( i = 1 ; i <= compartmentCount ; i++ )); do
        declare processorId=$((PROCESSOR_ID_OFFSET + i))
        declare surveyId=$((0 + i))
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
            -e "s/<person_id_offset>/$((i * 20))/" \
            -e "s/<first_log_event_id>/$((i * 20 + 1))/" \
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
