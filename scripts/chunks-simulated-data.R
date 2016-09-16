source('analysis.R', chdir = TRUE)

## ---- chunk-simulated-data-processing ----

# load simulated LOG_EVENTS.csv
# put it to seating-data/data/simulated/LOG_EVENTS.csv

# create artificial surveyData and personData

# do different preprocessing (shouldn't be much)

# generateMoreData -> seatingData

seatingData = generateMoreData(surveyData, logEventData)

