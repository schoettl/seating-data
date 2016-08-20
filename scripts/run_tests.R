library(RUnit)

source('seat_info.R')

testSuite = defineTestSuite('all tests', dirs = file.path('tests'), testFileRegexp = '^.*\\.R')

results = runTestSuite(testSuite)

printTextProtocol(results)
