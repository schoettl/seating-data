has_no_duplicate_ids = function() {
    function(x) {
        expect(length(x$ID) == length(unique(x$ID)),
               'ID column is not unique')
    }
}

has_monotonic_increasing_column = function(columnName) {
    function(x) {
        column = x[[columnName]]
        expect(class(column) != 'factor', 'column must not be a factor')
        equality = (column == sort(column))
        idsOfFails = x$ID[!equality]
        expect(all(equality),
               paste0('column ', columnName, ' is not monotonic increasing; ',
                      'rows with the following IDs mismatch: ',
                      paste(idsOfFails, collapse = ', ')))
    }
}

has_monotonic_increasing_column_per_survey = function(columnName) {
    function(x) {
        surveys = unique(x$SURVEY)
        for (i in surveys) {
            y = x %>% filter(SURVEY == i)
            has_monotonic_increasing_column(columnName)(y)
        }
    }
}
