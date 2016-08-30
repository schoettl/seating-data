
# Opposite of "count NAs"
countAvailableValues = function(x) sum(!is.na(x))

# Similar to what dplyr rowwise() does!
forEachRowAsVec = function(data, columns, func) {
    apply(data[, columns], 1, func)
}
