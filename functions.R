# Sum values in a column of a data frame.
#
# input:
#   d: a data frame or tibble
#   var: the name of a column of d, provided as a string
#
# output:
#   if the column exists and contains numbers:
#     sum(all values in the column)
#   otherwise:
#     NULL
#
sum_column <- function(d, var) {
  # default value to return
  result <- NULL
  x <- d[[var]] # alternative d$var ->
  if (!is.null(x)) {
    if (is.numeric(x)) {
      result <- sum(x)
    }
  }
  return(result)
}