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
  x <- d[[var]] # alternative d$var -> it worked for me ?? to check later
  if (!is.null(x)) {
    if (is.numeric(x)) {
      result <- sum(x)
    }
  }
  return(result)
}


# Sum values in a vector.
#
# ARGUMENTS:
# x: a vector
#
# RETURN VALUE:
# if the vector contains numbers, returns the sum of
# all values; otherwise, returns NULL
#
# [YOUR FUNCTION HERE]
my_sum <- function(x) {
  result <- NULL
  if (is.numeric(x)) {
    total = 0
    length = length(x)
    for (i in 1:length) {
      total = total + x[i]
    }
  result <- total
  }
  return(result)
}

# Sum values in a vector and divide it by a number
#
# ARGUMENTS:
# x: a vector
# k : a number
#
# RETURN VALUE:
# if the vector contains numbers, and k is a number returns the sum of
# all values divided by k; otherwise, returns NULL
#
# [YOUR FUNCTION HERE]
sum_divided_by <- function(x, k) {
  # initialize result as NULL
  # if our conditions aren't verified, this value will be returned
  # in place of our divided vector
  result <- NULL
  # we want the 2 conditions to be true
  if (is.numeric(x) & is.numeric(k)) {
    # we use our function with the argument x
    # and store the result in a variable
    sum_x <- my_sum(x)
    # we divide the sum by our number k and store it
    result = sum_x/k
  }
  return(result)
}


# Calculate the mean of a vector
#
# ARGUMENTS:
# x: a vector
#
# RETURN VALUE:
# if the vector contains numbers, returns the mean of a vector (i.e the sum of
# all values divided by the length of the vector); otherwise, returns NULL
#
# [YOUR FUNCTION HERE]
my_mean <- function(x) {
    # we store the number of element in x in our variable
    length = length(x)
    # we use this variable as the k argument in our function
    # there is no need to check here if x is a vector of number
    # as it is already implemented in our sum_divided_by function
    result = sum_divided_by(x, length)
  return(result)
}



# Return a violin plot.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the dependent variable, provided as a
#     string
# grouping_var: the name of a column of d containing a grouping variable, provided as a string
#
# RETURN VALUE:
# A ggplot plot object containing a violin plot, grouped by the values
# of the grouping variable.
#
grouped_violin_plot <- function(d, var, grouping_var) {
  # Create the base ggplot object
  p <- ggplot2::ggplot(d, ggplot2::aes_string(y=var,
                                              x=grouping_var,
                                              fill=grouping_var))
  # YOUR CODE HERE: Create a violin plot
  # we add a geom layer called geom_violin to our ggplot object
  p <- p + ggplot2::geom_violin()
  return(p)
}