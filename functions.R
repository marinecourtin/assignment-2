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
  # we add a geom layer called geom_violin to our ggplot object
  p <- p + ggplot2::geom_violin()
  return(p)
}


# Difference in the medians between two groups.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the dependent variable, provided as a string
# grouping_var: the name of a column of d containing a grouping variable, provided as a string
# group1: the value of grouping_var that corresponds to the first group
# group2: the value of grouping_var that corresponds to the second group
#
# RETURN VALUE:
# The median value of var for the first group, minus the median value of var for the second
# group.
#
difference_in_medians <- function(d, var, grouping_var, group1, group2) {
  d_1 <- dplyr::filter(d, get(grouping_var) == group1)
  d_2 <- dplyr::filter(d, get(grouping_var) == group2)
  result = median(d_1[[var]]) - median(d_2[[var]])
  return(result)
}


# Randomize the order of a column.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the variable to randomize, provided as a string
#
# RETURN VALUE:
# A data frame or tibble exactly the same as d, except with the order of
# var permuted randomly.
#
randomize <- function(d, var) {
  # d[[var]] contains our species for each dataline
  # we randomize its content, each dataline will be attributed one of the speciestags
  # present in the original d[[var]]
  d[[var]] <- sample(d[[var]], nrow(d))
  # this will result in a new dataframe where we can observe what our data would
  # look like if there was no systematic pattern between species / other variables
  # such as sepal width
    return(d)
}



# Perform a permutation test for two groups.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of the column in d on which the test statistic will be calculated,
# provided as a string
# grouping_var: the name of the column in d which gives the grouping
# group1: the value of grouping_var corresponding to the first group
# group2: the value of grouping_var corresponding to the second group
# statistic: a function yielding a test statistic, which takes as input
# a data frame, the name of a variable on which to calculate the
# test statistic, the name of a grouping variable, the value of
# the grouping variable corresponding to the first group, and
# the value of the grouping variable corresponding to the second
# group
# n_samples: the number of permutation samples to draw (default: 9999)
#
# RETURN VALUE:
#
# A list containing two elements:
#
#  - observed: the value of statistic() in d
#  - permuted: a vector containing the values of statistic() under n_samples
# permutations
#
permutation_twogroups <- function(d, var, grouping_var, group1, group2, statistic,
                                  n_samples=9999) {
  observed_statistic <- statistic(d, var, grouping_var, group1, group2)
  permutation_statistics <- rep(0, n_samples)
  for (i in 1:n_samples) {
    # we create a permutation using the randomize function
    permutation <- randomize(d, var)
    # we store the result of our statistic function on the permutation
    # at index i in the vector permutation_statistics
    permutation_statistics[i] <- statistic(permutation, var, grouping_var, group1, group2)
  }
  # we return a list for easy access to results for both observation on our
  # original data and on permutations
  result <- list(observed=observed_statistic,
                 permuted=permutation_statistics)
  return(result)
}

# Find most frequent value in a vector
# ARGUMENT:
# d : a vector of numbers
#
# RETURN VALUE :
# The most frequent value in vector d
most_frequent <- function(d) {
  result = sort(table(d),decreasing=TRUE)[1]
  return(result)
}

# Return 10 * the difference between most frequent values between 2 vectors
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the dependent variable, provided as a string
# grouping_var: the name of a column of d containing a grouping variable, provided as a string
# group1: the value of grouping_var that corresponds to the first group
# group2: the value of grouping_var that corresponds to the second group
#
# RETURN VALUE:
#
# A number which is 10* difference between the most frequent value for group 1
# and most frequent value for group 2
#
# Comment : the multiplication by 10 is just here to get a decent scale
# on our plot
#
new_test_statistic <- function(d, var, grouping_var, group1, group2) {
  d_1 <- dplyr::filter(d, get(grouping_var) == group1)
  d_2 <- dplyr::filter(d, get(grouping_var) == group2)
  result = most_frequent(d_1) - most_frequent(d_2)*10
  return(result)
}


permutation_pvalue_right <- function(p) {
  n_above <- sum(p$permuted >= p$observed)
  n_samples <- length(p$permuted)
  return((n_above + 1)/(n_samples + 1))
}
permutation_pvalue_left <- function(p) {
  n_below <- sum(p$permuted <= p$observed)
  n_samples <- length(p$permuted)
  return((n_below + 1)/(n_samples + 1))
}