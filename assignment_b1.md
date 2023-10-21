Assignment B1
================
Xin Wang
2023-10-21

### Section for Libraries

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(testthat)
```

    ## 
    ## Attaching package: 'testthat'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches

## Exercise 1: Make a Function (25 points)

In this exercise, you’ll be making a function and fortifying it. The
function need not be complicated. The function need not be “serious”,
but shouldn’t be nonsense.

*This function accepts arguments for grouping variables and summary
statistics, handle NA values, and utilize the ellipsis (…) to allow for
various summary functions.*

## Exercise 2: Document your Function (20 points)

In the same code chunk where you made your function, document the
function using roxygen2 tags.

``` r
#' Summarize Data by Group
#'
#' This function performs group-wise summarization of a data frame based on
#' specified grouping variables and summary statistics.
#'
#' @param data A data frame that contains the data to be summarized.
#' @param group_vars A character vector of grouping variables. These variables
#'        will be used to group the data.
#' @param ... A set of summary expressions using the "name = expression" format.
#'        These expressions will be applied to the columns in the data frame.
#'
#' @return A data frame with grouped summaries, where each group is defined by
#'         the specified grouping variables. The summary statistics for each
#'         column are calculated based on the expressions provided.
#'
#' @export
summarize_data <- function(data, group_vars, ...) {
  summary_exprs <- enquos(...)
  
  result <- data %>%
    group_by(across(all_of(group_vars), .names = "group")) %>%
    summarise(!!!summary_exprs, .groups = "drop")
  
  return(result)
}
```

## Exercise 3: Include examples (15 points)

Demonstrate the usage of your function with a few examples. Use one or
more new code chunks, describing what you’re doing.

1.  I create a data frame called `STAT_545_grades` with columns for
    course parts, work numbers, worksheet grades, and assignment grades.

2.  I use function, `summarize_data`, to group and summarize this data.
    Specifically, I group the data by `course_part` and calculate the
    mean of `worksheet_grade` and the sum of `assignment_grade` for each
    group, while ignoring missing values.

3.  The result is stored in the `result` variable and displayed in the
    console. It provides summaries of grades by course part.

``` r
STAT_545_grades <- data.frame(
  course_part = c("A", "A", "B", "B"),
  work_number = c("1", "3", "1", "3"),
  worksheet_grade = c(10, 9, 8, NA),
  assignment_grade = c(36, 78, 25, 27)
)

# Usage of the summarize_data function
result <- summarize_data(STAT_545_grades, group_vars = c("course_part"), 
                        mean_worksheet_grade = mean(worksheet_grade, na.rm = TRUE),
                        sum_assignment_grade = sum(assignment_grade, na.rm = TRUE))

result
```

    ## # A tibble: 2 × 3
    ##   group mean_worksheet_grade sum_assignment_grade
    ##   <chr>                <dbl>                <dbl>
    ## 1 A                      9.5                  114
    ## 2 B                      8                     52

## Exercise 4: Test the Function (25 points)

Write formal tests for your function. You should use at least three
non-redundant uses of an expect\_() function from the testthat package,
and they should be contained in a test_that() function (or more than
one). They should all pass.

``` r
# Test case 1: Vector with no NAs
test_that("Test Case 1: Vector with No NAs", {
  data <- data.frame(
    Group = c("A", "A", "B", "B"),
    Value = c(10, 15, 20, 25)
  )
  result <- summarize_data(data, group_vars = "Group", Mean_Value = mean(Value))
  expected_result <- data.frame(Group = c("A", "B"), Mean_Value = c(12.5, 22.5))
  expect_true(all.equal(result, expected_result, check.attributes = FALSE))
})
```

    ## Test passed 🥳

``` r
# Test case 2: Vector with NAs
test_that("Test Case 2: Vector with NAs", {
  data <- data.frame(
    Group = c("A", "A", "B", "B"),
    Value = c(10, 15, NA, 25)
  )
  result <- summarize_data(data, group_vars = "Group", Mean_Value = mean(Value, na.rm = TRUE))
  expected_result <- data.frame(Group = c("A", "B"), Mean_Value = c(12.5, 25))
  expect_equal(result$Mean_Value, expected_result$Mean_Value)
})
```

    ## Test passed 🌈

``` r
# Test Case 3: Vector of length 0
test_that("Test Case 3: Vector of Length 0", {
  data <- data.frame(
    Group = character(0),
    Value = numeric(0)
  )
  result <- summarize_data(data, group_vars = "Group", Mean_Value = mean(Value, na.rm = TRUE))
  expect_length(result$Mean_Value, 0)
})
```

    ## Test passed 🌈
