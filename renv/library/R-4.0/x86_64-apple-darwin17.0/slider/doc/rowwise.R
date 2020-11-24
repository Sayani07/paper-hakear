## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(slider)
library(dplyr, warn.conflicts = FALSE)

## -----------------------------------------------------------------------------
example <- tibble(
  x = 1:4,
  y = letters[1:4]
)

example

## -----------------------------------------------------------------------------
slide(example$x, ~.x)

slide(example$x, ~.x, .before = 2)

## -----------------------------------------------------------------------------
slide(example, ~.x)

## -----------------------------------------------------------------------------
# Current row + 2 before
slide(example, ~.x, .before = 2)

# Center aligned, with no partial results
slide(example, ~.x, .before = 1, .after = 1, .complete = TRUE)

## -----------------------------------------------------------------------------
parameters <- tibble(
  n = 1:3,
  min = c(0, 10, 100),
  max = c(1, 100, 1000)
)

parameters

## -----------------------------------------------------------------------------
set.seed(123)

slide(parameters, ~runif(.x$n, .x$min, .x$max))

## -----------------------------------------------------------------------------
company <- tibble(
  day = rep(c(1, 2), each = 5),
  sales = sample(100, 10),
  n_calls = sales + sample(1000, 10)
)

company

## -----------------------------------------------------------------------------
company %>%
  mutate(sales_roll = slide_dbl(sales, mean, .before = 2, .complete = TRUE))

company %>%
  group_by(day) %>%
  mutate(sales_roll = slide_dbl(sales, mean, .before = 2, .complete = TRUE))

## -----------------------------------------------------------------------------
company %>%
  mutate(
    regressions = slide(
      .x = ., 
      .f = ~lm(sales ~ n_calls, .x), 
      .before = 2, 
      .complete = TRUE
    )
  )

## ---- error=TRUE--------------------------------------------------------------
company %>%
  mutate(
    log_n_calls = log(n_calls),
    regressions = slide(
      .x = ., 
      .f = ~lm(sales ~ log_n_calls, .x), 
      .before = 2, 
      .complete = TRUE
    )
  )

## ---- error=TRUE--------------------------------------------------------------
company %>%
  mutate(
    log_n_calls = log(n_calls),
    example = slide(., ~.x$log_n_calls)
  )

## ---- error=TRUE--------------------------------------------------------------
company %>%
  group_by(day) %>%
  mutate(
    regressions = slide(
      .x = ., 
      .f = ~lm(sales ~ n_calls, .x), 
      .before = 2, 
      .complete = TRUE
    )
  )

## -----------------------------------------------------------------------------
company %>%
  mutate(
    log_n_calls = log(n_calls),
    regressions = slide(
      .x = tibble(sales = sales, log_n_calls = log_n_calls), 
      .f = ~lm(sales ~ log_n_calls, .x), 
      .before = 2, 
      .complete = TRUE
    )
  )

## -----------------------------------------------------------------------------
company %>%
  group_by(day) %>%
  mutate(
    regressions = slide(
      .x = tibble(sales = sales, n_calls = n_calls), 
      .f = ~lm(sales ~ n_calls, .x), 
      .before = 2, 
      .complete = TRUE
    )
  )

## -----------------------------------------------------------------------------
single_group_regressions <- function(.data_group, ...) {
  regressions <- slide(
      .x = .data_group, 
      .f = ~lm(sales ~ n_calls, .x), 
      .before = 2, 
      .complete = TRUE
    )
  
  mutate(.data_group, regressions = regressions)
}

## -----------------------------------------------------------------------------
day_one <- filter(company, day == 1)
single_group_regressions(day_one)

## -----------------------------------------------------------------------------
company %>%
  group_by(day) %>%
  group_modify(single_group_regressions)

