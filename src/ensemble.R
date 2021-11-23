library(RobustRankAggreg)
library(FSelector)
library(caret)
library(tidyverse)
library(e1071)
library(furrr)
library(R.utils)
library(R.matlab)
library(magrittr)

plan(multisession)
timeout <- 60

read_df <- function(file) {
  if (endsWith(file, ".txt")) {
    read_csv(file, col_names = FALSE) %>%
      rename(C = last_col()) %>%
      mutate(C = as_factor(C))
  } else {
    with(
      readMat(file),
      X %>%
        as_tibble() %>%
        add_column(C = as_factor(Y))
    )
  }
}

method_names <- c("RRA", "geom.mean", "mean", "median", "min", "stuart")
funcs <- method_names %>%
  map(~ function(ranks) {
    RobustRankAggreg::aggregateRanks(ranks, method = .)
  }) %>%
  set_names(method_names)

compute_rankings <- function(df) {
  rba <- relief(C ~ ., df, neighbours.count = 5, sample.size = 10) %>%
    arrange(desc(attr_importance)) %>%
    rownames()

  su <- symmetrical.uncertainty(C ~ ., df) %>%
    arrange(desc(attr_importance)) %>%
    rownames()

  list(rba, su)
}

compute_aggregation <- function(ranks, test_df, train_df, aggregator,
                                threshold = 0.5) {

  # Rank aggregation
  global_rank <- aggregator(ranks)

  threshold %>%
    map_dfr(function(threshold) {
      # Threshold selection
      selected <- global_rank %>%
        slice_head(prop = threshold) %>%
        pull(Name)

      train_df_red <- train_df %>%
        select(all_of(selected), "C")

      # Train model with reduced dataset and test it
      model <- e1071::naiveBayes(C ~ ., data = train_df_red)
      y_pred <- predict(model, newdata = test_df)

      conf_mat <- caret::confusionMatrix(table(y_pred, test_df$C))

      c(conf_mat$overall, conf_mat$byClass) %>%
        t() %>%
        as_tibble() %>%
        add_column(threshold = threshold)
    })
}

# If it is a dataframe, add the columns, otherwise create new tibble
add_column_or_new <- function(.data, ...) {
  if (is.data.frame(.data)) {
    tibble::add_column(.data, ...)
  } else {
    tibble::tibble(...)
  }
}

process_df <- function(file, k = 10, timeout = 60, seed = 1234,
                       threshold = 1:10 / 10) {
  df <- read_df(file)

  print(glue::glue("Processing: '{file}'..."))

  df %>%
    pull(C) %>%
    createFolds(k = k, list = TRUE) %>%
    future_imap_dfr(function(fold, fold_name) {
      test_df <- df %>%
        dplyr::slice(fold)
      train_df <- df %>%
        dplyr::slice(-fold)

      # Compute ranks for all different filters
      ranks <- compute_rankings(train_df)

      funcs %>%
        imap_dfr(function(aggregator, name) {
          withTimeout({
              compute_aggregation(ranks, test_df, train_df, aggregator,
                threshold = threshold
              )
            },
            timeout = timeout,
            onTimeout = "warning"
          ) %>%
            add_column_or_new(
              method = name,
              fold = fold_name
            )
        })
    }, .options = furrr_options(seed = seed)) %>%
    add_column(dataset = file)
}

k <- 10
dataset_folder <- "../datasets"
seed <- 123456
set.seed(seed)

res <- dataset_folder %>%
  list.files(full.names = TRUE, recursive = TRUE) %>%
  keep(~ endsWith(., ".mat")) %T>% print() %>%
  future_map_dfr(
    ~ process_df(., k = k, timeout = timeout, seed = seed),
    .options = furrr_options(seed = seed)
  )

res %>%
  write_rds("resultat.rds")
