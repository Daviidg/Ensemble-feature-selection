library(FSelector)
library(caret)
library(tidyverse)
library(e1071)
library(furrr)
library(R.matlab)
library(magrittr)

plan(multisession)

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

compute_rankings <- function(df) {
  rba <- if (ncol(df) < 100) {
    relief(C ~ ., df, neighbours.count = 5, sample.size = 10) %>%
      arrange(desc(attr_importance)) %>%
      rownames()
  } else {
    NULL
  }

  su <- symmetrical.uncertainty(C ~ ., df) %>%
    arrange(desc(attr_importance)) %>%
    rownames()

  jmim <- df %>%
    select(-C) %>%
    praznik::JMIM(df$C, k = ncol(df) - 1) %>%
    as.data.frame() %>%
    arrange(desc(score)) %>%
    rownames()

  mrmr <- df %>%
    select(-C) %>%
    praznik::MRMR(df$C, k = ncol(.), positive = FALSE) %>%
    as.data.frame() %>%
    arrange(desc(score)) %>%
    rownames()

  list(rba, su, jmim, mrmr) %>%
    discard(is.null)
}

process_df <- function(file, k = 10, seed = 1234,
                       threshold = 1:10 / 20) {
  df <- read_df(file)

  df %>%
    pull(C) %>%
    createFolds(k = k, list = TRUE) %>%
    write_rds(paste0("fold_", basename(file), ".rds")) %>%
    future_imap(function(fold, fold_name) {
      test_df <- df %>%
        dplyr::slice(fold)
      train_df <- df %>%
        dplyr::slice(-fold)

      # Compute ranks for all different filters
      compute_rankings(train_df)
    }, .options = furrr_options(seed = seed))
}

k <- 10
seed <- 123456
set.seed(seed)

commandArgs(trailingOnly = TRUE) %T>%
  print() %>%
  map_dfr(function(file) {
    gc()
    message(paste("Processing", file))
    process_df(file, k = k, seed = seed) %>%
      write_rds(paste0("resultat_metric_", basename(file), ".rds"))
  })
