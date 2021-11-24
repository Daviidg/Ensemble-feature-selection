library(RobustRankAggreg)
library(FSelector)
library(caret)
library(tidyverse)
library(e1071)
library(furrr)
library(R.matlab)
library(magrittr)
library(praznik)

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

funcs <- list()
funcs[["GA"]] <- function(ranks) {
  do.call("cbind", ranks) %>%
    t() %>%
    RankAggreg::RankAggreg(ncol(ranks), method = "GA")
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
    praznik::MRMR(df$C, k = ncol(.), positive = TRUE) %>%
    as.data.frame() %>%
    arrange(desc(score)) %>%
    rownames()

  list(rba, su, jmim, mrmr) %>%
    discard(is.null)
}

compute_aggregation <- function(ranks, test_df, train_df, aggregator,
                                thresholds) {
  start_time <- Sys.time()

  # Rank aggregation
  global_rank <- aggregator(ranks)

  # Time execution
  time <- Sys.time() - start_time

  thresholds %>%
    map_dfr(function(threshold) {
      # Threshold selection
      selected <- global_rank %>%
        slice_head(prop = threshold) %>%
        pull(Name)

      if (length(selected) == 0) {
        return(tibble::tibble(threshold = threshold))
      }

      train_df_red <- train_df %>%
        select(all_of(selected), "C")

      # Train model with reduced dataset and test it
      model <- e1071::naiveBayes(C ~ ., data = train_df_red)
      y_pred <- predict(model, newdata = test_df)

      conf_mat <- caret::confusionMatrix(table(y_pred, test_df$C))

      c(conf_mat$overall, conf_mat$byClass) %>%
        t() %>%
        as_tibble() %>%
        add_column(threshold = threshold, n_variables = length(selected))
    }) %>%
    add_column(time = time)
}

process_df <- function(file, funcs, k = 10, seed = 1234,
                       threshold = 1:10 / 20) {
  df <- read_df(file)

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
          compute_aggregation(ranks, test_df, train_df, aggregator,
            threshold = threshold
          ) %>%
            add_column(
              method = name,
              fold = fold_name
            )
        })
    }, .options = furrr_options(seed = seed)) %>%
    add_column(dataset = file)
}

k <- 10
seed <- 123456
set.seed(seed)

commandArgs(trailingOnly = TRUE) %T>%
  print() %>%
  map_dfr(function(file) {
    gc()
    message(paste("Processing", file))
    process_df(file, funcs, k = k, seed = seed) %T>%
      write_rds(paste0("resultat_GA_", basename(file), ".rds"))
  })
