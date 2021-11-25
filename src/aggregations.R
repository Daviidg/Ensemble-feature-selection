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

method_names <- c("RRA", "geom.mean", "mean", "median", "min", "stuart")
funcs <- method_names %>%
  map(~ function(ranks) {
    RobustRankAggreg::aggregateRanks(ranks, method = .)
  }) %>%
  set_names(method_names)

funcs[["GA"]] <- function(ranks) {
  v <- do.call("cbind", ranks) %>%
    t() %>%
    RankAggreg::RankAggreg(k = ncol(.), method = "GA", verbose = FALSE)

  data.frame(Name = v$top.list)
}

compute_model <- function(train_df_red, test_df) {
  # Train model with reduced dataset and test it
  model <- e1071::naiveBayes(C ~ ., data = train_df_red)
  y_pred <- predict(model, newdata = test_df)

  conf_mat <- caret::confusionMatrix(table(y_pred, test_df$C))

  c(conf_mat$overall, conf_mat$byClass) %>%
    t() %>%
    as_tibble()
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

      compute_model(train_df_red, test_df) %>%
        add_column(
          threshold = threshold,
          n_variables = length(selected)
        )
    }) %>%
    add_column(time = time)
}

metrics <- c("rba", "su", "JMIM", "MRMR")
ranks <- list.files(".", pattern = "resultat_rank.*.rds") %>%
  map_dfr(function(file_name) {
    read_rds(file_name) %>%
      as_tibble() %>%
      add_column(file = file_name) %>%
      mutate(dataset = stringr::str_match(file, "resultat_rank_(.*).rds")[, 2])
  }) %>%
  mutate(high_dim = endsWith(dataset, ".mat")) %>%
  group_by(dataset) %>%
  mutate(metric = 1:n() + high_dim) %>%
  ungroup() %>%
  mutate(metric = as_factor(metrics[metric])) %>%
  pivot_longer(starts_with("Fold"),
    names_prefix = "Fold", names_to = "fold", values_to = "ranking"
  )

folds <- list.files(".", pattern = "fold_.*.rds") %>%
  map_dfr(function(file) {
    read_rds(file) %>%
      t() %>%
      as_tibble() %>%
      add_column(dataset = stringr::str_match(file, "fold_(.*).rds")[, 2]) %>%
      pivot_longer(starts_with("Fold"),
        names_prefix = "Fold", names_to = "fold", values_to = "fold_list"
      )
  })

data <- left_join(ranks, folds, by = c("dataset", "fold")) %>%
  mutate(fold = as.numeric(fold))

process_df <- function(file_name, funcs, k = 10, seed = 1234,
                       threshold = 1:10 / 20) {
  df <- read_df(file_name)

  data %>%
    filter(dataset == basename(file_name)) %>%
    group_by(fold) %>%
    group_split() %>%
    future_map_dfr(function(df_rank) {
      ranks <- df_rank$ranking
      fold <- df_rank$fold_list[[1]]
      fold_name <- df_rank$fold[[1]]

      test_df <- df %>%
        dplyr::slice(fold)
      train_df <- df %>%
        dplyr::slice(-fold)

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
    add_column(dataset = file_name)
}

k <- 10
seed <- 123456
set.seed(seed)

commandArgs(trailingOnly = TRUE) %>%
  map_dfr(function(file) {
    gc()
    message(paste("Processing", file))
    process_df(file, funcs, k = k, seed = seed) %>%
      write_rds(paste0("resultat_agg_", basename(file), ".rds"))
  })
