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

compute_model <- function(train_df_red, test_df) {
  # Train model with reduced dataset and test it
  model <- e1071::naiveBayes(C ~ ., data = train_df_red)
  y_pred <- predict(model, newdata = test_df)

  conf_mat <- caret::confusionMatrix(table(y_pred, test_df$C))

  c(conf_mat$overall, conf_mat$byClass) %>%
    t() %>%
    as_tibble()
}

compute_thresholds <- function(rank, test_df, train_df, thresholds) {
  thresholds %>%
    map_dfr(function(threshold) {
      # Threshold selection
      selected <- rank %>%
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
    })
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

process_df <- function(file_name, k = 10, seed = 1234,
                       threshold = 1:10 / 20) {
  df <- read_df(file_name)

  data %>%
    filter(dataset == basename(file_name)) %>%
    group_by(fold) %>%
    group_split() %>%
    future_map_dfr(function(df_rank) {
      ranks <- df_rank$ranking %>%
        set_names(df_rank$metric)
      fold <- df_rank$fold_list[[1]]
      fold_name <- df_rank$fold[[1]]

      test_df <- df %>%
        dplyr::slice(fold)
      train_df <- df %>%
        dplyr::slice(-fold)

      ranks %>% imap_dfr(function(rank, metric) {
        data.frame(Name = rank) %>%
          compute_thresholds(test_df, train_df, threshold) %>%
          add_column(
            metric = metric,
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
    process_df(file, k = k, seed = seed) %>%
      write_rds(paste0("resultat_no_agg_", basename(file), ".rds"))
  })
