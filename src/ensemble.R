library(RobustRankAggreg)
library(FSelector)
library(caret)
library(tidyverse)
library(e1071)
library(furrr)

plan(multisession)

file <- "../datasets/low_dim/sonar.txt"
df <- read.table(file, header = FALSE, sep = ",") %>%
  rename(C = last_col()) %>%
  mutate(C = as_factor(C))

summary(df)

set.seed(12456)
k <- 10

folds <- df %>%
  pull(C) %>%
  createFolds(k = k, list = TRUE)

summary(df)

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
    as_tibble()
}

res <- folds %>%
  future_imap(function(fold, fold_name) {
    test_df <- df %>% slice(fold)
    train_df <- df %>% slice(-fold)

    # Compute ranks for all different filters
    ranks <- compute_rankings(train_df)

    funcs %>%
      imap(function(aggregator, name) {
        print(c(fold_name, name))
        compute_aggregation(ranks, test_df, train_df, aggregator) %>%
          add_column(
            method = name,
            fold = fold_name
          )
      }) %>%
      reduce(~ bind_rows(.x, .y))
  }, .options = furrr_options(seed = 123)) %>%
  reduce(~ bind_rows(.x, .y))

res %>%
  write_rds("resultat.rds")
