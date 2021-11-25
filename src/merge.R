library(tidyverse)

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

folds_ranks <- left_join(ranks, folds, by = c("dataset", "fold")) %>%
  mutate(fold = as.numeric(fold)) %>%
  write_rds("folds_ranks.rds")

metrics <- list.files(".", pattern = ".*agg3.*.rds") %>%
  map_dfr(read_rds) %>%
  mutate(file = dataset, dataset = basename(file)) %>%
  mutate(high_dim = endsWith(dataset, ".mat")) %>%
  mutate(dimensionality = ifelse(high_dim, "High", "Low")) %>%
  mutate(time = as.numeric(time) * 1000) %>%
  mutate(ensemble = TRUE) %>%
  write_rds("metrics.rds")

metrics_simple <- list.files(".", pattern = ".*no_agg.*.rds") %>%
  map_dfr(read_rds) %>%
  mutate(file = dataset, dataset = basename(file)) %>%
  mutate(high_dim = endsWith(dataset, ".mat")) %>%
  mutate(dimensionality = ifelse(high_dim, "High", "Low")) %>%
  rename(method = metric) %>%
  mutate(ensemble = FALSE) %>%
  write_rds("metrics_simple.rds")
