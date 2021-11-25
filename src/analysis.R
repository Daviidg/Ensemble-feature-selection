library(tidyverse)
library(kableExtra)

folds_ranks <- read_rds("folds_ranks.rds")
metrics <- read_rds("metrics.rds")
metrics_simple <- read_rds("metrics_simple.rds")

metrics_simple_nofold <- metrics_simple %>%
  group_by(dataset, method, threshold, high_dim, dimensionality, ensemble) %>%
  summarise(across(where(is.numeric), mean), .groups = "drop")

metrics_nofold <- metrics %>%
  group_by(dataset, method, threshold, high_dim, dimensionality, ensemble) %>%
  summarise(across(where(is.numeric), mean), .groups = "drop")

metrics_all_nofold <- bind_rows(metrics_nofold, metrics_simple_nofold)

## PLOTS

(execution_time <- metrics_nofold %>%
  ggplot(aes(time, method, fill = dimensionality)) +
  geom_boxplot() +
  scale_x_log10() +
  coord_flip() +
  labs(
    y = "Aggregation method", x = "Execution time (ms)",
    fill = "Dimensionality"
  )
)

(accuracy_high_all <- metrics_all_nofold %>%
  filter(high_dim) %>%
  ggplot(aes(threshold, Accuracy, color = method)) +
  facet_wrap(~dataset) +
  geom_line(aes(linetype = ensemble)) +
  labs(color = "Method", x = "Threshold")
)

(accuracy_low_all <- metrics_all_nofold %>%
  filter(!high_dim) %>%
  ggplot(aes(threshold, Accuracy, color = method)) +
  facet_wrap(~dataset) +
  geom_line(aes(linetype = ensemble)) +
  labs(color = "Method", x = "Threshold")
)

(accuracy_high <- metrics_nofold %>%
  filter(high_dim) %>%
  ggplot(aes(threshold, Accuracy, color = method)) +
  facet_wrap(~dataset) +
  geom_line(linetype = "dashed") +
  labs(color = "Method", x = "Threshold")
)

(accuracy_high_simple <- metrics_simple_nofold %>%
  filter(high_dim) %>%
  ggplot(aes(threshold, Accuracy, color = method)) +
  facet_wrap(~dataset) +
  geom_line(linetype = "dashed") +
  labs(color = "Metric", x = "Threshold")
)

(accuracy_low <- metrics_nofold %>%
  filter(!high_dim) %>%
  filter(dataset != "iris.txt") %>%
  ggplot(aes(threshold, Accuracy, color = method)) +
  facet_wrap(~dataset) +
  geom_line(linetype = "dashed") +
  labs(color = "Method", x = "Threshold")
)

(accuracy_low_simple <- metrics_simple_nofold %>%
  filter(!high_dim) %>%
  filter(dataset != "iris.txt") %>%
  ggplot(aes(threshold, Accuracy, color = method)) +
  facet_wrap(~dataset) +
  geom_line(linetype = "dashed") +
  labs(color = "Metric", x = "Threshold")
)

(acc_th_low <- metrics %>%
  filter(!high_dim) %>%
  filter(dataset != "iris.txt") %>%
  ggplot(aes(method, Accuracy)) +
  geom_boxplot() +
  facet_wrap(~dataset) +
  coord_flip() +
  labs(x = "Aggregation method")
)

metrics %>%
  filter(!high_dim) %>%
  filter(dataset != "iris.txt") %>%
  ggplot(aes(method, Accuracy)) +
  stat_summary(fun = mean) +
  coord_flip() +
  labs(x = "Aggregation method")

(acc_th_high <- metrics %>%
  filter(high_dim) %>%
  ggplot(aes(method, Accuracy)) +
  geom_violin() +
  stat_summary(fun = median) +
  facet_wrap(~dataset) +
  coord_flip() +
  labs(x = "Aggregation method")
)

ggsave("plots/execution_time.pdf", execution_time, width = 7, height = 4)
ggsave("plots/accuracy_high.pdf", accuracy_high, width = 7, height = 4)
ggsave("plots/accuracy_low.pdf", accuracy_low, width = 7, height = 4)
ggsave("plots/accuracy_high_simple.pdf", accuracy_high_simple,
  width = 7, height = 4
)
ggsave("plots/accuracy_low_simple.pdf", accuracy_low_simple,
  width = 7, height = 4
)
ggsave("plots/accuracy_th_low.pdf", acc_th_low, width = 7, height = 4)
ggsave("plots/accuracy_th_high.pdf", acc_th_high, width = 7, height = 4)

metrics %>% filter(dataset == "colon.mat", )

options(knitr.table.format = "latex")

decimalplaces <- function(x) {
  if (abs(x - round(x)) > .Machine$double.eps^0.5) {
    nchar(strsplit(sub("0+$", "", as.character(x)),
      ".",
      fixed = TRUE
    )[[1]][[2]])
  } else {
    return(0)
  }
}

decimalplaces(0.13)

siunitx <- function(acc, sd) {
  if (is.na(acc) || is.na(sd)) {
    return(NA)
  }

  sd <- signif(sd, 2)
  dp <- decimalplaces(sd)
  acc <- round(acc, dp)
  sd <- sd * 10^decimalplaces(sd)
  paste0(acc, "(", sd, ")")
}

table <- metrics %>%
  filter(dataset != "iris.txt") %>%
  group_by(dataset, method, threshold) %>%
  summarise(
    accuracy = mean(Accuracy), sd = sd(Accuracy),
    .groups = "drop"
  ) %>%
  mutate(value = siunitx(accuracy, sd)) %>%
  select(-accuracy, -sd) %>%
  pivot_wider(names_from = "method", values_from = value)

table

sink("table.tex")
table %>%
  kable(
    "latex",
    caption = "Accuracies",
    booktabs = TRUE,
    digits = 3
  ) %>%
  kable_styling(latex_options = c("HOLD_position", "scale_down"))
sink()

metrics %>%
  filter(dataset == "colon.mat" & method == "GA") %>%
  select(Accuracy, threshold, method, dataset) %>%
  view()

metrics %>%
  filter(dataset == "colon.mat") %>%
  group_by(method) %>%
  summarise(mean = mean(Accuracy), median = median(Accuracy))

metrics %>%
  group_by(dimensionality, method) %>%
  summarise(
    mean = mean(Accuracy, na.rm = TRUE),
    median = median(Accuracy, na.rm = TRUE),
    sd = sd(Accuracy, na.rm = TRUE)
  )
