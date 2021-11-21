library(RobustRankAggreg)
library(FSelector)
library(caret)
library(tidyverse)

file <- '../datasets/low-dim/iris.txt'
df <- read.table(file, header = FALSE, sep = ",")

names <- c(1:(ncol(df)-1),'C')
names(df) <- names

set.seed(12456)
k = 10

folds <- createFolds(df[,ncol(df)], k = k, list = TRUE)
accuracies <- list()

compute_rankings <- function(df) {
  rba <- relief(C~., df, neighbours.count = 5, sample.size = 10) %>%
    pull(attr_importance) %>%
    rank(x=-., ties.method='random')
  
  su <- symmetrical.uncertainty(C~., df) %>%
    pull(attr_importance) %>%
    rank(x=-., ties.method='random')
  
  list(rba, su)
}


for (i in c(1:k)) {
  testIndex <- folds[[i]]
  test_df <- df[testIndex,]
  train_df <- df[-testIndex,]
  
  # Compute ranks for all different filters
  ranks <- compute_rankings(train_df)
  
  # Rank aggregation
  global_rank <- aggregateRanks(ranks, method='mean')

  # Threshhold selection

  # Model
}


