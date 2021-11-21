library(RobustRankAggreg)
library(FSelector)
library(caret)
library(tidyverse)
library(e1071)

file <- '../datasets/low_dim/sonar.txt'
df <- read.table(file, header = FALSE, sep = ",")

names <- c(1:(ncol(df)-1))
names(df) <- c(paste("Var", names, sep = "_"), 'C')

set.seed(12456)
k = 10

folds <- createFolds(df[,ncol(df)], k = k, list = TRUE)

accuracies <- list()
subset_size <- list()

summary(df)

compute_rankings <- function(df) {
  rba <- relief(C~., df, neighbours.count = 5, sample.size = 10) %>%
    arrange(-attr_importance) %>%
    rownames()
  
  su <- symmetrical.uncertainty(C~., df) %>%
    arrange(-attr_importance) %>%
    rownames()
  
  list(rba, su)
}

model=naiveBayes(C~., data=df)
predict(model, newdata = train, type = "class")

for (i in c(1:k)) {
  testIndex <- folds[[i]]
  test_df <- df[testIndex,]
  train_df <- df[-testIndex,]

  # Compute ranks for all different filters
  ranks <- compute_rankings(train_df)
  
  # Rank aggregation
  global_rank <- aggregateRanks(ranks, method='mean')

  # Threshhold selection
  threshhold <- 0.5
  cutoff <- floor(ncol(df)* threshhold)
  selected <- global_rank[1:cutoff,1]
  
  train_df_red <- train_df[,c(selected, 'C')]

  # Train model with reduced dataset and test it
  model <- naiveBayes(C~., data=train_df_red)
  y_pred <- predict(model, newdata = test_df)
  
  acc <- confusionMatrix(table(y_pred, test_df$C))$overall['Accuracy']
  accuracies <- c(accuracies, acc)
  subset_size <- c(subset_size, length(selected))
}

accuracies

subset_size

