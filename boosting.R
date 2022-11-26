library(tidyverse)
library(xgboost)
library(SHAPforxgboost)
library(patchwork)
setwd("../Desktop/Duke Univeristy MB Program/BIOSTAT 707/BIOSTAT707-Project/")
source("./utils.R")
source("./bootstrapping.R")

set.seed(1)
df <- read.csv("./Data/df_classes.csv")
df <- prep_data(df)
splits <- train_test_split(df, frac = 0.9)
train <- splits[["train"]]
test <- splits[["test"]]

# features <- standardize_train_test_features(train, test)

# train_x <- features$train_x
train_x <- model.matrix(Mortality ~ 0 + . -County.FIPS, data = train)
train_y <- train$Mortality
# test_x <- features$test_x
test_x <- model.matrix(Mortality ~ 0 + . -County.FIPS, data = test)
test_y <- test$Mortality

# prepare xgb format data
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

## gb-tree -------------------

xgb_params <- list(booster = "gbtree", objective = "reg:squarederror", eta = 0.3, max_depth = 10)
xgb_cv <- xgb.cv(
  params = xgb_params,
  data = xgb_train,
  metrics = list("rmse"),
  nrounds = 50,
  nfold = 10,
  early_stopping_rounds = 5,
  verbose = TRUE
)
xgb_plot_training(xgb_cv$evaluation_log)
xgb_model <- xgb.train(params = xgb_params, data = xgb_train, nround = 10)

test_yhat <- predict(xgb_model, newdata = xgb_test)
sqrt(mean((test_y - test_yhat)^2))

bootstrapping(test_y, test_yhat, B = 1000) %>% quantile(probs = c(0.025, 0.975))



# importance
xgb.plot.importance(
  xgb.importance(model = xgb_model),
  top_n = 30,
  measure = "Gain",
  main = "XGboost feature importance based on Gain",
  xlim = c(0, 0.12),
  cex=0.7
)
# shap value
shap <- shap.prep(xgb_model = xgb_model,
                  X_train = train_x[sample(nrow(train_x), 20000),],
                  top_n = 10)
p0 <- shap.plot.summary(shap)
p0 +
  ggtitle("Top 10 important features based on SHAP value")

shap_values <- shap.values(xgb_model = xgb_model, X_train = train_x[sample(nrow(train_x), 20000), ])
fig_list <- lapply(names(shap_values$mean_shap)[1:5], 
                   shap.plot.dependence, data_long = shap)
gridExtra::grid.arrange(grobs = fig_list, ncol = 3)


