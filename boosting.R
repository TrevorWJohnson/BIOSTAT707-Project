library(tidyverse)
library(xgboost)
library(SHAPforxgboost)
library(patchwork)
source("./utils.R")

df <- read.csv("./Data/df_classes.csv")
df <- df %>% select(-FIPS, -State, -Year, -County, -forest, -dev)

splits <- train_test_split(df)
train <- splits[["train"]]
test <- splits[["test"]]
features <- standardize_train_test_features(train, test)
train_x <- features[["train_x"]]
train_y <- train$Mortality
test_x <- features[["test_x"]]
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
xgb_model <- xgb.train(params = xgb_params, data = xgb_train, nround = 15)
test_yhat <- predict(xgb_model, newdata = xgb_test)
sqrt(mean((test_y - test_yhat)^2))

# importance
xgb.plot.importance(xgb.importance(model = xgb_model))
# shap value
shap <- shap.prep(xgb_model = xgb_model, X_train = train_x[sample(nrow(train_x), 2000),])
p0 <- shap.plot.summary(shap, )
for (v in shap.importance(shap, names_only = TRUE, top_n = 5)) {
  p <- shap.plot.dependence(shap, v, color_feature = "auto",
                            alpha = 0.5) +
    ggtitle(v)
  p0 <- p0 + p
}
p0 + plot_layout(ncol = 3)

## gb-linear -----------------

xgb_params <- list(booster = "gblinear", objective = "reg:squarederror", eta = 0.5)
xgb_cv <- xgb.cv(
  params = xgb_params,
  data = xgb_train,
  metrics = list("rmse"),
  nrounds = 50,
  nfold = 5,
  early_stopping_rounds = 10,
  verbose = TRUE
)
xgb_plot_training(xgb_cv$evaluation_log)
xgb_model <- xgb.train(params = xgb_params, data = xgb_train, nround = 15)
test_yhat <- predict(xgb_model, newdata = xgb_test)
sqrt(mean((test_y - test_yhat)^2))

