library(tidyverse)
library(glmnet)
library(olsrr)
setwd("../Desktop/Duke Univeristy MB Program/BIOSTAT 707/BIOSTAT707-Project/")

df <- read.csv("./Data/df_classes.csv")
# drop index, FIPS, State, Year
df <- df %>% select(-FIPS, -State, -Year, -County, -forest, -dev)
df_summarized <- df %>% 
  group_by(County.FIPS) %>% 
  summarise_all(mean) %>% 
  select(-County.FIPS)
df_summarized[,2:(ncol(df_summarized)-2)] <- scale(df_summarized[,2:(ncol(df_summarized)-2)])
df_summarized$Forest.Classes..FIXED. <- as.factor(df_summarized$Forest.Classes..FIXED.)
df_summarized$Development.Classes..FIXED. <- as.factor(df_summarized$Development.Classes..FIXED.)

# select fixed effect
lasso <- cv.glmnet(
  scale(model.matrix(Mortality ~ 0 + ., data = df_summarized), center = TRUE, scale = TRUE),
  df_summarized$Mortality,
  family = "gaussian",
  alpha = 1,
  nfolds = 10
)

sqrt(lasso$cvm[lasso$lambda == lasso$lambda.min])

# -----------

prep_fixed_data <- function(df) {
  df <- df %>% select(-FIPS, -State, -Year, -County, -forest, -dev)
  df_summarized <- df %>% 
    group_by(County.FIPS) %>% 
    summarise_all(mean) %>% 
    select(-County.FIPS)
  df_summarized[,2:(ncol(df_summarized)-2)] <- scale(df_summarized[,2:(ncol(df_summarized)-2)])
  df_summarized$Forest.Classes..FIXED. <- as.factor(df_summarized$Forest.Classes..FIXED.)
  df_summarized$Development.Classes..FIXED. <- as.factor(df_summarized$Development.Classes..FIXED.)
  return(df_summarized)
}

## Stepwise regression ----------------------
# 
## --- --- --- --- --- --- --- --- --- --- --- --- --- 

df_summary <- prep_fixed_data(df)
print(dim(df_summary))
fixed_lm <- lm(Mortality ~ ., data = df_summary)
stepwise <- ols_step_both_p(fixed_lm, pent = 0.05, prem = 0.05)

## Truncated data ----------------------
# select variables for data reduction
## --- --- --- --- --- --- --- --- --- --- --- --- --- 
df <- read.csv("./Data/df_classes.csv")
split <- train_test_split(df)

train <- split[["train"]]
train_fixed <- prep_fixed_data(df)
fixed_lm <- lm(Mortality ~ ., data = train_fixed)
stepwise <- ols_step_both_p(fixed_lm, pent = 0.05, prem = 0.05)
selected_var <- stepwise$predictors

# drop county fips code
train_trunc <- split$train %>% 
  select(-FIPS, -County.FIPS, -State, -Year, -County, -forest, -dev) %>% 
  select(Mortality, all_of(selected_var))
train_trunc[, c(2, 4:21)] <- scale(train_trunc[, c(2, 4:21)])

test_trunc <- split$test %>% 
  select(-FIPS, -County.FIPS, -State, -Year, -County, -forest, -dev) %>% 
  select(Mortality, all_of(selected_var))
test_trunc[, c(2, 4:21)] <- scale(test_trunc[, c(2, 4:21)])

stepwise_model <- lm(Mortality ~ ., data = train_trunc)

get_train_rmse <- function(model) {
  sqrt(mean(model[["residuals"]]^2))
}

get_test_rmse <- function(model, test) {
  yh <- predict(model, newdata = test)
  y <- test$Mortality
  sqrt(mean((y - yh)^2))
}





