library(tidyverse)
library(glmnet)
library(olsrr)
setwd("../Desktop/Duke Univeristy MB Program/BIOSTAT 707/BIOSTAT707-Project/")

df <- read.csv("./df.csv")
# drop index, FIPS, State, Year
df <- df %>% select(-X, -FIPS, -State, -Year, -County)

df_summarized <- df %>% 
  group_by(County.FIPS) %>% 
  summarise_all(mean) %>% 
  select(-County.FIPS)
# 3104 counties

# select fixed effect
lasso <- cv.glmnet(
  scale(model.matrix(Mortality ~ 0 + ., data = df_summarized), center = TRUE, scale = TRUE),
  df_summarized$Mortality,
  family = "gaussian",
  alpha = 1,
  nfolds = 10
)

fixed_lm <- lm(Mortality ~ ., data = df_summarized)
stepwise <- ols_step_both_p(fixed_lm, pent = 0.05, prem = 0.05)


