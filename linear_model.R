library(tidyverse)
library(glmnet)
library(olsrr)
setwd("../Desktop/Duke Univeristy MB Program/BIOSTAT 707/BIOSTAT707-Project/")
source("./utils.R")

get_test_rmse <- function(model, test) {
  yh <- predict(model, newdata = test)
  y <- test$Mortality
  sqrt(mean((y - yh)^2))
}

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

df <- read.csv("./Data/df_classes.csv")
df <- prep_data(df)
set.seed(1)
splits <- train_test_split(df, frac = 0.8)
train <- splits[["train"]]
test <- splits[["test"]]


## Baseline ---------------
sqrt(mean((test$Mortality - mean(train$Mortality))^2))

## Stepwise regression ----------------------

linear_model <- lm(Mortality ~ . - County.FIPS, data = train)
aic_stepwise <- ols_step_backward_aic(linear_model)
selected_var_aic_stepwise <- aic_stepwise$predictors
# top 10
# [1] "Prop.Shrub.Scrub"             "Developed.Medium.Intensity"   "Developed.High.Intensity"    
# [4] "Prop.Woody.Wetlands"          "Prop.Developed.Low.Intensity" "Prop.Deciduous.Forest"       
# [7] "Prop.Evergreen.Forest"        "Prop.Herbaceous.Land"         "Developed.Low.Intensity"     
# [10] "Deciduous.Forest"

lm_formula <- reformulate(
  setdiff(selected_var_aic_stepwise, "Mortality"), 
  response="Mortality"
)
selected_lm <- lm(lm_formula, data = train)
get_test_rmse(selected_lm, test)

### Lasso -------------------------

train_x <- model.matrix(Mortality ~ 0 + . - County.FIPS, data = train)
test_x <- model.matrix(Mortality ~ 0 + . - County.FIPS, data = test)
lasso <- cv.glmnet(x = train_x, y = train$Mortality, alpha = 1, trace.it = 0)

coef(lasso, s = "lambda.min") -> lasso_coef
selected_var_lasso <- lasso_coef@Dimnames[[1]][which(lasso_coef != 0)][-1]

test_yhat <- predict(lasso, newx = test_x, s = "lambda.min") %>% 
  as.vector()
sqrt(mean((test_yhat - test$Mortality)^2))

# [1] "Prop.Shrub.Scrub"              "Developed.Medium.Intensity"    "Developed.High.Intensity"     
# [4] "Prop.Woody.Wetlands"           "Prop.Developed.Low.Intensity"  "Prop.Deciduous.Forest"        
# [7] "Prop.Evergreen.Forest"         "Prop.Herbaceous.Land"          "Developed.Low.Intensity"      
# [10] "Deciduous.Forest"              "Forest.Classes..FIXED.2"       "Prop.Emergent.Herbaceous.Land"
# [13] "Cultivated.Crops"              "Forest.Classes..FIXED.3"       "Developed.Open.Space"         
# [16] "No..Polluting.Sites"           "Prop.Open.Water"               "Prop.Barren.Land"             
# [19] "Prop.Perennial.Snow"           "Prop.Mixed.Forest"             "Prop.Unclassified"            
# [22] "Evergreen.Forest"              "Shrub.Scrub"                   "Prop.Developed.Open.Space"    
# [25] "Unclassified"                  "Hay.Pasture"                   "Perennial.Snow"               
# [28] "Barren.Land"                   "Emergent.Herbaceous.Land"      "Herbaceous.Land"              
# [31] "Development.Classes..FIXED.2"

bootstrapping(test$Mortality, test_yhat, B = 5000) %>% quantile(probs = c(0.025, 0.975))

