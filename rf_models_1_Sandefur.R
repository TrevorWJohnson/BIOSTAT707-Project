library(tidyverse)
library(rpart.plot)
library(caret)
library(randomForest)
library(Metrics)
library(insurancerating)


getwd()
setwd("~/Fall 2022/707/Project/Data")

df_classes <- read.csv("df_classes.csv")

# Removing Old Class Variable that had issues:

df_classes <- subset(df_classes, select = -c(forest, dev))

# Splitting df_classes into a Train and Test Set:

set.seed(200)

## 70/30 split:
df_classes$assignment <- sample(c(TRUE, FALSE), 
                              replace = TRUE,
                              size = nrow(df_classes),
                              prob = c(.9,.1))

df.train <- df_classes %>% filter(assignment == TRUE)

df.test <- df_classes %>% filter(assignment ==  FALSE)

## Removing Assignment Variables:

df.train <- subset(df.train, select = -assignment)

df.test <- subset(df.test, select = -assignment)

## Removing Geographic Label Variables:

df.train <- df.train %>% select(-FIPS, 
                                -County.FIPS, 
                                -State,
                                -County,
                                -Year)

df.test <- df.test %>% select(-FIPS, 
                                -County.FIPS, 
                                -State,
                                -County,
                                -Year)
### Testing some subsetting functions
df.test %>% select(-starts_with("prop")) %>% View()





# Applying Decision Tree to Training set:

dt <- rpart::rpart(data = df.test %>% select(-starts_with("prop")),
             formula = Mortality ~.,
             method = "anova")

dt2 <- rpart::rpart(data = df.test %>% select(starts_with("prop"),
                                              No..Polluting.Sites,
                                              Open.Water,
                                              Perennial.Snow,
                                              Mortality
                                              ),
                          formula = Mortality ~.,
                          method = "anova")

# Note that rpart finds optimal model with lowest MSE via internal CV method.

summary(dt)

rpart.plot(dt, extra = 100)

# Applying model to test set and calculating MSE
dt.test <- predict(dt, df.test)

## Sum of Square Prediction Errors
SSPE <- sum((dt.test-df.test$Mortality)^2)

##Mean Square Prediction Error
MSPE <- SSPE/nrow(df.test)

sqrt(MSPE)


## Extremely Poor Result

# Random Forest with Latent Classes and all other Covariates Work ===============================================
gc()
set.seed(200)

rf <- randomForest(Mortality~., data = df.train)

summary(rf)
rf

prf <- predict(rf)
prf_test <- predict(data = df.test, rf)



# Random Forest without Latent Classes Work =============================================
df.train2 <- df.train %>% 
  select(-Forest.Classes..FIXED.,
         -Development.Classes..FIXED.)

set.seed(200)

rf_no_lc <- randomForest(Mortality ~., data = df.train2)

prf3 <- predict(rf_no_lc)
prf_test3 <- predict(data = df.test, rf_no_lc)

# Random Forest without Prop Vars =============================================
set.seed(200)

rf_no_prop <- randomForest(Mortality ~., data = df.train %>%select(-starts_with("prop")))

prf4 <- predict(rf_no_prop)
prf_test4 <- predict(data = df.test, rf_no_prop)

# Random Forest without RAW Vars =============================================
set.seed(200)

df.train3 <- df.train %>% 
  select(starts_with("prop"),
         Forest.Classes..FIXED.,
         Development.Classes..FIXED.,
         Mortality,
         No..Polluting.Sites)


rf_no_raw <- randomForest(Mortality ~., data = df.train3)

prf5 <- predict(rf_no_raw)
prf_test5 <- predict(data = df.test, rf_no_raw)

# Random Forest with ONLY Latent Classes (and Polluting Sites)

set.seed(200)


rf_no_class_only <- randomForest(Mortality ~ No..Polluting.Sites +
                                   Forest.Classes..FIXED. +
                                   Development.Classes..FIXED., 
                          data = df.train)

prf6 <- predict(rf_no_class_only)
prf_test6 <- predict(data = df.test, rf_no_class_only)

rf_class_only <- rf_no_class_only

# Models
rf
rf_class_only 
rf_no_prop
rf_no_raw
rf_no_lc

# Test rMSE results
rmse(df.test$Mortality, predict(rf, df.test))
rmse(df.test$Mortality, predict(rf_no_class_only, df.test))
rmse(df.test$Mortality, predict(rf_no_prop, df.test))
rmse(df.test$Mortality, predict(rf_no_raw, df.test))
rmse(df.test$Mortality, predict(rf_no_lc, df.test))


# Plots
importance(rf)
varImpPlot(rf, type = 2, 
           main = "Random Forest Model With All Covariates")


#Bootstrap ========================================



# Bootstraped Predicted Values Function:
boot.rf.rmse <- function(model, reps, df){
  
  #Progress Bar
  pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                       max = reps,   # Maximum value of the progress bar
                       style = 3,    # Progress bar style (also available style = 1 and style = 2)
                       width = 50,   # Progress bar width. Defaults to getOption("width")
                       char = "=") 
  
  
  rmse.store <- NULL
  yhat <- matrix( 
                 nrow = nrow(df),
                 ncol = reps)
  boot.df.list <- list()
  yboot <- NULL

for(i in 1:reps){
 boot.df.list[[i]] <- sample_n(df, 
                        replace = TRUE, 
                        size = nrow(df))
 
 
}


  
for(j in 1:reps){
  yhat[,j] <- predict(model, boot.df.list[[j]])
  
  
  
  rmse.store[j] <- Metrics::rmse(as.vector(unlist(boot.df.list[[j]][1])), yhat[,j])
  
  setTxtProgressBar(pb,j)
}
  close(pb)
  return(list(yhat = yhat, rmse = rmse.store, boot.data = boot.df.list, yboot = yboot))
  
}


# Bootstrapped Predicted Values
results <- boot.rf.rmse(model = rf, 
                            reps = 1000, 
                            df = df.test)

# RMSE Sample Calculation
hist(results$rmse, main = "Bootstrap  Sample (n = 1000)",
     sub = "Random Forest",
     xlab = "RMSE")

quantile(results$rmse, probs = c(.025, .975))

hist(df_classes$Mortality, main = "Histogram of Mortality ")

(summary(df_classes$Mortality))

ggplot(data = df_classes, aes(x = Mortality))+
  geom_boxplot()+
  facet_wrap(~State)
