#' train test split at county level
#'
#' @param df with "County.FIPS" variable
#' @param
#' @return list[test, train]
train_test_split <- function(df, frac = 0.8) {
  county_fips_code <- unique(df$County.FIPS)
  M <- length(county_fips_code)
  train_county_fips <-
    sample(county_fips_code, size = ceiling(frac * 3104))
  test_county_fips <- setdiff(county_fips_code, train_county_fips)
  return(list(train = df[df$County.FIPS %in% train_county_fips, ],
              test = df[df$County.FIPS %in% test_county_fips, ]))
}

#' plot training history of the XgBoost CV
#' 
#' @param
#' @param
#' @return
#' @examples
xgb_plot_training <- function(hist) {
  plot(hist$iter, hist$train_rmse_mean, col = "blue", "l",
       xlab = "iter", ylab = "RMSE")
  lines(hist$iter, hist$test_rmse_mean, col = "orange")
  legend("topright", legend=c("train", "test"),
         col=c("blue", "orange"), lty=c(1,1))
}

#' standardize train test features
#' 
#' @train
#' @test
#' @return list of standardized data
#' @examples
standardize_train_test_features <- function(train, test) {
  train_x <- train %>% select(-County.FIPS, -Mortality) %>% 
    mutate(Forest.Classes..FIXED. = as.factor(Forest.Classes..FIXED.),
           Development.Classes..FIXED. = as.factor(Development.Classes..FIXED.))
  test_x <- test %>% select(-County.FIPS, -Mortality) %>% 
    mutate(Forest.Classes..FIXED. = as.factor(Forest.Classes..FIXED.),
           Development.Classes..FIXED. = as.factor(Development.Classes..FIXED.))
  train_sd <- train_x %>% select_if(is.numeric) %>% sapply(sd)
  train_mean <- train_x %>% select_if(is.numeric) %>% sapply(mean)
  num_cols <- which(sapply(train_x, is.numeric))
  train_x[, num_cols] <- scale(train_x[, num_cols])
  test_x[, num_cols] <- sweep(sweep(test_x[, num_cols], 2, train_mean, "-"), 2, train_sd, "/")
  
  train_x <- model.matrix(~ 0 + ., data = train_x) 
  test_x <- model.matrix(~ 0 + ., data = test_x) 
  return(list(train_x = train_x, test_x = test_x))
}



