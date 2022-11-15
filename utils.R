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
