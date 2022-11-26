library(progress)

#' bootstrapping for model rmse
#' 
#' @param
#' @param
#' @return
#' @examples
bootstrapping <- function(y, yhat, B = 5000) {
  pb <- progress_bar$new(
    format = "  downloading [:bar] :percent eta: :eta",
    total = B, clear = FALSE, width= 60)
  rmses <- rep(NA, B)
  n <- length(y)
  for (b in seq_len(B)) {
    pb$tick()
    boot_idx <- sample.int(n, n, replace = TRUE)
    boot_y <- y[boot_idx]
    boot_yhat <- yhat[boot_idx]
    rmses[b] <- sqrt(mean((boot_y - boot_yhat)^2))
  }
  return(rmses)
}

