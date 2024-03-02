#' Compute Portfolio Return
#'
#' @param weights weights of portfolio positions
#' @param returns returns of the positions of the portfolio
#' @param verbose prints out the result additionally
#'
#' @return numeric value
#' @export

mu_p <- function(weights, returns, verbose=TRUE){
  if(verbose){
    cat(paste("Portfolio Return: ", sum(weights*returns)))
  }
  return(invisible(sum(weights*returns)))
}
