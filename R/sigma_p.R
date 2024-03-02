#' Compute Portfolio Variance and Standard Deviation (Volatility)
#'
#' @param weights weights of portfolio positions
#' @param correlation_matrix correlation matrix of the portfolio
#' @param volatility volatility of single assets
#' @param verbose prints out the result additionally
#'
#' @return

sigma_p <- function(weights, correlation_matrix, volatility, verbose=TRUE){

  portfolio_variance <- 0

  for(i in 1:length(weights)){
    for(j in 1:length(weights)){
      portfolio_variance <- portfolio_variance + weights[i]*weights[j]*correlation_matrix[i, j]*volatility[i]*volatility[j]
    }
  }

  portfolio_standard_deviation <- sqrt(portfolio_variance)

  if(verbose){
    cat(paste("Portfolio Variance: ", portfolio_variance, "\n",
              "Portfolio Standard Deviation: ", portfolio_standard_deviation, sep=""))
  }

  return(invisible(list(var=portfolio_variance, std=portfolio_standard_deviation)))

}
