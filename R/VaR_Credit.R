#' Compute Value at Risk (VaR) of a Credit Risk
#'
#' @param credit Credit that was taken or given out
#' @param pd Probability of default
#' @param alpha Confidence Niveau
#'
#' @return numeric value
#' @export

VaR_Credit <- function(credit, pd, alpha){

  VaR <- rep(0, length(alpha))

  for(i in 1:length(alpha)){
    VaR[i] <- sum(credit*pmin(pd, alpha[i]))
    cat(paste("VaR ", alpha[i], ": ", VaR[i], "\n", sep=""))
  }

  return(invisible(VaR))

}
