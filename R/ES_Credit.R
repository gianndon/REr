#' Compute Expected Shortfall (ES) of a Credit Risk
#'
#' @param credit Credit that was taken or given out
#' @param pd Probability of default
#' @param alpha Confidence Niveau
#'
#' @return numeric value
#' @export

ES_Credit <- function(credit, pd, alpha){

  ES <- rep(0, length(alpha))

  for(i in 1:length(alpha)){
    ES[i] <- sum(credit*pmin(pd, alpha[i])/alpha[i])
    cat(paste("ES ", alpha[i], ": ", ES[i], "\n", sep=""))
  }

  return(invisible(ES))

}
