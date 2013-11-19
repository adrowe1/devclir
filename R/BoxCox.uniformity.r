#' BoxCox.uniformity
#'
#' Returns a Chi-square statistic for the uniformity of the distribution of Age data points after a Box-Cox transformation with a chosen lambda
#'
#' Function to calculate the Chi square statistic after transforming the distribution of covariate values by a given lambda and. 
#' The total number of bins used in the initial counting process is floor(sqrt(length(covariate)))
#' There should probably be a warning thrown if the number of data points entered is too few to be meaningful.
#'
#' @param lambda numeric value used in the Box-Cox transform
#' @param covariate numeric vector usually of sample age values which will be transformed
#' @return a single Chi-square statistic for the distributional uniformity of the transformed data set
#' @author Alexander D. Rowe
#' @seealso \code{\link{cov.transform.age}}
#' @examples
#' plot(seq(-1,2,0.1), mapply(lambda=seq(-1,2,0.1), BoxCox.uniformity, MoreArgs = list(covariate = rexp(1000,1)) ))
#' @export
#' @import car bcPower
#' 

BoxCox.uniformity <- function(lambda, covariate) {
    bins <- floor(sqrt(length(covariate)))
    chisq.test(hist(bcPower(covariate, lambda), plot=FALSE, breaks=bins)$counts)$statistic  
}    