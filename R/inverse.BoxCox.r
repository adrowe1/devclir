#' inverse.BoxCox
#'
#' Computes the inverse Box-Cox transformation of input values for a given lambda
#'
#' Function to calculate the inverse Box-Cox transformation of input values for a given lambda
#'
#' @param lambda numeric value used in the inverse Box-Cox transform
#' @param value numeric value or vector to be inverse-transformed
#' @return a vector of reverse-transformed data points
#' @author Alexander D. Rowe
#' @seealso \code{\link{cov.transform.age}}
#' @examples
#' plot(seq(-1,2,0.1), mapply(lambda=seq(-1,2,0.1), BoxCox.uniformity, MoreArgs = list(covariate = rexp(1000,1)) ))
#' @export 
#' 
inverse.BoxCox <- function(value, lambda) {
    if (lambda == 0) {
        exp(value) 
    }
    else {
        (lambda*value + 1)^(1/lambda)
    }
}
