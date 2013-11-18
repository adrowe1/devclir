#' fun1
#'
#' This is the Description section
#'
#' This is the Details section
#'
#' @param x numeric. this is multiplied by 100 to determine the length of the returned vector
#' @return a numeric vector of random deviates of length \code{100 * x}
#' @author your name
#' @seealso \code{\link{fun2}}
#' @examples
#' fun1(2)
#' length(fun1(20))
#' @export
fun1 <- function(x){
    rnorm(100*x)
}