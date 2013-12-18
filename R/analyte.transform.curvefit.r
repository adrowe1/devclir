#' analyte.transform.curvefit
#'
#' Function to determine how the age category in a given dataset should be transformed in order to equally weight the age range when clustering
#'
#' This gives the user the option to choose whether or not to transform the age data, and if so, whether to use a log or a Box-Cox transform for optimal uniformity
#'
#' @param dataset data frame containing marker values and at least one column containing age data.
#' @param age.colname character string containing the exact name of the column containing age data.
#' @param age.units character array containing the shorthand values used to define days, weeks, months and years, where necessary in data files
#' @return a list containing the original data with appended Box-Cox transformed ages and cluster IDs, 
#' the optimal lambda, a plot of lambda vs chi-statistic and a ggplot of the transformed, log-transformed and untransformed distributions for comparison
#' @author Alexander D. Rowe
#' @seealso \code{\link{BoxCox.uniformity}}
#' @examples
#' cov.transform.age(data.frame(analyte=rnorm(20,1), Age=rexp(20,1)*10 ))
#' @export
#' @import car ggplot2 MASS gam


analyte.transform.curvefit <- function(cov.transformed.data, chosen.analyte, chosen.covariates=c("ages")) {
    data <- cov.transformed.data[[1]]
    # only relevant details
    dat <- ddply(data, .(age.cluster), summarize, transformed.age.years=median(transformed.age.years), C16.0=median(C16.0))
    ggplot(data, aes(x=transformed.age.years, y=C16.0, colour=age.cluster)) + 
        geom_point() + 
        scale_y_log10() +
        geom_point(data=dat, aes(x=transformed.age.years, y=C16.0), colour="black") +
        geom_smooth(data=dat, aes(x=transformed.age.years, y=C16.0), colour="black")
    
}
    

splines  <- gam(log10(C18.0) ~ s(transformed.age.years, 5), data=updated.data)
updated.data$pred<-10^(predict(splines, updated.data))
ggplot(updated.data, aes(x=transformed.age.years, y=C18.0, colour=Gender)) + geom_point() + geom_smooth() + scale_y_log10() + geom_line(data=updated.data, aes(x=transformed.age.years, y=pred), size=3)

# http://people.bath.ac.uk/sw283/mgcv/


knots <- 100

fitM <- gam(CRE~s(transformed.age.years, bs="cc", k=knots), knots=list( seq(min(output$transformed.age.years), max(output$transformed.age.years), length.out =10) ), data=output[output$gender==1,] )
fitF <- gam(CRE~s(transformed.age.years, bs="cc", k=knots), knots=list( seq(min(output$transformed.age.years), max(output$transformed.age.years), length.out =10) ), data=output[output$gender==0,] )

output[output$gender==1,]$pred <- predict(fitM, output[output$gender==1,])
output[output$gender==0,]$pred <- predict(fitF, output[output$gender==0,])      

ggplot(output, aes(x=transformed.age.years, y=CRE, colour=gender)) + 
    geom_point(alpha=0.3) + 
    scale_y_log10() + 
    geom_line(data=output, aes(x=transformed.age.years, y=pred))