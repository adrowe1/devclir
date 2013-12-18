#' cov.transform.age
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
#' @import car ggplot2


cov.transform.age <- function(dataset, age.colname="Age", age.units=c("d","w","m","y") ){
    # First identify age variable
    ages <- dataset[,age.colname]
    # if ages is not numeric, assume there are dat and month values and fix these
    if ( !is.numeric(ages) ) {
        # fix cases and get age units out
        ages <- tolower(ages)
        age.units <- tolower(age.units)
        # substitute age week, month and year characters
        ages <- gsub(age.units[1], "d" ,ages)
        ages <- gsub(age.units[2], "w" ,ages)
        ages <- gsub(age.units[3], "m" ,ages)
        ages <- gsub(age.units[4], "y" ,ages)
        # get days out
        ages.days <- rep("0d", length(ages) )
        ages.days[grep("d", ages)]  <- ages[grep("d", ages)]
        ages.days <- as.numeric(gsub("d", "", ages.days))
        # get weeks out
        ages.weeks <- rep("0w", length(ages) )
        ages.weeks[grep("w", ages)]  <- ages[grep("w", ages)]
        ages.weeks <- as.numeric(gsub("w", "", ages.weeks))
        # get months out
        ages.months <- rep("0m", length(ages) )
        ages.months[grep("m", ages)]  <- ages[grep("m", ages)]
        ages.months <- as.numeric(gsub("m", "", ages.months))
        # get years out
        ages.years <- rep("0y", length(ages) )
        ages.years[grep("[dwm]", ages, invert=TRUE)] <- ages[grep("[dwm]", ages, invert=TRUE)]
        ages.years <- as.numeric(gsub("y", "", ages.years))
        # translate all ages into years, using dividing factors of 365, 52 and 12 for days, weeks, months respectively
        ages.days <- ages.days/365
        ages.weeks <- ages.weeks/52
        ages.months <- ages.months/12
        ages <- ages.days + ages.weeks + ages.months + ages.years
    }
    # find the optimal transform for age data, such that the data is as close to uniformly distributed as possible
    # histogram the data into bins (total bins = sqrt total data) then minimise a X-square to test for uniformity
    lambdas <- seq(-2, 2, by=0.02)
    transform.results <- data.frame(lambdas, 
                                    X.squared=mapply(lambda=lambdas, BoxCox.uniformity, MoreArgs = list(covariate = ages) )    
                                    )
    # add smoothing data
    transform.results$smoothed.X.squared <- predict(loess(X.squared ~ lambdas, transform.results, span=0.15), transform.results$lambdas)
    # ggplot of chi-square vs lambda
    x.sq.vs.lambda.plot <- ggplot(transform.results, aes(x=lambdas, y=X.squared)) + 
        geom_point() + 
        geom_smooth(span=0.15, colour="red") + 
        scale_y_log10() + 
        xlab("Lambda for Box-Cox transform") + 
        ylab("Chi-square statistic to be minimized")
    # return the lambda which gives the most uniformly distributed covariate - minimum of the smoothed X square statistics
    optimal.lambda <- transform.results[transform.results$smoothed.X.squared==min(transform.results$smoothed.X.squared),]$lambdas
    # build a data frame containing count data for untransformed, log transformed and optimally transformed data, for plotting and comparison
    bins <- floor(sqrt(length(ages)))
    opt <- hist(bcPower(ages, optimal.lambda), plot=FALSE, breaks=bins)
    unt <- hist(ages, plot=FALSE, breaks=bins)
    log <- hist(log10(ages), plot=FALSE, breaks=bins)
    # combine into data frame
    uniformity <- data.frame(mids=c(opt$mids, unt$mids, log$mids), 
                             counts=c(opt$counts, unt$counts, log$counts), 
                             type=c(rep("Optimal", length(opt$mids)) , rep("Untransformed", length(unt$mids)), rep("Log transformed", length(log$mids)) )
                             )
    
    uniformity2 <- data.frame(mids=c(inverse.BoxCox(opt$mids, optimal.lambda), unt$mids, 10^(log$mids) ), 
                             counts=c(opt$counts, unt$counts, log$counts), 
                             type=c(rep(paste("Box-Cox lambda =", optimal.lambda, sep=" "), length(opt$mids)) , rep("Untransformed", length(unt$mids)), rep("Log transformed", length(log$mids)) )
    )
    
    
    plot.uniformity <- ggplot(uniformity2[uniformity2$counts>0,], aes(x=mids, y=counts, colour=type, fill=type)) + 
        geom_bar(stat="identity", position="identity", width=(1/length(opt$mids)), alpha=0.3) + 
        facet_grid(type~.) + 
        scale_x_log10(breaks=signif(2^(ceil(log2(min( uniformity2$mids))):floor(log2(max( uniformity2$mids)))), digits=2) ) + 
        scale_y_log10() + 
        guides(fill = guide_legend(override.aes = list(alpha = 1))) + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
        xlab("Age of group members (years)") +
        ylab("Samples in evenly spaced transformed-age clusters")
    # push a column of numeric ages onto dataset
    dataset$ages <- ages
    # push a transformed age column onto the original dataset
    dataset$transformed.age.years  <- bcPower( ages, optimal.lambda )
    # add a column for age cluster as defined by the Box-Cox transformed ages
    dataset$age.cluster <- as.factor(cut(bcPower(ages, optimal.lambda), breaks=opt$breaks, labels=FALSE))
    #assemble output
    to.user <- list(dataset, optimal.lambda, x.sq.vs.lambda.plot, plot.uniformity)
    # return output
    to.user
}

