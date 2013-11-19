\name{BoxCox.uniformity}
\alias{BoxCox.uniformity}
\title{BoxCox.uniformity}
\usage{
  BoxCox.uniformity(lambda, covariate)
}
\arguments{
  \item{lambda}{numeric value used in the Box-Cox
  transform}

  \item{covariate}{numeric vector usually of sample age
  values which will be transformed}
}
\value{
  a single Chi-square statistic for the distributional
  uniformity of the transformed data set
}
\description{
  Returns a Chi-square statistic for the uniformity of the
  distribution of Age data points after a Box-Cox
  transformation with a chosen lambda
}
\details{
  Function to calculate the Chi square statistic after
  transforming the distribution of covariate values by a
  given lambda and. The total number of bins used in the
  initial counting process is
  floor(sqrt(length(covariate))) There should probably be a
  warning thrown if the number of data points entered is
  too few to be meaningful.
}
\examples{
plot(seq(-1,2,0.1), mapply(lambda=seq(-1,2,0.1), BoxCox.uniformity, MoreArgs = list(covariate = rexp(1000,1)) ))
}
\author{
  Alexander D. Rowe
}
\seealso{
  \code{\link{cov.transform.age}}
}

