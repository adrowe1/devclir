\name{cov.transform.age}
\alias{cov.transform.age}
\title{cov.transform.age}
\usage{
  cov.transform.age(dataset, age.colname = "Age",
    age.units = c("d", "w", "m", "y"))
}
\arguments{
  \item{dataset}{data frame containing marker values and at
  least one column containing age data.}

  \item{age.colname}{character string containing the exact
  name of the column containing age data.}

  \item{age.units}{character array containing the shorthand
  values used to define days, weeks, months and years,
  where necessary in data files}
}
\value{
  a list containing the original data with appended Box-Cox
  transformed ages, the optimal lambda and a ggplot of the
  transformed, log-transformed and untransformed
  distributions for comparison
}
\description{
  Function to determine how the age category in a given
  dataset should be transformed in order to equally weight
  the age range when clustering
}
\details{
  This gives the user the option to choose whether or not
  to transform the age data, and if so, whether to use a
  log or a Box-Cox transform for optimal uniformity
}
\examples{
cov.transform.age(data.frame(analyte=rnorm(20,1), Age=rexp(20,1)*10 ))
}
\author{
  Alexander D. Rowe
}
\seealso{
  \code{\link{BoxCox.uniformity}}
}

