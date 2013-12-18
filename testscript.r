# install the library if it is not already there
if (!"devclir" %in% installed.packages()) {
    library(devtools)
    install_github("devclir", "adrowe1")
}

# a quick test script once the package is installed

library(devclir)
library(ggplot2)
data(testdata)
# process the test dataset
tmp <- cov.transform.age(testdata)
# the third component of the output list is a ggplot... should plot easily as follows
tmp[[3]]
# as is the 4th component
tmp[[4]]
# the best lambda for age?
tmp[[2]]
# data plus B-C transformed age plus age clustering?
updated.data <- tmp[[1]]
head(updated.data)       

# try the help..
?cov.transform.age  

# plot the age data and show that the cluster sizes are reasonable
ggplot(tmp[[1]], aes(x=transformed.age.years, y=Gender, colour=age.cluster)) + geom_jitter(size=3)



