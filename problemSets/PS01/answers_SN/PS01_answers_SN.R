#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}


# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

#add my libraries
install.packages("ggplot2")
library("ggplot2")


#####################
# Problem 1
#####################

#create vector y

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

#calculate sample mean

#mean is the sum of the elements in vector y divided by the number of elements in y.

count_y <- length(y)
calc_mean_y <- sum(y)/count_y

func_mean_y <- mean(y)

#calculate stddev

#evaluate the vector of demeaned values demeaned_y_i = func_mean_y - y_i

y_minus <- y - mean(y)
y_minus_sqr <- y_minus^2

#sum the sqr of the vector elements 
demeaned_sum <- sum(y_minus_sqr)

variance <-demeaned_sum/(length(y)-1)
standard_deviation <- sqrt(variance)

func_stddev <- sd(y)
#YAY, they match.

#calculate stderr of the sample -- divide stddev by sqrt of sample size
standard_error <- standard_deviation/ sqrt(length(y))

#check with ggplot function
func_mean_se <- (mean_se(y, mult = 1))

# egads, this returns a dataframe.  now i need to figure out how to use that.
# and need absolute value of .... 
#(thanks, stack overflow)
func_stderr <- abs(func_mean_se[[2]] - func_mean_se[[1]])

#Find the 90% confidence interval for the average student IQ in the school.

#margin of error == stderr x tscore


#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/expenditure.txt", header=T)
