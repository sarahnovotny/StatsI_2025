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
#install.packages("ggplot2")
library("ggplot2")
library(stargazer)

#####################
# Problem 1
#####################

#create vector y

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

#calculate sample mean
#mean is the sum of the elements in vector y divided by the number of elements in y.

count_y <- length(y)
calc_mean_y <- sum(y)/count_y
# verify with the function
func_mean_y <- mean(y)

#calculate stddev
#evaluate the vector of demeaned values demeaned_y_i = func_mean_y - y_i
y_minus <- y - mean(y)
y_minus_sqr <- y_minus^2

#sum the sqr of the vector elements 
demeaned_sum_sqr <- sum(y_minus_sqr)

# calculate variance
# sum of the demeaned values^2 divided by the sample size-1 
# n-1 also called degrees of freedom
variance <-demeaned_sum_sqr/(length(y)-1)

#calculate stddev 
standard_deviation <- sqrt(variance)

# verify with the function
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

##  --------

#Find the 90% confidence interval for the average student IQ in the school.

z90 <-qt((1 - .90)/2, df= (count_y -1), lower.tail = FALSE)
lower_90 <- func_mean_y - ( z90 * func_stddev/ sqrt(count_y))
upper_90 <- func_mean_y + ( z90 * func_stddev/ sqrt(count_y))
confint90 <- c(lower_90, upper_90)


# ?t.test
# H0 ---  mean_y = mu = 100
# Halpha --- mean_y is either greater or less than mu=100
# two sided t-test
# not terribly useful here... but good practice
mu0 <- 100

# T_stat <- (mean_y  - 100)/ ( std_dev/ sqrt(n) ), degrees of freedom = (length(y) -1)
t_stat <- (calc_mean_y  - mu0)/(standard_error)
#print(t_stat )
#?pt
p_value <-  2*pt(abs(t_stat), df = (length(y) -1), lower.tail = FALSE) # that random 2 cuz two-sided test.  (slide 25)
# print(p_value)

# p_value =< alpha ===> does not support H0. per Week2Slide20 

# verify with internal functions
# alternative hypothesis (Halpha) is that the true mean or population parameter is not equal to hypothesized value 100.
two_sided_hypothesis_test <- t.test(y, alternative = "two.sided", mu=100, conf.level = 0.95)
# print(two_sided_hypothesis_test)

##  --------

# H0 ---  mean_y =< mu = 100
# Halpha ------ mean_y > mu = 100
# one sided t-test
mu0 <- 100

# T_stat <- (mean_y  - 100)/ ( std_dev/ sqrt(n) ), degrees of freedom = (n-1)
t_stat <- (calc_mean_y  - mu0)/(standard_error)
# print(t_stat)

#?pt
one_sided_p_value <-  pt(t_stat, df = (length(y) -1), lower.tail = FALSE) #  one-sided test.  (slide 25) 
# print(one_sided_p_value)

# p_value > alpha = 0.05 ===> supports H0.  sample mean(IQ) =< than population.
# alternative hypothesis (Halpha) was that the true mean of the sample is greater than a hypothesized value 100.
# p_value does not support Halpha

# verify with internal functions
one_sided_hypothesis_test <- t.test(y, alternative = "greater", mu=100, conf.level = 0.95)
# print(one_sided_hypothesis_test)

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/expenditure.txt", header=T)

# explore the data
# print(expenditure)
str(expenditure)
typeof(expenditure)
dim(expenditure)
head(expenditure)

 
# plot(expenditure)

pdf("./problemSets/PS01/my_answers/Q2_Y_X1_X2_X3.pdf")
pairs(expenditure[,2:5], main = "examine relationships between Y, X1, X2, X3", col = expenditure$Region)
dev.off()


regression1 <- lm(X3~X1, data=expenditure)
# now save that output to a file that you can read in later to your answers
# make it easier for when we need to do this again, let's create a function
output_stargazer <- function(outputFile, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=TRUE)
}

output_stargazer("./problemSets/PS01/my_answers/regression_output_X3_X1.tex", regression1)

y <- expenditure$Y
x1 <- expenditure$X1
x2 <- expenditure$X2
x3 <- expenditure$X3

pdf("./problemSets/PS01/my_answers/Q2_prettier_Y_X1_X2_X3.pdf")

plot(x1, y, main="Per capita income vs. \n Expenditure on shelters by state", xlab="Income", ylab="Shelter expenditure", col="blue", cex=1)

plot(x2, y, main="Number of residents per 100k that are insecure vs. \n Per capita expenditure on shelters by state", 
     xlab="Number of residents per 100k that are insecure", ylab="Shelter expenditure", col="blue", cex=1)

plot(x3, y, main="Number of people per 1k residing in urban areas vs. \n Per capita expenditure on shelters by state", 
     xlab="Number of people per 1k residing in urban areas", ylab="Shelter expenditure", col="blue", cex=1)
dev.off()

pdf("./problemSets/PS01/my_answers/Q2_spending_by_region_colored.pdf")
ggplot(data = expenditure, aes(x=X1,y =Y, color=Region)) + geom_point() +
  labs(
    x = "Per capita personal income in state", 
    y = "Per capita expenditure on shelters/housing assistance in state", 
    colour = "regions",
    title = "Per capita personal income in state vs.\nPer capita expenditure on shelters/housing assistance in state ",
    subtitle = "Colored by region"
  )
dev.off()

pdf("./problemSets/PS01/my_answers/Q2_average_spending_by_region.pdf")

# bah, can't get the x axis grouping to number correctly.
# ggplot(data = expenditure, aes(group=Region,y =Y)) + geom_boxplot() +
#   labs(
#     x = "Region", 
#     y = "Per capita expenditure on shelters/housing assistance", 
#     title = "Region vs. Per capita expenditure on shelters/housing assistance",
#     subtitle = "Box Plot showing average per region"
#   )

region_names <- c("Northeast", "North Central", "South", "West")
boxplot(expenditure$Y ~ expenditure$Region, 
        names=region_names,
  main = "Region vs. Per capita expenditure on shelters/housing assistance",
  xlab = "Region",
  ylab = "Per capita expenditure on shelters/housing assistance")

dev.off()

pdf("./problemSets/PS01/my_answers/Q2_scatter_spending_by_region_colored.pdf")
plot(expenditure$X1, expenditure$Y, main="Per capita income vs Expenditure on shelters by state", xlab="Income", 
     ylab="Shelter expenditure", col= expenditure$Region, cex=1)

legend("topleft", 
       legend=region_names, 
       col=1:length(region_names), 
       pch=1)

dev.off()

regression2 <- lm(X1~Y, data=expenditure)
output_stargazer("./problemSets/PS01/my_answers/regression_output_X1_Y.tex", regression2)