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

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/incumbents_subset.csv")

# Question 1
# We are interested in knowing how the diﬀerence in campaign spending between incumbent
# and challenger aﬀects the incumbent’s vote share.
# 1. Run a regression where the outcome variable is voteshare and the explanatory variable
# is difflog.

vote_share_model <-lm(inc.sub$voteshare~inc.sub$difflog)

# 2. Make a scatterplot of the two variables and add the regression line.


pdf("./voteshare_by_difflog.pdf")
plot(inc.sub$difflog, inc.sub$voteshare, 
  main="Difference in Campaign Spending vs Incumbant Vote Share", 
  xlab="Difference in Spending", 
ylab="Incumbant Vote Share")
abline(lm(inc.sub$voteshare~inc.sub$difflog),col='blue')

dev.off()

#
# 3. Save the residuals of the model in a separate object.

vote_share_model_residuals <- vote_share_model$residuals

# 4. Write the prediction equation.
#
### Y_voteshare = unname(vote_share_model$coefficients[1]) + unname(vote_share_model$coefficients[2]) * X_difference

# 2
# Question 2
# We are interested in knowing how the diﬀerence between incumbent and challenger’s spend-
# ing and the vote share of the presidential candidate of the incumbent’s party are related.
# 1. Run a regression where the outcome variable is presvote and the explanatory variable
# is difflog.

pres_vote_model <-lm(inc.sub$presvote~inc.sub$difflog)
#
# 2. Make a scatterplot of the two variables and add the regression line.
#
pdf("./presvote_by_difflog.pdf")
plot(inc.sub$difflog, inc.sub$presvote, 
  main="Difference in Campaign Spending vs PresVote", 
  xlab="Difference in Spending", 
ylab="PresVote")
abline(lm(inc.sub$presvote~inc.sub$difflog),col='blue')
dev.off()

#
# 3. Save the residuals of the model in a separate object.
#
pres_vote_model_residuals <- pres_vote_model$residuals
#
# 4. Write the prediction equation.
#
### Y_presvote = unname(pre_vote_model$coefficients[1]) + unname(pres_vote_model$coefficients[2]) * X_difference
# 
# Question 3
# We are interested in knowing how the vote share of the presidential candidate of the incum-
# bent’s party is associated with the incumbent’s electoral success.
#
# 1. Run a regression where the outcome variable is voteshare and the explanatory variable
# is presvote.

vote_share_presvote_model <-lm(inc.sub$voteshare~inc.sub$presvote)
#
# 2. Make a scatterplot of the two variables and add the regression line.

pdf("./voteshare_by_presvote.pdf")
plot(inc.sub$presvote, inc.sub$voteshare, 
  main="PresVote vs Incumbant Vote Share", 
  xlab="PresVote", 
ylab="Incumbant Vote Share")
abline(lm(inc.sub$voteshare~inc.sub$presvote),col='blue')

dev.off()
#
# 3. Write the prediction equation.
# 
### Y_voteshare = unname(vote_share_presvote_model$coefficients[1]) + unname(vote_share_presvote_model$coefficients[2]) * X_presvote
#
# Question 4
# The residuals from part (a) tell us how much of the variation in voteshare is not explained
# by the diﬀerence in spending between incumbent and challenger. The residuals in part (b)
# tell us how much of the variation in presvote is not explained by the diﬀerence in spending
# between incumbent and challenger in the district.
#
# 1. Run a regression where the outcome variable is the residuals from Question 1 and the
# explanatory variable is the residuals from Question 2.
#
residuals_regression <-lm(vote_share_model_residuals~pres_vote_model_residuals)
#
# 2. Make a scatterplot of the two residuals and add the regression line.
pdf("./presvote_residuals_by_vote_share_residuals.pdf")
plot(pres_vote_model_residuals, vote_share_model_residuals, 
  main="PresVote residuals vs Incumbant Vote Share residuals", 
  xlab="PresVote residuals", 
ylab="Incumbant Vote Share residuals")
abline(lm(vote_share_model_residuals~pres_vote_model_residuals),col='blue')

dev.off()
#
# 3. Write the prediction equation.
#
### Y_voteshare_residual = unname(residuals_regression$coefficients[1]) + unname(residuals_regression$coefficients[2]) * X_presvote_residual
# 
# Question 5
# What if the incumbent’s vote share is aﬀected by both the president’s popularity and the
# diﬀerence in spending between incumbent and challenger?
#
# 1. Run a regression where the outcome variable is the incumbent’s voteshare and the
# explanatory variables are difflog and presvote.
#
vote_share_multiV_model <-lm(inc.sub$voteshare~inc.sub$difflog +inc.sub$presvote)
#
# 2. Write the prediction equation.
#
### Y_votesharedual = unname(vote_share_multiV_model$coefficients[1]) + unname(vote_share_multiV_model$coefficients[2]) * X_difflog + unname(vote_share_multiV_model$coefficients[3]) * X_presvote
#
# 3. What is it in this output that is identical to the output in Question 4? Why do you
# think this is the case?