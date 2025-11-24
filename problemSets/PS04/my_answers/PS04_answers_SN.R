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
lapply(c("car"),  pkgTest)

detachAllPackages()
library(car)
data(Prestige)
help(Prestige)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Question 1: Economics
# We would like to study whether individuals with higher levels of income have more prestigious
# jobs. Moreover, we would like to study whether professionals have more prestigious jobs than
# blue and white collar workers.

# (a) Create a new variable professional by recoding the variable type so that professionals
# are coded as 1, and blue and white collar workers are coded as 0 (Hint: ifelse).

Prestige$professional <- ifelse(as.character(Prestige$type)=="prof", 1, 0)


# (b) Run a linear model with prestige as an outcome and income, professional, and the
# interaction of the two as predictors (Note: this is a continuous × dummy interaction.)

prestige_income_professional_model <- lm(prestige~income + 
     professional + 
     income:professional,
   data=Prestige)

# (c) Write the prediction equation based on the result.

# Y_Prestige = prestige_income_professional_model$coefficients[1] +
#             prestige_income_professional_model$coefficients[2] * X_income +
#             prestige_income_professional_model$coefficients[3] * X_professional +
#             prestige_income_professional_model$coefficients[4] * X_inc_prof_interaction +
#             error_term

# (d) Interpret the coeﬃcient for income.
#
# (e) Interpret the coeﬃcient for professional.
#
# (f) What is the eﬀect of a $1,000 increase in income on prestige score for professional
# occupations? In other words, we are interested in the marginal eﬀect of income when
# the variable professional takes the value of 1. Calculate the change in ˆ y associated
# with a $1,000 increase in income based on your answer for (c).
#
# (g) What is the eﬀect of changing one’s occupations from non-professional to professional
# when her income is $6,000? We are interested in the marginal eﬀect of professional
# jobs when the variable income takes the value of 6,000. Calculate the change in ˆ y
# based on your answer for (c).


# Question 2: Political Science
# Researchers are interested in learning the eﬀect of all of those yard signs on voting prefer-
# ences.1 Working with a campaign in Fairfax County, Virginia, 131 precincts were randomly
# divided into a treatment and control group. In 30 precincts, signs were posted around the
# precinct that read, “For Sale: Terry McAuliﬀe. Don’t Sellout Virgina on November 5.”
# Below is the result of a regression with two variables and a constant. The dependent
# variable is the proportion of the vote that went to McAuliﬀ’s opponent Ken Cuccinelli. The
# first variable indicates whether a precinct was randomly assigned to have the sign against
# McAuliﬀe posted. The second variable indicates a precinct that was adjacent to a precinct
# in the treatment group (since people in those precincts might be exposed to the signs).
#
# 1Donald P. Green, Jonathan S. Krasno, Alexander Coppock, Benjamin D. Farrer, Brandon Lenoir, Joshua
# N. Zingher. 2016. “The eﬀects of lawn signs on vote outcomes: Results from four randomized field experi-
# ments.” Electoral Studies 41: 143-150.
#
# (a) Use the results from a linear regression to determine whether having these yard signs
# in a precinct aﬀects vote share (e.g., conduct a hypothesis test with α=
# .05).


# (b) Use the results to determine whether being next to precincts with these yard signs
# aﬀects vote share (e.g., conduct a hypothesis test with α=
# .05).

# (c) Interpret the coeﬃcient for the constant term substantively.

# (d) Evaluate the model fit for this regression. What does this tell us about the importance
# of yard signs versus other factors that are not modeled?

