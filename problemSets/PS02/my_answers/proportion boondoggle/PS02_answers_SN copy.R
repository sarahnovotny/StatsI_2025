#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list = ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c(
    "package:stats",
    "package:graphics",
    "package:grDevices",
    "package:utils",
    "package:datasets",
    "package:methods",
    "package:base"
  )
  package.list <- search()[ifelse(unlist(gregexpr("package:", search())) ==
                                    1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list) > 0)
    for (package in package.list)
      detach(package, character.only = TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}


# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(), pkgTest)

#add my libraries
#install.packages("ggplot2")
library("ggplot2")
library(stargazer)



#Problem Set 2

# Question 1: Political Science
# The following table was created using the data from a study run in a major Latin American
# city.1 As part of the experimental treatment in the study, one employee of the research
# team was chosen to make illegal left turns across traffic to draw the attention of the police
# officers on shift. Two employee drivers were upper class, two were lower class drivers, and the
# identity of the driver was randomly assigned per encounter. The researchers were interested
# in whether officers were more or less likely to solicit a bribe from drivers depending on their
# class (officers use phrases like, “We can solve this the easy way” to draw a bribe). The table
# below shows the resulting data.

bribery_frequency_data <- matrix (c(14, 6, 7, 7, 7, 1), nrow = 2, dimnames = list(
  c("Upper Class", "Lower Class"),
  c("Not Stopped", "Bribe requested", "Stopped/Given Warning")
))

# (a) Calculate the χ2 test statistic by hand/manually (even better if you can do ”by hand”
# in R).

#   # for every element, calculate the row total x column total / grand total observations
#   # return a matrix
# i know there's a way to do this with matrices or loops and functions, but i'm not quick enough with it.
# so, create two vectors and manually build a matrix.

observed_row_sum <- rowSums(bribery_frequency_data)
observed_column_sum <- colSums(bribery_frequency_data)
observed_total_sum <- sum(bribery_frequency_data)

bribery_frequency_expected <- matrix(c ((observed_row_sum[1] * observed_column_sum[1] /
                                           observed_total_sum),
                                        (observed_row_sum[2] * observed_column_sum[1] /
                                           observed_total_sum),
                                        (observed_row_sum[1] * observed_column_sum[2] /
                                           observed_total_sum),
                                        (observed_row_sum[2] * observed_column_sum[2] /
                                           observed_total_sum),
                                        (observed_row_sum[1] * observed_column_sum[3] /
                                           observed_total_sum),
                                        (observed_row_sum[2] * observed_column_sum[3] /
                                           observed_total_sum)
), nrow = 2, dimnames = list(
  c("Upper Class", "Lower Class"),
  c("Not Stopped", "Bribe requested", "Stopped/Given Warning")
))

#calculate the chi^2 test statistic manually
# chi^2 = sum of ( (frequency observed - frequency expected)^2 divided by frequency expected )

chi_sq_numerator <- (bribery_frequency_data - bribery_frequency_expected)^2
chi_sq_denominator <- bribery_frequency_expected

chi_sq_bribery_frequency_data <- sum(chi_sq_numerator / chi_sq_denominator)

? chisq.test

x <- chisq.test(bribery_frequency_data)

# (b) Now calculate the p-value from the test statistic you just created (in R).2 What do you
# conclude if α= 0.1?

pchisq(chi_sq_bribery_frequency_data,
       df = 2,
       lower.tail = FALSE)

x$p.value

# (c) Calculate the standardized residuals for each cell and put them in the table below.
# similarly here, definitely a better way to do this with matrices and or functions.
row_proportion <- (observed_row_sum / observed_total_sum)
column_proportion <- (observed_column_sum / observed_total_sum)

std_error_bribery_frequency <- matrix(
  c(
    sqrt(
      bribery_frequency_expected[1, 1] * (1 - row_proportion[1]) * (1 - column_proportion[1])
    ),
    sqrt(
      bribery_frequency_expected[2, 1] * (1 - row_proportion[2]) * (1 - column_proportion[1])
    ),
    sqrt(
      bribery_frequency_expected[1, 2] * (1 - row_proportion[1]) * (1 - column_proportion[2])
    ),
    sqrt(
      bribery_frequency_expected[2, 2] * (1 - row_proportion[2]) * (1 - column_proportion[2])
    ),
    sqrt(
      bribery_frequency_expected[1, 3] * (1 - row_proportion[1]) * (1 - column_proportion[3])
    ),
    sqrt(
      bribery_frequency_expected[2, 3] * (1 - row_proportion[2]) * (1 - column_proportion[3])
    )
  ),
  nrow = 2,
  dimnames = list(
    c("Upper Class", "Lower Class"),
    c("Not Stopped", "Bribe requested", "Stopped/Given Warning")
  )
)


standardized_residual_bribery_frequency <- (bribery_frequency_data - bribery_frequency_expected) /
  std_error_bribery_frequency

x$stdres
#woooo they match

# (d) How might the standardized residuals help you interpret the results?


# Question 2: Economics
# Chattopadhyay and Duflo were interested in whether women promote different policies than
# men. Answering this question with observational data is pretty difficult due to potential
# confounding problems (e.g. the districts that choose female politicians are likely to system-
# atically differ in other aspects too). Hence, they exploit a randomized policy experiment in
# India, where since the mid-1990s, 1/3 of village council heads have been randomly reserved
# for women. A subset of the data from West Bengal can be found at the following link:
#   https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv
# Each observation in the data set represents a village and there are two villages associated
# with one GP (i.e. a level of government is called ”GP”). Figure 1 below shows the names and
# descriptions of the variables in the dataset. The authors hypothesize that female politicians
# are more likely to support policies female voters want. Researchers found that more women
# complain about the quality of drinking water than men. You need to estimate the effect
# of the reservation policy on the number of new or repaired drinking water facilities in the
# villages.

?read.csv2
west_bengal_chatt_duflo_data <- read.csv(url("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv") )
str(west_bengal_chatt_duflo_data)
west_bengal_chatt_duflo_data$female <- factor(west_bengal_chatt_duflo_data$female)
west_bengal_chatt_duflo_data_female <- subset(west_bengal_chatt_duflo_data, female ==1)
west_bengal_chatt_duflo_data_male <- subset(west_bengal_chatt_duflo_data, female ==0)

# function for formatting the regression table
output_stargazer <- function(outputFile, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=TRUE)
}


# (a) State a null and alternative (two-tailed) hypothesis.

# H0 - the proportion of water to irrigation repair remains the same 
# regardless of whether or not there were reserved seats
# Ha - the proportion of water to irrigation repair increases where 
# there are reserved seats for women 

# (b) Run a bivariate regression to test this hypothesis in R (include your code!).

regression <- lm(water~irrigation,  data=west_bengal_chatt_duflo_data)
regression_female <- lm(water~irrigation,  data=west_bengal_chatt_duflo_data_female)
regression_male <- lm(water~irrigation,  data=west_bengal_chatt_duflo_data_male)
?lm
output_stargazer("./problemSets/PS02/my_answers/regression_output_irrigation_water.tex", regression)
output_stargazer("./problemSets/PS02/my_answers/regression_output_irrigation_water_female.tex", regression_female)
output_stargazer("./problemSets/PS02/my_answers/regression_output_irrigation_water_male.tex", regression_male)



pdf("./problemSets/PS02/my_answers/irrigation_vs_water_colored.pdf")
ggplot(data = west_bengal_chatt_duflo_data, aes(x=irrigation,y =water, color=female)) + geom_point() + 
         scale_color_discrete(labels = c("M", "F")) +
  labs(
    x = "New or Repaired Irrigation Facilities", 
    y = "New or Repaired Water Facilities", 
    colour = "Gender",
    title = "New or Repaired Irrigation Facilities vs. Water Facilities ",
    subtitle = "Colored by Leader's Gender"
  )
dev.off()


# (c) Interpret the coefficient estimate for reservation policy.
