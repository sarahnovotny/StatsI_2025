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

bribery_frequency_data <- matrix (c(14, 6, 7, 7, 7, 1), nrow = 2, dimnames = list(
  c("Upper Class", "Lower Class"),
  c("Not Stopped", "Bribe requested", "Stopped/Given Warning")
))

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

numerator <- (bribery_frequency_data - bribery_frequency_expected)^2
denominator <- bribery_frequency_expected

chi_sq_bribery_frequency_data <- sum(numerator / denominator)

? chisq.test

x <- chisq.test(bribery_frequency_data)

pchisq(chi_sq_bribery_frequency_data,
       df = 2,
       lower.tail = FALSE)

#similarly here, definitely a better way to do this with matrices and or functions.
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
