rm(list=ls()) # Removes list objects
graphics.off() #removes plots open

library(plyr)
library(dplyr)
library(xlsx)

# Set the working directory specific to your computer. ctrl+shift+h on a mac shortcuts to bring up the menu
setwd("your working directory here")

# Import your .xlsx file into a data frame. 
mouseID_test <- read.xlsx2(file="matchingIDs_sample.csv", 1, header = TRUE)


# Make the IDs factors instead of numeric, for the purposes of this example
mouseID_test$test_1 <- factor(mouseID_test$test_1)
mouseID_test$test_2 <- factor(mouseID_test$test_2)

# make list of unique mouse IDs for each test
t1_id <- unique(mouseID_test$test_1)
t2_id <- unique(mouseID_test$test_2)

# at this point we know some mice match across both tests, and some don't. Which mice are only present in one test?

# Define "outersect" function. This is better than just using setdiff, which is asymmetric
outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}

exclude <- outersect(t1_id, t2_id)
exclude # these are all the IDs that don't match up across test

# make new dataframe with just the IDs that match across both tests
test_match <- subset(mouseID_test, !(mouseID_test$test_1 %in% exclude))
