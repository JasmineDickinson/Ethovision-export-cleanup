### Clear your workspace.

rm(list=ls()) # Removes list objects
graphics.off() #removes plots open

library(dplyr)
library(xlsx)

# Set the working directory specific to your computer. ctrl+shift+h on a mac shortcuts to bring up the menu
setwd("your working directory here")

# Import your excel file into a data frame. 
ethov_data <- read.xlsx2(file="Ethov_whole_session_sample.csv", 1, header = TRUE)

# Clean up the header titles. This will be specific to your file. New title on the leftside of the = sign old title on the right. This could all fit on one line but I aligned it to make it easier to see. Note, I didn't rename all of the headers, since some of them aren't important and I will cut them out in a later step.

ethov_data <- rename(ethov_data,
                     mouseid = Independent.Variable, 
                     test = Independent.Variable.1,
                     treatment = Independent.Variable.2,
                     distance_moved = Distance.moved,
                     velocity = Velocity,
                     total_duration_sec = In.zone.2.1,
                     neutralzone_frequency = In.zone.2.3,
                     neutralzone_duration = In.zone.2.4,
                     hotzone_frequency = In.zone.2.6,
                     hotzone_duration = In.zone.2.7,
                     inner_ring_frequency = In.zone.2.9,
                     inner_ring_duration = In.zone.2.10)

# Remove the first 3 rows because they're unnecessary
ethov_data <- ethov_data[-(1:3), ]  

# Subset your data frame to just the columns your relabeled/ the columns you want
ethov_data <- subset(ethov_data, select = c("mouseid", "test", "treatment", "distance_moved", "velocity", "total_duration_sec", "neutralzone_frequency", "neutralzone_duration", "hotzone_frequency", "hotzone_duration", "inner_ring_frequency", "inner_ring_duration"))

# Make all the numbers usable. There's something wonky with the ethovision export files, so this just cleans everything up and makes it work with with R. Make sure this includes all the columns with numeric data you wish to use
factorconvert <- function(f){as.numeric(levels(f))[f]}
ethov_data[,4:14] <- lapply(ethov_data[,4:12], factorconvert) # don't worry if you get an error that says "NAs introducted by coercion"


### Beyond this point, everything will need to be tailored to your specific data-set. But here are some commonly used things I do to organize things to my liking.


# Filter your master data frame into separate tests. This is absolutely necessary if you have multiple experiments in one file

test1 <- "test1"
test1_ethov <- filter(ethov_data, test %in% test1)

test2 <- "test2"
test2_ethov <- filter(ethov_data, test %in% test2)

# you could also filter by selecting everything that is not in your list
test2_again <- filter(ethov_data, !(test %in% test1))

# you can also make a loop to do this in one step if you have lots of different tests in one file.
tests <- unique(ethov_data$test)
for (i in 1:length(tests)){
  assign(paste0(tests[i]), data.frame(subset(ethov_data, test == tests[i])))
}

# exclude all mice that don't pass a certain threshold, such as a minimum distance or velocity
ethov_data_velocity <- subset(ethov_data, velocity > 1.90) 

# resetting the levels. For subsets from a master data frame, the meta data still reflects the original df. This can be a latent issue if the number of levels for a factor changes. But it can be easily reset. For example, in test1_ethov, there are 2 levels for the "test" column, even though there is actually only 1 level.

levels(test1_ethov$test) #check the levels
test1_ethov$test <- factor(test1_ethov$test)
levels(test1_ethov$test) #check the levels again

# arrange the data frame. For example, in test2_ethov, the row orders have saline and drug inter-mixed. It can be useful to arrange things so if you export it to an excel file, it's easy to copy and paste group values.
test2_ethov <- arrange(test2_ethov, treatment)

# you can also arrange multiple things in one line. The order of the columns you specify will denote the order in which they're arranged. ie, arrange by treatment type, then by mouseid
test2_ethov <- arrange(test2_ethov, treatment, mouseid)

# If you have two different ethovision export files you need to stitch together, you can do that with the "rbind" function. Both data frames need to have the same column headings.

ethov_data2 <- ethov_data #duplicate the original df as an example
stitched_df <- rbind(ethov_data, ethov_data2)

# Calculating percentages of time the animal did something. For example, for test1 the % of time the animal was in the hot zone out of a 300 second session
test1_ethov$perc_time <- (test1_ethov$hotzone_duration/300)*100

# Outputting to excel. This example shows how to fit multiple data frames into on excel file on multiple sheets. The filename has to be exactly the same

write.xlsx(x = ethov_data, file = "Ethovision_whole_data.xlsx", sheetName = "master sheet", row.names = FALSE)
write.xlsx(x = test1_ethov, file = "Ethovision_whole_data.xlsx", sheetName = "test1", row.names = FALSE, append=TRUE)
write.xlsx(x = test2_ethov, file = "Ethovision_whole_data.xlsx", sheetName = "test2", row.names = FALSE, append=TRUE)

