# This script calculates the position of entrance height as percent of the nest.
# Kathryn Busby
# mkbusby@email.arizona.edu
# May 20, 2021

# Load libraries.

library(tidyverse)

# Bring in the file.

entcent <- read.csv(file="data/Entrances.csv")

# Clean up, rename columns.

entcent <- entcent %>%
  rename("tunnelup"=TunnelLengthUp.mm., "tunneldown"=TunnelLengthDown.mm.)

# Make a new column where the length up is depicted as a percentage of the total
# length.

entcent$percentup <- entcent$tunnelup/(entcent$tunnelup+entcent$tunneldown)*100

# Get rid of NAs in the new percent column.

entcent <- entcent[-which(is.na(entcent$percentup)),]

# Now we can see the average centeredness, sample size, and standard deviation.

percentup.mean <- mean(entcent$percentup)
percentup.sd <- sd(entcent$percentup)
percentup.n <- nrow(entcent)

# Output a note to store these results.

sink(file = paste0("Output/",Sys.Date(),"_EntranceCentering.txt"))

cat("The mean entrance hole is positioned up this percent of the tunnel:\n")
print(percentup.mean)
cat("The standard deviation of the above percentage is:\n")
print(percentup.sd)
cat("The sample size of stalks used for assessing entrance centeredness is:\n")
print(percentup.n)

sink()
