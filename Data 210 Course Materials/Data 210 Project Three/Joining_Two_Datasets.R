library(tidyverse)

setwd("~/Dropbox/Great Bay ALL/GBCC Data 210/Data Files/Hospital Compare")

d1 <- read_csv("Complications_and_Deaths_-_Hospital.csv")

unique(d1$`Measure ID`)

# For the purposes of this project we are only interested in data that has to do with
# mortality rates. When we see the list of unique values associated with Measure ID,
# we can eliminate anything starting with PSI or COMP
#
# One way to do this is to create a logical vector that is TRUE if the Measure ID has MORT in it
# FALSE if the Measure ID does not have MORT in it

is_mortality <- grepl("MORT(.*)", d1$'Measure ID')

# Now attach that vector to our dataset

d1 <- cbind(is_mortality, d1)

# Notice the number of variables in our dataset just increased from 19 to 20
# Now we will extract only the rows of data for which is_mortality = TRUE

d1 <- d1[d1$is_mortality == TRUE,]

# Since this dataset is going to be "pasted" to the hospital information data, we have several columns
# of redundant or superfluous information. Let's start by reducing the number of columns to only the data
# that we need, and that will tie each measure to a specific provider (provider ID)
# For comparison sake, we will use mortality score for each provider and ONLY the score

d1 <- d1 %>% janitor::clean_names()
d1 <- as.data.frame(cbind(d1$provider_id, d1$measure_id, d1$score,
            d1$measure_start_date, d1$measure_end_date))
# The names of variables got messed with. Fixing that.

names(d1) <- c("provider_id", "measure_id", "score", 
               "measure_start_date", "measure_end_date")

d2 <- group_by(d1, provider_id) %>% 
  spread(measure_id, score) 

# Now compare this with the hospital information data and "join" the two data frames together
# Note that variable data types can be coerced to a specific type

d3 <- read_csv("Hospital_General_Information.csv")
d3 <- d3 %>% janitor::clean_names()
d4 <- full_join(d2,d3,by="provider_id")


