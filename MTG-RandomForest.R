
library(randomForest) # Used for the random forest
library(dplyr)        # Used to manipulate data frames

source('randomForestInterpreter.R')



# Variables ---------------------------------------------------------------

game_threshold <- .005                      # played in at least 0.5% of games (roughly 13 in 2660)
set.seed(1096)                              # Create a seed to make this reproducible
numTrees <- 500                             # how many trees do we want
data_file <- "data/output 2019-04-15.csv"   # The location of the data


# Data preparation --------------------------------------------------------

# Load the data
dat = read.csv(data_file, header = TRUE, stringsAsFactors=FALSE)

# Remove the id column
dat <- select(dat, -id)

# Replace NAs with 0s
dat[is.na(dat)] <- 0

# Replace blanks with 0s
dat <- data.frame(sapply(dat, sub, pattern="^$", replacement=0), stringsAsFactors=FALSE)

# Remove the set row and the color row
dat.only_data_rows <- dat[3:nrow(dat),]

# Remove cards that haven't been played much
dat.relevant_columns <- dat.only_data_rows %>%
  select(which(colMeans((. != 0)) > game_threshold))

# Convert all of the values to numbers
dat.numeric <- data.frame(sapply(dat.relevant_columns, as.numeric))

# Convert the winloss column to a factor
# Doing this is probably correct, but doesn't give a prediction score
#dat.numeric$winloss <- as.factor(dat.numeric$winloss)


# Perform random forest ---------------------------------------------------

rf = randomForest(winloss ~ ., data = dat.numeric, ntree = numTrees)


influences <- averageInfluenceOfForestFeature(rf)

for (i in 1:length(influences)) {
  print(paste(influences[[i]][[1]],influences[[i]][[2]]),collapse="\t")
}





