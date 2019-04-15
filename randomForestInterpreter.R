

# Libraries ---------------------------------------------------------------

library(randomForest) # Used for the random forest



# averageInfluenceOfForestFeature -----------------------------------------

# This function takes a random forest object (from randomForest) and loops
# through all the trees and all the features to identify the average
# influence of each feature. It returns a list of features and their
# average influence.
averageInfluenceOfForestFeature <- function (rf) {
  allFeatures <- list()
  
  for (i in 1:rf$ntree) {
    newFeatures <- influenceOfEachTreeFeature(rf, i)
    for (row in 1:length(newFeatures)) {
      allFeatures <- nestedListAppendToRow(allFeatures, newFeatures[[row]][[1]], 1, newFeatures[[row]][[2]], 2)
    }
  }
  
  outputFeatures <- list()
  for (i in 1:length(allFeatures)) {
    feature <- allFeatures[[i]][[1]]
    sum = 0
    count = 0
    for (j in 1:length(allFeatures[[i]][[2]])) {
      if (length(allFeatures[[i]][[2]][[j]]) > 0) {
        for (k in 1:length(allFeatures[[i]][[2]][[j]])) {
          sum = sum + allFeatures[[i]][[2]][[j]][[k]]
          count = count + 1
        }
      } else {
        sum = sum + allFeatures[[i]][[2]][[j]]
        count = count + 1
      }
    }
    outputFeatures[[length(outputFeatures)+1]] <- list(feature,sum/count)
  }
  return(outputFeatures)
}


# influenceOfEachTreeFeature ----------------------------------------------

# This function takes a random forest object (from randomForest) and an 
# integer representing the tree in the randomForest. It identifies the
# features of the tree and their changes in prediction when increased.
# Since we are only interested in what happens when a feature increases,
# we only look at the right daughter of each node.
# The function returns a nest list containing each feature and a list
# changes at each node in the tree.
influenceOfEachTreeFeature <- function (rf, k) {
  tree <- getTree(rf, k=k, labelVar=TRUE)
  features = list()
  for (row in 1:nrow(tree)) {
    feature <- toString(tree[row, 'split var'])
    current_value <- tree[row, 'prediction']
    if (feature != 'NA') {
      next_node_right <- tree[row, 'right daughter']
      next_value_right <- tree[next_node_right, 'prediction']
      change <- next_value_right - current_value
      features <- nestedListAppendToRow(features, feature, 1, change, 2)
    }
  }
  return(features)
}



# nestedListAppendToRow ---------------------------------------------------

# This function takes a nested list that contains a list as one of the 
# cells in the nested list and appends a value to that list in the cell. 
# For example:
# [[feature1,values1(1,2,3)],[feature2,values2(4,5,6)]]
#
# The function returns an update list.
nestedListAppendToRow <- function (nestedList, rowName, rowNameCol, value, valueCol) {
  matchingRow = FALSE
  if (length(nestedList) > 0) {
    for (row in 1:length(nestedList)) {
      if (nestedList[[row]][rowNameCol] == rowName) {
        matchingRow = row
        break
      }
    }
  }
  if (matchingRow != FALSE) {
    nestedList[[matchingRow]][[valueCol]] = append(nestedList[[matchingRow]][[valueCol]],value)
  } else {
    nestedList[[length(nestedList)+1]] <- list(rowName,list(value))
  }
  return(nestedList)
}



