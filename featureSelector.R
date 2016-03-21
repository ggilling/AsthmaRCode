#load a dataset and use it as the main source of data
library(mlbench)
# data(HouseVotes84)

#calculate weights for each atribute using some function
# weights <- SOME_FUNCTION(Class~., HouseVotes84)
weights <- chi.squared (Self.Train.C1 ~ ., data = notSparse.Train.wordMatrix)
print(weights)

#select a subset of 5 features with the lowest weight
significantOnes <- cutoff.k(weights, 50)

#print the results
f <- as.simple.formula(subset, "Class")
print(f)
