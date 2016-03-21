## Required for stemmer to work:
Sys.setenv(NOAWT=TRUE)
## Load textminer, dependency:
library(tm)
library(RWeka)
library(FSelector)
## Generate mapping for Corpus reader
map.train <- list(FollowersCount = "user.followers_count", ListedCount = "user.listed_count",
FavouritesCount = "user.favourites_count", 
Author = "user.screen_name", 
Retweeted = "retweeted", 
RetweetCount = "retweet_count", 
Content = "cleanedText", 
Self = "Self", 
Family = "Family", 
Friend = "Friend", 
Other = "Other", 
Unidentified = "Unidentified", 
Complaint = "Complaint", 
Medication = "Medication", 
Symptoms = "Symptoms", 
Triggers = "Triggers", 
Sports = "Sports", 
Allergies = "Allergies", 
Needed.Intervention = "Needed.Intervention", 
Suggestions = "Suggestions",
News = "News",
Information = "Information", 
LostInhaler = "Lost_Inhaler", 
Joke = "Joke", 
Spam = "Spam", 
Questions = "Questions", 
NonEnglish = "Non.English", 
Future = "Future" 
)

## Make a reader
myReader <- readTabular(mapping = map.train)

## Put the Corpus into a variable
asthma.new.Corpus <- Corpus(DataframeSource(newData.Master), readerControl = list(reader = myReader)) 

#### PREPROCESSING
cleaner <- function(theText){
	patterns = c("[:=].?)+", "[:=].?[(]+", "[[:punct:]]_[[:punct:]]+", "[:=].?s+", "[:=].?3+", "(&gt;)+" ,"(&lt;)+" , "[?]+" , "[!]+", "@", "#", "http:", "(&amp;)+" )

result = c(" SMILE ", " FROWN ", " FACE ", " CONFUSE ", " CATFACE ", " GREATER ", " LESSER " ," QUESTION ", " EXCLAIM ", " ATUSER ", " HASHTAG ", " HTTP ", " AND " )
for (i in 1:length(patterns)) {
	theText <- gsub(pattern = patterns[i], replacement = result[i], x = theText)	
}
return(theText)
}



asthma.Copy <- asthma.Corpus
for (j in 1:length(asthma.Copy)){
	asthma.Copy[[j]] = tolower(asthma.Copy[[j]])
	for (i in 1:length(patterns)){
		asthma.Copy[[j]] <- gsub(patterns[i], result[i], asthma.Copy[[j]])
		}
	}



asthma.new.Copy.noPunct <- tm_map(asthma.new.Corpus, removePunctuation)
asthma.new.Copy.noPunct.stemmed <- tm_map(asthma.new.Copy.noPunct, stemDocument)

  BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
  asthmaBigram.dtm <- DocumentTermMatrix(asthma.new.Copy.noPunct.stemmed, control = list(tokenize = BigramTokenizer))

	####################
## Doc-Term this business:
asthma.new.Copy.docTerm <- DocumentTermMatrix(asthma.new.Copy.noPunct.stemmed)

## Get rid of sparse terms:

asthma.new.Copy.docTerm.notSparse <- removeSparseTerms(asthma.new.Copy.docTerm, sparse = 0.995)
asthmaBigram.notSparse <- removeSparseTerms(asthmaBigram.dtm, sparse = 0.995)

## Put it into a format we can actually use:
asthma.new.wordMatrix <- as.data.frame((as.matrix(asthma.new.Copy.docTerm.notSparse)))
asthma.new.bigramMatrix <- as.data.frame((as.matrix(asthmaBigram.notSparse)))


##### Set Training, Test subset data:
### FIX TO MAKE REFERENCE TO ABOVE....

asthma.new.TrainingSet <- asthma.new.wordMatrix
#asthma.Copy.Train <- asthma.Copy.docTerm.notSparse[1:853]
#asthma.Copy.Test <- asthma.Copy.docTerm.notSparse[854:1138]
#asthma.Copy.Train <- as.data.frame((as.matrix(asthma.Copy.Train)))
#asthma.Copy.Test <- as.data.frame((as.matrix(asthma.Copy.Test)))

extra_tags = c("retweeted", "retweet_count", "Resp")

for(tag in extra_tags){
	asthma.new.TrainingSet[,tag] <- newData.Master[,tag]
#	asthma.Copy.Train[,tag] = asthma.tweets[,tag][1:853]
#	asthma.Copy.Test[,tag] = asthma.tweets[,tag][854:1138]
}
asthma.Copy.Train$user.followers_count <- NULL
asthma.Copy.Train$user.listed_count <- NULL
asthma.Copy.Train$user.favourites_count <- NULL
asthma.Copy.Test$user.followers_count <- NULL
asthma.Copy.Test$user.listed_count <- NULL
asthma.Copy.Test$user.favourites_count <- NULL


########### Variables we want to predict:c
#"Self"                 
#[9] "Family"                "Friend"                "Other"                 "Unidentified"         
#[13] "Complaint"             "Medication"            "Symptoms"              "Triggers"             
#[17] "Sports"                "Allergies"             "Needed.Intervention"   "Suggestions"          
#[21] "Lost_Inhaler"          "Joke"                  "Spam"                  "Questions"            
#[25] "Non.English"           "Future"

classifications <- newData.Master[19:38]


#Column = "Symptoms"
#trainBounds = c(1, 853)
#testBounds = c(854, 1138)
#Train.C1 <- classifications[,Column][trainBounds[1]:trainBounds[2]]
#Test.C1 <- classifications[,Column][testBounds[1]:testBounds[2]]

#theData = asthma.Copy.Train
theData <- asthma.new.TrainingSet

#theTestSet = asthma.Copy.Test
#TrainingCategories = Train.C1
#TrueTestCategories = Test.C1
TrainingCategories <- classifications[,"Self"]

as.simple.formula <- function (attributes, class) 
{
    return(as.formula(paste((paste(class, paste(attributes, sep = "`", collapse = "` + `"), sep = " ~ `")), "", sep = "`")))
}


svmClassPredictions = data.frame(rep(0, length(TrueTestCategories)))
svmClassFeaturePredictions = data.frame(rep(0, length(TrueTestCategories)))
knnClassPredictions = data.frame(rep(0, length(TrueTestCategories)))
svmClassPredictions <- svmClassPredictions[-1]
svmClassFeaturePredictions <- svmClassFeaturePredictions[-1]
knnClassPredictions <- knnClassPredictions[-1]
for (Column in names(classifications)){
	print(paste("Running Statistics for ", Column, sep = ""))
#	Train.C1 <- classifications[,Column][trainBounds[1]:trainBounds[2]]
#	Test.C1 <- classifications[,Column][testBounds[1]:testBounds[2]]
#Train.C1 <- classifications$Spam[trainBounds[1]:trainBounds[2]]
#Test.C1 <- classifications$Spam[testBounds[1]:testBounds[2]]

#	Data = asthma.Copy.Train
#	Test = asthma.Copy.Test
#	TrainCats = Train.C1
#	True = Test.C1
	fun <- learnWeights(Data, TrainCats)
# fun <- as.simple.formula("http", "Train.C1")
	knnClassPredictions[,Column] <- runKNN(Data, TrainCats, Test, True)
	svmClassPredictions[,Column] <- runSVM(Data, TrainCats, Test, True)
	svmClassFeaturePredictions[,Column] <- runSVM(Data, TrainCats, Test, True, fun)
}

for(Column in names(classifications)){
	print(table(svmClassPredictions[,Column], classifications[,Column][testBounds[1]:testBounds[2]], dnn = c("svmPredictions", Column)))
}

for(Column in names(classifications)){
	print(table(svmClassFeaturePredictions[,Column], classifications[,Column][testBounds[1]:testBounds[2]], dnn = c("Features+svmPredictions", Column)))
}

for(Column in names(classifications)){
	print(table(knnClassPredictions[,Column], classifications[,Column][testBounds[1]:testBounds[2]], dnn = c("K-NearestNeighbor", Column)))
}


learnWeights<- function(theData, TrainingCategories){
	library(FSelector)
	weights <- chi.squared (TrainingCategories ~ ., data = theData)
	print(weights)

#select a subset of 5 features with the lowest weight
	weights.exist = subset(weights, weights$attr_importance > 0.001)
	print(weights.exist)
	if (length(weights.exist$attr_importance) > 0) {
		significantOnes <- cutoff.k(weights, length(weights.exist$attr_importance))
		print(significantOnes)
#print the results
		f <- as.simple.formula(significantOnes, "Train.C1")
		print(f)
	}
	else {f <- NULL}
	return(f) ## should this actually be significant ones?
}


runKNN <- function(theData, TrainingCategories, theTestSet, TrueTestCategories){
library(class)
Test.Predicted <- knn(theData, theTestSet, as.factor(TrainingCategories))

(nnTable <- table("1-NN" = Test.Predicted, Test = TrueTestCategories))
print(nnTable)
Accuracy.knn <- sum(diag(nnTable))/nrow(theTestSet)
print(Accuracy.knn)
	return(Test.Predicted)
}



runSVM <- function(theData, TrainingCategories, theTestSet, TrueTestCategories, funct = NULL){
	library(kernlab)
	if(is.null(funct) == FALSE){
		ksvmTrain <- ksvm(funct, data = theData)
	
	}
	else{
		ksvmTrain <- ksvm(TrainingCategories ~., data = theData)}
	svmCl <- predict(ksvmTrain, theTestSet)
	svmTable <- table(TrueTestCategories, svmCl)
	print(svmTable)
	Accuracy.svm <- sum(diag(svmTable))/nrow(theTestSet)
	print(Accuracy.svm)
	return(svmCl)
}

decTree <- function(TrainingCategories, theData){
	library(rpart)
	returnMe <- rpart <- rpart(TrainingCategories ~., data = theData)
	returnMe
}

library(klaR)
#NaiveBayesModel <- NaiveBayes(f, data = theData, laplace = 1)
NaiveBayesModel <- NaiveBayes(Train.C1 ~ http + attack + asthma + asma + got + had + now + 
    not + lesser + retweet_count + treatment, data = theData, laplace = 1)
NaiveBayesModel.pred <- predict(NaiveBayesModel, newdata = theTestSet)

NaiveTable <- table(Actual = TrueTestCategories, Predicted = NaiveBayesModel.pred$class)
Accuracy.Naive <- sum(diag(NaiveTable)/nrow(theTestSet))


# summary(data.frame(self = TrainingCategories, inhaler = notSparse.Train.wordMatrix$inhaler,  dog = notSparse.Train.wordMatrix$dog))




#### random forest
runDecTree <- function(f, theData, TrainingCategories){

	library(party)
	asthma.ctree <- ctree(f, data = theData)
	Table.ctree <- table(Predicted = predict(asthma.ctree), TrainingCategories)
	table(TrainingCategories)
	Accuracy.ctree <- sum(diag(Table.ctree))/nrow(theData)
	print(paste("Accuracy: ", Accuracy.ctree))
	asthma.ctree
}

