# what you have to do:

# 1 - to lower
# 2 - replace smilies, ats, hashtags with SMILE; FROWN; also questionmarks http://'s
    	    # replace: \:.?\)+.* --> SMILE
	    	       # : \:.?\(+.* --> FROWN
		       # : \^\_\^.* --> SMILE
		       # : \:.?s+.* --> CONFUSE
		       # : \:.?3.* --> CATFACE
		       # : \.\_\..* --> FROWN
		       # : (\&gt\;)+ --> GREAT
		       # : (\&lt\;)+ --> LESS
		       # : \?+ --> QUESTION
		       # : \!+ --> EXCLAIM
		       # -----> Note: sometimes words have smiles attached. Make sure to put a space
		       # -----> all of the eyes can be equals signs
		       # : \&amp\; --> and
		       # : 
# 3 - stem?
# 4 - replace remaining punctuation
Sys.setenv(NOAWT=TRUE) 
library(tm)
library(RWeka)
map.train <- list(FollowersCount = "user.followers_count", ListedCount = "user.listed_count",
FavouritesCount = "user.favourites_count", 
Author = "user.screen_name", 
Retweeted = "retweeted", 
RetweetCount = "retweet_count", 
Content = "text", 
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
LostInhaler = "Lost_Inhaler", 
Joke = "Joke", 
Spam = "Spam", 
Questions = "Questions", 
NonEnglish = "Non.English", 
Future = "Future", 
Other.1 = "Other.1")

myReader <- readTabular(mapping = map.train)
asthma.Corpus <- Corpus(DataframeSource(asthma.tweets), readerControl = list(reader = myReader)) 

####

patterns = c("[:=].?)+", "[:=].?[(]+", "[[:punct:]]_[[:punct:]]+", "[:=].?s+", "[:=].?3+", "(&gt;)+" ,"(&lt;)+" , "[?]+" , "[!]+", "@", "#", "http:", "(&amp;)+" )

result = c(" SMILE ", " FROWN ", " FACE ", " CONFUSE ", " CATFACE ", " GREATER ", " LESSER " ," QUESTION ", " EXCLAIM ", " ATUSER ", " HASHTAG ", " HTTP ", " AND " )

 

asthma.Copy <- asthma.Corpus
for (j in 1:length(asthma.Copy)){
	asthma.Copy[[j]] = tolower(asthma.Copy[[j]])
	for (i in 1:length(patterns)){
		asthma.Copy[[j]] <- gsub(patterns[i], result[i], asthma.Copy[[j]])
		}
	}

asthma.Copy.noPunct <- tm_map(asthma.Copy, removePunctuation)
asthma.Copy.noPunct.stemmed <- tm_map(asthma.Copy.noPunct, stemDocument)

	####################

asthma.Copy.docTerm <- DocumentTermMatrix(asthma.Copy.noPunct.stemmed)

asthma.Copy.docTerm.notSparse <- removeSparseTerms(asthma.Copy.docTerm, sparse = 0.995)

asthma.Copy.wordMatrix <- as.data.frame((as.matrix(asthma.Copy.docTerm.notSparse)))

##### Set Training, Test subset data:

asthma.Copy.Train <- asthma.Copy.docTerm.notSparse[1:853]
asthma.Copy.Test <- asthma.Copy.docTerm.notSparse[854:1138]
asthma.Copy.Train <- as.data.frame((as.matrix(asthma.Copy.Train)))
asthma.Copy.Test <- as.data.frame((as.matrix(asthma.Copy.Test)))

###########
Self.Train.C1 <- asthma.tweets$Self[1:853]
Self.Test.C1 <- asthma.tweets$Self[854:1138]


weights <- chi.squared (Self.Train.C1 ~ ., data = asthma.Copy.Train)
print(weights)

#select a subset of 5 features with the lowest weight
significantOnes <- cutoff.k(weights, 16)
weights.exist.Spam = subset(weights, weights$attr_importance > 0.001)
#print the results
f <- as.simple.formula(significantOnes, "Self.Train.C1")
print(f)


library(class)
Self.Test.Predicted <- knn(asthma.Copy.Train, asthma.Copy.Test, as.factor(Self.Train.C1))

(nnTable <- table("1-NN" = Self.Test.Predicted, Self = Self.Test.C1))

Accuracy.knn <- sum(diag(nnTable))/nrow(asthma.Copy.Test)
print(Accuracy.knn)

library(kernlab)
ksvmTrain <- ksvm(Self.Train.C1 ~ ., data = asthma.Copy.Train)
svmCl <- predict(ksvmTrain, asthma.Copy.Test)
svmTable <- table(Self.Test.C1, svmCl)
Accuracy.svm <- sum(diag(svmTable))/nrow(asthma.Copy.Test)
print(Accuracy.svm)



library(rpart)
asthma.rpart <- rpart(Self.Train.C1 ~., data = asthma.Copy.Train)


library(klaR)
naiveAsthma.self <- NaiveBayes(Self.Train.C1 ~ http + attack + asthma + asma + got + had + now + 
    not + lesser + treatment, data = asthma.Copy.Train, laplace = 1)

naiveAsthma.self.pred <- predict(naiveAsthma.self, newdata = asthma.Copy.Test)

table(Actual = Self.Test.C1, Predicted = naiveAsthma.self.pred$class)



summary(data.frame(self = Self.Train.C1, inhaler = notSparse.Train.wordMatrix$inhaler,  dog = notSparse.Train.wordMatrix$dog))




#### random forest

library(party)
asthma.ctree <- ctree(f, data = asthma.Copy.Train)
table(Predicted = predict(asthma.ctree), Self.Train.C1)
table(Self.Train.C1)

