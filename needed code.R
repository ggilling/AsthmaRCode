library(tm)


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
Corpus.Train <- Corpus(DataframeSource(train), readerControl = list(reader = myReader))
Corpus.Test <- Corpus(DataframeSource(test), readerControl = list(reader = myReader))


self.tweets = subset(asthma.tweets, asthma.tweets$Self == 1)
Self.Corpus <- Corpus(DataframeSource(self.tweets), readerControl = list(reader = myReader))

asthma.docTerm <- DocumentTermMatrix(asthma.Corpus)
Train.docTerm <- asthma.docTerm[1:853]
Test.docTerm <- asthma.docTerm[854:1138]


Self.docTerm <- DocumentTermMatrix(Self.Corpus)


Train.docTerm <- DocumentTermMatrix(Corpus.Train)

Self.Train.C1 <- asthma.tweets$Self[1:853]
Self.Test.C1 <- asthma.tweets$Self[854:1138]

Self.Test.Predicted <- knn(Train.docTerm, Test.docTerm, as.factor(Self.Train.C1))

(nnTable <- table("1-NN" = Self.Test.Predicted, Self = Self.Test.C1))

Accuracy <- sum(diag(nnTable))/nrow(Self.Test.C1)



Train.wordMatrix = as.data.frame((as.matrix(Train.docTerm)) )
Test.wordMatrix = as.data.frame((as.matrix(Test.docTerm)))

### Find frequent terms in the Self ones:

findFreqTerms(Self.docTerm, 5)

### actually there's too much overlap
asthma.noPunct <- tm_map(asthma.Corpus, removePunctuation)
asthma.noPunct.docTerm <- DocumentTermMatrix(asthma.noPunct)

asthma.notSparse.docTerm <- removeSparseTerms(asthma.noPunct.docTerm, sparse = 0.995)
dim(asthma.notSparse.docTerm)
asthma.notSparse.Train <- asthma.notSparse.docTerm[1:853]
asthma.notSparse.Test <- asthma.notSparse.docTerm[854:1138]
notSparse.Train.wordMatrix <- as.data.frame((as.matrix(asthma.notSparse.Train)))
notSparse.Test.wordMatrix <- as.data.frame((as.matrix(asthma.notSparse.Test)))

### as.formula(paste('y', paste(names(mydata)[continuous], collapse='+'), sep='~'))
Self.Train.C1 <- as.factor(Self.Train.C1)
Self.Test.C1 <- as.factor(Self.Test.C1)

ksvmTrain <- ksvm(Self.Train.C1 ~ ., data = notSparse.Train.wordMatrix)
svmCl <- predict(ksvmTrain, notSparse.Test.wordMatrix)
svmTable <- table(Self.Test.C1, svmCL)

### Decision Tree:

### rpart (formula, data=, method=, control=)
asthma.nopunct.rpart <- rpart(Self.Train.C1 ~., data = noPunct.Train.wordMatrix)



## Naive Bayes

naiveAsthma.self <- naiveBayes(Self.Train.C1 ~ ., data = data.frame(inhaler = notSparse.Train.wordMatrix$inhaler,  dog = notSparse.Train.wordMatrix$dog), laplace = 0)

naiveAsthma.self.pred <- predict(naiveAsthma.self, newdata = data.frame(inhaler = notSparse.Test.wordMatrix$inhaler,  dog = notSparse.Test.wordMatrix$dog))

table(Actual = Self.Test.C1, Predicted = naiveAsthma.self.pred)

summary(data.frame(self = Self.Train.C1, inhaler = notSparse.Train.wordMatrix$inhaler,  dog = notSparse.Train.wordMatrix$dog))


#### This requires feature selection and a formula in f:

ksvmTrain <- ksvm(f, data = notSparse.Train.wordMatrix)
svmCl <- predict(ksvmTrain, notSparse.Test.wordMatrix)
svmTable <- table(Self.Test.C1, svmCl)

Accuracy <- sum(diag(svmTable))/nrow(test)
