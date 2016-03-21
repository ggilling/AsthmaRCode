Sys.setenv(NOAWT=TRUE)
## Load textminer, dependency:
library(tm)
library(RWeka)
library(FSelector)
### REDEFINE AS.SIMPLE.FORMULA from Fselector
as.simple.formula <- function (attributes, class) 
{
    return(as.formula(paste((paste(class, paste(attributes, sep = "`", collapse = "` + `"), sep = " ~ `")), "", sep = "`")))
}

### READ IN FILE

newData.Master <- read.csv(file.choose(), header = TRUE)
summary(newData.Master)
## ADD RESP CATEGORY

respOnes <- grep("(breath)|(wh?eez)|(cough)|(lung)", newData.Master$text, ignore.case = TRUE)
newData.Master$Resp = rep(0, length(newData.Master$text))
for (x in respOnes){
newData.Master$Resp[x] <- 1 }
newData.Master$Resp


# PREPROCESS
cleaner <- function(theText){
	patterns = c("[:=].?)+", "[:=].?[(]+", "[[:punct:]]_[[:punct:]]+", "[:=].?s+", "[:=].?3+", "(&gt;)+" ,"(&lt;)+" , "[?]+" , "[!]+", "@", "#", "http:", "(&amp;)+" )

result = c(" SMILE ", " FROWN ", " FACE ", " CONFUSE ", " CATFACE ", " GREATER ", " LESSER " ," QUESTION ", " EXCLAIM ", " ATUSER ", " HASHTAG ", " HTTP ", " AND " )
for (i in 1:length(patterns)) {
	theText <- gsub(pattern = patterns[i], replacement = result[i], x = theText)	
}
return(theText)
}


cleanedText <- sapply(newData.Master$text, cleaner)
newData.Master$cleanedText = cleanedText





### MAKE A READER
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


### CLEAN IT


asthma.new.Copy.noPunct <- tm_map(asthma.new.Corpus, removePunctuation)
asthma.new.Copy.noPunct.stemmed <- tm_map(asthma.new.Copy.noPunct, stemDocument)

  BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
  asthmaBigram.dtm <- DocumentTermMatrix(asthma.new.Copy.noPunct.stemmed, control = list(tokenize = BigramTokenizer))

	####################
## Doc-Term this business:
asthma.new.Copy.docTerm <- DocumentTermMatrix(asthma.new.Copy.noPunct.stemmed)

## Get rid of sparse terms:

asthma.new.Copy.docTerm.notSparse <- removeSparseTerms(asthma.new.Copy.docTerm, sparse = 0.995)
asthmaBigram.notSparse <- removeSparseTerms(asthmaBigram.dtm, sparse = 0.998)
## Put it into a format we can actually use:
asthma.new.wordMatrix <- as.data.frame((as.matrix(asthma.new.Copy.docTerm.notSparse)))
asthma.new.bigramMatrix <- as.data.frame((as.matrix(asthmaBigram.notSparse)))

asthma.new.TrainingSet <- asthma.new.wordMatrix
asthma.bigram.TrainingSet <- asthma.new.bigramMatrix
asthma.Bigram.all <- as.data.frame(as.matrix(asthmaBigram.dtm))
asthma.new.all <- as.data.frame(as.matrix(asthma.new.Copy.docTerm))


classifications <- newData.Master[19:38]

extra_tags = c("retweeted", "retweet_count", "Resp")

for(tag in extra_tags){
#	asthma.new.TrainingSet[,tag] <- newData.Master[,tag]
#	asthma.bigram.TrainingSet[,tag] <- newData.Master[,tag]
	asthma.new.all[,tag] <- newData.Master[,tag]
#	asthma.Bigram.all[,tag] <- newData.Master[,tag]
}


makeZeroes <- function(x) {
	if (!is.na(x) & x == 1) {
		return(1)
	}
	else{ return(0) }
	
}

### MAKE CLASSIFICATIONS BE A THING

for (cat in names(classifications)) {
	
	classifications[,cat] <- as.factor(sapply(classifications[,cat], makeZeroes))
}
classifications$AllOthers = as.factor(ifelse(classifications$Family == 1 | classifications$Friend == 1 | classifications$Other == 1 | classifications$Unidentified == 1, 1, 0))

### LEARN WEIGHTS IF THEY MATTER TO YOU.
form <- as.simple.formula(names(asthma.new.TrainingSet), "classifications[,1]")

weights <- chi.squared (form , data = asthma.new.TrainingSet)
weights.exist = subset(weights, weights$attr_importance > 0.001)
signif.unigram <- cutoff.k(weights, length(weights.exist$attr_importance))
unigram.formula <- as.simple.formula(signif.unigram, "classifications[,1]")

form.bigram <- as.simple.formula(names(asthma.bigram.TrainingSet), "classifications[,1]")
weights.bigrams <- chi.squared(form.bigram, data = asthma.bigram.TrainingSet)
weights.bigrams.exist <- subset(weights.bigrams, weights.bigrams$attr_importance > 0.001)
signif.bigram <- cutoff.k(weights.bigrams, length(weights.bigrams.exist$attr_importance))
bigram.formula <- as.simple.formula(signif.bigram, "classifications[,1]")

### OTHERWISE YOU CAN JUST TRAIN YOUR MODELS

library(kernlab)


for (cat in names(classifications)) {
ksvmTrain <- ksvm (classifications[,cat] ~. , data = asthma.new.TrainingSet, cross = 10, na.action = na.omit)
#ksvmTrain <- ksvm(unigram.formula, data = asthma.new.TrainingSet, cross = 5, na.action = na.omit)
#fitted(ksvmTrain)
baseline = sum((as.integer(classifications[,cat]) - 1)/length(classifications[,cat]))
print (paste(cat, cross(ksvmTrain)," Baseline:", baseline, sep = " "))
print (paste("", cat, sep = "       "))
print (table("Predicted" = fitted(ksvmTrain),  classifications[,cat]))
}


for (cat in names(classifications)) {

ksvmBigramTrain <- ksvm (classifications[,"Self"] ~. , data = asthma.bigram.TrainingSet, cross = 10, na.action = na.omit)
#ksvmBigramTrain <- ksvm (bigram.formula , data = asthma.bigram.TrainingSet, cross = 10, na.action = na.omit)
#fitted(ksvmBigramTrain)
cross(ksvmBigramTrain)
table(fitted(ksvmBigramTrain), classifications[,"Self"])

baseline = sum((as.integer(classifications[,cat]) - 1)/length(classifications[,cat]))
print (paste(cat, cross(ksvmBigramTrain)," Baseline:", baseline, sep = " "))
print (paste("", cat, sep = "       "))
print (table("Predicted" = fitted(ksvmBigramTrain),  classifications[,cat]))
}

ksvmAllTrain <- ksvm(classifications[,"Self"] ~. , data = asthma.new.all, cross = 10, na.action = na.omit)