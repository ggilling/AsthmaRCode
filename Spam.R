Spam.Train.C1 <- asthma.tweets$Spam[1:853]
Spam.Test.C1 <- asthma.tweets$Spam[854:1138]

weights.Spam <- chi.squared (Spam.Train.C1 ~ ., data = asthma.Copy.Train)
print(weights)

weights.exist.Spam = subset(weights.Spam, weights.Spam$attr_importance >  0.001)
significantOnes.Spam <- cutoff.k(weights.Spam, length(weights.Spam$attr_importance))

f.Spam <- as.simple.formula(significantOnes.Spam, "Spam.Train.C1")
print(f.Spam)
Spam.Test.Predicted <- knn(asthma.Copy.Train, asthma.Copy.Test, as.factor(Spam.Train.C1))

(nnTable <- table("1-NN" = Spam.Test.Predicted, Self = Spam.Test.C1))

Accuracy <- sum(diag(nnTable))/nrow(asthma.Copy.Test)

ksvmTrain.Spam <- ksvm(Spam.Train.C1 ~ buy + http + tco9uqectfz + albuterol + greater + mano + remedi + con + natur + medicin + pharmaci + atuser + treatment , data = asthma.Copy.Train)
svmCl.Spam <- predict(ksvmTrain.Spam, asthma.Copy.Test)
svmTable.Spam <- table(Spam.Test.C1, svmCl.Spam)

##### MEDICATION ######

Medication.Train.C1 <- asthma.tweets$Medication[1:853]
Medication.Test.C1 <- asthma.tweets$Medication[854:1138]

weights.Medication <- chi.squared (Medication.Train.C1 ~ ., data = asthma.Copy.Train)
print(weights)

weights.exist.Medication = subset(weights.Medication, weights.Medication$attr_importance >  0.001)
significantOnes.Medication <- cutoff.k(weights.Medication, length(weights.Medication$attr_importance))

f.Medication <- as.simple.formula(significantOnes.Medication, "Medication.Train.C1")
print(f.Medication)
Medication.Test.Predicted <- knn(asthma.Copy.Train, asthma.Copy.Test, as.factor(Medication.Train.C1))

(nnTable <- table("1-NN" = Medication.Test.Predicted, Self = Medication.Test.C1))

Accuracy <- sum(diag(nnTable))/nrow(asthma.Copy.Test)
print(Accuracy)

ksvmTrain.Medication <- ksvm(Medication.Train.C1 ~ inhal + pharoahemonch + insomnian + speak + rememb + been + grade + sinc + pump + asthma + ive + sleep + asthm + need + gettin + has + http + asma , data = asthma.Copy.Train)
svmCl.Medication <- predict(ksvmTrain.Medication, asthma.Copy.Test)
svmTable.Medication <- table(Medication.Test.C1, svmCl.Medication)
print(svmTable.Medication)


#### random forest

library(party)
asthma.ctree <- ctree(f, data = asthma.Copy.Train)
table(Predicted = predict(asthma.ctree), Self.Train.C1)
table(Self.Train.C1)