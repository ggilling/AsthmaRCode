newData.Master <- read.csv(file.choose(), header = TRUE)
summary(newData.Master)
respOnes <- grep("(breath)|(wh?eez)|(cough)|(lung)", newData.Master$text, ignore.case = TRUE)
newData.Master$Resp = rep(0, length(newData.Master$text))
for (x in respOnes){
newData.Master$Resp[x] <- 1 }
newData.Master$Resp
