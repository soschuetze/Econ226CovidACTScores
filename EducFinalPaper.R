setwd("/Users/sinclaireschuetze/Desktop/EducFinal")
adm2019 <- read.csv("adm2019.csv",header=TRUE)
adm2020 <- read.csv("adm2020.csv",header=TRUE)
ef2019 <- read.csv("ef2019a.csv",header=TRUE)
ef2020 <- read.csv("ef2020a.csv",header=TRUE)

data2019 <- merge(ef2019, adm2019, by = 'row.names', all = TRUE)
data2019$yr2019 <- 1
data2019$yr20 <- 0
head(data2019)
dim(data2019)
data2020 <- merge(ef2020, adm2020, by = 'row.names', all = TRUE)
data2020$yr2019 <- 0
data2020$yr20 <- 1
head(data2020)

dfCombined <- rbind(data2019, data2020)
head(dfCombined)

dfCombined <- dfCombined[complete.cases(dfCombined), ]
head(dfCombined)

dfCombined <- dfCombined[,-c(1,2,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53)]
head(dfCombined)

dfCombined <- dfCombined[,-c(29, 31, 33, 35, 37, 39,41,5153,55,57,59,61,63)]
head(dfCombined)

dfCombined <- dfCombined[,-c(35,36,37,38,39,40,41,42,43,44,46,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81,83,85)]
head(dfCombined)

dfCombined <- dfCombined[,-c(59,61,63,65,67)]
head(dfCombined)
dim(dfCombined)

dfACT <- dfCombined[,-c(63, 62, 61, 60, 58, 57, 56, 55, 54, 51, 52, 50)]
head(dfACT)

dfACT <- dfACT[,-c(1,2,3,4)]
head(dfACT)

dfACT[complete.cases(dfACT),]
dim(dfACT)
head(dfACT)

dfACT <- dfACT[,-c(1,2,5,6,8,9,11,12,14,15,17,18,20,21,23,24,25,26,27,28,29,30,31)]
head(dfACT)

dfACT <- dfACT[,-c(15,16,17,18,19,20,21,22)]
head(dfACT)

dfACT <- dfACT[,-c(17)]

alias(lm(ACTCM75~., data = dfACT))

library("car")
options(scipen=999)
lm <- lm(ACTCM75 ~., data = dfACT)
vif(lm, threshold = 10)

head(dfACT)
dfVIF1 <- dfACT[,-c(11)]
lm2 <- lm(ACTCM75 ~., data = dfVIF1)
vif(lm2, threshold = 10)

dfVIF2 <- dfVIF1[,-c(1)]
lm3 <- lm(ACTCM75 ~., data = dfVIF2)
vif(lm3, threshold = 10)

dfVIF3 <- dfVIF2[,-c(9)]
lm4 <- lm(ACTCM75 ~., data = dfVIF3)
vif(lm4, threshold = 10)

dfVIF4 <- dfVIF3[,-c(7)]
lm5 <- lm(ACTCM75 ~., data = dfVIF4)
vif(lm5, threshold = 10)

ACT75 <- dfVIF4[,-c(8)]
lm6 <- lm(ACTCM75 ~., data = ACT75)
vif(lm6, threshold = 10)

head(dfCombined)
ACT25 <- ACT75
head(ACT75)
ACT25 <- ACT25[,-c(11)]
ACT25$ACTCM25 <- dfCombined$ACTCM25
head(ACT25)
dim(ACT25)

ACT25$pctACT20 <- ACT25$yr20*ACT25$ACTPCT
ACT75$pctACT20 <- ACT75$yr20*ACT75$ACTPCT

finalModel75 <- lm(ACTCM75~., data = ACT75)
summary(finalModel75)

finalModel25 <- lm(ACTCM25~., data = ACT25)
summary(finalModel25)


