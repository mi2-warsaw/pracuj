setwd("~/Downloads/tabelezprzefiltrowanychdanych/")

df <- read.csv("prog_tag_skills_tableDS.csv")
df2 <- read.csv("prog_tag_skills_table.csv")

dfn1 <- colnames(df)
dfn2 <- colnames(df2)
wspolneKolumny <- intersect(dfn1, dfn2)


summary(df)

sort(colSums(df[,-1]))
round(100*sort(colMeans(df[,-1])), 1)

## Lasy losowe
library(randomForest)
df$Is_Programmist <- factor(df$Is_Programmist)

rf <- randomForest(Is_Programmist ~ ., data=df[,wspolneKolumny[-1]])
rf
varImpPlot(rf)

scory <- predict(rf, df2[,-2], type = "prob")

library(party)
tree <- ctree(Is_Programmist ~ ., data=df[,-1], controls = ctree_control(mincriterion = 0, minsplit = 1, minbucket = 1))
plot(tree)

library(ROCR)
pred <- prediction( scory[,2], df2$Is_Programmist)
perf <- performance(pred,"tpr","fpr")
plot(perf, colorize=T)

mojePredykcje <- ifelse(scory[,2] > 0.5, "To programista", "Nie jest programista")

table(mojePredykcje, df2$Is_Programmist)

## Regresja logistyczna

model <- glm(Is_Programmist ~ ., data=df[,wspolneKolumny[-1]], family = "binomial")
summary(model)

scoryGLM <- predict(model, df2[,-2], type = "response")

predGLM <- prediction( scoryGLM, df2$Is_Programmist)
perfGLM <- performance(predGLM,"tpr","fpr")
plot(perf, colorize=T)
plot(perfGLM, colorize=T, add=TRUE)

## Histogramy
hist(scoryGLM,100)
hist(scory,100)


indeksy <- which(scory[,2] > 0.5 & df2$Is_Programmist == "0")
df2[indeksy,1:5]
