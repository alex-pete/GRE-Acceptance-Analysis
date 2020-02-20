library(Hmisc) # function describe()
library(pastecs)  #stat.desc()
library(psych)  #describe()   #describeBy()
library(doBy) #summaryBy()
library(dplyr)
attach(data)
library(tidyverse) # general
library(ggalt) # dumbbell plots
library(countrycode) # continent
library(rworldmap) # quick country-level heat maps
library(gridExtra) # plots
library(broom) # significant trends within countries


# Grad data set

dat <- read.csv("C:/Users/Samuel/Downloads/graduate-admissions (1)/Admission_Predict_Ver1.1.csv")
str(dat)
cor(dat)
pairs(dat)

dat = dat[,-1]
head(dat)
pc = princomp(dat, cor = TRUE)
summary(pc)
pc$loadings
biplot(pc, xlab = "First principal component",
       ylab = "Second principal component",
       main = "Biplot for Graduate Admissions")
#Imp: TOEFL; Research; GRE
# screeplot
screeplot(pc, col = "blue", pch = 16, 
          type = "lines", cex = 2, lwd = 2, main = " ")
str(dat)
fa.fit <- factanal(x=dat, factors=2, rotation="varimax", scores="regression")
fa.fit
#Imp: GRE; TOEFL; CGPA; Research
plot(fa.fit$loadings,type="n",main="factor loadings") # set up plot 
text(fa.fit$loadings,labels=names(dat),cex=.7) # add variable names
abline(h=0,v=0)


#                       MANOVA

man = manova(cbind(dat$GRE.Score, dat$TOEFL.Score, dat$University.Rating, dat$SOP, dat$LOR, dat$CGPA, dat$Research) ~ dat$Chance.of.Admit)
summary(man)
mean(man$residuals)
sd(man$residuals)
cor(dat)
??aov
var1 = manova(cbind(dat$GRE.Score, dat$Research, dat$TOEFL.Score)~ dat$Chance.of.Admit)
var1
pc$loadings

plot(dat$GRE.Score ~ dat$Chance.of.Admit)
plot(dat$Research ~dat$Chance.of.Admit)
plot(dat$TOEFL.Score~dat$Chance.of.Admit)
plot(dat$University.Rating~dat$Chance.of.Admit)



# VISUALIZATION

library(ggplot2)
ggplot(dat, aes(x=dat$Chance.of.Admit, y=dat$CGPA, color = dat$University.Rating))+
  geom_point() +
  labs(title = "Admission Based on College GPA and University Rank", x = "Chance of Admission", y = "College GPA", colour = "University Rank") +
  geom_smooth()


#colored by research
ggplot(dat, aes(x=dat$Chance.of.Admit, y=dat$CGPA, color = dat$Research))+
  geom_point() +
  labs(title = "Admission Based on College GPA and Research", x = "Chance of Admission", y = "College GPA", colour = "Research") +
  geom_smooth()

ggplot(dat, aes(x=dat$Chance.of.Admit, y=dat$GRE.Score, color = dat$Research))+
  geom_point() +
  labs(title = "Admission Based on GRE Score and Research", x = "Chance of Admission", y = "GRE Score", colour = "Research") +
  geom_smooth()
#by rank
ggplot(dat, aes(x=dat$Chance.of.Admit, y=dat$GRE.Score, color = dat$University.Rating))+
  geom_point() +
  labs(title = "Admission Based on GRE Score and University Rank", x = "Chance of Admission", y = "GRE Score", colour = "University Rank") +
  geom_smooth()
ggplot(dat, aes(x=dat$Chance.of.Admit, fill = dat$University.Rating, freq = FALSE)) +
  geom_histogram(binwidth = 0.01, color = "blue") +
  geom_density()

#RANDOM FOREST
set.seed(123)
library(randomForest)
library(tidyverse)
library(caret)
train.samples = dat$Chance.of.Admit %>%
  createDataPartition(p=0.8, list=FALSE)
train.dat = dat[train.samples,]
test.dat = dat[-train.samples,]
dat$

R.F. = randomForest(Chance.of.Admit ~ ., data = train.dat, ntree = 500, mtry = 3, importance = TRUE)
plot(R.F.)
#Predict on test
pred <- predict(R.F., train.dat) #Predictions on Test Set for each Tree
test.err = with(test.dat, mean( (test.dat$Chance.of.Admit - pred)^2)) #Mean Squared Test Error
str(test.dat$Chance.of.Admit)
test.err
str(pred)

#visual
ggplot(train.dat, aes(x=train.dat$Chance.of.Admit, y=pred, color = "pink"))+
  geom_point()+
  geom_smooth() +
  labs(title = "Predicted vs Actual", x = "Actual Chance of Admission", y="Predicted Chance of Admission", color = "")




head(pred)
summary(R.F.)
R.F.$importance
varImpPlot(R.F.)


lg.fit = glm(dat$Chance.of.Admit ~ dat$GRE.Score + dat$CGPA)
lg.fit
summary(lg.fit)
??glm
pairs(dat)
cor(dat)
attach(dat)


ld = lda(Chance.of.Admit ~ ., data = dat)
#predict model on test data
predictions = ld %>% predict(test.dat)
#model Accuracy
mean(predictions$class==test.dat$Chance.of.Admit)

ld$scaling

#Subset data into research and no-research
dat.re = subset(dat, Research == 1)
dat.nore = subset(dat, Research == 0)
#T-test of Chance of Admissions in dat.re vs dat.nore
t.test(x=dat.re$Chance.of.Admit, y=dat.nore$Chance.of.Admit, mu=0)




library(plyr)

# NEXT UP
dat.ov = subset(dat, Chance.of.Admit > 0.75)
dat2 = as.factor(dat.ov$University.Rating)
### Histogram of university ranking frequency of candidates with greater than .75 chance
ggplot(dat.ov, aes(x=University.Rating, fill=dat2))+
  geom_histogram(binwidth = 0.5) + 
  labs(title = "University Rank for Candidates Admission Chance > 0.75", x="University Rank", y="Number of Candidates", fill = "University Rank")

dat3 = as.factor(dat.ov$Research)
ggplot(dat.ov, aes(x=Research, fill=dat3))+
  geom_histogram(bins = 3)
