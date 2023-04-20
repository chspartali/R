diet <- read.csv("diet.csv")
str(diet)
questionnaire <- read.csv("questionnaire.csv")
str(questionnaire)
data.combined <- merge(diet, questionnaire, by.x='SEQN')
#making if..else statement

isDIABETIC <- function(PROTEIN){
if (is.na(PROTEIN) == TRUE) {
return(-1)
} else if (PROTEIN< 54) {
return (0)
} else if (PROTEIN >= 54) {
return (1)
}
}
#new value and reapet statement for all columns
diabetics <- NULL
for (i in 1:nrow(data.combined)) {
  diabetics<- c(diabetics, isDIABETIC(data.combined[i,"DR1TPROT"]))
}
#input diabetic to new data and deleting NA values
diet$diabetics<- as.factor(diabetics)
str(diet$diabetics)
diabetic_diet<- diet[,34:43]
diabetic_diet$diabetics<- as.factor(diabetics)
diabetic_diet<- na.omit(diabetic_diet)
#normalize arithmitical values
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))
#in put diabetic to new data and knn
diabetic_diet_new <- as.data.frame(lapply(diabetic_diet[,-11], normalize))
diabetic_diet_new$diabetics <- diabetic_diet [,11]
str(diabetic_diet_new)
diabetic_train<-diabetic_diet_new[1:8431,]
diabetic_test<- diabetic_diet_new[8431:8531,]
diabetic_train_label<- diabetic_diet_new[1:8431, 11]
diabetic_test_label<-diabetic_diet_new[8431:8531, 11]
library(class)
diabetic_test_pred<- knn(train= diabetic_train, test = diabetic_test,
                         cl=diabetic_train_label, k=5)
library(gmodels)
CrossTable(x= diabetic_test_label, y=diabetic_test_pred, prop.chisq=FALSE)

#CORRELATION
str(data.combined$DR1DRSTZ)
summary(data.combined$DR1DRSTZ)
summary(data.combined$HIQ011)
library(carData)
library(car)
b<-cov(data.combined$HIQ011,data.combined$DR1DRSTZ)/ (data.combined$HIQ011)
b

matrix(b)
plot(data.combined$HIQ011, data.combined$DR1DRSTZ, main="FODD-HEALTH INSURANCE", xlab="INSURANCE", ylab="FOOD 24H", pch=5)
abline(lm(data.combined$DR1DRSTZ~data.combined$HIQ011), col="red")
abline(lm(data.combined$DR1DRSTZ~data.combined$CBD090), col="green")
lines(lowess(data.combined$HIQ011,data.combined$DR1DRSTZ), col="blue")

# estimate alpha manually
a <- mean(data.combined$DR1DRSTZ) - b * mean(data.combined$HIQ011)
a

lx <- seq(1,2, length=9813)
ly <- a + b *lx
plot(lx,ly)
# calculate the correlation of launch data
r <- cov(data.combined$HIQ011,data.combined$DR1DRSTZ) /
  (sd(data.combined$HIQ011) * sd(data.combined$DR1DRSTZ))
r
cor(data.combined$HIQ011,data.combined$DR1DRSTZ)

r * (sd(data.combined$DR1DRSTZ) / sd(data.combined$HIQ011))
model <-lm(HIQ011~DR1DRSTZ, data = data.combined)
model
summary(model)

