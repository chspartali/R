school<- read.csv("synthetic_school_enrollment_data.csv")
str(school)
#desiciontree

table(school$Biology.of.the.Cell)

# look at two characteristics of the loan
summary(school$Major)
summary(school$Minor)
subject <- lapply( school, factor) 


subject<-data.frame(sapply(subject, \(x) +as.logical(x)))

subjects<-subject[-1]
subjects<-subjects[-2]
subjects<-subjects[-1]

# split the data frames
subjects_train <- subjects[1:175, ]
subjects_test  <- subjects[176:250, ]
subjects_train_label<- subjects[1:175, 1]
subjects_test_label<-subjects[176:250, 1]

library(class)
subjects_test_pred<- knn(train= subjects_train, test = subjects_test,
                         cl=subjects_train_label, k=5)
library(gmodels)
CrossTable(x= subjects_test_label, y=subjects_test_pred, prop.chisq=FALSE)

str(subjects$Biology.of.the.Cell)

#ANN
library(neuralnet)

# simple ANN with only a single hidden neuron
set.seed(12345)
# to guarantee repeatable results
concrete_model <- neuralnet(formula = Genetics ~ Molecular.Biology + Evolution + Biochemistry +Neurobiology+Animal.Behavior,
                            data = subjects_train)

# visualize the network topology
plot(concrete_model)

## Step 4: Evaluating model performance ----
# obtain model results

model_results <- compute(concrete_model, subjects_test[2:7])
# obtain predicted strength values

predicted_sub <- model_results$net.result

# examine the correlation between predicted and actual values
cor(predicted_sub, subjects_test$Genetics)

## Step 5: Improving model performance ----
# a more complex neural network topology with 5 hidden neurons

set.seed(12345) 
# to guarantee repeatable results
concrete_model2 <- neuralnet(Genetics ~ Molecular.Biology + Evolution + Biochemistry +Neurobiology+Animal.Behavior,
                             data = subjects_train, hidden=5)

                             

# plot the network

plot(concrete_model2)

# evaluate the results as we did before

model_results2 <- compute(concrete_model2, subjects_test[2:7])
predicted_sub2 <- model_results2$net.result
cor(predicted_sub2, subjects_test$Genetics)





























