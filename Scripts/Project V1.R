# Pipeline example:
# upload data -> create partition -> tunning parameters -> train model -> test and results


#Load data ####

complete.responses <- 
  read.csv('c://Users/riqui/Desktop/Ubiqum course/Project 4/Data Sets/CompleteResponses.csv')

#load libraries ####

library(mlbench)
library(caret)
library(C50)

#Understand the data  ####

isTRUE(is.na(complete.responses)) #no missing values
summary(complete.responses)
str(complete.responses)


#change the pertinent variables from integers to factors as they are labels ####
#also changing to acer and sony
complete.responses$brand <- factor(complete.responses$brand)
complete.responses$elevel <- factor(complete.responses$elevel)
complete.responses$car <- factor(complete.responses$car)
complete.responses$zipcode <- factor(complete.responses$zipcode)

complete.responses$brand<- sub(pattern = 0,
                         replacement = 'Acer', 
                         x = complete.responses$brand)

complete.responses$brand<- sub(pattern = 0,
                               replacement = 'Acer', 
                               x = complete.responses$brand)
complete.responses$brand<- sub(pattern = 1,
                               replacement = 'Sony', 
                               x = complete.responses$brand)
#set seed ####

set.seed(123)


#Feature selection ####

  #I am running a preliminary model to detect the important variables
  #This variables will get removed not to bias the model  

# intraining <- createDataPartition(complete.responses$brand,
#                                   p = 0.75,
#                                   list = FALSE)
# 
# training <- complete.responses[intraining,]
# testing <- complete.responses[-intraining,]
# 
# fitcontrol <-  trainControl(method = 'cv')
# 
# decission.tree.model <- train(brand ~ .,
#                               data =  training,
#                               method = 'rpart'
#                               )
#                               
# varImp(decission.tree.model)
# 
#   #Most important ones: 
#   #Salary
#   #age
#   #the rest of the results were negligible, but will add car for testing
#   #C50 model 
# 
#Defining testing and training samples ####


intraining <- createDataPartition(complete.responses$brand,
                                  p = 0.75,
                                  list = FALSE)

training <- complete.responses[intraining,]
testing <- complete.responses[-intraining,]


#C50 MODEL CREATION FOR TESTING ####

training<- training[,-c(3,5,4,6)]

fitcontrol<- trainControl(method = 'repeatedcv', number = 10, repeats = 1)


c5.model <- train(brand ~.,
                  data = training,
                  method = 'C5.0',
                  trcontrol = fitcontrol,
                  tunelenght = 2)

  #Chosen trial 20: Accuracy = 0.9113365 , Kappa = 0.811951
  #Adding elevel: Accuracy = 0.913496 , Kappa = 0.816506
  #Adding zip code: Accuracy = 0.913061 , Kappa = 0.815627
  # I have decided to run it with only age and salary given the
  #results as elevel did not add a relevant amount of accuracy

c5.predictions <- predict(c5.model,testing)

confusionMatrix(c5.predictions,testing$brand)

  #Results (test)
  #Accuracy : 0.9285  
  #Kappa: 0.8474    


# TRAINING RANDOM FOREST MODEL ####
  
fitcontrol2<- trainControl(method = 'repeatedcv', number = 10, repeats = 1)


rf.model<- train(brand~ .,
                 data = training,
                 model = 'rf',
                 tuneLength = 5,
                 trControl = fitcontrol2
                 )

  #Results (training)
  #Accuracy: 0.904767
  #Kappa: 0.797519

 

rf.predictions <- predict(rf.model,testing)

confusionMatrix(rf.predictions,testing$brand)

  #Results (test)
  #Accuracy: 0.9111
  #Kappa: 0.8117


  #C5.0 seems to be more effective for predicting with this data set. 


#Load incomplete survey  ####

incomplete.survey <- 
  read.csv('c://Users/riqui/Desktop/Ubiqum course/Project 4/Data Sets/SurveyIncomplete.csv')

#Prediction over incomplete survey ####

prediction.incomplete.survey <- predict(c5.model,incomplete.survey)


# Stupid post resample step ####

postResample(c5.predictions,testing$brand)

  #Results
  #Accuracy: 0.9284559
  #Kappa: 0.847363


# Summary of predicted results #### 

summary(prediction.incomplete.survey)

  #Acer 1834
  #Sony 3166
  #Prefered brand in prediction is Sony, similar results to the existing 
  #10k sample surveys


incomplete.survey$brand <- prediction.incomplete.survey

#Visuallizations ####

temp <- data.frame(x=incomplete.survey$brand)

ggplot(incomplete.survey, aes(x=brand, fill=brand))+ 
  geom_bar(color='black')+
  labs(title = 'Prediction by brand')+
  labs(y = 'Totals',
       x = 'Preferences')

ggplot(complete.all, aes(x=brand, fill=brand))+ 
  geom_bar(color='black')+
  labs(title = 'Prediction by brand')+
  labs(y = 'Totals',
       x = 'Preferences')


ggplot(Ages, aes(x=region,fill=region))+geom_bar(col="black")
