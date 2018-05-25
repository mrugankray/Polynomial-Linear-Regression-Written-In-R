#import the dataset
dataset = read.csv('50_Startups.csv')

#factoring categorical data 
dataset$State = factor(dataset$State,
                         levels = c('New York','California','Florida'),
                         labels = c(1,2,3))

#spliting the dataset into training set and test set
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit,SplitRatio = 0.8)
training_set = subset(dataset,split == TRUE)
test_set = subset(dataset,split ==FALSE)

#fitting regressor in training set
regressor = lm(formula = Profit ~ . , data = training_set)

#predicting the profit
y_pred = predict(regressor, newdata = test_set)

#backward elimination
regressor = lm(formula = Profit ~ R.D.Spend+Administration+Marketing.Spend+State, data = dataset)
summary(regressor)
#eliminating State
regressor = lm(formula = Profit ~ R.D.Spend+Administration+Marketing.Spend, data = dataset)
summary(regressor)
#eliminating Administration
regressor = lm(formula = Profit ~ R.D.Spend+Marketing.Spend, data = dataset)
summary(regressor)
#eliminating Marketing.Spend
regressor = lm(formula = Profit ~ R.D.Spend, data = dataset)
summary(regressor)
y_pred_opt = predict(regressor, newdata = test_set)
#visualising the training set results
ggplot() +
  geom_point(aes(x = training_set$R.D.Spend,y = training_set$Profit),
             color = 'red') +
  geom_line(aes(x = training_set$R.D.Spend,y = predict(regressor,newdata = training_set)),
            color = 'blue') +
  ggtitle('  R.D.Spend vs Profit(training set)') +
  xlab('R.D.Spend') +
  ylab('Profit')
#visualising the test set results
ggplot() +
  geom_point(aes(x = test_set$R.D.Spend,y = test_set$Profit),
             color = 'red') +
  geom_line(aes(x = training_set$R.D.Spend,y = predict(regressor,newdata = training_set)),
            color = 'blue') +
  ggtitle('  R.D.Spend vs Profit(test set)') +
  xlab('R.D.Spend') +
  ylab('Profit')