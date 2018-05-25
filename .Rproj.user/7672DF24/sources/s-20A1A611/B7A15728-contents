#import the dataset
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[2:3]

#fitting linear regressor into dataset
lin_reg = lm(formula = Salary ~ . , data = dataset)

#fitting polynomial regressor into dataset
dataset$Level_2 = dataset$Level^2
dataset$Level_3 = dataset$Level^3
dataset$Level_4 = dataset$Level^4
poly_reg = lm(formula = Salary ~ . ,data = dataset)

#visualising the linear regression results
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary), color = 'red')+
  geom_line(aes(x = dataset$Level, y = predict(lin_reg, newdata = dataset)), color = 'blue')+
  ggtitle('True or Bluff(linear regression)')+
  xlab('Level')+
  ylab('Salary')

#visualising the polynomial regression results
library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary), color = 'red')+
  geom_line(aes(x = x_grid, y = predict(poly_reg,
                                        newdata = data.frame(Level = x_grid,
                                                             Level_2 = x_grid^2,
                                                             Level_3 = x_grid^3,
                                                             Level_4 = x_grid^4))),
            colour = 'blue') +
  ggtitle('True or Bluff(linear regression)')+
  xlab('Level')+
  ylab('Salary')

#predicting a new result using linear regression
y_pred_linear_regression = predict(lin_reg, newdata = data.frame(Level = 6.5))

#predicting a new result using polynomial regression
y_pred_polynomial_regression = predict(poly_reg, newdata = data.frame(Level = 6.5,
                                               Level_2 = 6.5^2,
                                               Level_3 = 6.5^3,
                                               Level_4 = 6.5^4))