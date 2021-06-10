#load relevant libraries:
library(splines)

#load data:
source('code_data.R')

#determine knot positions:
number.knots <- 4
degree <- 2
knots <- seq.int(from = 0, to = 1, length.out = number.knots + 2)[-c(1, number.knots + 2)]

model <- lm(y ~ bs(x, knots = knots, degree = degree), data = data)
summary(model)

# or by specifying the degrees of freedom: 
model <- lm(y ~ bs(x, df = number.knots + degree), data = data)
summary(model)