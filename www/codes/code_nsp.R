#load relevant libraries:
library(splines)

#load data:
load("data.RData")

#determine knot positions: following the guidelines of [Hmisc::rcspline.eval()]
number.knots <- 4

outer <- if (number.knots > 3) 0.05 else 0.1 
if (number.knots > 6) {
  outer <- 0.025
}

p <- seq(outer, 1 - outer, length = number.knots)
all <- quantile(data$x, p, na.rm = TRUE)
Boundary.knots <- c(all[1], all[number.knots])
knots <- all[-c(1,number.knots)]

model <- lm(y ~ ns(x, knots = knots, Boundary.knots =  Boundary.knots), data = data)
summary(model)