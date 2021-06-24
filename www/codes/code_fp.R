#load relevant libraries:
library(mfp)

#load data:
source('code_data.R')

model <- mfp(y~fp(x), data)

summary(model)
