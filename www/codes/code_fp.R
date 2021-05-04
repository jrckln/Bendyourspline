#load relevant libraries:
library(mfp)

#load data:
load("data.RData")

# Transform input data:
pT <- fp.scale(data$x) #fp.scale() determines which transformation is appropriate
transformed <- (data$x + pT$shift)/pT$scale #actual transformation

model <- mfp(x ~ fp(transformed, df=4, scale=F), data = data)
summary(model)
