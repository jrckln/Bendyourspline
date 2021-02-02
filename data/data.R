load(file="data/data_list.RData")

sample.sizes <- c("100" = 100, "1000" = 1000, "all" = NA)
gender <- list("Female"="female", "Male"="male", "Both"=c("female", "male"))

opt.intercept <- function(fitted, data, interval){
  f <- function(intercept,fitted,data) sum((intercept+fitted-data)^2)
  intercept.min <- optimize(f, interval, tol = 0.0001, fitted = fitted, data=data)
  return (intercept.min)
}