### Script loads nhanes data and computes the max. R2 according to mfp fit and saves the data_list
### Run once a new variable pair is added
library(mfp)

load(file="data/nhanes_BP.Rdata")

data_list <- list("Bmi~Age" = list("data" = nhanes_BP[,c("ID", "age", "bmi", "gender")], 
                                   "x"="age", "y"="bmi", "x_unit" = "years")
                  )
sample.sizes <- c("1%" = 0.01, "10%" = 0.1, "50%" = 0.5, "100%" = 1)
gender <- list("Female"="female", "Male"="male", "Both"=c("female", "male"))

fp.scale <- function(x){  
  ### taken from package <mfp>
  ### 2008-06
  scale <- 1
  shift <- 0
  if (min(x) <= 0) {
    z <- diff(sort(x))
    shift <- min(z[z > 0]) - min(x)
    shift <- ceiling(shift * 10)/10
  }
  range <- mean(x + shift)
  scale <- 10^(sign(log10(range)) * round(abs(log10(range))))
  
  list(
    shift = shift,
    scale = scale
  )
}

if(!exists("input")){
  seed <- 14
} else {
  seed <- input$seed
}

save(data_list, file = "data/data_list.RData")
