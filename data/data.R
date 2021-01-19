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

var_coefs <- function(DF, y, intercept){
  X <-cbind(1, DF$fp1, DF$fp2)
  XtX <- t(X)%*%X
  inv.XtX <- solve(XtX)
  sigmasq <- (1/nrow(DF))*sum((intercept+DF$fp-DF[,y])^2)
  var.beta <- sigmasq*inv.XtX
  return(var.beta)
}
se_fitted <- function(DF, y, intercept){
  var.beta <- var_coefs(DF, y, intercept)
  res <- var.beta[1,1]+var.beta[2,2]*DF$fp1*DF$fp1+var.beta[3,3]*DF$fp2*DF$fp2+2*var.beta[1,2]*DF$fp1+2*var.beta[1,3]*
    DF$fp2+2*var.beta[2,3]*DF$fp1*DF$fp2
  return(sqrt(res))
}

load(file="data/data_list.RData")

sample.sizes <- c("100" = 100, "1000" = 1000, "all" = NA)
gender <- list("Female"="female", "Male"="male", "Both"=c("female", "male"))


if(FALSE){
  data <- data_list$`Bmi~Age`$data
        
  x <- data[,"age"]
  pT <- fp.scale(x)
  transformed <- (x + pT$shift)/pT$scale
  
  pow1 <- 1
  fp1 <- transformed^pow1
  
  pow2 <- 1
  fp2 <- transformed^pow2*log(transformed)
  
  fp <- 10*fp1 + 5*fp2
  DF <- cbind(data, transformed, fp, fp1, fp2)
}






