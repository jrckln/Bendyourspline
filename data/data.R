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

quantInv <- function(distr, value) ecdf(distr)(value)

load("data/data_list.RData")

sample.sizes <- c("1%" = 0.01, "10%" = 0.1, "20%" = 0.2, "30%" = 0.3,  "50%" = 0.5, "75%"=0.75,  "100%" = 1)
gender <- list("Female"="female", "Male"="male", "Both"=c("female", "male"))

opt.intercept <- function(fitted, data, interval){
  f <- function(intercept,fitted,data) sum((intercept+fitted-data)^2)
  intercept.min <- optimize(f, interval, tol = 0.0001, fitted = fitted, data=data)
  return (intercept.min)
}

prismCodeBlock <- function(code) {
  tagList(
    HTML(paste("<pre><code class='language-r'>", code, "</code></pre>")),
    tags$script("Prism.highlightAll()")
  )
}

