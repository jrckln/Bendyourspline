### Script loads nhanes data and computes the max. R2 according to mfp fit and saves the data_list
### Run once a new variable pair is added

load(file="data/nhanes_BP.Rdata")

data_list <- list("Bmi~Age" = list("data" = nhanes_BP[,c("ID", "age", "bmi", "gender")], 
                                   "x"="age", "y"="bmi", "x_unit" = "years"))
sample.sizes <- c("100" = 100, "1000" = 1000, "all" = NA)
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

# FP max R2 calc: 
for(dataset in names(data_list)){
    var_list <- data_list[[dataset]]
    data_list[[dataset]]$fittedR2 <- list()
    for(sample_size in sample.sizes){
        for(sex in gender){
            data <- var_list$data[var_list$data[,"gender"] %in% sex,]
            if(is.na(sample_size)){
                ind <- 1:nrow(data)
            } else {
              set.seed(14)
                ind <- sample(1:nrow(data), sample_size)
            }
            data <- data[ind,]
            
            x <- data[,var_list$x]
            pT <- fp.scale(x)
            transformed <- (x + pT$shift)/pT$scale
            
            fit <- mfp(as.formula(paste0(var_list$y, "~ fp(transformed, df=4, scale=F)")), data = data)
            rss <- sum((fit$residuals)^2)
            sstot <- sum((data[,var_list$y]-mean(data[,var_list$y]))^2)
            fittedR2 <- 1-rss/sstot
            if(length(sex)>1){
                sex_ind <- "both"
            } else {
                sex_ind <- sex
            }
            if(is.na(sample_size)){
                sample_size_ind <- "all"
            } else {
                sample_size_ind <- sample_size
            }
            index <- paste0(sample_size_ind,"_" ,sex_ind)
            data_list[[dataset]]$fittedR2[[index]] <- fittedR2
        }
    }
}
save(data_list, file = "data/data_list.RData")
