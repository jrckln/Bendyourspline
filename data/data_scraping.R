rm(list = ls(), envir = globalenv()) ## to prevent cross over from old runs
library(tidyverse)

#Data Scraping 
#A list with the entries: 
    # "x" = <variable_name> #char; column name in the dataset (will be used to subset the dataframe)
    # "y" = <response_name> #char; column name in the dataset (will be used to subset the dataframe)
    # "data" = ... # dataframe containing the data: at least including the columns <variable_name>, <response_name>, gender (containing "male" or "female")
    # "x_unit" = ... #char; unit of variable
    # "y_unit" = ... #char; unit of response
    # "name" = "<response_name>~<variable_name>"  # This will be displayed in the selection 
    # 'x_name = <name> #char; name of variable to display in plots
    # 'y_name = <name> #char; name of variable to display in plots

data_list <- list()

##### Average TISS ~ Mean blood pressure
library(Hmisc)
getHdata(support, what = "data") #set parameter what to 'all' to additionally open a browser window with a description of variables
#name: support

data <- na.omit(support[, c('sex', 'meanbp', 'avtisst')])
names(data)[1] <- c('gender')

avgtiss_mbp <- list(
    'x' = 'meanbp', 'y'= 'avtisst', 
    'data' = data, 
    'x_unit' = 'mmHg', 'y_unit'= 'points', 
    'name' = 'Average TISS ~ Mean blood pressure',
    'x_name' = 'Mean blood pressure', 'y_name' = 'Average TISS'
)
#save(avgtiss_mbp, file = 'data/AverageTISSMeanbloodpressure.RData')
data_list[[avgtiss_mbp$name]] <- avgtiss_mbp

##### Bmi ~ Age
library(nhanesA)
data <- merge(nhanes('BMX_E')[,c('SEQN', 'BMXBMI')],nhanes('DEMO_E')[,c('SEQN', 'RIDAGEYR', 'RIAGENDR')])
data <- na.omit(select(data, -SEQN))
names(data) <- c('bmi', 'age', 'gender')
data <- data %>% mutate(gender = ifelse(gender == 2, 'female', 'male'))

bmi_age <- list(
    'x' = 'age', 'y'= 'bmi', 
    'data' = data, 
    'x_unit' = 'years', 'y_unit'= 'kg/m2', 
    'name' = 'Bmi ~ Age',
    'x_name' = 'Age', 'y_name' = 'BMI'
)
#save(bmi_age, file = 'data/BmiAge.RData')
data_list[[bmi_age$name]] <- bmi_age

##### Diastolic blood pressure ~ Age
library(nhanesA)
data <- merge(nhanes('BPX_E')[,c('SEQN', 'BPXDI1')],nhanes('DEMO_E')[,c('SEQN', 'RIDAGEYR', 'RIAGENDR')])
data <- na.omit(select(data, -SEQN))
names(data) <- c('dbp', 'age', 'gender')
data <- data %>% mutate(gender = ifelse(gender == 2, 'female', 'male'))

dbp_age <- list(
    'x' = 'age', 'y'= 'dbp', 
    'data' = data, 
    'x_unit' = 'years', 'y_unit'= 'mmHg', 
    'name' = 'Diastolic blood pressure ~ Age',
    'x_name' = 'Age', 'y_name' = 'Diastolic blood pressure'
)
#save(dbp_age, file = 'data/DiastolicbloodpressureAge.RData')
data_list[[dbp_age$name]] <- dbp_age

##### Height ~ Age
library(Hmisc)
getHdata(FEV, what = "data") #set parameter what to 'all' to additionally open a browser window with a description of variables
data <- na.omit(FEV[, c('age','sex', 'height')])
names(data)[2] <- c('gender')

height_age <- list(
    'x' = 'age', 'y'= 'height', 
    'data' = data, 
    'x_unit' = 'years', 'y_unit'= 'cm', 
    'name' = 'Height ~ Age',
    'x_name' = 'Age', 'y_name' = 'Height'
)
#save(height_age, file = 'data/HeightAge.RData')
data_list[[height_age$name]] <- height_age


save(data_list, file="data/data_list.RData")
