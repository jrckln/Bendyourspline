library(Hmisc)
library(nhanesA)
library(tidyverse)


variable <- readline(prompt="Choose variable pair to download: \n
                     1 - Average TISS ~ Mean blood pressure \n
                     2 - Bmi ~ Age\n
                     3 - Diastolic blood pressure ~ Age\n
                     4 - Height ~ Age")

data <- switch(variable, 
               "1" = {getHdata(support, what = "data")
                   data <- na.omit(support[, c('sex', 'meanbp', 'avtisst')])
                   names(data) <- c('gender', 'x', 'y')
                   data},
               "2" = {data <- merge(nhanes('BMX_E')[,c('SEQN', 'BMXBMI')],nhanes('DEMO_E')[,c('SEQN', 'RIDAGEYR', 'RIAGENDR')])
                    data <- na.omit(select(data, -SEQN))
                    names(data) <- c('y', 'x', 'gender')
                    data <- data %>% mutate(gender = ifelse(gender == 2, 'female', 'male'))
                    data
               },
               "3" = {data <- merge(nhanes('BPX_E')[,c('SEQN', 'BPXDI1')],nhanes('DEMO_E')[,c('SEQN', 'RIDAGEYR', 'RIAGENDR')])
                    data <- na.omit(select(data, -SEQN))
                    names(data) <- c('y', 'x', 'gender')
                    data <- data %>% mutate(gender = ifelse(gender == 2, 'female', 'male'))
                    data},
               "4" = {getHdata(FEV, what = "data")
                   data <- na.omit(FEV[, c('age','sex', 'height')])
                   names(data)[2] <- c('x', 'gender', 'y')
                   data
                }
                )