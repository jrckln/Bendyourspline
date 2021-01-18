function(input, output, session){
    
    #############################################
    #######         Data            #############
    #############################################
    
    
    output$information.vars <- renderUI({
        var <- as.character(input$variable)
        var_list <- data_list[[var]]
        HTML(paste0('Chosen variable pair: ', input$variable, "<br>", 
                    var_list$x, " in ", var_list$x_unit, "<br>", 
                    "filtered for sex: ", input$gender
                    ))
    })
    
    var_list_reac <- reactive({
        var <- as.character(input$variable)
        var_list <- data_list[[var]]
        
        var_list$data <- var_list$data[var_list$data[,"gender"] %in% gender[[input$gender]],]

        if(input$sample.size == "all"){
            ind <- 1:nrow(var_list$data)
        } else {
            set.seed(14)
            n <- sample.sizes[input$sample.size]
            ind <- sample(1:nrow(var_list$data), n)
        }
        var_list$data <- var_list$data[ind,]
        var_list
    })
    
    #############################################
    #######           FP            #############
    #############################################

    min.range.coef.fp <- eventReactive(input$increase_range.fp,{
        input$increase_range.fp*(-1)
        
    },ignoreNULL = FALSE)
    max.range.coef.fp <- eventReactive(input$increase_range.fp,{
        input$increase_range.fp
        
    },ignoreNULL = FALSE)
    
    #+ and - buttons for FP coefs
    val.coefs.fp <- reactiveValues(val.coef1 = 0, val.coef2 = 0)
    observeEvent(input$add_val_coef1.fp, {
        val.coefs.fp$val.coef1 <- input$coef1.fp+0.01
    })
    observeEvent(input$minus_val_coef1.fp, {
        val.coefs.fp$val.coef1 <- input$coef1.fp-0.01
    })
    observeEvent(input$add_val_coef2.fp, {
        val.coefs.fp$val.coef2 <- input$coef2.fp+0.01
    })
    observeEvent(input$minus_val_coef2.fp, {
        val.coefs.fp$val.coef2 <- input$coef2.fp-0.01
    })

    observe({
        updateSliderInput(session, "coef1.fp", value = val.coefs.fp$val.coef1)
      })
    observe({    
        updateSliderInput(session, "coef2.fp", value = val.coefs.fp$val.coef2)
    })    
    #slider inputs - to increase coefficient range
    output$slider.coef1.fp <- renderUI({
        min = min.range.coef.fp()-1
        max = max.range.coef.fp()+1
        sliderInput("coef1.fp",label="", min = min, max = max, value = val.coefs.fp$val.coef1, step = 0.01)
    })
    output$slider.coef2.fp <- renderUI({
        min = min.range.coef.fp()-1
        max = max.range.coef.fp()+1
        sliderInput("coef2.fp",label="",min = min, max = max, value = val.coefs.fp$val.coef2, step = 0.01)
    })
    
    output$formula.fp <- renderUI({
        pow1 <- as.numeric(input$power1.fp)
        
        trans1 <- paste0("x^{", pow1, "}")
        if(pow1 == 0) trans1 <- "log(x)"
        if(pow1 == 1) trans1 <- "x"
        
        fp_fun <- paste(input$intercept.fp , " + ", input$coef1.fp, "\\cdot", trans1)
        
        pow2 <- as.numeric(input$power2.fp)
        trans2 <- paste0("x^{", pow2, "}")
        if(pow2 == 0) trans2 <- "log(x)"
        if(pow2 == 1) trans2 <- "x"
        if(pow1 == pow2) trans2 <- paste(trans2, "\\cdot \\log(x)")
          
        fp_fun <- paste(fp_fun, "+", input$coef2.fp, "\\cdot", trans2)
        withMathJax(paste0(
            "$$", fp_fun, "$$"
        ))
    })
    
    output$transformation.fp <- renderUI({
        var_list <- var_list_reac()
        data <- var_list$data
        
        x <- data[,var_list$x]
        pT <- fp.scale(x)
        
        withMathJax(paste0(
            "$$ x =  \\frac{ \\text{", var_list$x , "} + ", pT$shift, "}{", pT$scale ,"}$$"
        ))
    })
    
    FPdata <- reactive({
        var_list <- var_list_reac()
        data <- var_list$data
        
        x <- data[,var_list$x]
        pT <- fp.scale(x)
        transformed <- (x + pT$shift)/pT$scale
        
        pow1 <- as.numeric(input$power1.fp)
        if(pow1 == 0) {
            fp1<-log(transformed)
        } else {
            fp1 <- transformed^pow1
        }
        
        pow2 <- as.numeric(input$power2.fp)
        if (pow2==0){
            fp2 <- log(transformed)
        } else {
            fp2 <- transformed^pow2
        }
        if(pow1 == pow2) fp2 <- log(transformed) * fp2
        
        
        fp <- as.numeric(input$coef1.fp)*fp1 + as.numeric(input$coef2.fp)*fp2
        if(length(fp)==0){
            
            fp <- rep(0,nrow(data))
        }
        
        DF <- cbind(data, transformed, fp)
        DF
    })
    
    output$plot.FP <- renderPlotly({
        var_list <- var_list_reac()
        data <- var_list$data
        
        x <- data[,var_list$x]
        pT <- fp.scale(x)
        
        intercept <- input$intercept.fp
        
        DF <- FPdata()
        
        DF <- DF %>% 
            group_by(!!sym(var_list$x)) %>% 
            mutate(y_mean = mean(!!sym(var_list$y)))
        
        p <- ggplot(data=DF)
        
        if(input$add_y.fp){
            p <- p + geom_point(aes(x=!!sym(var_list$x), y=!!sym(var_list$y)), color = "lightgrey")
        }
        if(input$add_mean.fp){
            p <- p + geom_line(aes(x=!!sym(var_list$x), y = y_mean), color = "blue")
        }
        
        p <- p +geom_line(aes(x=!!sym(var_list$x), y = intercept+fp)) + 
            theme_minimal() +
            ylab(var_list$y)
        ggplotly(p)
    })
    
    calcR2 <- reactive({
        var_list <- var_list_reac()
        data <- var_list$data
        x <- data[,var_list$x]
        pT <- fp.scale(x)
        DF <- FPdata()
        fp <- input$intercept.fp+DF[, "fp"]
        
        sstot <- sum((DF[,var_list$y]-mean(DF[,var_list$y]))^2) #total sum of squares
        ssres <- sum((DF[, var_list$y]-fp)^2)  #residual sum of squares
        1-ssres/sstot
    })
    
    calcadjR2 <- reactive({
        R2 <- calcR2()
        var_list <- var_list_reac()
        data <- var_list$data
        if(input$gender == "Both"){
            sex_ind <- "both"
        } else {
            sex_ind <- gender[input$gender]
        }
        index <- paste0(input$sample.size,"_" ,sex_ind)
        maxR2 <- var_list$fittedR2[[index]]
        p <- ifelse(input$coef1.fp == 0& input$coef2.fp == 0, 0, ifelse(any(input$coef1.fp == 0, input$coef2.fp == 0),1,2))
        c(R2, 1-(1-R2)*(nrow(data)-1)/(nrow(data)-1-p), maxR2)
    })
    output$stats.fp <-  renderUI({
        stats <- calcadjR2()
        HTML(paste0("R <sup>2</sup>: ", round(stats[1], 3), "<br> adj. R <sup>2</sup>: ", round(stats[2], 3), 
                    "<br> max. R <sup>2</sup> for this setting: ", round(stats[3], 3)))
    })
    
    #############################################
    #######        B-splines        #############
    #############################################

    
    session$onSessionEnded(stopApp) #automatically stop when closing browser
    }
