function(input, output, session){
    
    #############################################
    #######         Data            #############
    #############################################
    
    
    output$transformation <- renderUI({
        HTML(paste0('Got ya!, you chose ', input$variable))
    })
    
    var_list_reac <- reactive({
        var <- as.character(input$variable)
        var_list <- data_list[[var]]
        
        var_list$data <- var_list$data[var_list$data[,"gender"] %in% gender[[input$gender]],]
        print(nrow(var_list$data))
        
        if(input$sample.size == "all"){
            n <- nrow(var_list$data)
            ind <- 1:nrow(var_list$data)
        } else {
            n <- sample.sizes[input$sample.size]
            ind <- sample(1:nrow(var_list$data), n)
        }
        var_list$data <- var_list$data[ind,]
        var_list
    })
    
    #############################################
    #######           FP            #############
    #############################################
    
    
    
    output$plot.FP <- renderPlotly({
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
        
        intercept <- input$intercept.fp
    
        DF <- cbind(data, transformed, fp)
        DF <- DF %>% 
            group_by(!!sym(var_list$x)) %>% 
            mutate(y_mean = mean(!!sym(var_list$y)))
        
        p <- ggplot(data=DF)
        
        if(input$add_y){
            p <- p + geom_point(aes(x=!!sym(var_list$x), y=!!sym(var_list$y)), color = "lightgrey")
        }
        
        p <- p +
            geom_line(aes(x=!!sym(var_list$x), y = intercept+fp*pT$scale-pT$shift)) +
            geom_line(aes(x=!!sym(var_list$x), y = y_mean), color = "blue") +
            theme_minimal()
        ggplotly(p)
    })

    
    #############################################
    #######        B-splines        #############
    #############################################
    
}
