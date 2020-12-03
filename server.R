function(input, output, session){
    
    #############################################
    #######         Data            #############
    #############################################
    
    
    output$transformation <- renderUI({
        HTML(paste0('Got ya!, you chose ', input$variable))
    })
    
    #############################################
    #######           FP            #############
    #############################################
    
    warning.fp <- reactiveVal()
    warning.fp(" ")
    
    output$plot.FP <- renderPlotly({
        var <- as.character(input$variable)
        x <- data.FP[[var]]
        transformed <- (x+input$shift)/input$scale
        
        if(any(transformed<0)) {
            warning.fp("Warning: data has negative values")
            p <- ggplot()
            return(p)
        }
        
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
    
        DF <- cbind(transformed,fp) %>% data.frame()
        p <- ggplot(data=DF, aes(x=transformed, y=fp)) +
            geom_line() + theme_minimal()
        ggplotly(p)
    })

    
    #############################################
    #######        B-splines        #############
    #############################################
    
    
    output$plot.bsplines <- renderPlotly({
        degree <- input$order.bsplines
        var <- as.character(input$variable)
        x <- data.FP[[var]]
        df <- 5
        max.val <- max(x)
        knots <- seq(max.val/(df-degree+1),max.val-max.val/(df-degree+1), length.out=df-degree) # determine equally distributed knots
        coef <- round(runif(df, min=0, max=1),2) # choose random coefficients
        linb <- bs(x, degree = degree, knots=knots)
        y <- linb %*% coef
        
        DF <- cbind(x,y, linb) %>% data.frame()
        names(DF)[1:2] <- c("x", "y")
        p <- ggplot(data=DF, aes(x=x)) + geom_line(aes(y=y))
        for(i in names(DF)[-c(1,2)]){ 
            p<-p+geom_line(aes_string(y=i), color="grey")
        }
        for(i in knots){
            p <- p + geom_vline(xintercept = i, col="lightgrey")
        }
        ggplotly(p)
    })
}
