function(input, output, session){
    output$variable_input <- renderUI({
    selectInput("variable", "Choose a variable:",names(data.FP))
    })
    output$transformation <- renderUI({
        HTML(paste0('Got ya!, you chose ', input$variable))
    })
    
    #############################################
    #######           FP            #############
    #############################################

    output$plot.FP <- renderPlotly({
        Data <- data.FP[[as.character(input$variable)]]
        transformed <- (Data+input$shift)/input$scale
        DF <- cbind(Data,transformed) %>% data.frame()
        p <- plot_ly(data = DF, alpha = 0.6) %>% 
            add_histogram(x = ~ Data) %>% 
            add_histogram(x = ~ transformed)
        p
    })
    
    #############################################
    #######        B-splines        #############
    #############################################
    
    observeEvent(list(input$order.bsplines, input$variable), {
    })
    
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
