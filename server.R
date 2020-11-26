shinyServer(function(input, output, session) {
    
    output$transformation <- renderUI({
        HTML(paste0('Got ya!, you chose ', input$variable))
})
    output$plot.FP <- renderPlotly({
        Data <- data.FP[[input$variable]][[1]]
        transformed <- (Data+input$shift)/input$scale
        DF <- cbind(Data,transformed) %>% data.frame()
        p <- plot_ly(data = DF, alpha = 0.6) %>% 
            add_histogram(x = ~ Data) %>% 
            add_histogram(x = ~ transformed)
        p
 })
    output$plot.bspline <- renderPlotly({
        
 })
})
