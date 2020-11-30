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
        var <- as.character(input$variable)
        Data <- data.FP[[var]]
        shift <- as.numeric(input$shift)
        scale <- as.numeric(input$scale)
        transformed <- (Data+shift)/scale
        DF <- cbind(Data,transformed) %>% data.frame()
        p <- plot_ly(data = DF, alpha = 0.6) %>% 
            add_histogram(x = ~ Data) %>% 
            add_histogram(x = ~ transformed)
        p
    })
    
    #############################################
    #######        B-splines        #############
    #############################################
    
    ###default number of knots: degree+1? / min number of knots?
    inserted.min <- c() #to remove div in case of different order of spline
    coefsID.bsplines <- c() #to access value 
    posID.bsplines <- c() #to access value
    observeEvent(input$order.bsplines, {
        #remove UI after order of spline is changed: 
        for (i in 1:length(inserted.min)){
            removeUI(
                selector = paste0('#', inserted.min[i])
            )
            inserted <<- inserted[-length(inserted)]
            coefsID.bsplines <<- coefsID.bsplines[-length(coefsID.bsplines)]
            posID.bsplines <<- posID.bsplines[-length(posID.bsplines)]
        }
        #add coef und position inputs for certain order
        for(i in 1:(input$order.bsplines+1)){
            id = paste0("minknot", i)
            insertUI(
                selector = '#placeholder_bsplines_min', #TODO: here multihandle slider?
                ui = tags$div(numericInput(paste0('pos_', id), paste('Position for knot', i),min = 0, max = 10, value = 0),
                              numericInput(paste0('coef_', id), paste('Coefficient ', i),min = 0, max = 10, value = 0),
                        id=id)
          )
            coefsID.bsplines <<- c(coefsID.bsplines, paste0('coef_', id))
            posID.bsplines <<- c(posID.bsplines, paste0('pos_', id))
            inserted.min <<- c(inserted.min, id)
        }
        for(i in 1:(input$order.bsplines)){
            id = paste0("minknot", i+input$order.bsplines+1)
            insertUI(
                selector = '#placeholder_bsplines_min', #TODO: here multihandle slider?
                ui = tags$div(numericInput(paste0('coef_', id), paste('Coefficient ', i+input$order.bsplines+1),min = 0, max = 10, value = 0),
                        id=id)
          )
            coefsID.bsplines <<- c(coefsID.bsplines, paste0('coef_', id))
            inserted.min <<- c(inserted.min, id)
        }
    })
    
    
    #to add and remove numeric input for number of knots
    inserted <- c() #contains the ids of all added sliderInputs
    
     observeEvent(input$insertBtn, {
        btn <- input$insertBtn
        id <- paste0('knot', btn)
        insertUI(
          selector = '#placeholder_bsplines',
          ui = tags$div(numericInput(paste0('pos_', id), paste('Position for knot', btn),min = 0, max = 10, value = 0),
                        numericInput(paste0('coef_', id), paste('Coefficient ', btn),min = 0, max = 10, value = 0),
                        id=id)
          )
        inserted <<- c(inserted, id)
        coefsID.bsplines <<- c(coefsID.bsplines, paste0('coef_', id))
        posID.bsplines <<- c(posID.bsplines, paste0('pos_', id))
     })
  
     observeEvent(input$removeBtn, {
        removeUI(
          selector = paste0('#', inserted[length(inserted)])
        )
        inserted <<- inserted[-length(inserted)]
        coefsID.bsplines <<- coefsID.bsplines[-length(coefsID.bsplines)]
        posID.bsplines <<- posID.bsplines[-length(posID.bsplines)]
      })
     
     #Data creation for bsplines plot
     dataInput <- reactive({
        degree <- as.numeric(input$order.bsplines)
        var <- as.character(input$variable)
        
        #collect coefs and position of knots
        coef.knots.ind <- match(coefsID.bsplines, names(input))
        pos.knots.ind <- match(posID.bsplines, names(input))
        coefs.knots <- c()
        for(i in coef.knots.ind){ #must use single string to index into reactivevalues
            coefs.knots <-c(coefs.knots, input[[names(input)[i]]])
        }
        pos.knots <- c()
        for(i in pos.knots.ind){ #must use single string to index into reactivevalues
            pos.knots <-c(pos.knots, input[[names(input)[i]]])
        }
        x <- data.FP[[var]]
        max.val <- max(x)
        #default values for knots
        #knots <- seq(max.val/(df-degree+1),max.val-max.val/(df-degree+1), length.out=length(coefsID.bsplines)) # determine equally distributed knots
        
        linb <- bs(x, degree = degree, knots=pos.knots)
        y <- linb %*% coefs.knots
        
        DF <- cbind(x,y, linb) %>% data.frame()
        names(DF)[1:2] <- c("x", "y")
        DF
    })
    
    #plotly plot on main panel in methods/bsplines
    output$plot.bsplines <- renderPlotly({
        DF <- dataInput()
        p <- ggplot(data=DF, aes(x=x)) + geom_line(aes(y=y), color ="black", size=2)+theme_minimal()
        for(i in names(DF)[-c(1,2)]){ 
            p<-p+geom_line(aes_string(y=i), color="grey")
        }
        #for(i in knots){
        #    p <- p + geom_vline(xintercept = i, col="lightgrey")
        #}
        ggplotly(p)
    })
}
