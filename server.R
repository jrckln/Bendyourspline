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

    
    #############################################
    #######        B-splines        #############
    #############################################
    
    
    
    output$plot.bs <- renderPlotly({
        var_list <- var_list_reac()
        data <- var_list$data
        
        x <- data[,var_list$x]
        
        df <- input$df.bs
        degree <- input$degree.bs
        
        b <- bs(x, degree=degree, knots=quantile(x, c(1/3, 2/3)))

        #push up to data creation - no need for recalc every time
        data <- data %>% 
            group_by(!!sym(var_list$x)) %>% 
            mutate(y_mean = mean(!!sym(var_list$y)))
        
        intercept <- input$intercept.bs
        spline <- apply(b, 1, function(x, coefs = NULL) {
            res <- 0
            for(i in x){
                res <- res + i
            }
            res
        })

        spline <- intercept + spline
        p <- ggplot(data=data)
        if(input$add_y.bs){
            p <- p + geom_point(aes(x=!!sym(var_list$x), y=!!sym(var_list$y)), color = "lightgrey")
        }
        if(input$add_mean.bs){
            p <- p + geom_line(aes(x=!!sym(var_list$x), y = y_mean), color = "blue")
        }
        if(input$add_knots_pos.bs){
            knots <- attr(b, "knots")
            for(i in knots){
                p <- p + geom_vline(xintercept=i, color = "red")
            }
        }
        
        p <- p +geom_line(aes(x=!!sym(var_list$x), y = spline)) + 
                theme_minimal() +
                ylab(var_list$y) 
        ggplotly(p)
    })
    
    
    session$onSessionEnded(stopApp) #automatically stop when closing browser
}
