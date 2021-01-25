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
    
    observeEvent(input$seed, { #recalculate max R2 values if seed is changed:
        if(input$seed!=14){
                source("data_generation.R")
        }
        source("data/data.R")
        showNotification(span(tagList(icon("check"), "Done. ")), id="done_notif", duration = 7)
    })
    
    var_list_reac <- reactive({
        var <- as.character(input$variable)
        var_list <- data_list[[var]]
        
        var_list$data <- var_list$data[var_list$data[,"gender"] %in% gender[[input$gender]],]

        if(input$sample.size == "all"){
            ind <- 1:nrow(var_list$data)
        } else {
            set.seed(input$seed)
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
    
    
    session$onSessionEnded(stopApp) #automatically stop when closing browser
}
