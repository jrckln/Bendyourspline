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
    
    #dynamic insert of slider for positions of knots
    inserted.pos.bs <- c()
    
    observeEvent(input$nknots.bs, {
        if(length(inserted.pos.bs)>0){
            for(i in 1:length(inserted.pos.bs)){
              removeUI(
                selector = paste0('#', inserted.pos.bs[i])
              )
            }
            inserted.pos.bs <- c()
        }
        default.pos.knots.bs <- seq.int(from = 0, to = 1, length.out = input$nknots.bs + 2)[-c(1, input$nknots + 2)]
        var_list <- var_list_reac()
        data <- var_list$data
        x <- data[,var_list$x]
        default.pos.knots.bs <- quantile(x, default.pos.knots.bs)
        num <- as.numeric(input$nknots.bs)
        id <- paste0('pos', 1:num)
        for(i in 1:num){
            insertUI(
                selector = '#placeholder_pos_bs',
                ui = tags$div(sliderInput(paste0(id[i], "_inner"), label = paste0("Position of knot ", i), 
                                          value=default.pos.knots.bs[i], step=0.01, 
                                          min=min(x), max=max(x)), id=id[i])
            )
            inserted.pos.bs <<- c(id, inserted.pos.bs)
          }
    })
    
    #dynamic insert of slider for coefficients for each spline basis function
    inserted.coef.bs <- c()
    observeEvent(c(input$nknots.bs, input$degree.bs), {
        num <- input$degree.bs + input$nknots.bs #+1 for intercept but intercept is extra
        if(length(inserted.coef.bs)>0){
            for(i in 1:length(inserted.coef.bs)){
              removeUI(
                selector = paste0('#', inserted.coef.bs[i])
              )
            }
            inserted.coef.bs <- c()
        }
        id <- paste0('coef', 1:num)
        for(i in 1:num){
            insertUI(
                selector = '#placeholder_coef_bs',
                ui = tags$div(sliderInput(paste0(id[i], "_inner"), label = paste0("Coefficient ", i), 
                                          value=1, step=0.01, 
                                          min=-10, max=10), id=id[i])
            )
            inserted.coef.bs <<- c(id, inserted.coef.bs)
          }
    })
    
    getpos.bs <- reactive({
        #get values of knot positions: 
        ind <- match(paste0("pos", 1:input$nknots.bs, "_inner"), names(input))
        ind <- ind[!(is.na(ind))]
        pos <- c()
        for(i in ind){
            pos <- c(pos, input[[names(input)[i]]])
        }
        pos
    })
    
    getcoef.bs <- reactive({
        num <- input$degree.bs + input$nknots.bs
        #get values of coefficients: 
        ind <- match(paste0("coef", 1:num, "_inner"), names(input))
        ind <- ind[!(is.na(ind))]
        coef <- c()
        for(i in ind){
            coef <- c(coef, input[[names(input)[i]]])
        }
        coef
    })
    
    output$plot.bs <- renderPlotly({
        var_list <- var_list_reac()
        data <- var_list$data
        
        x <- data[,var_list$x]
        
        degree <- input$degree.bs
        
        pos <- getpos.bs()
        b <- bs(x, degree=degree, knots=pos)

        #TODO: push up to data creation - no need for recalc every time
        data <- data %>% 
            group_by(!!sym(var_list$x)) %>% 
            mutate(y_mean = mean(!!sym(var_list$y)))
        
        coefs <- getcoef.bs()
        spline <- apply(b, 1, function(x, coefs.in = coefs) {
            res <- 0
            for(i in 1:length(x)){
                res <- res + coefs.in[i]*x[i]
            }
            res
        })
        
        intercept <- input$intercept.bs
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
                p <- p + geom_vline(xintercept=i, color = "red", linetype="dashed")
            }
        }
        
        p <- p +geom_line(aes(x=!!sym(var_list$x), y = spline)) + 
                theme_minimal() +
                ylab(var_list$y) 
        ggplotly(p)
    })
    
    
    session$onSessionEnded(stopApp) #automatically stop when closing browser
}
