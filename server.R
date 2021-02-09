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
    #coefficient values - save for possibility to adjust with buttons
    val.coefs.bs <- c(0,0,0) # take care of default vals here: 
    # degree = 1 and 2 internal knots -> 2 coefficients
    # does not need to be reactive since it should always be called with getcoef.bs()
    
    range.coefs.bs <- reactiveValues(range.coef.left = -1, range.coef.right = 1)
    observeEvent(input$increase_range.bs, {
        range.coefs.bs$range.coef.left <- range.coefs.bs$range.coef.left-1
        range.coefs.bs$range.coef.right <- range.coefs.bs$range.coef.right+1
    })
    observeEvent(input$decrease_range.bs, {
        range.coefs.bs$range.coef.left <- range.coefs.bs$range.coef.left+1
        range.coefs.bs$range.coef.right <- range.coefs.bs$range.coef.right-1
    })
    #update range of coefficient sliders
    observe({
        num <- input$degree.bs + input$nknots.bs
        #get values of coefficients: 
        ind <- paste0("coef", 1:num, "_inner")
        for(i in ind){
            updateSliderInput(session, i, max = range.coefs.bs$range.coef.right, min=range.coefs.bs$range.coef.left)
        }
    })
    
    
    observeEvent(input$nknots.bs, {
        if(length(inserted.pos.bs)!=0){ #case of update
            if(length(inserted.pos.bs)>as.numeric(input$nknots.bs)){
                #remove difference: 
                num <- length(inserted.pos.bs) - input$nknots.bs
                for(i in 1:num){
                  removeUI(
                    selector = paste0('#', inserted.pos.bs[length(inserted.pos.bs)]) #remove the last ones
                  )
                    inserted.pos.bs <<- inserted.pos.bs[-length(inserted.pos.bs)]
                }
            }else if(length(inserted.pos.bs)<as.numeric(input$nknots.bs)){
                default.pos.knots.bs <- seq.int(from = 0, to = 1, length.out = input$nknots.bs + 2)[-c(1, input$nknots + 2)]
                var_list <- var_list_reac()
                data <- var_list$data
                x <- data[,var_list$x]
                default.pos.knots.bs <- quantile(x, default.pos.knots.bs)
                num <- (length(inserted.pos.bs)+1):input$nknots.bs
                id <- paste0('pos', num)
                for(i in 1:length(num)){
                    insertUI(
                        selector = '#placeholder_pos_bs',
                        ui = tags$div(sliderInput(paste0(id[i], "_inner"), label = paste0("Position of knot ", 
                                                                                          length(inserted.pos.bs)+i), 
                                                  value=default.pos.knots.bs[i], step=0.01, 
                                                  min=min(x), max=max(x)), id=id[i])
                    )
                    inserted.pos.bs <<- c(inserted.pos.bs, id[i])
                }
            }
        }else{ #case of init
            default.pos.knots.bs <- seq.int(from = 0, to = 1, length.out = input$nknots.bs + 2)[-c(1, input$nknots + 2)]
            var_list <- var_list_reac()
            data <- var_list$data
            x <- data[,var_list$x]
            default.pos.knots.bs <- quantile(x, default.pos.knots.bs)
            num <- 1:input$nknots.bs
            id <- paste0('pos', num)
            for(i in 1:length(num)){
                insertUI(
                    selector = '#placeholder_pos_bs',
                    ui = tags$div(sliderInput(paste0(id[i], "_inner"), label = paste0("Position of knot ", i), 
                                              value=default.pos.knots.bs[i], step=0.01, 
                                              min=min(x), max=max(x)), id=id[i])
                )
                inserted.pos.bs <<- c(inserted.pos.bs, id[i])
            }
        }
    })
    
    #dynamic insert of slider for coefficients for each spline basis function
    inserted.coef.bs <- c()
    observeEvent(c(input$nknots.bs, input$degree.bs), {
        num <- input$degree.bs + input$nknots.bs #+1 for intercept but intercept is extra
        if(length(inserted.coef.bs)!=0){ #case of update
            #num is the number of desired inputs
            if(length(inserted.coef.bs)>num){
                if(length(inserted.coef.bs)>0){
                    toomuch <- length(inserted.coef.bs)-num
                    for(i in 1:toomuch){
                      removeUI(
                        selector = paste0('#', inserted.coef.bs[length(inserted.coef.bs)])
                      )
                    inserted.coef.bs <<- inserted.coef.bs[-length(inserted.coef.bs)]
                    }
                }
            }else if(length(inserted.coef.bs)<num){
                toinsert <- (length(inserted.coef.bs)+1):num
                id <- paste0('coef', toinsert)
                for(i in 1:length(toinsert)){
                    insertUI( #place inserted UI in div for complete removal
                        selector = '#placeholder_coef_bs',
                        ui = tags$div(
                            actionButton(paste0(id[i], "_inner_minus"), "", icon = icon("minus-square"), 
                                         style='padding:4px; font-size:80%; vertical-align: -150%;background: #D6D6D6;', class="minus"),
                            sliderInput(paste0(id[i], "_inner"), label = paste0("Coefficient ", 
                                                                                          length(inserted.coef.bs)+i), 
                                                  value=1, step=0.01, min=-1, max=1, width='80%'), 
                            actionButton(paste0(id[i], "_inner_plus"), "", icon = icon("plus-square"), 
                                         style='padding:4px; font-size:80%; vertical-align: -150%; background: #D6D6D6;', class="plus"),
                            id=id[i]
                        )
                    )
                    inserted.coef.bs <<- c(inserted.coef.bs, id[i])
                }
            }
        } else { #case of init
            id <- paste0('coef', 1:num)
            for(i in 1:num){
                insertUI(
                    selector = '#placeholder_coef_bs',
                    ui = tags$div(
                        actionButton(paste0(id[i], "_inner_minus"), "", icon = icon("minus-square"), 
                                     style='padding:4px; font-size:80%; vertical-align: -150%; background: #D6D6D6;', class="minus"), 
                        # class to control only this button group
                        # and get last id with JS (see tab bsplines)
                        sliderInput(paste0(id[i], "_inner"), label = paste0("Coefficient ", i), 
                                              value=1, step=0.01, 
                                              min=-1, max=1, width='80%'),
                        actionButton(paste0(id[i], "_inner_plus"), "", icon = icon("plus-square"), 
                                         style='padding:4px; font-size:80%; vertical-align: -150%; background: #D6D6D6;', class="plus"),
                        id=id[i])
                )
                inserted.coef.bs <<- c(inserted.coef.bs, id[i])
            }
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
    
    getintercept.bs <- reactive({
        if(input$adjust_intercept.bs){
            var_list <- var_list_reac()
            data <- var_list$data
            x <- data[,var_list$x]
            degree <- input$degree.bs
            pos <- getpos.bs()
            b <- bs(x, degree=degree, knots=pos)
            coefs <- getcoef.bs()
            spline <- apply(b, 1, function(x, coefs.in = coefs) {
                res <- 0
                for(i in 1:length(x)){
                    res <- res + coefs.in[i]*x[i]
                }
                res
            })
            intercept <- opt.intercept(fitted=spline, data=data[,var_list$y], interval=c(min(data[,var_list$y]), max(data[,var_list$y])))$minimum
            return(intercept)
        } else {
            return(input$intercept.bs)
        }
    })
    
    output$intercept.bs <- renderUI({
        intercept.val <- getintercept.bs()
        if(input$adjust_intercept.bs){
            div(HTML(paste0("Intercept fitted using LS to: ", round(intercept.val, 3))))
        }
        else {
             div(HTML(paste0("Intercept set to: ", round(intercept.val, 3))))
        }
    })
    
    getid_minus.bs <- reactive({
        #returns quoted expression
        num <- input$degree.bs + input$nknots.bs
        ind <- match(paste0("coef", 1:num, "_inner_minus"), names(input))
        ind <- ind[!(is.na(ind))]
        ind
    })
    
    getid_plus.bs <- reactive({
        #returns quoted expression
        num <- input$degree.bs + input$nknots.bs
        ind <- match(paste0("coef", 1:num, "_inner_plus"), names(input))
        ind <- ind[!(is.na(ind))]
        ind
    })
    
    #trigger when any minus button was clicked: modifies input value
    observeEvent(input$last_btn_minus, {
        coefs <- getcoef.bs()
        val.coefs.bs <- coefs
        #determine button of which coef id was clicked: id of button is coef<idnumber>_inner_minus
        id.coef <- gsub("_minus", "", input$last_btn_minus)
        ind <- as.numeric(gsub("_inner", "", gsub("coef","",id.coef)))
        val <- getcoef.bs()
        val <- val[ind] -0.01
        updateSliderInput(session, inputId = id.coef, value = val)
    })
    
    observeEvent(input$last_btn_plus, {
        coefs <- getcoef.bs()
        val.coefs.bs <- coefs
        #determine button of which coef id was clicked: id of button is coef<idnumber>_inner_minus
        id.coef <- gsub("_plus", "", input$last_btn_plus)
        ind <- as.numeric(gsub("_inner", "", gsub("coef","",id.coef)))
        val <- getcoef.bs()
        val <- val[ind] + 0.01
        updateSliderInput(session, inputId = id.coef, value = val)
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
        
        intercept <- getintercept.bs()
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
    
    output$basis_plot.bs<- renderPlotly({
        var_list <- var_list_reac()
        data <- var_list$data
        x <- data[,var_list$x]
        degree <- input$degree.bs
        pos <- getpos.bs()
        b <- bs(x, degree=degree, knots=pos)
        all.knots <- sort(c(attr(b,"Boundary.knots") ,attr(b, "knots")))
        bounds <- range(all.knots)
        knot.values <- set_colnames(predict(b, all.knots),str_c("S", seq_len(ncol(predict(b, all.knots)))))
        newx <- seq(bounds[1], bounds[2], length.out = 100+1)
        interp.values <- set_colnames(predict(b, newx),str_c("S", seq_len(ncol(predict(b, newx)))))
        knot.df <- melt(data.frame(x=all.knots, knot.values), id.vars="x", variable.name="Spline", value.name="y")
        interp.df <- melt(data.frame(x=newx, interp.values),id.vars="x", variable.name="Spline", value.name="y")
        p <- ggplot(interp.df) +
            aes(x=x, y=y, color=Spline) +
            geom_line() +
            scale_color_manual(values = col) + theme_minimal() + theme(legend.position = "none") + 
            ggtitle("Spline basis functions")
        ggplotly(p)
    })
    
    calcR2.bs <- reactive({
        var_list <- var_list_reac()
        data <- var_list$data
        x <- data[,var_list$x]
        degree <- input$degree.bs
        pos <- getpos.bs()
        b <- bs(x, degree=degree, knots=pos)
        coefs <- getcoef.bs()
        spline <- apply(b, 1, function(x, coefs.in = coefs) {
            res <- 0
            for(i in 1:length(x)){
                res <- res + coefs.in[i]*x[i]
            }
            res
        })
        spline <- spline + getintercept.bs()
        model <- lm(as.formula(paste0(var_list$y, " ~ bs(", var_list$x, ", df=", degree+length(pos),")")), data=data)
        fitted <- model$fitted
        p <- model$rank
        sstot <- sum((data[,var_list$y]-mean(data[,var_list$y]))^2) #total sum of squares
        ssres <- sum((data[, var_list$y]-spline)^2)  #residual sum of squares
        ssres_fitted <- sum((data[, var_list$y]-fitted)^2)  #residual sum of squares fitted
        c(1-ssres/sstot, 1-ssres_fitted/sstot, p)
    })
    
    calcadjR2.bs <- reactive({
        res <- calcR2.bs()
        R2 <- res[1]
        maxR2 <- res[2]
        p <- res[3]
        var_list <- var_list_reac()
        data <- var_list$data
        if(input$gender == "Both"){
            sex_ind <- "both"
        } else {
            sex_ind <- gender[input$gender]
        }
        index <- paste0(input$sample.size,"_" ,sex_ind)
        c(R2, 1-(1-R2)*(nrow(data)-1)/(nrow(data)-1-p), maxR2)
    })
    
    output$stats.bs <-  renderUI({
        stats <- calcadjR2.bs()
        nknots <- input$nknots.bs
        degree <- input$degree.bs
        HTML(paste0("R <sup>2</sup>: ", round(stats[1], 3), "<br> adj. R <sup>2</sup>: ", round(stats[2], 3), 
                    "<br> max. R <sup>2</sup> for degree of ", degree , " and ", nknots," internal knots: ", round(stats[3], 3)))
    })
    
    #reset button 
    observeEvent(input$reset_input.bs, {
        shinyjs::reset("inputs.bs") #id of tab to reset
    })
    session$onSessionEnded(stopApp) #automatically stop when closing browser
}
