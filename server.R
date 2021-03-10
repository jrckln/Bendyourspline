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
    
    #increase range of coefs
    range.coefs.fp <- reactiveValues(range.coef.left.fp = -1, range.coef.right.fp = 1)
    observeEvent(input$increase_range.fp, {
        range.coefs.fp$range.coef.left.fp <- range.coefs.fp$range.coef.left.fp-1
        range.coefs.fp$range.coef.right.fp <- range.coefs.fp$range.coef.right.fp+1
    })
    observeEvent(input$decrease_range.fp, {
        range.coefs.fp$range.coef.left.fp <- range.coefs.fp$range.coef.left.fp+1
        range.coefs.fp$range.coef.right.fp <- range.coefs.fp$range.coef.right.fp-1
    })
    observe({
        updateSliderInput(session, "coef1.fp", max = range.coefs.fp$range.coef.right.fp, min=range.coefs.fp$range.coef.left.fp)
        updateSliderInput(session, "coef2.fp", max = range.coefs.fp$range.coef.right.fp, min=range.coefs.fp$range.coef.left.fp)
    })
    val.coefs.fp <- reactiveValues(val.coef1 = 0, val.coef2 = 0)
    observeEvent(c(input$coef1.fp, input$coef2.fp),{
        val.coefs.fp$val.coef1 <- input$coef1.fp
        val.coefs.fp$val.coef2 <- input$coef2.fp
    })
    
    #+ and - buttons for FP coefs
    
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
    #for reset button 
    observeEvent(input$reset_input.fp, {
        val.coefs.fp$val.coef1 <- 0
        val.coefs.fp$val.coef2 <- 0
        range.coefs.fp$range.coef.left.fp <- -1
        range.coefs.fp$range.coef.right.fp <- 1
    })
    observe({
        updateSliderInput(session, "coef1.fp", value = val.coefs.fp$val.coef1)
      })
    observe({    
        updateSliderInput(session, "coef2.fp", value = val.coefs.fp$val.coef2)
    })    
    #slider inputs - to increase coefficient range
    output$slider.coef1.fp <- renderUI({
        sliderInput("coef1.fp",label="", min = range.coefs.fp$range.coef.left.fp, 
                    max = range.coefs.fp$range.coef.right.fp, value = val.coefs.fp$val.coef1, step = 0.01)
    })
    output$slider.coef2.fp <- renderUI({
        sliderInput("coef2.fp",label="",min = range.coefs.fp$range.coef.left.fp,
                    max = range.coefs.fp$range.coef.right.fp, value = val.coefs.fp$val.coef2, step = 0.01)
    })
    
    output$formula.fp <- renderUI({
        pow1 <- as.numeric(input$power1.fp)
        
        trans1 <- paste0("x^{", pow1, "}")
        if(pow1 == 0) trans1 <- "\\log(x)"
        if(pow1 == 1) trans1 <- "x"
        coef1 <- as.numeric(input$coef1.fp)
        if(coef1 >= 0) {
          coef1 <- paste0(" + ", coef1)
        } else {
          coef1 <- coef1
        }
        
        fp_fun <- paste(round(getintercept.fp(),2), coef1, "\\cdot", trans1)
        
        pow2 <- as.numeric(input$power2.fp)
        trans2 <- paste0("x^{", pow2, "}")
        if(pow2 == 0) trans2 <- "\\log(x)"
        if(pow2 == 1) trans2 <- "x"
        if(pow1 == pow2) trans2 <- paste(trans2, "\\cdot \\log(x)")
        coef2 <- as.numeric(input$coef2.fp)
        if(coef2 >= 0) {
          coef2 <- paste0(" + ", coef2)
        } else {
          coef2 <- coef2
        }
          
        fp_fun <- paste(fp_fun, coef2, "\\cdot", trans2)
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
    
    getintercept.fp <- reactive({
        if(input$adjust_intercept.fp){
            var_list <- var_list_reac()
            data <- FPdata()
            intercept <- opt.intercept(fitted=data$fp, data=data[,var_list$y], interval=c(min(data[,var_list$y]), max(data[,var_list$y])))$minimum
            return(intercept)
        } else {
            return(input$intercept.fp)
        }
    })
    
    output$intercept.fp <- renderUI({
        intercept.val <- getintercept.fp()
        if(input$adjust_intercept.fp){
            HTML(paste0("Intercept fitted using LS to: ", intercept.val))
        }
        else {
             HTML(paste0("Intercept set to: ", intercept.val))
        }
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
        
        DF <- cbind(data, transformed, fp, fp1, fp2)
        DF
    })
    
    output$plot.FP <- renderPlotly({
        var_list <- var_list_reac()
        data <- var_list$data
        
        x <- data[,var_list$x]
        pT <- fp.scale(x)
        
        intercept <- getintercept.fp()
        
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
        
        # if(input$add_CI.fp){
        #   SE.fitted <- se_fitted(DF, var_list$y, intercept)
        #   p <- p+geom_ribbon(aes(x = !!sym(var_list$x),ymin=intercept+fp-1.96*SE.fitted,ymax=intercept+fp+1.96*SE.fitted),alpha=0.2)
        # }
        p <- p +geom_line(aes(x=!!sym(var_list$x), y = intercept+fp)) + 
                theme_minimal() +
                ylab(var_list$y) 
        ggplotly(p)
    })
    
    calcR2.fp <- reactive({
        var_list <- var_list_reac()
        data <- var_list$data
        x <- data[,var_list$x]
        pT <- fp.scale(x)
        DF <- FPdata()
        fp <- getintercept.fp()+DF[, "fp"]
        
        sstot <- sum((DF[,var_list$y]-mean(DF[,var_list$y]))^2) #total sum of squares
        ssres <- sum((DF[, var_list$y]-fp)^2)  #residual sum of squares
        1-ssres/sstot
    })
    
    calcadjR2.fp <- reactive({
        R2 <- calcR2.fp()
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
        stats <- calcadjR2.fp()
        HTML(paste0("R <sup>2</sup>: ", round(stats[1], 3), "<br> adj. R <sup>2</sup>: ", round(stats[2], 3), 
                    "<br> max. R <sup>2</sup> for this setting: ", round(stats[3], 3)))
    })
    
    #reset button
    observeEvent(input$reset_input.fp, {
       shinyjs::reset("inputs.fp") #id of tab to reset
    })
    observe({
      if(input$adjust_intercept.fp){
        shinyjs::disable("intercept.fp")
      }
      if(!input$adjust_intercept.fp){
        shinyjs::enable("intercept.fp")
      }
    })
    
    observe({
      if(!(all(input$add_mean.fp, input$add_y.fp))){
        shinyjs::disable("adjust_intercept.fp")
        shinyjs::disable("intercept.fp")
      }
      if(any(input$add_mean.fp, input$add_y.fp)){
        shinyjs::enable("adjust_intercept.fp")
        shinyjs::enable("intercept.fp")
      }
    })
    
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
      req(input$nknots.bs)
        num <- input$degree.bs + input$nknots.bs
        #get values of coefficients:
        ind <- paste0("bs_coef", 1:num, "_inner")
        for(i in ind){
            updateSliderInput(session, i, max = range.coefs.bs$range.coef.right, min=range.coefs.bs$range.coef.left)
        }
    })
    
    observeEvent(input$nknots.bs, {
      req(input$nknots.bs)
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
                default.pos.knots.bs <- seq.int(from = 0, to = 1, length.out = input$nknots.bs + 2)[-c(1, input$nknots.bs + 2)]
                var_list <- var_list_reac()
                data <- var_list$data
                x <- data[,var_list$x]
                
                default.pos.knots.bs <- quantile(x, default.pos.knots.bs)
                num <- (length(inserted.pos.bs)+1):input$nknots.bs
                id <- paste0('bs_pos', num)
                for(i in 1:length(num)){
                    insertUI(
                        selector = '#placeholder_pos_bs',
                        ui = tags$div(sliderInput(paste0(id[i], "_inner"), label = paste0("Position of knot ",
                                                                                          length(inserted.pos.bs)+i),
                                                  value=default.pos.knots.bs[num-1+i], step=0.01,
                                                  min=min(x), max=max(x)), id=id[i])
                    )
                    inserted.pos.bs <<- c(inserted.pos.bs, id[i])
                }
            }
        }else{ #case of init
            default.pos.knots.bs <- seq.int(from = 0, 
                                            to = 1, 
                                            length.out = input$nknots.bs +2)[-c(1, input$nknots.bs + 2)]
            var_list <- var_list_reac()
            data <- var_list$data
            x <- data[,var_list$x]
            default.pos.knots.bs <- quantile(x, default.pos.knots.bs)
            num <- 1:input$nknots.bs
            id <- paste0('bs_pos', num)
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
      req(input$nknots.bs)
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
                id <- paste0('bs_coef', toinsert)
                for(i in 1:length(toinsert)){
                    insertUI( #place inserted UI in div for complete removal
                        selector = '#placeholder_coef_bs',
                        ui = tags$div(
                            actionButton(paste0(id[i], "_inner_minus"), "", icon = icon("minus-square"),
                                         style='padding:4px; font-size:80%; vertical-align: -150%;background: #D6D6D6;', class="minus_bs"),
                            sliderInput(paste0(id[i], "_inner"), label = paste0("Coefficient ",
                                                                                          length(inserted.coef.bs)+i),
                                                  value=0, step=0.01, min=-1, max=1, width='70%'),
                            actionButton(paste0(id[i], "_inner_plus"), "", icon = icon("plus-square"),
                                         style='padding:4px; font-size:80%; vertical-align: -150%; background: #D6D6D6;', class="plus_bs"),
                            id=id[i]
                        )
                    )
                    inserted.coef.bs <<- c(inserted.coef.bs, id[i])
                }
            }
        } else { #case of init
            id <- paste0('bs_coef', 1:num)
            for(i in 1:num){
                insertUI(
                    selector = '#placeholder_coef_bs',
                    ui = tags$div(
                        actionButton(paste0(id[i], "_inner_minus"), "", icon = icon("minus-square"),
                                     style='padding:4px; font-size:80%; vertical-align: -150%; background: #D6D6D6;', class="minus_bs"),
                        # class to control only this button group
                        # and get last id with JS
                        sliderInput(paste0(id[i], "_inner"), label = paste0("Coefficient ", i),
                                              value=0, step=0.01,
                                              min=-1, max=1, width='70%'),
                        actionButton(paste0(id[i], "_inner_plus"), "", icon = icon("plus-square"),
                                         style='padding:4px; font-size:80%; vertical-align: -150%; background: #D6D6D6;', class="plus_bs"),
                        id=id[i])
                )
                inserted.coef.bs <<- c(inserted.coef.bs, id[i])
            }
        }

    })
    
    getpos.bs <- reactive({
        req(input$nknots.bs)
        #get values of knot positions:
        ind <- match(paste0("bs_pos", 1:input$nknots.bs, "_inner"), names(input))
        ind <- ind[!(is.na(ind))]
        pos <- c()
        for(i in ind){
            pos <- c(pos, input[[names(input)[i]]])
        }
        pos
    })

    getcoef.bs <- reactive({
      req(input$nknots.bs)
        num <- input$degree.bs + input$nknots.bs
        #get values of coefficients:
        ind <- match(paste0("bs_coef", 1:num, "_inner"), names(input))
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
            HTML(paste0("Intercept fitted using LS to: ", round(intercept.val, 3)))
        }
        else {
            HTML(paste0("Intercept set to: ", round(intercept.val, 3)))
        }
    })

    getid_minus.bs <- reactive({
      req(input$nknots.bs)
        #returns quoted expression
        num <- input$degree.bs + input$nknots.bs
        ind <- match(paste0("bs_coef", 1:num, "_inner_minus"), names(input))
        ind <- ind[!(is.na(ind))]
        ind
    })

    getid_plus.bs <- reactive({
      req(input$nknots.bs)
        #returns quoted expression
        num <- input$degree.bs + input$nknots.bs
        ind <- match(paste0("bs_coef", 1:num, "_inner_plus"), names(input))
        ind <- ind[!(is.na(ind))]
        ind
    })

    #trigger when any minus button was clicked: modifies input value
    observeEvent(input$last_btn_minus_bs, {
        coefs <- getcoef.bs()
        val.coefs.bs <- coefs
        #determine button of which coef id was clicked: id of button is coef<idnumber>_inner_minus
        id.coef <- gsub("_minus", "", input$last_btn_minus_bs)
        ind <- as.numeric(gsub("_inner", "", gsub("bs_coef","",id.coef)))
        val <- getcoef.bs()
        val <- val[ind] -0.01
        updateSliderInput(session, inputId = id.coef, value = val)
    })

    observeEvent(input$last_btn_plus_bs, {
        coefs <- getcoef.bs()
        val.coefs.bs <- coefs
        #determine button of which coef id was clicked: id of button is coef<idnumber>_inner_minus
        id.coef <- gsub("_plus", "", input$last_btn_plus_bs)
        ind <- as.numeric(gsub("_inner", "", gsub("bs_coef","",id.coef)))
        val <- getcoef.bs()
        val <- val[ind] + 0.01
        updateSliderInput(session, inputId = id.coef, value = val)
    })

    output$plot.bs <- renderPlotly({
      req(input$nknots.bs)
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
      req(input$nknots.bs)
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
    observe({
      if(input$adjust_intercept.bs){
        shinyjs::disable("intercept.bs")
      }
      if(!input$adjust_intercept.bs){
        shinyjs::enable("intercept.bs")
      }
    })
    observe({
      if(!(all(input$add_mean.bs, input$add_y.bs))){
        shinyjs::disable("adjust_intercept.bs")
        shinyjs::disable("intercept.bs")
      }
      if(any(input$add_mean.bs, input$add_y.bs)){
        shinyjs::enable("adjust_intercept.bs")
        shinyjs::enable("intercept.bs")
      }
    })
    
    #############################################
    #######      Natural-splines    #############
    #############################################
    
    #dynamic insert of slider for positions of knots
    inserted.pos.nsp <- c()
    #coefficient values - save for possibility to adjust with buttons
    val.coefs.nsp <- c(0,0,0) # take care of default vals here
    # does not need to be reactive since it should always be called with getcoef.nsp()

    range.coefs.nsp <- reactiveValues(range.coef.left = -1, range.coef.right = 1)
    observeEvent(input$increase_range.nsp, {
        range.coefs.nsp$range.coef.left <- range.coefs.nsp$range.coef.left-1
        range.coefs.nsp$range.coef.right <- range.coefs.nsp$range.coef.right+1
    })
    observeEvent(input$decrease_range.nsp, {
        range.coefs.nsp$range.coef.left <- range.coefs.nsp$range.coef.left+1
        range.coefs.nsp$range.coef.right <- range.coefs.nsp$range.coef.right-1
    })
    #update range of coefficient sliders
    observe({
      req(input$nknots.nsp)
        num <- 1 + input$nknots.nsp
        #get values of coefficients:
        ind <- paste0("nsp_coef", 1:num, "_inner")
        for(i in ind){
            updateSliderInput(session, i, max = range.coefs.nsp$range.coef.right, min=range.coefs.nsp$range.coef.left)
        }
    })
    
    output$boundary_knots.nsp <- renderUI({
      req(input$nknots.nsp)
      var_list <- var_list_reac()
      data <- var_list$data
      x <- data[,var_list$x]
      default.pos.knots.nsp <- seq.int(from = 0, 
                                             to = 1,
                                             length.out = input$nknots.nsp + 2)[-c(1, input$nknots.nsp + 2)]
      pos <- quantile(x, default.pos.knots.nsp)
      div(
      sliderInput("boundary1.nsp", "Position of Boundary knot 1", min=min(x), max=pos[1], value=min(x), step=0.1),
      sliderInput("boundary2.nsp", "Position of Boundary knot 2", min=pos[length(pos)], max=max(x), value=max(x), step=0.1)
      )
    })
    #update max and min val of boundary knots slider according to second and second to last knot position
    observe({
      pos <- getpos.nsp()
      updateSliderInput(session, "boundary1.nsp", max=pos[1])
      updateSliderInput(session, "boundary2.nsp", min = pos[length(pos)])
    })


    observeEvent(input$nknots.nsp, {
      req(input$nknots.nsp)
        if(length(inserted.pos.nsp)!=0){ #case of update
            if(length(inserted.pos.nsp)>as.numeric(input$nknots.nsp)){
                #remove difference:
                num <- length(inserted.pos.nsp) - input$nknots.nsp
                for(i in 1:num){
                  removeUI(
                    selector = paste0('#', inserted.pos.nsp[length(inserted.pos.nsp)]) #remove the last ones
                  )
                    inserted.pos.nsp <<- inserted.pos.nsp[-length(inserted.pos.nsp)]
                }
            }else if(length(inserted.pos.nsp)<as.numeric(input$nknots.nsp)){
                var_list <- var_list_reac()
                data <- var_list$data
                x <- data[,var_list$x]
                default.pos.knots.nsp <- seq.int(from = (input$boundary1.nsp-min(x))/(max(x)-min(x)),
                                                 to = (input$boundary2.nsp-min(x))/(max(x)-min(x)),
                                                 length.out = input$nknots.nsp + 2)[-c(1, input$nknots.nsp + 2)]
                default.pos.knots.nsp <- quantile(x, default.pos.knots.nsp)
                num <- (length(inserted.pos.nsp)+1):input$nknots.nsp
                id <- paste0('nsp_pos', num)
                for(i in 1:length(num)){
                    insertUI(
                        selector = '#placeholder_pos_nsp',
                        ui = tags$div(sliderInput(paste0(id[i], "_inner"), label = paste0("Position of knot ",
                                                                                          length(inserted.pos.nsp)+i),
                                                  value=default.pos.knots.nsp[num-1+i], step=0.01,
                                                  min=min(x), max=max(x)), id=id[i])
                    )
                    inserted.pos.nsp <<- c(inserted.pos.nsp, id[i])
                }
            }
        }else{ #case of init
            default.pos.knots.nsp <- seq.int(from = 0, 
                                             to = 1,
                                             length.out = input$nknots.nsp + 2)[-c(1, input$nknots.nsp + 2)]
            var_list <- var_list_reac()
            data <- var_list$data
            x <- data[,var_list$x]
            default.pos.knots.nsp <- quantile(x, default.pos.knots.nsp)
            num <- 1:input$nknots.nsp
            id <- paste0('nsp_pos', num)
            for(i in 1:length(num)){
                insertUI(
                    selector = '#placeholder_pos_nsp',
                    ui = tags$div(sliderInput(paste0(id[i], "_inner"), label = paste0("Position of knot ", i),
                                              value=default.pos.knots.nsp[i], step=0.01,
                                              min=min(x), max=max(x)), id=id[i])
                )
                inserted.pos.nsp <<- c(inserted.pos.nsp, id[i])
            }
        }
    })

    #dynamic insert of slider for coefficients for each spline basis function
    inserted.coef.nsp <- c()
    observeEvent(c(input$nknots.nsp), {
      req(input$nknots.nsp)
        num <- 1 + input$nknots.nsp #+1 for intercept but intercept is extra
        if(length(inserted.coef.nsp)!=0){ #case of update
            #num is the number of desired inputs
            if(length(inserted.coef.nsp)>num){
                if(length(inserted.coef.nsp)>0){
                    toomuch <- length(inserted.coef.nsp)-num
                    for(i in 1:toomuch){
                      removeUI(
                        selector = paste0('#', inserted.coef.nsp[length(inserted.coef.nsp)])
                      )
                    inserted.coef.nsp <<- inserted.coef.nsp[-length(inserted.coef.nsp)]
                    }
                }
            }else if(length(inserted.coef.nsp)<num){
                toinsert <- (length(inserted.coef.nsp)+1):num
                id <- paste0('nsp_coef', toinsert)
                for(i in 1:length(toinsert)){
                    insertUI( #place inserted UI in div for complete removal
                        selector = '#placeholder_coef_nsp',
                        ui = tags$div(
                            actionButton(paste0(id[i], "_inner_minus"), "", icon = icon("minus-square"),
                                         style='padding:4px; font-size:80%; vertical-align: -150%;background: #D6D6D6;', class="minus_nsp"),
                            sliderInput(paste0(id[i], "_inner"), label = paste0("Coefficient ",
                                                                                          length(inserted.coef.nsp)+i),
                                                  value=0, step=0.01, min=-1, max=1, width='80%'),
                            actionButton(paste0(id[i], "_inner_plus"), "", icon = icon("plus-square"),
                                         style='padding:4px; font-size:80%; vertical-align: -150%; background: #D6D6D6;', class="plus_nsp"),
                            id=id[i]
                        )
                    )
                    inserted.coef.nsp <<- c(inserted.coef.nsp, id[i])
                }
            }
        } else { #case of init
            id <- paste0('nsp_coef', 1:num)
            for(i in 1:num){
                insertUI(
                    selector = '#placeholder_coef_nsp',
                    ui = tags$div(
                        actionButton(paste0(id[i], "_inner_minus"), "", icon = icon("minus-square"),
                                     style='padding:4px; font-size:80%; vertical-align: -150%; background: #D6D6D6;', class="minus_nsp"),
                        # class to control only this button group
                        # and get last id with JS (see tab bsplines)
                        sliderInput(paste0(id[i], "_inner"), label = paste0("Coefficient ", i),
                                              value=0, step=0.01,
                                              min=-1, max=1, width='80%'),
                        actionButton(paste0(id[i], "_inner_plus"), "", icon = icon("plus-square"),
                                         style='padding:4px; font-size:80%; vertical-align: -150%; background: #D6D6D6;', class="plus_nsp"),
                        id=id[i])
                )
                inserted.coef.nsp <<- c(inserted.coef.nsp, id[i])
            }
        }

    })

    getpos.nsp <- reactive({
      req(input$nknots.nsp)
        #get values of knot positions:
        ind <- match(paste0("nsp_pos", 1:input$nknots.nsp, "_inner"), names(input))
        ind <- ind[!(is.na(ind))]
        pos <- c()
        for(i in ind){
            pos <- c(pos, input[[names(input)[i]]])
        }
        pos
    })

    getcoef.nsp <- reactive({
      req(input$nknots.nsp)
        num <- 1 + input$nknots.nsp
        #get values of coefficients:
        ind <- match(paste0("nsp_coef", 1:num, "_inner"), names(input))
        ind <- ind[!(is.na(ind))]
        coef <- c()
        for(i in ind){
            coef <- c(coef, input[[names(input)[i]]])
        }
        coef
    })

    getintercept.nsp <- reactive({
        if(input$adjust_intercept.nsp){
            var_list <- var_list_reac()
            data <- var_list$data
            x <- data[,var_list$x]
            pos <- getpos.nsp()
            b <- ns(x,knots=pos, Boundary.knots = c(input$boundary1.nsp, input$boundary2.nsp))
            coefs <- getcoef.nsp()
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
            return(input$intercept.nsp)
        }
    })

    output$intercept.nsp <- renderUI({
        intercept.val <- getintercept.nsp()
        if(input$adjust_intercept.nsp){
            div(HTML(paste0("Intercept fitted using LS to: ", round(intercept.val, 3))))
        }
        else {
             div(HTML(paste0("Intercept set to: ", round(intercept.val, 3))))
        }
    })

    getid_minus.nsp <- reactive({
        #returns quoted expression
        num <- 1 + input$nknots.nsp
        ind <- match(paste0("nsp_coef", 1:num, "_inner_minus"), names(input))
        ind <- ind[!(is.na(ind))]
        ind
    })

    getid_plus.nsp <- reactive({
        #returns quoted expression
        num <- 1 + input$nknots.nsp
        ind <- match(paste0("nsp_coef", 1:num, "_inner_plus"), names(input))
        ind <- ind[!(is.na(ind))]
        ind
    })

    #trigger when any minus button was clicked: modifies input value
    observeEvent(input$last_btn_minus_nsp, {
        coefs <- getcoef.nsp()
        val.coefs.nsp <- coefs
        #determine button of which coef id was clicked: id of button is coef<idnumber>_inner_minus
        id.coef <- gsub("_minus", "", input$last_btn_minus_nsp)
        ind <- as.numeric(gsub("_inner", "", gsub("nsp_coef","",id.coef)))
        val <- getcoef.nsp()
        val <- val[ind] -0.01
        updateSliderInput(session, inputId = id.coef, value = val)
    })

    observeEvent(input$last_btn_plus_nsp, {
        coefs <- getcoef.nsp()
        val.coefs.nsp <- coefs
        #determine button of which coef id was clicked: id of button is coef<idnumber>_inner_minus
        id.coef <- gsub("_plus", "", input$last_btn_plus_nsp)
        ind <- as.numeric(gsub("_inner", "", gsub("nsp_coef","",id.coef)))
        val <- getcoef.nsp()
        val <- val[ind] + 0.01
        updateSliderInput(session, inputId = id.coef, value = val)
    })

    output$plot.nsp <- renderPlotly({
      req(input$nknots.nsp)
        var_list <- var_list_reac()
        data <- var_list$data

        x <- data[,var_list$x]

        pos <- getpos.nsp()
        if(any(is.null(input$boundary1.nsp), is.null(input$boundary2.nsp))){
          boundaries <- c(min(x), max(x))
        } else {
          boundaries <-c(input$boundary1.nsp, input$boundary2.nsp)
        }
        b <- ns(x, knots=pos, Boundary.knots = boundaries)

        #TODO: push up to data creation - no need for recalc every time
        data <- data %>%
            group_by(!!sym(var_list$x)) %>%
            mutate(y_mean = mean(!!sym(var_list$y)))

        coefs <- getcoef.nsp()
        spline <- apply(b, 1, function(x, coefs.in = coefs) {
            res <- 0
            for(i in 1:length(x)){
                res <- res + coefs.in[i]*x[i]
            }
            res
        })

        intercept <- getintercept.nsp()
        spline <- intercept + spline
        p <- ggplot(data=data)
        if(input$add_y.nsp){
            p <- p + geom_point(aes(x=!!sym(var_list$x), y=!!sym(var_list$y)), color = "lightgrey")
        }
        if(input$add_mean.nsp){
            p <- p + geom_line(aes(x=!!sym(var_list$x), y = y_mean), color = "blue")
        }
        if(input$add_knots_pos.nsp){
            knots <- attr(b, "knots")
            boundaries <- attr(b, "Boundary.knots")
            for(i in knots){
                p <- p + geom_vline(xintercept=i, color = "red", linetype="dashed")
            }
            p <- p + geom_vline(xintercept=boundaries[1], color = "red4", linetype="dashed")+
              geom_vline(xintercept=boundaries[2], color = "red4", linetype="dashed")
        }

        p <- p +geom_line(aes(x=!!sym(var_list$x), y = spline)) +
                theme_minimal() +
                ylab(var_list$y)
        ggplotly(p)
    })

    output$basis_plot.nsp<- renderPlotly({
      req(input$nknots.nsp)
        var_list <- var_list_reac()
        data <- var_list$data
        x <- data[,var_list$x]
        pos <- getpos.nsp()
        if(any(is.null(input$boundary1.nsp), is.null(input$boundary2.nsp))){
          boundaries <- c(min(x), max(x))
        } else {
          boundaries <-c(input$boundary1.nsp, input$boundary2.nsp)
        }
        b <- ns(x, knots=pos, Boundary.knots = boundaries)
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
        if(input$add_knots_pos.nsp){
            knots <- attr(b, "knots")
            boundaries <- attr(b, "Boundary.knots")
            for(i in knots){
                p <- p + geom_vline(xintercept=i, color = "red", linetype="dashed")
            }
            p <- p + geom_vline(xintercept=boundaries[1], color = "red4", linetype="dashed")+
              geom_vline(xintercept=boundaries[2], color = "red4", linetype="dashed")
        }
        ggplotly(p)
    })

    calcR2.nsp <- reactive({
        var_list <- var_list_reac()
        data <- var_list$data
        x <- data[,var_list$x]
        pos <- getpos.nsp()
        b <- ns(x, knots=pos)
        coefs <- getcoef.nsp()
        spline <- apply(b, 1, function(x, coefs.in = coefs) {
            res <- 0
            for(i in 1:length(x)){
                res <- res + coefs.in[i]*x[i]
            }
            res
        })
        spline <- spline + getintercept.nsp()
        model <- lm(as.formula(paste0(var_list$y, " ~ ns(", var_list$x, ", df=", 1+length(pos),
                                      ", Boundary.knots=c(", ifelse(is.null(input$boundary1.nsp), min(x), input$boundary1.nsp),
                                      ",",ifelse(is.null(input$boundary2.nsp), max(x), input$boundary2.nsp), "))")), data=data)
        fitted <- model$fitted
        p <- model$rank
        sstot <- sum((data[,var_list$y]-mean(data[,var_list$y]))^2) #total sum of squares
        ssres <- sum((data[, var_list$y]-spline)^2)  #residual sum of squares
        ssres_fitted <- sum((data[, var_list$y]-fitted)^2)  #residual sum of squares fitted
        c(1-ssres/sstot, 1-ssres_fitted/sstot, p)
    })

    calcadjR2.nsp <- reactive({
        res <- calcR2.nsp()
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

    output$stats.nsp <-  renderUI({
        stats <- calcadjR2.nsp()
        nknots <- input$nknots.nsp
        HTML(paste0("R <sup>2</sup>: ", round(stats[1], 3), "<br> adj. R <sup>2</sup>: ", round(stats[2], 3),
                    "<br> max. R <sup>2</sup> for ", nknots," internal knots and Boundary knots set to ", input$boundary1.nsp," and ", input$boundary2.nsp ," : ", round(stats[3], 3)))
    })

    #reset button
    observeEvent(input$reset_input.nsp, {
        shinyjs::reset("inputs.nsp") #id of tab to reset
    })
    observe({
      if(input$adjust_intercept.nsp){
        shinyjs::disable("intercept.nsp")
      }
      if(!input$adjust_intercept.nsp){
        shinyjs::enable("intercept.nsp")
      }
    })
    observe({
      if(!(all(input$add_mean.nsp, input$add_y.nsp))){
        shinyjs::disable("adjust_intercept.nsp")
        shinyjs::disable("intercept.nsp")
      }
      if(any(input$add_mean.nsp, input$add_y.nsp)){
        shinyjs::enable("adjust_intercept.nsp")
        shinyjs::enable("intercept.nsp")
      }
    })
    
    session$onSessionEnded(stopApp) #automatically stop when closing browser
}