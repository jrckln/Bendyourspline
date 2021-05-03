function(input, output, session){
  
    observe_helpers(help_dir = "help_mds")
  
    observeEvent(input$link_methods, {
      updateNavbarPage(session, "navbar", "Methods")
    })
  
    #############################################
    #######         Data            #############
    #############################################
    
    output$information.vars <- renderUI({
        var <- as.character(input$variable)
        var_list <- data_list[[var]]
        # HTML(paste0('Chosen variable pair: ', input$variable, "<br>", 
        #             var_list$x, " in ", var_list$x_unit, "<br>", 
        #             "filtered for sex: ", input$gender
        #             ))
        HTML("")
    })
    
    observeEvent(input$seed, { #recalculate max R2 values if seed is changed:
        if(input$seed!=14){
                source("data_generation.R")
                source("data/data.R")
                showNotification(span(tagList(icon("check"), "Done. ")), id="done_notif", duration = 7)
        }
    })
    
    var_list_reac <- reactive({
        var <- input$variable
        var_list <- data_list[[var]]
        var_list$data <- var_list$data[var_list$data[,"gender"] %in% gender[[input$gender]],]

        set.seed(input$seed)
        n <- floor(sample.sizes[input$sample.size]*nrow(var_list$data))
        ind <- sample(1:nrow(var_list$data), n)
        var_list$data <- var_list$data[ind,]
        var_list
    })
    
    #############################################
    #######           FP            #############
    #############################################
    
    #increase/decrease range of coefs
    range_fp <- coef_range("fp")
    observeEvent(range_fp(), {
      updateSliderInput(session, "val_coef1_fp-coef", min = (-1)*range_fp(), max = range_fp())
      updateSliderInput(session, "val_coef2_fp-coef", min = (-1)*range_fp(), max = range_fp())
    })
    
    coef1.fp <- sliderpl("val_coef1_fp")
    coef2.fp <- sliderpl("val_coef2_fp")

    
    output$formula.fp <- renderUI({
        pow1 <- as.numeric(input$power1.fp)
        
        trans1 <- paste0("x^{", pow1, "}")
        if(pow1 == 0) trans1 <- "\\log(x)"
        if(pow1 == 1) trans1 <- "x"
        coef1 <- as.numeric(coef1.fp())
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
        coef2 <- as.numeric(coef2.fp())
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
            intercept <- opt.intercept(fitted=data$fp, data=data[,"y"], interval=c(min(data[,"y"]), max(data[,"y"])))$minimum
            return(intercept)
        } else {
            return(input$intercept.fp)
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
        
        
        fp <- as.numeric(coef1.fp())*fp1 + as.numeric(coef2.fp())*fp2
        if(length(fp)==0){
            fp <- rep(0,nrow(data))
        }
        data <- data[, c(var_list$x, var_list$y)]
        names(data) <- c("x","y")
        DF <- cbind(data, transformed, fp, fp1, fp2)
        attr(DF, "names_vars") <- c(var_list$x, var_list$y)
        DF
    })
    
    output$plot.fp <- renderPlotly({
        intercept <- getintercept.fp()
        
        DF <- FPdata()
        p <- ggplot(data=DF)
        
        if(input$add_y.fp){
            p <- p + geom_point(aes(x=x, y=y), color = "lightgrey")
        }
        if(input$add_loess.fp){
            p <- p + geom_smooth(aes(x=x, y=y), method = "loess", formula = "y~x")
        }
        
        p <- p +geom_line(aes(x=x, y = intercept+fp)) + 
                theme_minimal() +
                ylab(attr(DF, "names_vars")[2]) + 
                xlab(attr(DF, "names_vars")[1])
        ggplotly(p)
    })
    
    output$basis_plot.fp <- renderPlotly({
        DF <- FPdata()
        p <- ggplot(data=DF) + 
            geom_line(aes(x=x, y=fp1)) +
            geom_line(aes(x=x, y=fp2))+ 
            theme_minimal()+
          ylab("")
        ggplotly(p)
    })
    
    calcR2.fp <- reactive({
        DF <- FPdata()
        fp <- getintercept.fp()+DF[, "fp"]

        fit <- mfp(x~ fp(transformed, df=4, scale=F), data = DF)
        rss <- sum((fit$residuals)^2)
        sstot <- sum((DF[,"y"]-mean(DF[,"y"]))^2)
        fittedR2 <- 1-rss/sstot
        ssres <- sum((DF[, "y"]-fp)^2)  #residual sum of squares
        c(1-ssres/sstot,fittedR2)
    })
    
    calcadjR2.fp <- reactive({
        res <- calcR2.fp() 
        R2 <- res[1]
        maxR2 <- res[2]
        var_list <- var_list_reac()
        data <- var_list$data
        
        p <- ifelse(coef1.fp() == 0& coef2.fp() == 0, 0, ifelse(any(coef1.fp() == 0, coef2.fp() == 0),1,2))
        c(R2, 1-(1-R2)*(nrow(data)-1)/(nrow(data)-1-p), maxR2)
    })
    
    observe({
      stats_val <- calcadjR2.fp()
      intercept_val <- getintercept.fp()
      stats("stats_fp", stats_val, intercept_val)
    })
    
    #reset button
    observeEvent(input$reset_input.fp, {
       reset("inputs.fp") #id of tab to reset
    })

    observe({
      if(input$adjust_intercept.fp){
        disable("intercept.fp")
      }
      if(!input$adjust_intercept.fp){
        enable("intercept.fp")
      }
    })
    
    observe({
      if(!(all(input$add_mean.fp, input$add_y.fp))){
        disable("adjust_intercept.fp")
        disable("intercept.fp")
      }
      if(any(input$add_mean.fp, input$add_y.fp)){
        enable("adjust_intercept.fp")
        enable("intercept.fp")
      }
    })
    
    #reset button
    observeEvent(input$reset_input.fp, {
       shinyjs::reset("inputs.fp")
    })
    
    #############################################
    #######        B-splines        #############
    #############################################
    
    inserted.pos.bs <- c()
    range_bs <- coef_range("bs")
    
    #update range of coefficient sliders
    observeEvent(range_bs(), {
      req(input$nknots.bs)
      num <- input$degree.bs + input$nknots.bs
      #get values of coefficients:
      ind <- paste0("bs_coef", 1:num, "-coef")
      for(i in ind){
        updateSliderInput(session, i, min=(-1)*range_bs(), max = range_bs())
      }
    })
    
    observeEvent(c(input$nknots.bs,input$variable), {
        req(input$nknots.bs)
        if(length(inserted.pos.bs)!=0){ #case of update or variable change
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
                                                                                          num[i]),
                                                  value=default.pos.knots.bs[num[i]], step=0.01,
                                                  min=min(x), max=max(x), ticks = FALSE), id=id[i])
                    )
                    inserted.pos.bs <<- c(inserted.pos.bs, id[i])
                }
            } else if(length(inserted.pos.bs)==as.numeric(input$nknots.bs)){ #case of variable change
                # update default knot positions
                default.pos.knots.bs <- seq.int(from = 0, to = 1, length.out = input$nknots.bs + 2)[-c(1, input$nknots.bs + 2)]
                var_list <- var_list_reac()
                data <- var_list$data
                x <- data[,var_list$x]
                default.pos.knots.bs <- quantile(x, default.pos.knots.bs)
                num <- 1:input$nknots.bs
                id <- paste0('bs_pos', num)
                for(i in 1:length(num)){
                  updateSliderInput(session, paste0(id[i], "_inner"), min=min(x), max=max(x),value = as.numeric(default.pos.knots.bs[num[i]]))
                }
            }
        }else{ #case of init
            default.pos.knots.bs <- seq.int(from = 0, to = 1, length.out = input$nknots.bs + 2)[-c(1, input$nknots.bs + 2)]
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
                                              min=min(x), max=max(x), ticks = FALSE), id=id[i])
                )
                inserted.pos.bs <<- c(inserted.pos.bs, id[i])
            }
        }
    })

    #dynamic insert of slider for coefficients for each spline basis function
    inserted.coef.bs <- c()
    coef_vals_bs <- reactiveValues()
    
    observeEvent(c(input$nknots.bs, input$degree.bs, input$variable), {
        req(input$nknots.bs)
        num <- input$degree.bs + input$nknots.bs 
        if(length(inserted.coef.bs)!=0){ #case of update
            if(length(inserted.coef.bs)>num){
                    toomuch <- length(inserted.coef.bs)-num
                    for(i in 1:toomuch){
                      removeUI(
                        selector = paste0('#', inserted.coef.bs[length(inserted.coef.bs)])
                      )
                      coef_vals_bs[[inserted.coef.bs[[length(inserted.coef.bs)]]]] <- NULL
                      inserted.coef.bs <<- inserted.coef.bs[-length(inserted.coef.bs)]
                    }
            }else if(length(inserted.coef.bs)<num){
                toinsert <- (length(inserted.coef.bs)+1):num
                id <- paste0('bs_coef', toinsert)
                for(i in 1:length(toinsert)){
                  insertUI(
                    selector = '#placeholder_coef_bs',
                    ui = sliderplUI(id[i], range_slider = range_bs())
                  )
                  coef_vals_bs[[id[i]]] <- sliderpl(id[i])
                  inserted.coef.bs <<- c(inserted.coef.bs, id[i])
                }
            }
        } else { #case of init
            id <- paste0('bs_coef', 1:num)
            for(i in 1:num){
              insertUI(
                    selector = '#placeholder_coef_bs',
                    ui = sliderplUI(id[i])
              )
              coef_vals_bs[[id[i]]] <- sliderpl(id[i])
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
        if(length(pos)!=input$nknots.bs){
          return(numeric(input$nknots.bs))
        } else {
          return(pos)
        }
    })

    getcoef.bs <- reactive({
      req(input$nknots.bs)
      num <- input$degree.bs + input$nknots.bs
      #get values of coefficients:
      ind <- paste0("bs_coef", 1:num)
      coef <- c()
      for(i in ind){
          coef <- c(coef, as.numeric(coef_vals_bs[[i]]()))
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
    
    getoptfit.bs <- reactive({
      var_list <- var_list_reac()
      x <- var_list$data[,var_list$x]
      degree <- input$degree.bs
      pos <- getpos.bs()
      b <- bs(x, degree=degree, knots=pos)
      DF <- cbind(var_list$data, b)
      colnames(DF) <- c(colnames(var_list$data), paste0("spline", 1:ncol(b)))
      optfit <- lm(as.formula(paste0(var_list$y, "~", paste0(paste0("spline", 1:ncol(b)), collapse="+"))), data=DF)
      optfit$coefficients
    })
    
    output$plot.bs <- renderPlotly({
        req(input$nknots.bs)
        var_list <- var_list_reac()
        data <- var_list$data
        x <- data[,var_list$x]
        degree <- input$degree.bs

        pos <- getpos.bs()
        b <- bs(x, degree=degree, knots=pos)

        coefs <- getcoef.bs()
        intercept <- getintercept.bs()
        spline <- rowSums(b %*% coefs)+intercept

        p <- ggplot(data=data)
        if(input$add_y.bs){
            p <- p + geom_point(aes(x=!!sym(var_list$x), y=!!sym(var_list$y)), color = "lightgrey")
        }
        if(input$add_loess.bs){
            p <- p + geom_smooth(aes(x=!!sym(var_list$x), y=!!sym(var_list$y)), method = "loess", formula = "y~x")
        }
        if(input$add_knots_pos.bs){
          knots <- attr(b, "knots")
          quant <- round(quantInv(x, knots),2)
          y_coord <- rowSums(predict(b, knots) %*% coefs)+intercept
          knots_df <- data.frame("x" = knots, 
                                 "y" = y_coord)
          p <- p + geom_point(data=knots_df, aes(x=x, y=y), alpha = 0.5)+
              annotate(geom = "text", x = knots, y = y_coord, label = paste("Q ",quant), hjust = "left")
        }
        if(input$add_optfit.bs){
          optcoef <- getoptfit.bs()
          optline <- as.numeric(cbind(1,b) %*% optcoef)
           p <- p + geom_line(aes(x=!!sym(var_list$x), y = optline), color = "orange")
        }

        p <- p +geom_line(aes(x=!!sym(var_list$x), y = spline)) +
                theme_minimal() +
                ylab(var_list$y)
        ggplotly(p)
    })
    
    observe({
      stats_val <- calcadjR2.bs()
      intercept_val <- getintercept.bs()
      stats("stats_bs", stats_val, intercept_val)
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
        req(input$nknots.bs)
        var_list <- var_list_reac()
        data <- var_list$data
        x <- data[,var_list$x]
        degree <- input$degree.bs
        pos <- getpos.bs()
        b <- bs(x, degree=degree, knots=pos)
        coefs <- getcoef.bs()
        spline <- rowSums(b %*% coefs)+getintercept.bs()
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
    #reset button
    observeEvent(input$reset_input.bs, {
       reset("inputs.bs") #id of tab to reset
    })
    observe({
      if(input$adjust_intercept.bs){
        disable("intercept.bs")
      }
      if(!input$adjust_intercept.bs){
        enable("intercept.bs")
      }
    })
    observe({
      if(!(all(input$add_mean.bs, input$add_y.bs))){
        disable("adjust_intercept.bs")
        disable("intercept.bs")
      }
      if(any(input$add_mean.bs, input$add_y.bs)){
        enable("adjust_intercept.bs")
        enable("intercept.bs")
      }
    })

    
    #############################################
    #######      Natural-splines    #############
    #############################################
    
    #dynamic insert of slider for positions of knots
    inserted.pos.nsp <- c()
    range_nsp <- coef_range("nsp")
    
    #update range of coefficient sliders
    observeEvent(range_nsp(), {
      req(input$nknots.nsp)
      num <- 1 + input$nknots.nsp
      #get values of coefficients:
      ind <- paste0("nsp_coef", 1:num, "-coef")
      for(i in ind){
        updateSliderInput(session, i, min = (-1)*range_nsp(), max = range_nsp())
      }
    })
    
    output$boundary_knots.nsp <- renderUI({
      req(input$nknots.nsp)
      var_list <- var_list_reac()
      data <- var_list$data
      x <- data[,var_list$x]
      default.pos.knots.nsp <- seq.int(from = 0,to = 1,length.out = input$nknots.nsp + 2)[-c(1, input$nknots.nsp + 2)]
      pos <- quantile(x, default.pos.knots.nsp)
      div(
      sliderInput("boundary1.nsp", "Position of Boundary knot 1", min=min(x), max=pos[1]-0.1, value=min(x), step=0.1, ticks = FALSE),
      sliderInput("boundary2.nsp", "Position of Boundary knot 2", min=pos[length(pos)]+0.1, max=max(x), value=max(x), step=0.1, ticks = FALSE)
      )
    })
    #update max and min val of boundary knots slider according to second and second to last knot position
    observe({
      pos <- getpos.nsp()
      updateSliderInput(session, "boundary1.nsp", max=pos[1]-0.1)
      updateSliderInput(session, "boundary2.nsp", min = pos[length(pos)]+0.1)
    })


    observeEvent(c(input$nknots.nsp,input$variable), {
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
                                                                                          num[i]),
                                                  value=default.pos.knots.nsp[num[i]], step=0.01,
                                                  min=min(x), max=max(x), ticks = FALSE), id=id[i])
                    )
                    inserted.pos.nsp <<- c(inserted.pos.nsp, id[i])
                }
            }else if(length(inserted.pos.nsp)==as.numeric(input$nknots.nsp)){ #case of variable change
                # update default knot positions
                var_list <- var_list_reac()
                data <- var_list$data
                x <- data[,var_list$x]
                updateSliderInput(session, "boundary1.nsp", value = min(x))
                updateSliderInput(session, "boundary2.nsp", value = max(x))
                default.pos.knots.nsp <- seq.int(from = 0,
                                                 to = 1,
                                                 length.out = input$nknots.nsp + 2)[-c(1, input$nknots.nsp + 2)]
                default.pos.knots.nsp <- quantile(x, default.pos.knots.nsp)
                num <- 1:input$nknots.nsp
                id <- paste0('nsp_pos', num)
                for(i in 1:length(num)){
                  updateSliderInput(session, paste0(id[i], "_inner"), min=min(x), max=max(x),  value = as.numeric(default.pos.knots.nsp[num[i]]))
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
                                              min=min(x), max=max(x), ticks = FALSE), id=id[i])
                )
                inserted.pos.nsp <<- c(inserted.pos.nsp, id[i])
            }
        }
    })

    #dynamic insert of slider for coefficients for each spline basis function
    inserted.coef.nsp <- c()
    coef_vals_nsp <- reactiveValues()
    
    observeEvent(c(input$nknots.nsp,input$variable), {
      req(input$nknots.nsp)
        num <- 1 + input$nknots.nsp #+1 for intercept but intercept is extra
        if(length(inserted.coef.nsp)!=0){ #case of update
            #num is the number of desired inputs
            if(length(inserted.coef.nsp)>num){
                    toomuch <- length(inserted.coef.nsp)-num
                    for(i in 1:toomuch){
                      removeUI(
                        selector = paste0('#', inserted.coef.nsp[length(inserted.coef.nsp)])
                      )
                      coef_vals_nsp[[inserted.coef.nsp[[length(inserted.coef.nsp)]]]] <- NULL
                      inserted.coef.nsp <<- inserted.coef.nsp[-length(inserted.coef.nsp)]
                    }
            }else if(length(inserted.coef.nsp)<num){
                toinsert <- (length(inserted.coef.nsp)+1):num
                id <- paste0('nsp_coef', toinsert)
                for(i in 1:length(toinsert)){
                    insertUI(
                    selector = '#placeholder_coef_nsp',
                    ui = sliderplUI(id[i], range_slider = range_nsp())
                  )
                  coef_vals_nsp[[id[i]]] <- sliderpl(id[i])
                  inserted.coef.nsp <<- c(inserted.coef.nsp, id[i])
                }
            }
        } else { #case of init
            id <- paste0('nsp_coef', 1:num)
            for(i in 1:num){
                insertUI(
                    selector = '#placeholder_coef_nsp',
                    ui = sliderplUI(id[i])
                  )
                  coef_vals_nsp[[id[i]]] <- sliderpl(id[i])
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
        if(length(pos)!=input$nknots.nsp){
          return(numeric(input$nknots.nsp))
        } else {
          return(pos)
        }
    })

    getcoef.nsp <- reactive({
      req(input$nknots.nsp)
      num <- 1 + input$nknots.nsp
      #get values of coefficients:
      ind <- paste0("nsp_coef", 1:num)
      coef <- c()
      for(i in ind){
          coef <- c(coef, as.numeric(coef_vals_nsp[[i]]()))
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
    
    observe({
      stats_val <- calcadjR2.nsp()
      intercept_val <- getintercept.nsp()
      stats("stats_nsp", stats_val, intercept_val)
    })
    
    getoptfit.nsp <- reactive({
      var_list <- var_list_reac()
      x <- var_list$data[,var_list$x]
      degree <- input$degree.nsp
      pos <- getpos.nsp()
      if(any(is.null(input$boundary1.nsp), is.null(input$boundary2.nsp))){
          boundaries <- c(min(x), max(x))
        } else {
          boundaries <-c(input$boundary1.nsp, input$boundary2.nsp)
      }
      b <- ns(x, knots=pos, Boundary.knots = boundaries)
      DF <- cbind(var_list$data, b)
      colnames(DF) <- c(colnames(var_list$data), paste0("spline", 1:ncol(b)))
      optfit <- lm(as.formula(paste0(var_list$y, "~", paste0(paste0("spline", 1:ncol(b)), collapse="+"))), data=DF)
      optfit$coefficients
    })

    output$plot.nsp <- renderPlotly({
        req(input$nknots.nsp)
        req(input$boundary1.nsp)
        req(input$boundary2.nsp)
      
        var_list <- var_list_reac()
        data <- var_list$data

        x <- data[,var_list$x]

        pos <- getpos.nsp()
        boundaries <-c(input$boundary1.nsp, input$boundary2.nsp)
        b <- ns(x, knots=pos, Boundary.knots = boundaries)

        coefs <- getcoef.nsp()
        intercept <- getintercept.nsp()
        spline <- rowSums(b %*% coefs)+intercept
        
        p <- ggplot(data=data)
        if(input$add_y.nsp){
            p <- p + geom_point(aes(x=!!sym(var_list$x), y=!!sym(var_list$y)), color = "lightgrey")
        }
        if(input$add_loess.nsp){
            p <- p + geom_smooth(aes(x=!!sym(var_list$x), y=!!sym(var_list$y)), method = "loess", formula = "y~x")
        }
        if(input$add_knots_pos.nsp){
            knots <- attr(b, "knots")
            boundaries <- attr(b, "Boundary.knots")
            knots <- c(boundaries[1], knots, boundaries[2])
            quant <- round(quantInv(x, knots),2)
            y_coord <-rowSums(predict(b, knots) %*% coefs)+intercept
            knots_df <- data.frame("x" = knots, 
                                   "y" = y_coord)
            p <- p + geom_point(data=knots_df, aes(x=x, y=y), alpha = 0.5)+
              annotate(geom = "text", x = knots, y = y_coord, label = paste("Q ",quant), hjust = "left")
        }
        if(input$add_optfit.nsp){
          optcoef <- getoptfit.nsp()
          optline <- as.numeric(cbind(1,b) %*% optcoef)
           p <- p + geom_line(aes(x=!!sym(var_list$x), y = optline), color = "orange")
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
        bounds <- c(min(x), max(x))
        pos <- getpos.nsp()
        if(any(is.null(input$boundary1.nsp), is.null(input$boundary2.nsp))){
          boundaries <- c(min(x), max(x))
        } else {
          boundaries <-c(input$boundary1.nsp, input$boundary2.nsp)
        }
        b <- ns(x, knots=pos, Boundary.knots = boundaries)
        all.knots <- sort(c(bounds ,attr(b, "knots")))
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
      req(input$nknots.nsp)
      req(input$boundary1.nsp)
      req(input$boundary2.nsp)
        var_list <- var_list_reac()
        data <- var_list$data
        x <- data[,var_list$x]
        pos <- getpos.nsp()
        b <- ns(x, knots=pos, Boundary.knots = c(input$boundary1.nsp, input$boundary2.nsp))
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

    #reset button
    observeEvent(input$reset_input.nsp, {
        reset("inputs.nsp") #id of tab to reset
    })
    observe({
      if(input$adjust_intercept.nsp){
        disable("intercept.nsp")
      }
      if(!input$adjust_intercept.nsp){
        enable("intercept.nsp")
      }
    })
    observe({
      if(!(all(input$add_mean.nsp, input$add_y.nsp))){
        disable("adjust_intercept.nsp")
        disable("intercept.nsp")
      }
      if(any(input$add_mean.nsp, input$add_y.nsp)){
        enable("adjust_intercept.nsp")
        enable("intercept.nsp")
      }
    })

    
    session$onSessionEnded(stopApp) #automatically stop when closing browser
}