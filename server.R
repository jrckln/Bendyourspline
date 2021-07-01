function(input, output, session){
  
    query_modal <- modalDialog(
      title = 'Note',
      HTML(
        '
        <p>
        This website is still in the development phase. In case of crashes or unexpected behavior please report at 
        <a href="https://github.com/ljiricka/Bendyourspline/issues">Github</a>. <br>
        Thank you for your understanding!
        </p>
        '
      ),
      easyClose = F
    )
  
    # Show the model on start up ...
    showModal(query_modal)
  
  
    observe_helpers(help_dir = "help_mds")
  
    observeEvent(input$link_methods, {
      updateNavbarPage(session, "navbar", "Methods")
    })
    
    #code download module
    codeServer("code_fp", filename=c("www/codes/code_fp.R", "www/codes/helpers.R", "www/codes/code_data.R"))
    codeServer("code_bs", filename=c("www/codes/code_bs.R", "www/codes/code_data.R"))
    codeServer("code_nsp", filename=c("www/codes/code_nsp.R", "www/codes/code_data.R"))
    
    #starting tour guide
    observeEvent(input$help, {
          guide$init()$start()
    })
    
    #hide tour button if not on methods tabs
    observeEvent(input$navbar, {
       choice = input$navbar
       if(choice == "Methods"){
         runjs("document.getElementById('help').style.visibility = 'visible';")
       } else {
         runjs("document.getElementById('help').style.visibility = 'hidden';")
       }
    })
  
    #############################################
    #######         Data            #############
    #############################################
    
    output$information_vars <- renderUI({
        var <- as.character(input$variable)
        var_list <- data_list[[var]]
        filtered <- getdata()
        obs <- ifelse(input$sample.size == '100%', 
                      paste0('all observations (', nrow(var_list$data), ')'),
                      paste0(length(filtered$x), " out of ", nrow(var_list$data), ' observations'))
        filt <- ifelse(input$gender == "Both", "", paste0(', filtered for gender: ', input$gender))
        HTML(paste0('<p> Showing ', obs, filt, ' of variable ', var_list$x_name, ' in ', var_list$x_unit, 
                    ' with ', var_list$y_name, ' in ', var_list$y_unit, ' as response. </p>'
                    ))
    })

    getdata <- reactive({
      req(input$seed)
      var <- input$variable
      var_list <- data_list[[var]]
      var_list$data <- var_list$data[var_list$data[,"gender"] %in% gender[[input$gender]],]
      set.seed(input$seed)
      n <- floor(sample.sizes[input$sample.size]*nrow(var_list$data))
      ind <- sample(1:nrow(var_list$data), n)
      var_list$data <- var_list$data[ind,]
      x <- var_list$data[,var_list$x]
      data <- list("x" = x, 
                    "y" = var_list$data[, var_list$y], 
                   "names_vars" = c(var_list$x, var_list$y))
      return(data)
    })
    
    #create basic plot for response: 
    basic_plot <- eventReactive(c(input$add_y.fp, input$add_y.bs, input$add_y.nsp,
                                  input$variable, input$seed, input$gender, input$sample.size), {
       #get current tab: 
       cur_tab = input$tabsetmethods
       data <- getdata()
       names_vars <- data$names_vars
       data <- data.frame("x" = data$x, "y" = data$y)
       p <- ggplot(data = data)+ 
            theme_minimal() 
       if(cur_tab == 'Fractional Polynomials' & input$add_y.fp){
          p <- p + geom_point(aes(x=x, y=y), color = "lightgrey")+
                ylab(names_vars[2]) + 
                xlab(names_vars[1])
          return(p)
       } else if(cur_tab == 'bsplines' & input$add_y.bs){
          p <- p + geom_point(aes(x=x, y=y), color = "lightgrey")+
                ylab(names_vars[2]) + 
                xlab(names_vars[1])
          return(p)
       } else if(cur_tab == 'nsplines' & input$add_y.nsp){
          p <- p + geom_point(aes(x=x, y=y), color = "lightgrey")+
                ylab(names_vars[2]) + 
                xlab(names_vars[1])
          return(p)
       } else {
         return(p)
       }
    })
    
    #############################################
    #######           FP            #############
    #############################################
    
    #intercept: 
    output$intercept_slider_fp <- renderUI({
      data <- getdata()
      sliderInput("intercept.fp",label="Intercept",min = 0, max = round(max(data$y),0), value = 0, step = 0.1)
    })
    
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
        
        fp_fun <- paste(round(as.numeric(input$intercept.fp),2), coef1, "\\cdot", trans1)
        
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
        data <- getdata()
        x <- data$x
        pT <- fp.scale(x)
        withMathJax(paste0(
            "$$ x =  \\frac{ \\text{", data$names_vars[1] , "} + ", pT$shift, "}{", pT$scale ,"}$$"
        ))
    })
    
    observeEvent(input$adjust_intercept.fp, {
        req(input$intercept.fp)
        data <- FPdata()
        intercept <- opt.intercept(fitted=data$fp, data=data$y, interval=c(0, max(data$y)))$minimum
        updateSliderInput(session, 'intercept.fp', value = intercept)
    })
    
    FPdata <- reactive({
        data <- getdata()
        
        x <- data$x
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
        names_vars <- data$names_vars
        data <- data.frame('x' = data$x, 'y' = data$y)
        
        DF <- cbind(data, transformed, fp, fp1, fp2)
        attr(DF, "names_vars") <- names_vars
        DF
    })
    
    output$plot.fp <- renderPlotly({
        req(input$intercept.fp)
        intercept <- as.numeric(input$intercept.fp)
        
        DF <- FPdata()
        p <- basic_plot()
        
        if(input$add_loess_fp){
            p <- p + suppressWarnings(geom_smooth(data = DF, aes(x=x, y=y, text = "LOESS smoother"), 
                                                  method = "loess", formula = "y~x", se=FALSE, color = loesscol, size=0.5))
        }
        
        p <- p +geom_line(data = DF, aes(x=x, y = intercept+fp)) 
        if(input$add_optfit_fp){
          optcoef <- getoptfit.fp()
          p <- p + suppressWarnings(geom_line(data = DF, aes(x=x, y = optcoef[1]+ optcoef[2]*fp1 + optcoef[3]*fp2, 
                                 text="Optimal fit based on current settings"), color = optfitcol))
        }
        ggplotly(p, tooltip="text")
    })
    
    output$basis_plot.fp <- renderPlotly({
        DF <- FPdata()
        p <- ggplot(data=DF) + 
            geom_line(aes(x=x, y=fp1), color = col[1]) +
            geom_line(aes(x=x, y=fp2), color = col[2])+ 
            theme_minimal()+ ylab("")+xlab(attr(DF, "names_vars")[1])
        ggplotly(p)
    })
    
    calcR2.fp <- reactive({
        DF <- FPdata()
        fp <- as.numeric(input$intercept.fp)+DF$fp
        fit <- mfp(y~fp(transformed, df=4, scale=F), data = DF)
        rss <- sum((DF[,"y"]- fit$fitted)^2)
        sstot <- sum((DF$y-mean(DF$y))^2)
        fittedR2 <- 1-rss/sstot
        ssres <- sum((DF$y-fp)^2)  #residual sum of squares
        R2 <- 1-ssres/sstot
        maxR2 <-fittedR2
        
        prederr <- sd(DF$y-fp)
        
        p <- ifelse(coef1.fp() == 0& coef2.fp() == 0, 0, ifelse(any(coef1.fp() == 0, coef2.fp() == 0),1,2))
        c(R2, 1-(1-R2)*(nrow(DF)-1)/(nrow(DF)-1-4), maxR2, prederr)
    })
    
    observe({
      vals <- calcR2.fp()
      stats("stats_fp", vals)
    })
    
    getoptfit.fp <- reactive({
      DF <- FPdata()
      optfit <- lm(y ~ fp1+fp2, data=DF)
      optfit$coefficients
    })
    
    #reset button
    observeEvent(c(input$reset_input.fp, input$variable), {
       reset("inputs.fp") #id of tab to reset
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
    
    
    #############################################
    #######        B-splines        #############
    #############################################
    
    #intercept: 
    output$intercept_slider_bs <- renderUI({
      data <- getdata()
      sliderInput("intercept.bs",label="Intercept",min = 0, max = round(max(data$y),0), value = 0, step = 0.1)
    })
    
    
    getbasis.bs <- reactive({
      validate(
        need(is.numeric(input$degree.bs) & input$nknots.bs < 5, 'Please provide a valid degree.'),
        need(is.numeric(input$nknots.bs) & input$nknots.bs < 10, 'Please provide a valid number of knots.')
      )
      data <- getdata()
      degree <- input$degree.bs
      pos <- getpos.bs()
      b <- bs(data$x, degree=degree, knots=pos)
      colnames(b) <- paste0("spline", 1:ncol(b))
      data <- list("x" = data$x, 
                    "y" = data$y, 
                   "b" = b, 
                   "names_vars" = data$names_vars)
      return(data)
    })
    
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
    
    #position of knots: 
    observeEvent(c(input$nknots.bs,input$variable), {
        req(input$nknots.bs, input$degree.bs)
        validate(
          need(is.numeric(input$degree.bs) & input$degree.bs < 5, 'Please provide a valid degree.'),
          need(is.numeric(input$nknots.bs) & input$nknots.bs < 10, 'Please provide a valid number of knots.')
        )
        num <- input$nknots.bs
        # num ... the number of knots that should be there
        # length(inserted.pos.bs) ... the number of nots that are actually there
        if(length(inserted.pos.bs)!=0){ #case of update or variable change
            if(length(inserted.pos.bs)>num){
                #remove difference:
                difference <- length(inserted.pos.bs) - num
                for(i in 1:difference){
                  #remove the last added coefficient slider
                  removeUI(
                    selector = paste0('#', inserted.pos.bs[length(inserted.pos.bs)])
                  )
                  # remove the last added slider from the list
                  inserted.pos.bs <<- inserted.pos.bs[-length(inserted.pos.bs)]
                }
            }else if(length(inserted.pos.bs)< num){
                #determine default knot positions: Artificially add two knots so that these two artificial knots 
                #are the boundary knots which are then removed (to put the other in the middle). 
                default.pos.knots.bs <- seq.int(from = 0, to = 1, length.out = input$nknots.bs + 2)[-c(1, input$nknots.bs + 2)]
                data <- getbasis.bs()
                default.pos.knots.bs <- round(quantile(data$x, default.pos.knots.bs), 3)
                toinsert <- (length(inserted.pos.bs)+1):num # which to insert
                id <- paste0('bs_pos', toinsert)
                for(i in 1:length(toinsert)){
                    insertUI(
                        selector = '#placeholder_pos_bs',
                        ui = tags$div(sliderInput(paste0(id[i], "_inner"), label = paste0("Position of knot ",toinsert[i]),
                                                  value=default.pos.knots.bs[toinsert[i]], step=0.1,
                                                  min=min(data$x), max=max(data$x), ticks = FALSE), id=id[i])
                    )
                    inserted.pos.bs <<- c(inserted.pos.bs, id[i])
                }
            } else if(length(inserted.pos.bs)== num){ #case: variable change
                # update default knot positions according to values of new variable
                default.pos.knots.bs <- seq.int(from = 0, to = 1, length.out = num + 2)[-c(1, num + 2)]
                data <- getbasis.bs()
                default.pos.knots.bs <- round(quantile(data$x, default.pos.knots.bs), 3)
                toupdate <- 1:num #update all available sliders
                id <- paste0('bs_pos', toupdate)
                for(i in 1:length(toupdate)){
                  updateSliderInput(session, paste0(id[i], "_inner"), min=min(data$x), max=max(data$x),
                                    value = as.numeric(default.pos.knots.bs[toupdate[i]]))
                }
            }
        }else{ #case of initialisation:
            default.pos.knots.bs <- seq.int(from = 0, to = 1, length.out = input$nknots.bs + 2)[-c(1, input$nknots.bs + 2)]
            data <- getbasis.bs()
            default.pos.knots.bs <- round(quantile(data$x, default.pos.knots.bs), 3)
            toinsert <- 1:num
            id <- paste0('bs_pos', toinsert)
            for(i in 1:length(toinsert)){
                insertUI(
                    selector = '#placeholder_pos_bs',
                    ui = tags$div(sliderInput(paste0(id[i], "_inner"), label = paste0("Position of knot ", i),
                                              value=default.pos.knots.bs[i], step=0.1,
                                              min=min(data$x), max=max(data$x), ticks = FALSE), id=id[i])
                )
                inserted.pos.bs <<- c(inserted.pos.bs, id[i])
            }
        }
    })

    #dynamic insert of slider for coefficients for each spline basis function
    inserted.coef.bs <- c()
    coef_vals_bs <- list()
    
    # coefficient sliders
    observeEvent(c(input$nknots.bs, input$degree.bs, input$variable), {
        validate(
          need(is.numeric(input$degree.bs) & input$degree.bs < 5, 'Please provide a valid degree.'),
          need(is.numeric(input$nknots.bs) & input$nknots.bs < 10, 'Please provide a valid number of knots.')
        )
        req(input$nknots.bs)
        req(input$degree.bs)
        num <- input$degree.bs + input$nknots.bs 
        # num ... the number of coefficient sliders that should be there
        # length(inserted.pos.bs) ... the number of coefficients that are actually there
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
                    ui = sliderplUI(id[i], range_slider = range_bs(), label = paste0('$$\\beta_',length(inserted.coef.bs)+ i, "$$"))
                  )
                  coef_vals_bs[[id[i]]] <<- sliderpl(id[i])
                  inserted.coef.bs <<- c(inserted.coef.bs, id[i])
                }
            }
        } else { #case of initialisation
            id <- paste0('bs_coef', 1:num)
            for(i in 1:num){
              insertUI(
                    selector = '#placeholder_coef_bs',
                    ui = sliderplUI(id[i], label = paste0('$$\\beta_', i, "$$"))
              )
              coef_vals_bs[[id[i]]] <<- sliderpl(id[i])
              inserted.coef.bs <<- c(inserted.coef.bs, id[i])
            }
        }

    })
    
    getpos.bs <- reactive({
        req(input$nknots.bs)
        #get values of knot positions:
        names <- paste0("bs_pos", 1:input$nknots.bs, "_inner")
        pos <- c()
        for(i in names){
          pos <- c(pos, input[[i]])
        }
        pos <- pos[!is.na(pos)]
        if(length(pos)!=input$nknots.bs){
          return(numeric(input$nknots.bs))
        } else {
          return(pos)
        }
    })

    getcoef.bs <- reactive({
      req(input$nknots.bs, input$degree.bs)
      req(length(coef_vals_bs) == input$degree.bs + input$nknots.bs)
      num <- input$degree.bs + input$nknots.bs
      #get values of coefficients:
      ind <- paste0("bs_coef", 1:num)
      coef <- c()
      # if(length(coef_vals_bs)!= num){
      #     return(numeric(num))
      # }
      for(i in ind){
          coef <- c(coef, as.numeric(coef_vals_bs[[i]]()))
      }
      return(coef)
    })

    observeEvent(input$adjust_intercept.bs, {
      req(input$intercept.bs)
      data <- getbasis.bs()
      coefs <- getcoef.bs()
      spline <- rowSums(data$b %*% coefs)
      intercept <- opt.intercept(fitted=spline, data=data$y, interval=c(0, max(data$y)))$minimum
      updateSliderInput(session, "intercept.bs", value = intercept)
    })
    
    getoptfit.bs <- reactive({
      data <- getbasis.bs()
      data <- data.frame(cbind(data$x, data$y, data$b))
      names(data) <- c("x","y", paste0("spline", 1:(ncol(data)-2)))
      optfit <- lm(as.formula(paste0("y~", paste0(paste0("spline", 1:(ncol(data)-2)), collapse="+"))), data=data)
      optfit$coefficients
    })
    
    output$plot.bs <- renderPlotly({
        req(input$nknots.bs, input$intercept.bs)
        data <- getbasis.bs()
        b <- data$b
        
        data <- data.frame("x" = data$x, "y" = data$y)
        pos <- getpos.bs()
        coefs <- getcoef.bs()
        intercept <- as.numeric(input$intercept.bs)
        spline <- rowSums(b %*% coefs)+intercept

        p <- basic_plot()
        
        if(input$add_loess_bs){
            p <- p + suppressWarnings(geom_smooth(data = data, aes(x=x, y=y, text = "LOESS smoother"),
                                                  method = "loess", formula = "y~x", se=FALSE, color = loesscol, size=0.5))
        }
        if(input$add_knots_pos.bs){
          knots <- attr(b, "knots")
          quant <- round(quantInv(data$x, knots),2)
          y_coord <- max(data$y)
          knots_df <- data.frame("x" = knots,
                                 "y" = y_coord)
          p <- p + suppressWarnings(geom_vline(data=knots_df,
                                               aes(xintercept=x, text = "Spline knot and corresponding quantile"),
                                               color = "#D3D3D3"))+
              annotate(geom = "text", x = knots, y = y_coord, label = paste("Q ",quant), hjust = "left")
        }
        if(input$add_optfit_bs){
          optcoef <- getoptfit.bs()
          optline <- as.numeric(cbind(1,b) %*% optcoef)
           p <- p + suppressWarnings(geom_line(data = data, aes(x=x, y = optline, text = "Optimal fit based on current knot positions"), color = optfitcol))
        }

        p <- p +geom_line(data = data, aes(x=x, y = spline)) 
        ggplotly(p, tooltip = "text")
    })
    
    observe({
      vals <- calcR2.bs()
      stats("stats_bs", vals)
    })

    output$basis_plot.bs<- renderPlotly({
        req(input$nknots.bs)
        data <- getbasis.bs()
        names_vars <- data$names_vars
        b <- data$b
        data <- cbind(data$x, data$y)
        degree <- input$degree.bs
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
            xlab(names_vars[1])+ylab("")
        if(input$add_knots_pos.nsp){
          p <- p + geom_vline(xintercept=all.knots, color = "#D3D3D3")
        }
        ggplotly(p)
    })

    calcR2.bs <- reactive({
        req(input$nknots.bs)
        data <- getbasis.bs()
        b <- data$b
        data <- data.frame("x" = data$x, "y"= data$y)
        coefs <- getcoef.bs()
        degree <- input$degree.bs
        spline <- rowSums(b %*% coefs)+as.numeric(input$intercept.bs)
        model <- lm(as.formula(paste0("y ~ bs(x, df=", degree+length(coefs),")")), data=data)
        fitted <- model$fitted
        p <- model$rank
        sstot <- sum((data$y-mean(data$y))^2) #total sum of squares
        ssres <- sum((data$y-spline)^2)  #residual sum of squares
        ssres_fitted <- sum((data$y-fitted)^2)  #residual sum of squares fitted
        R2 <- 1-ssres/sstot
        maxR2 <- 1-ssres_fitted/sstot
        
        prederr <- sd(data$y-spline)
        
        c(R2, 1-(1-R2)*(nrow(data)-1)/(nrow(data)-1-p), maxR2, prederr)
        })
    
    #reset button
    observeEvent(c(input$reset_input.bs, input$variable), {
       reset("inputs.bs") #id of tab to reset
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
    
    #intercept: 
    output$intercept_slider_nsp <- renderUI({
      data <- getdata()
      sliderInput("intercept.nsp",label="Intercept",min = 0, max = round(max(data$y), 0), value = 0, step = 0.1)
    })
    
    #dynamic insert of slider for positions of knots
    inserted.pos.nsp <- c()
    range_nsp <- coef_range("nsp")
    
    getbasis.nsp <- reactive({
      req(input$boundary1.nsp, input$boundary2.nsp)
      validate(
        need(is.numeric(input$nknots.nsp) & input$nknots.nsp < 10, 'Please provide a valid number of internal knots.')
      )
      data <- getdata()
      pos <- getpos.nsp()
      b <- ns(data$x, knots = pos, Boundary.knots = c(input$boundary1.nsp, input$boundary2.nsp))
      colnames(b) <- paste0("spline", 1:ncol(b))
      data <- list("x" = data$x, 
                    "y" = data$y,
                   "b" = b, 
                   "names_vars" = data$names_vars)
      return(data)
    })
    
    output$boundary_knots.nsp <- renderUI({
      req(input$nknots.nsp)
      data <- getdata()
      pos <- seq.int(from = 0,to = 1,length.out = input$nknots.nsp + 2)[-c(1, input$nknots.nsp + 2)]
      pos <- round(quantile(data$x, pos), 3)
      maxx <- round(max(data$x), 3)
      minx <- round(min(data$x), 3)
      div(
      sliderInput("boundary1.nsp", "Position of Boundary knot 1", min=minx, max=pos[1]-0.1, value=minx, 
                  step=0.1, ticks = FALSE),
      sliderInput("boundary2.nsp", "Position of Boundary knot 2", min=pos[length(pos)]+0.1, max=maxx, 
                  value=maxx, step=0.1, ticks = FALSE)
      )
    })
    #update max and min val of boundary knots slider according to second and second to last knot position
    observeEvent({c(input[[paste0("nsp_pos1_inner")]], input[[paste0("nsp_pos",input$nknots.nsp,"_inner")]])},{
      pos <- getpos.nsp()
      updateSliderInput(session, "boundary1.nsp", max=pos[1]-0.1)
      updateSliderInput(session, "boundary2.nsp", min = pos[length(pos)]+0.1)
    })
    
    
    observeEvent(input$variable, {
      data <- getdata()
      minx <- round(min(data$x), 3)
      maxx <- round(max(data$x), 3)
      default.pos.knots.nsp <- seq.int(from = 0,to = 1,length.out = input$nknots.nsp + 2)[-c(1, input$nknots.nsp + 2)]
      default.pos.knots.nsp <- round(quantile(data$x, default.pos.knots.nsp), 3)
      toupdate <- 1:input$nknots.nsp
      id <- paste0('nsp_pos', toupdate)
      for(i in 1:length(toupdate)){
        updateSliderInput(session, paste0(id[i], "_inner"), min=minx, max=maxx,
                         value = as.numeric(default.pos.knots.nsp[toupdate[i]]))
      }
      updateSliderInput(session, "boundary1.nsp", value = minx, min = minx, max= default.pos.knots.nsp[1]-0.1) 
      updateSliderInput(session, "boundary2.nsp", value = maxx, min = default.pos.knots.nsp[length(default.pos.knots.nsp)]+0.1, max= maxx)
    })
    
    # Internal Knot positions
    observeEvent(c(input$nknots.nsp, input$variable), {
        validate(
          need(is.numeric(input$nknots.nsp) & input$nknots.nsp < 10, 'Please provide a valid number of internal knots.')
        )
        req(input$nknots.nsp)
        num <- input$nknots.nsp
        if(length(inserted.pos.nsp)!=0){ #case of update
            if(length(inserted.pos.nsp)> num){
                # remove difference (always the last one added)
                difference <- length(inserted.pos.nsp) - input$nknots.nsp
                for(i in 1:difference){
                  removeUI(
                    selector = paste0('#', inserted.pos.nsp[length(inserted.pos.nsp)])
                  )
                    inserted.pos.nsp <<- inserted.pos.nsp[-length(inserted.pos.nsp)]
                }
            }else if(length(inserted.pos.nsp) < num){
                data <- getdata()
                minx <- min(data$x)
                maxx <- max(data$x)
                default.pos.knots.nsp <- seq.int(from = (input$boundary1.nsp-minx)/(maxx-minx),
                                                 to = (input$boundary2.nsp-minx)/(maxx-minx),
                                                 length.out = input$nknots.nsp + 2)[-c(1, input$nknots.nsp + 2)]
                default.pos.knots.nsp <- round(quantile(data$x, default.pos.knots.nsp), 3)
                toinsert <- (length(inserted.pos.nsp)+1):input$nknots.nsp
                id <- paste0('nsp_pos', toinsert)
                for(i in 1:length(toinsert)){
                    insertUI(
                        selector = '#placeholder_pos_nsp',
                        ui = tags$div(sliderInput(paste0(id[i], "_inner"), label = paste0("Position of knot ", toinsert[i]),
                                                  value=default.pos.knots.nsp[toinsert[i]], step=0.1,
                                                  min=minx, max=maxx, ticks = FALSE), id=id[i])
                    )
                    inserted.pos.nsp <<- c(inserted.pos.nsp, id[i])
                }
            }
        }else{ #case of initialisation
            default.pos.knots.nsp <- seq.int(from = 0, to = 1,
                                             length.out = input$nknots.nsp + 2)[-c(1, input$nknots.nsp + 2)]
            data <- getdata()
            default.pos.knots.nsp <- round(quantile(data$x, default.pos.knots.nsp), 3)
            toinsert <- 1:num
            id <- paste0('nsp_pos', toinsert)
            for(i in 1:length(toinsert)){
                insertUI(
                    selector = '#placeholder_pos_nsp',
                    ui = tags$div(sliderInput(paste0(id[i], "_inner"), label = paste0("Position of knot ", i),
                                              value=default.pos.knots.nsp[i], step=0.1,
                                              min=min(data$x), max=max(data$x), ticks = FALSE), id=id[i])
                )
                inserted.pos.nsp <<- c(inserted.pos.nsp, id[i])
            }
        }
    })

    #dynamic insert of slider for coefficients for each spline basis function
    inserted.coef.nsp <- c()
    coef_vals_nsp <- list()
    
    # coefficients
    observeEvent(c(input$nknots.nsp), {
        validate(
          need(is.numeric(input$nknots.nsp) & input$nknots.nsp < 10, 'Please provide a valid number of internal knots.')
        )
        req(input$nknots.nsp)
        num <- 1 + input$nknots.nsp
        if(length(inserted.coef.nsp)!=0){ #case of update
            #num is the number of coefficients that should be there
            if(length(inserted.coef.nsp)>num){
                toomuch <- length(inserted.coef.nsp)-num
                for(i in 1:toomuch){
                  removeUI(selector = paste0('#', inserted.coef.nsp[length(inserted.coef.nsp)]))
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
                  coef_vals_nsp[[id[i]]] <<- sliderpl(id[i])
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
              coef_vals_nsp[[id[i]]] <<- sliderpl(id[i])
              inserted.coef.nsp <<- c(inserted.coef.nsp, id[i])
            }
        }
    })
    
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

    getpos.nsp <- reactive({
        req(input$nknots.nsp)
        #get values of knot positions:
        names <- paste0("nsp_pos", 1:input$nknots.nsp, "_inner")
        pos <- c()
        for(i in names){
            pos <- c(pos, input[[i]])
        }
        pos <- pos[!is.na(pos)]
        if(length(pos) != input$nknots.nsp){
          return(numeric(input$nknots.nsp))
        } else {
          return(pos)
        }
    })

    getcoef.nsp <- reactive({
      req(input$nknots.nsp)
      req(length(coef_vals_nsp) == 1 + input$nknots.nsp)
      num <- 1 + input$nknots.nsp
      #get values of coefficients:
      ind <- paste0("nsp_coef", 1:num)
      coef <- c()
      if(length(coef_vals_nsp)!= num){
          return(numeric(num))
      }
      for(i in ind){
          coef <- c(coef, as.numeric(coef_vals_nsp[[i]]()))
      }
      return(coef)
    })

    observeEvent(input$adjust_intercept.nsp, {
        req(input$intercept.nsp)
        data <- getbasis.nsp()
        coefs <- getcoef.nsp()
        spline <- rowSums(data$b %*% coefs)
        intercept <- opt.intercept(fitted=spline, data=data$y, interval=c(0, max(data$y)))$minimum
        updateSliderInput(session, "intercept.nsp", value = intercept)
    })
    
    observe({
      vals <- calcR2.nsp()
      stats("stats_nsp", vals)
    })
    
    getoptfit.nsp <- reactive({
      req(input$boundary1.nsp, input$boundary2.nsp)
      data <- getbasis.nsp()
      pos <- getpos.nsp()
      DF <- data.frame(cbind(data$x, data$y, data$b))
      colnames(DF) <- c("x", "y", paste0("spline", 1:ncol(data$b)))
      optfit <- lm(as.formula(paste0("y~", paste0(paste0("spline", 1:ncol(data$b)), collapse="+"))), data=DF)
      optfit$coefficients
    })

    output$plot.nsp <- renderPlotly({
        req(input$nknots.nsp, input$boundary1.nsp, input$boundary2.nsp)
        data <- getbasis.nsp()
        boundaries <-c(input$boundary1.nsp, input$boundary2.nsp)
        b <- data$b
        pos <- getpos.nsp()
        coefs <- getcoef.nsp()
        intercept <- as.numeric(input$intercept.nsp)
        spline <- rowSums(b %*% coefs)+intercept
        
        var_names <- data$names_vars
        data <- data.frame("x" = data$x, "y" = data$y)
        
        p <- basic_plot()

        if(input$add_loess_nsp){
            p <- p + suppressWarnings(geom_smooth(data = data, aes(x=x, y=y, text="LOESS smoother"), 
                                                  method = "loess", formula = "y~x", se=FALSE, color = loesscol, size=0.5))
        }
        if(input$add_knots_pos.nsp){
            knots <- attr(b, "knots")
            boundaries <- attr(b, "Boundary.knots")
            knots <- c(boundaries[1], knots, boundaries[2])
            quant <- round(quantInv(data$x, knots),2)
            y_coord <- max(data$y)
            knots_df <- data.frame("x" = knots, 
                                   "y" = y_coord)
            p <- p + suppressWarnings(geom_vline(data=knots_df, aes(xintercept=x, text = "Knots and corresponding quantiles"), 
                                                 color = "#D3D3D3"))+
                annotate(geom = "text", x = knots, y = y_coord, label = paste("Q ",quant), hjust = "left")
        }
        if(input$add_optfit_nsp){
          optcoef <- getoptfit.nsp()
          optline <- as.numeric(cbind(1,b) %*% optcoef)
           p <- p + suppressWarnings(geom_line(data = data, aes(x=x, y = optline, text = "Optimal fit based on current knot position"), color = optfitcol))
        }

        p <- p +geom_line(aes(x=x, y = spline))
        ggplotly(p, tooltip = "text")
    })

    output$basis_plot.nsp<- renderPlotly({
        req(input$nknots.nsp, input$boundary1.nsp, input$boundary2.nsp)
        data <- getbasis.nsp()
        bounds <- c(min(data$x), max(data$x))
        pos <- getpos.nsp()
        b <- data$b
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
            xlab(data$names_vars[1])+ylab("")
        if(input$add_knots_pos.nsp){
            knots <- attr(b, "knots")
            boundaries <- attr(b, "Boundary.knots")
            for(i in knots){
                p <- p + geom_vline(xintercept=i, color = "#D3D3D3")
            }
            p <- p + geom_vline(xintercept=boundaries[1], color = "#D3D3D3")+
              geom_vline(xintercept=boundaries[2], color = "#D3D3D3")
        }
        ggplotly(p)
    })

    calcR2.nsp <- reactive({
        req(input$nknots.nsp)
        data <- getbasis.nsp()
        b <- data$b
        coefs <- getcoef.nsp()
        spline <- rowSums(b %*% coefs)+ as.numeric(input$intercept.nsp)
        data <- data.frame("x"=data$x, "y"=data$y)
        
        # #Problem occurs when changing variables: data is changed before boundaries are updated -> higher boundaries than data on the right and vice versa
        # boundaries <- sort(c(max(min(data$x), input$boundary1.nsp), min(max(data$x), input$boundary2.nsp)))
        # cat(file = stderr(), "boundaries: ", boundaries, "\n")
        # cat(file = stderr(), "mn and max: ", min(data$x), max(data$x), "\n")
        
        #TODO: make dependent of boundary knots
        model <- lm(as.formula(paste0("y ~ ns(x, df=", ncol(b),")")), data=data) #",Boundary.knots=c(", boundaries[1], ",", boundaries[2] ,
        
        fitted <- model$fitted
        p <- model$rank
        sstot <- sum((data$y-mean(data$y))^2) #total sum of squares
        ssres <- sum((data$y-spline)^2)  #residual sum of squares
        ssres_fitted <- sum((data$y-fitted)^2)  #residual sum of squares fitted
        R2 <- 1-ssres/sstot
        maxR2 <- 1-ssres_fitted/sstot
        
        prederr <- sd(data$y-spline)
        
        c(R2, 1-(1-R2)*(nrow(data)-1)/(nrow(data)-1-p), maxR2, prederr)
    })

    #reset button
    observeEvent(c(input$reset_input.nsp,input$variable), {
        reset("inputs.nsp") #id of tab to reset
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
    
    #############################################
    #######      Exercises          #############
    #############################################
    
    validation_all <- reactiveValues()
    
    #for fractional polynomials: 
    observe({
      req(input$start_exercise_fp>0)
      titles <- input$exercise_fp
      newval <- switch(titles, 
                       test = c(
                         input$tabsetmethods == 'Fractional Polynomials' & input$variable == 'Bmi ~ Age' & !input$add_y.fp & !input$add_loess_fp & !input$add_optfit_fp, 
                         input[['val_coef1_fp-coef']] == 1, 
                         input$power2.fp == 2 & input[['val_coef2_fp-coef']] == 1, 
                         input[['val_coef1_fp-coef']] == -1, 
                         input[['val_coef1_fp-coef']] == 1 & input[['val_coef2_fp-coef']] == -1, 
                         input[['val_coef2_fp-coef']] >= -(input[['val_coef1_fp-coef']]/(40/100))/2 - 0.05 & input[['val_coef2_fp-coef']] <= -(input[['val_coef1_fp-coef']]/(40/100))/2 + 0.05, 
                         TRUE, 
                         input$power2.fp == 2 & input$power1.fp == 2, 
                         TRUE, 
                         TRUE
                       )
        
      )
      validation_all$fp <- newval
    })
    
    counter_exercise_fp <- reactiveVal(0)
    observe({
       counter <- counter_exercise_fp()
       if(counter == 0){
         runjs("document.getElementById('start_exercise_fp').style.visibility = 'visible';")
       } else {
         runjs("document.getElementById('start_exercise_fp').style.visibility = 'hidden';")
       }
    })
        
    output$next_exercise_fp <- renderUI({
      req(input$start_exercise_fp, input$exercise_fp)
      counter <- counter_exercise_fp()
      validation <- validation_all$fp
      which <- as.character(input$exercise_fp)
        if((counter <= length(exercises[['fp']][[which]])) & (counter > 0)){
          tagList(
            HTML('<p>', paste0(as.character(exercises[['fp']][[which]][counter])), '</p>'), 
            if(validation[counter]){
              tagList(
              HTML('<svg class="checkmark" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 52 52">
                  <circle class="checkmark__circle" cx="26" cy="26" r="25" fill="none"/>
                  <path class="checkmark__check" fill="none" d="M14.1 27.2l7.1 7.2 16.7-16.8"/>
                </svg>
                 '), 
              actionButton('next_exercise_btn_fp', 'Next')
              )
            } else {
              HTML('')
            }
          )
        } else if(counter > length(exercises[['fp']][[which]])){
            if(any(input$start_exercise_fp == 1)){
              runjs("InitializeConfetti();")
            } else {
              runjs("RestartConfetti();")
            }
            runjs("$('#exercise_modal').modal().focus();")
            counter_exercise_fp(0)
            return('')
        }
    })
        
    observeEvent(input$next_exercise_btn_fp, {
      if(input$next_exercise_btn_fp > 0) {
        newVal <- counter_exercise_fp() + 1
        counter_exercise_fp(newVal)
      }
    })
    observeEvent(input$start_exercise_fp, {
      if(input$start_exercise_fp > 0) {
        newVal <- counter_exercise_fp() + 1
        counter_exercise_fp(newVal)
      }
    })
    
    #for BSplines: 
    observe({
      req(input$start_exercise_bs>0)
      titles <- input$exercise_bs
      newval <- switch(titles, 
                       Christine = c(
                         TRUE
                       )
        
      )
      validation_all$bs <- newval
    })
    
    counter_exercise_bs <- reactiveVal(0)
    observe({
       counter <- counter_exercise_bs()
       if(counter == 0){
         runjs("document.getElementById('start_exercise_bs').style.visibility = 'visible';")
       } else {
         runjs("document.getElementById('start_exercise_bs').style.visibility = 'hidden';")
       }
    })
        
    output$next_exercise_bs <- renderUI({
      req(input$start_exercise_bs, input$exercise_bs)
      counter <- counter_exercise_bs()
      validation <- validation_all$bs
      which <- as.character(input$exercise_bs)
        if((counter <= length(exercises[['bs']][[which]])) & (counter > 0)){
          tagList(
            HTML('<p>', paste0(as.character(exercises[['bs']][[which]][counter])), '</p>'), 
            if(validation[counter]){
              tagList(
              HTML('<svg class="checkmark" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 52 52">
                  <circle class="checkmark__circle" cx="26" cy="26" r="25" fill="none"/>
                  <path class="checkmark__check" fill="none" d="M14.1 27.2l7.1 7.2 16.7-16.8"/>
                </svg>
                 '), 
              actionButton('next_exercise_btn_bs', 'Next')
              )
            } else {
              HTML('')
            }
          )
        } else if(counter > length(exercises[['bs']][[which]])){
            if(any(input$start_exercise_bs == 1, input$start_exercise_fp == 1)){
              runjs("InitializeConfetti();")
            } else {
              runjs("RestartConfetti();")
            }
            runjs("$('#exercise_modal').modal().focus();")
            counter_exercise_bs(0)
            return('')
        }
    })
        
    observeEvent(input$next_exercise_btn_bs, {
      if(input$next_exercise_btn_bs > 0) {
        newVal <- counter_exercise_bs() + 1
        counter_exercise_bs(newVal)
      }
    })
    observeEvent(input$start_exercise_bs, {
      if(input$start_exercise_bs > 0) {
        newVal <- counter_exercise_bs() + 1
        counter_exercise_bs(newVal)
      }
    })
    
    #for Natural splines: 
    observe({
      req(input$start_exercise_nsp>0)
      titles <- input$exercise_nsp
      newval <- switch(titles, 
                       'Advanced Exercise' = c(
                         input$tabsetmethods == 'nsplines' & input$variable == 'Height ~ Age' & input$add_y.bs & input$sample.size == '100%' & input$gender == 'Both', 
                         input$nknots.nsp == 2 & between(input$nsp_pos1_inner, 11.8, 12.2) & between(input$nsp_pos2_inner, 14.8, 15.2) & 
                           between(input$boundary1.nsp, 3.8, 4.2) & between(input$boundary2.nsp, 17.8, 18.2), 
                         input$adjust_intercept.nsp > 0, 
                         calcR2.nsp()[1]>0.65, 
                         between(input$nsp_pos1_inner, 9.8, 10.2), 
                         input$adjust_intercept.nsp > 1
                       )
      )
      validation_all$nsp <- newval
    })
    
    counter_exercise_nsp <- reactiveVal(0)
    observe({
       counter <- counter_exercise_nsp()
       if(counter == 0){
         runjs("document.getElementById('start_exercise_nsp').style.visibility = 'visible';")
       } else {
         runjs("document.getElementById('start_exercise_nsp').style.visibility = 'hidden';")
       }
    })
        
    output$next_exercise_nsp <- renderUI({
      req(input$start_exercise_nsp, input$exercise_nsp)
      counter <- counter_exercise_nsp()
      validation <- validation_all$nsp
      which <- as.character(input$exercise_nsp)
        if((counter <= length(exercises[['nsp']][[which]])) & (counter > 0)){
          tagList(
            HTML('<p>', paste0(as.character(exercises[['nsp']][[which]][counter])), '</p>'), 
            if(validation[counter]){
              tagList(
              HTML('<svg class="checkmark" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 52 52">
                  <circle class="checkmark__circle" cx="26" cy="26" r="25" fill="none"/>
                  <path class="checkmark__check" fill="none" d="M14.1 27.2l7.1 7.2 16.7-16.8"/>
                </svg>
                 '), 
              actionButton('next_exercise_btn_nsp', 'Next')
              )
            } else {
              HTML('')
            }
          )
        } else if(counter > length(exercises[['nsp']][[which]])){
            if(any(input$start_exercise_nsp == 1, input$start_exercise_fp == 1)){
              runjs("InitializeConfetti();")
            } else {
              runjs("RestartConfetti();")
            }
            runjs("$('#exercise_modal').modal().focus();")
            counter_exercise_nsp(0)
            return('')
        }
    })
        
    observeEvent(input$next_exercise_btn_nsp, {
      if(input$next_exercise_btn_nsp > 0) {
        newVal <- counter_exercise_nsp() + 1
        counter_exercise_nsp(newVal)
      }
    })
    observeEvent(input$start_exercise_nsp, {
      if(input$start_exercise_nsp > 0) {
        newVal <- counter_exercise_nsp() + 1
        counter_exercise_nsp(newVal)
      }
    })
    
    
    
    
    
    
    
    
    
    session$onSessionEnded(stopApp) #automatically stop when closing browser
}