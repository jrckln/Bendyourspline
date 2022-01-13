function(input, output, session){
  
    query_modal <- modalDialog(
      title = 'Note',
      HTML(
        '
        <p>
        This website is still in the development phase. In case of crashes or unexpected behavior please report at
        <a href="https://github.com/ljiricka/Bendyourspline/issues">Github</a>. Simply click on "New Issue" and let us know what you noticed. <br>
        Thank you for your understanding!
        </p>
        '
      ),
      easyClose = F, 
      footer = modalButton("Cancel")
    )
  
    # Show the model on start up ...
    showModal(query_modal)
    
    observe_helpers(help_dir = "help_mds")
  
    observeEvent(input$link_methods, {
      updateNavbarPage(session, "navbar", "Methods")
    })
    
    #code download module #TODO
    codeServer("code_fp", filename=c("www/codes/code_fp.R", "www/codes/helpers.R", "www/codes/code_data.R"))
    codeServer("code_bs", filename=c("www/codes/code_bs.R", "www/codes/code_data.R"))
    codeServer("code_nsp", filename=c("www/codes/code_nsp.R", "www/codes/code_data.R"))
  
    #############################################
    #######         Data            #############
    #############################################
    
    output$information_vars <- renderUI({
        var <- as.character(input$variable)
        if(var == "No data"){
          return(HTML(''))
        } else {
          var_list <- data_list[[var]]
          filtered <- getdata()
          obs <- ifelse(input$sample.size == '100%' & input$gender == "Both", 
                        paste0('all observations (', nrow(var_list$data), ')'),
                        paste0(length(filtered$x), " out of ", nrow(var_list$data), ' observations'))
          filt <- ifelse(input$gender == "Both", "", paste0(', filtered for gender: ', input$gender))
          HTML(paste0('<p> Showing ', obs, filt, ' of variable ', var_list$x_name, ' in ', var_list$x_unit, 
                      ' with ', var_list$y_name, ' in ', var_list$y_unit, ' as response. </p>'
                      ))
        }
    })

    getdata <- reactive({
      req(input$seed)
      var <- input$variable
      var_list <- data_list[[var]]
      var_list$data <- var_list$data[var_list$data[,"gender"] %in% gender[[input$gender]],]
      set.seed(input$seed)
      samplesize <- if(var == "No data") "100%" else input$sample.size
      n <- floor(sample.sizes[samplesize]*nrow(var_list$data))
      ind <- sample(1:nrow(var_list$data), n)
      var_list$data <- var_list$data[ind,]
      x <- var_list$data[,var_list$x]
      data <- list("x" = x, 
                    "y" = var_list$data[, var_list$y], 
                   "names_vars" = c(var_list$x, var_list$y))
      return(data)
    })
    
    output$responseplot <- renderPlot({
       cur_tab = input$inputsindividual
       data <- getdata()
       names_vars <- data$names_vars
       data <- data.frame("x" = data$x, "y" = data$y)
       response <- getresponse()
       col <- 'lightgrey'
      
       p <- ggplot()+ 
         geom_line(data = data, aes(x=x, y = getintercept()+response, color = "Response"))
       
       if(input$addloess){
            p <- p + geom_smooth(data = data, aes(x=x, y=y,  color = "LOESS smoother"),
                                 method = "loess", formula = "y~x", se=FALSE)
       }
       if(input$addoptfit){
          optfit <- getoptfit()$fitted
          p <- p + geom_line(data = data, aes(x=x, y = optfit,
                                              color = "Optimal fit")
                             )
       }
       if(any(input$showknots_bs, input$showknots_nsp)){
        pos <- switch(
          input$inputsindividual, 
          "B-Splines" = getpos.bs(), 
          "Natural Splines" = getpos.nsp() 
        )
        
        posx <- quantile(data$x, pos/100)
        y_coord <- max(data$y)
        
        p <- p + geom_vline(aes(xintercept=posx), color = "#D3D3D3") +
              annotate(geom = "text", x = posx, y = y_coord, label = paste("Q ", pos), hjust = "left", color = "#D3D3D3")
       }
       
       if(input$addy){
          p <- p + geom_point(data = data, aes(x=x, y=y), color = col)+
                ylab(names_vars[2]) + 
                xlab(names_vars[1])
       } else {
         p <- p + xlab('x') + 
                  ylab('response')
       }
       
       p <- p +
         scale_color_manual(values=c("LOESS smoother" = loesscol, 
                                     "Optimal fit" = optfitcol, 
                                     "Response" = 'black'), name = " ", drop = TRUE) + 
         theme_minimal() + 
         theme(legend.position = "bottom")
       return(p)
    })
    
    #intercept: 
    output$interceptslider <- renderUI({
      data <- getdata()
      sliderInput("intercept",
                  label="Intercept",
                  min = 0, 
                  max = 2.5*round(max(data$y),0), 
                  value = 0, 
                  step = 0.1)
    })
    
    observeEvent(input$adjustintercept, {
        data <- getdata()
        response <- getresponse()
        intercept <- opt.intercept(fitted=response, data=data$y, interval=c(0, max(data$y)))$minimum
        updateSliderInput(session, 'intercept', value = intercept)
    })
    
    getintercept <- reactive({
      var <- as.character(input$variable)
      if(var == "No data"){
        return(0)
      } else {
        req(input$intercept)
        return(input$intercept)
      }
    })
    
    getresponse <- reactive({
      basis <- switch(
          input$inputsindividual, 
          "Fractional Polynomials" = getbasis.fp(),
          "B-Splines" = getbasis.bs(), 
          "Natural Splines" = getbasis.nsp() 
        ) 
      basis <- basis[, !(names(basis) %in% c('x', 'y'))]
      coefs <- switch(
          input$inputsindividual, 
          "Fractional Polynomials" = getcoef.fp(),
          "B-Splines" = getcoef.bs(), 
          "Natural Splines" = getcoef.nsp() 
        ) 
      response <- as.vector(as.matrix(basis) %*% coefs)
      return(response)
    })
    
    getoptfit <- reactive({ 
      if(input$inputsindividual == "Fractional Polynomials"){
        data <- getdata()
        pT <- fp.scale(data$x)
        transformed <- (data$x + pT$shift)/pT$scale
        fit <- mfp(y~fp(transformed, df = 4, scale = FALSE), data = data.frame('transformed' = transformed, 
                                                                                  'y' = data$y))
        optfit <- list(
          'fitted' = fit$fitted, 
          'coefficients' = as.numeric(unname(fit$coefficients)), 
          'powers' = as.numeric(unname(fit$powers[1,]))
        )
      } else if(input$inputsindividual == "B-Splines"){
        data <- getbasis.bs()
        fit <- lm(as.formula(paste0("y~", paste0(paste0("spline", 1:(ncol(data)-2)), 
                                                    collapse="+"))), data=data)
        optfit <- list(
          'fitted' = fit$fitted, 
          'coefficients' = as.numeric(unname(fit$coefficients))
        )
      } else if(input$inputsindividual == "Natural Splines"){
        data <- getbasis.nsp()
        fit <- lm(as.formula(paste0("y~", paste0(paste0("spline", 1:(ncol(data)-2)), 
                                                    collapse="+"))), data=data)
        optfit <- list(
          'fitted' = fit$fitted, 
          'coefficients' = as.numeric(unname(fit$coefficients))
        )
      }
      return(optfit)
    })
    
    observeEvent(input$setoptfit, {
      optcoefs <- getoptfit()$coefficients
      intercept <- round(optcoefs[1], 1)
      optcoefs <- optcoefs[2:length(optcoefs)]
      absmax <- max(abs(optcoefs))
      coef_range_new <- ceiling(absmax/10)*10
      
      updateSliderInput(session, 'intercept', value = intercept) 
      
      if(input$inputsindividual == 'Fractional Polynomials'){
        updateSliderTextInput(session, "power1.fp", selected = getoptfit()$powers[1])
        updateSliderTextInput(session, "power2.fp", selected = getoptfit()$powers[2])
      }
      
      opentab <- switch(
        input$inputsindividual,
        "Fractional Polynomials" = 'fp',
        "B-Splines" = 'bs',
        "Natural Splines" = 'nsp'
      )

      for(i in 1:length(optcoefs)){
        updateSliderInput(session, paste0(opentab, '_coef',i,'-slider'),
                          min = (-1)*coef_range_new, max = coef_range_new,
                          value = optcoefs[i])
      }
    })
    
    output$basisplot <- renderPlot({ #TODO
      basis <- switch(
        input$inputsindividual,
        "Fractional Polynomials" = getbasis.fp(),
        "B-Splines" = getbasis.bs(),
        "Natural Splines" = getbasis.nsp()
      )
      p <- ggplot(data = basis)
      
      for(i in 1:(ncol(basis)-2)){
        p <- p + geom_line(aes_string(x = "x", y = names(basis)[i+2]), color = col[i])
      }
      p <- p +
        theme_minimal() + 
        ylab("") #TODO
        #xlab(ifelse(input$add_y_fp, attr(DF, "names_vars")[1], 'x'))
      return(p)
    })
    
    
    
    calcR2 <- reactive({
        y <- getdata()$y
        response <- getresponse()
        optfit <- getoptfit()$fitted
        
        sstot <- sum((y-mean(y))^2) #total sum of squares
        ssres <- sum((y-response)^2)  #residual sum of squares
        ssres_fitted <- sum((y-optfit)^2)  #residual sum of squares fitted
        R2 <- 1-ssres/sstot
        maxR2 <- 1-ssres_fitted/sstot
        
        p <- switch(
        input$inputsindividual,
        "Fractional Polynomials" = 4,
        "B-Splines" = ncol(getbasis.bs())-2,
        "Natural Splines" = ncol(getbasis.nsp())-2
      )
        
        c(1-(1-R2)*(length(response)-1)/(length(response)-1-p), 
          1-(1-maxR2)*(length(response)-1)/(length(response)-1-p))
        }
      )
    
    observe({
      vals <- calcR2()
      stats("stats", vals)
    }, priority = -100)
  
    #reset button
    observeEvent(c(input$resetinput, input$variable), {
      reset("inputs_fp") 
      reset("inputs_bs") 
      reset("inputs_nsp") 
    })
    
    coef_range_fp <- coef_range('fp')
    coef_range_bs <- coef_range('bs')
    coef_range_nsp <- coef_range('nsp')
    
    #increase/decrease range of coefs
    range <- reactive({
      tmp <- switch(
        input$inputsindividual,
        "Fractional Polynomials" = coef_range_fp(),
        "B-Splines" = coef_range_bs(),
        "Natural Splines" = coef_range_bs()
      )
      tmp
    })
    
    observeEvent(range(), {
      if(input$inputsindividual == "Fractional Polynomials"){
        updateSliderInput(session, "fp_coef1-slider", min = (-1)*range(), max = range())
        updateSliderInput(session, "fp_coef2-slider", min = (-1)*range(), max = range())
      } else if(input$inputsindividual == "B-Splines"){
        req(input$nknots.bs)
        num <- input$degree.bs + input$nknots.bs
        ind <- paste0("bs_coef", 1:num, "-slider")
        for(i in ind){
          updateSliderInput(session, i, min=(-1)*range(), max = range())
        }
      } else if(input$inputsindividual == "Natural Splines"){
        req(input$nknots.nsp)
        num <- 1 + input$nknots.nsp
        ind <- paste0("nsp_coef", 1:num, "-slider")
        for(i in ind){
          updateSliderInput(session, i, min = (-1)*range(), max = range())
        }
      }
    })
    
    observeEvent(input$variable, {
      if(input$variable == 'No data'){
        updateMaterialSwitch(session, 'addy', value = FALSE)
        updateMaterialSwitch(session, 'addloess', value = FALSE)
        updateMaterialSwitch(session, 'addoptfit', value = FALSE)
      }
    })
    
    #############################################
    #######           FP            #############
    #############################################
    
    coef1.fp <- sliderpl("fp_coef1")
    coef2.fp <- sliderpl("fp_coef2")

    
    output$formula.fp <- renderUI({
        data <- getdata()
        x <- data$x
        pT <- fp.scale(x)
        x <- paste0(
            "\\frac{ \\text{", data$names_vars[1] , "} + ", pT$shift, "}{", pT$scale ,"}"
        )
        pow1 <- as.numeric(input$power1.fp)
        trans1 <- paste0('\\left(',x,"\\right) ^{", pow1, "}")
        if(pow1 == 0) trans1 <- paste0("\\log \\left(",x,"\\right)")
        if(pow1 == 1) trans1 <- x
        coef1 <- as.numeric(coef1.fp())
        if(coef1 >= 0) {
          coef1 <- paste0(" + ", coef1)
        } else {
          coef1 <- coef1
        }
        fp_fun <- paste(round(getintercept(),2), coef1, "\\cdot", trans1)
        pow2 <- as.numeric(input$power2.fp)
        trans2 <-paste0('\\left(',x,"\\right) ^{", pow2, "}")
        if(pow2 == 0) trans2 <- paste0("\\log \\left(",x,"\\right)")
        if(pow2 == 1) trans2 <- x
        if(pow1 == pow2) trans2 <- paste0(trans2, "\\cdot \\log \\left(",x,"\\right)")
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
    
    getbasis.fp <- reactive({
        req(input$power1.fp, input$power2.fp)
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
        data <- data.frame('x' = data$x, 'y' = data$y)
        data <- cbind(data, fp1, fp2)
        return(data)
    })
    
    getcoef.fp <- reactive({
      c(as.numeric(coef1.fp()), as.numeric(coef2.fp()))
    })
    
    
    #############################################
    #######        B-splines        #############
    #############################################
    
    getbasis.bs <- reactive({
      validate(
        need(is.numeric(input$degree.bs) & input$degree.bs < 5, 'Please provide a valid degree.'),
        need(is.numeric(input$nknots.bs) & input$nknots.bs < 10, 'Please provide a valid number of knots.')
      )
      data <- getdata()
      degree <- input$degree.bs
      pos <- getpos.bs()
      b <- bs(data$x, degree=degree, knots=pos)
      colnames(b) <- paste0("spline", 1:ncol(b))
      data <- data.frame("x" = data$x, 
                    "y" = data$y)
      data <- cbind(data, b)
      return(data)
    })
    
    inserted.pos.bs <- c()
    
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
                default.pos.knots.bs <- seq.int(from = 0, to = 100, length.out = input$nknots.bs + 2)[-c(1, input$nknots.bs + 2)]
                data <- getdata()
                toinsert <- (length(inserted.pos.bs)+1):num # which to insert
                id <- paste0('bs_pos', toinsert)
                for(i in 1:length(toinsert)){
                    insertUI(
                        selector = '#placeholder_pos_bs',
                        ui = tags$div(sliderInput(paste0(id[i], '-slider'), label = '',
                                                  value=default.pos.knots.bs[toinsert[i]], step=1,
                                                  min=0, max=100, ticks = FALSE), 
                                      id=id[i])
                    )
                    inserted.pos.bs <<- c(inserted.pos.bs, id[i])
                }
            } else if(length(inserted.pos.bs)== num){ #case: variable change
                # update default knot positions according to values of new variable
                default.pos.knots.bs <- seq.int(from = 0, to = 100, length.out = num + 2)[-c(1, num + 2)]
                data <- getdata()
                toupdate <- 1:num #update all available sliders
                id <- paste0('bs_pos', toupdate)
                for(i in 1:length(toupdate)){
                  updateSliderInput(session, paste0(id[i], '-slider'), min=0, max=100,
                                    value = as.numeric(default.pos.knots.bs[toupdate[i]]))
                }
            }
        }else{ #case of initialisation:
            default.pos.knots.bs <- seq.int(from = 0, to = 100, length.out = input$nknots.bs + 2)[-c(1, input$nknots.bs + 2)]
            data <- getdata()
            toinsert <- 1:num
            id <- paste0('bs_pos', toinsert)
            for(i in 1:length(toinsert)){
                insertUI(
                    selector = '#placeholder_pos_bs',
                    ui = tags$div(sliderInput(paste0(id[i], '-slider'), label = '',
                                              value=default.pos.knots.bs[i], step=1,
                                              min=0, max=100, ticks = FALSE), 
                                  id=id[i])
                )
                inserted.pos.bs <<- c(inserted.pos.bs, id[i])
            }
        }
    }, priority = 100)

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
                      coef_vals_bs[[inserted.coef.bs[[length(inserted.coef.bs)]]]] <<- NULL
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
                  coef_vals_bs[[id[i]]] <<- sliderpl(id[i])
                  inserted.coef.bs <<- c(inserted.coef.bs, id[i])
                }
            }
        } else { #case of initialisation
            id <- paste0('bs_coef', 1:num)
            for(i in 1:num){
              insertUI(
                    selector = '#placeholder_coef_bs',
                    ui = sliderplUI(id[i])
              )
              coef_vals_bs[[id[i]]] <<- sliderpl(id[i])
              inserted.coef.bs <<- c(inserted.coef.bs, id[i])
            }
        }
    }, priority = 100)
    
    getpos.bs <- reactive({
        req(input$nknots.bs)
        #get values of knot positions:
        names <- paste0("bs_pos", 1:input$nknots.bs, '-slider')
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
      req(length(coef_vals_bs) == (input$degree.bs + input$nknots.bs))
      
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
    
    
    #############################################
    #######      Natural-splines    #############
    #############################################
    
    
    #dynamic insert of slider for positions of knots
    inserted.pos.nsp <- c()

    getbasis.nsp <- reactive({
      req(input$boundary1.nsp, input$boundary2.nsp)
      data <- getdata()
      pos <- getpos.nsp()
      b <- ns(data$x, knots = pos, Boundary.knots = c(input$boundary1.nsp, input$boundary2.nsp))
      colnames(b) <- paste0("spline", 1:ncol(b))
      data <- data.frame("x" = data$x, "y" = data$y)
      data <- cbind(data,b)
      return(data)
    })
    
    #update max and min val of boundary knots slider according to second and second to last knot position
    observeEvent({c(input[[paste0("nsp_pos1-slider")]], input[[paste0("nsp_pos",input$nknots.nsp,"-slider")]])},{
      pos <- getpos.nsp()
      updateSliderInput(session, "boundary1.nsp", max=pos[1]-0.1)
      updateSliderInput(session, "boundary2.nsp", min = pos[length(pos)]+0.1)
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
                default.pos.knots.nsp <- seq.int(from = 0,
                                                 to = 100,
                                                 length.out = input$nknots.nsp + 2)[-c(1, input$nknots.nsp + 2)]
                toinsert <- (length(inserted.pos.nsp)+1):input$nknots.nsp
                id <- paste0('nsp_pos', toinsert)
                for(i in 1:length(toinsert)){
                    insertUI(
                        selector = '#placeholder_pos_nsp',
                        ui = tags$div(sliderInput(paste0(id[i], "-slider"), label = '',
                                                  value=default.pos.knots.nsp[toinsert[i]], step=1,
                                                  min=0, max=100, ticks = FALSE), id=id[i])
                    )
                    inserted.pos.nsp <<- c(inserted.pos.nsp, id[i])
                }
            }
        }else{ #case of initialisation
            default.pos.knots.nsp <- seq.int(from = 0, to = 100,
                                             length.out = input$nknots.nsp + 2)[-c(1, input$nknots.nsp + 2)]
            data <- getdata()
            toinsert <- 1:num
            id <- paste0('nsp_pos', toinsert)
            for(i in 1:length(toinsert)){
                insertUI(
                    selector = '#placeholder_pos_nsp',
                    ui = tags$div(sliderInput(paste0(id[i], "-slider"), label = '',
                                              value=default.pos.knots.nsp[i], step=1,
                                              min=0, max=100, ticks = FALSE), id=id[i])
                )
                inserted.pos.nsp <<- c(inserted.pos.nsp, id[i])
            }
        }
    }, priority = 100)

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
    }, priority = 100)
    
    getpos.nsp <- reactive({
        req(input$nknots.nsp)
        #get values of knot positions:
        names <- paste0("nsp_pos", 1:input$nknots.nsp, "-slider")
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
      req(length(coef_vals_nsp) == (1 + input$nknots.nsp))
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
    
    
    #############################################
    #######      Exercises          #############
    #############################################
    
    
    #TODO!
    
    
    validation_all <- reactiveValues()
    
    #for fractional polynomials: 
    observe({
      req(input$start_exercise_fp>0)
      DF <- getbasis.fp()
      fp <- as.numeric(input$intercept.fp)+DF$fp
      titles <- input$exercise_fp
      newval <- switch(titles, 
                       Basic = c(
                         input$inputsindividual == 'Fractional Polynomials' & input$variable == 'Bmi ~ Age' & !input$add_y_fp & !input$add_loess_fp & !input$add_optfit_fp, 
                         input[['val_coef1_fp-coef']] == 1, 
                         input$power2.fp == 2 & input[['val_coef2_fp-coef']] == 1, 
                         input[['val_coef1_fp-coef']] == -1, 
                         input[['val_coef1_fp-coef']] == 1 & input[['val_coef2_fp-coef']] == -1, 
                         input[['val_coef2_fp-coef']] >= -(input[['val_coef1_fp-coef']]/(40/100))/2 - 0.05 & input[['val_coef2_fp-coef']] <= -(input[['val_coef1_fp-coef']]/(40/100))/2 + 0.05, 
                         TRUE, 
                         input$power2.fp == 2 & input$power1.fp == 2, 
                         input$power2.fp == 2 & input$power1.fp == 2 & getshape(fp, DF$x)=="cup", 
                         input$power2.fp == 2 & input$power1.fp == 2 & getshape(fp, DF$x)=="cap"
                       ), 
                       Advanced = c(
                         input$inputsindividual == 'Fractional Polynomials' & input$variable == 'Diastolic blood pressure ~ Age' & input$sample.size == '100%' & input$gender == 'Both', 
                         input$add_y_fp & input$add_loess_fp & !input$add_optfit_fp, 
                         input$intercept.fp >= (66-10) & input$intercept.fp <= (66+10), 
                         calcR2.fp()[1]>=0.08 & input[['val_coef2_fp-coef']] == 0,
                         input$power2.fp == 2 & calcR2.fp()[1]>=0.15, 
                         calcR2.fp()[1]>0.15, 
                         calcR2.fp()[1]>=0.259
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
            HTML(paste0('<div id="exercisecounts">', counter, "/", length(exercises[['fp']][[which]]), '</div>')),
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
      
      data <- getbasis.bs()
      b <- data$b
      pos <- getpos.bs()
      coefs <- getcoef.bs()
      intercept <- as.numeric(input$intercept.bs)
      spline <- rowSums(b %*% coefs)+intercept
      
      newval <- switch(titles, 
                       'Basic' = c(
                         input$inputsindividual == 'B-Splines' & !input$add_y_bs & !input$add_loess_bs & !input$add_optfit_bs & input$degree.bs == 1,
                         input[['bs_coef1-coef']] == 1 & input[['bs_coef2-coef']] == 0 & input[['bs_coef3-coef']] == 0, 
                         TRUE, 
                         input[['bs_coef1-coef']] == 1 & input[['bs_coef2-coef']] == 1 & input[['bs_coef3-coef']] == 0, 
                         input[['bs_coef1-coef']] == 0 & input[['bs_coef2-coef']] == 1 & input[['bs_coef3-coef']] == 1, 
                         input[['bs_coef1-coef']] == 1 & input[['bs_coef2-coef']] == 1 & input[['bs_coef3-coef']] == 1,
                         getshape(spline, data$x)=="cup" 
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
      
      data <- getbasis.nsp()
      b <- data$b
      pos <- getpos.nsp()
      coefs <- getcoef.nsp()
      intercept <- as.numeric(input$intercept.nsp)
      spline <- rowSums(b %*% coefs)+intercept
      
      titles <- input$exercise_nsp
      newval <- switch(titles, 
                       'Basic' = c(
                         input$inputsindividual == 'Natural Splines' & !input$add_y_nsp & !input$add_loess_nsp & !input$add_optfit_nsp,
                         input[['nsp_coef1-coef']] == 0 & input[['nsp_coef2-coef']] == 0 & input[['nsp_coef3-coef']] == 0, 
                         input[['nsp_coef1-coef']] == 1 & input[['nsp_coef2-coef']] == 0 & input[['nsp_coef3-coef']] == 0, 
                         TRUE, 
                         input[['nsp_coef1-coef']] == 1 & input[['nsp_coef2-coef']] == 1 & input[['nsp_coef3-coef']] == 0, 
                         input[['nsp_coef1-coef']] == 1 & input[['nsp_coef2-coef']] == 1 & input[['nsp_coef3-coef']] == 1,
                         input[['nsp_coef1-coef']] == -1, 
                         getshape(spline, data$x)=="cup" & input[['nsp_coef1-coef']] %in% c(-1,-0.5,0, 0.5,1) & input[['nsp_coef2-coef']] %in% c(-1,-0.5,0, 0.5,1) & input[['nsp_coef3-coef']] %in% c(-1,-0.5,0, 0.5,1), 
                         getshape(spline, data$x)=="cap" & input[['nsp_coef1-coef']] %in% c(-1,-0.5,0, 0.5,1) & input[['nsp_coef2-coef']] %in% c(-1,-0.5,0, 0.5,1) & input[['nsp_coef3-coef']] %in% c(-1,-0.5,0, 0.5,1)
                       ),
                       'Advanced' = c(
                         input$inputsindividual == 'Natural Splines' & input$variable == 'Height ~ Age' & input$add_y_bs & input$sample.size == '100%' & input$gender == 'Both', 
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