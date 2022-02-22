function(input, output, session){
  
    query_modal <- modalDialog(
      title = 'Note',
      HTML(
        '
        <p>
        This website is still in the development phase. In case of crashes or unexpected behavior please report at
        <a href="https://github.com/jrckln/Bendyourspline/issues">Github</a>. Simply click on "New Issue" and let us know what you noticed. <br>
        Thank you for your understanding!
        </p>
        '
      ),
      easyClose = FALSE, 
      footer = modalButton("Cancel")
    )
  
    # Show the model on start up ...
    #showModal(query_modal)
  
    observeEvent(input$link_methods, {
      updateNavbarPage(session, "navbar", "Methods")
    })
    
    #code download module
    
    observe({
        file <- switch (input$inputsindividual,
        "Fractional Polynomials" = "www/codes/code_fp.R",
        "B-Splines" = "www/codes/code_bs.R",
        "Natural Splines" = "www/codes/code_nsp.R"
        )
        codeServer("codemodule", filename=c(file,"www/codes/helpers.R", "www/codes/code_data.R"))
    })
        
        
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
    
    rangesdata <- reactiveValues()
    
    getdata <- reactive({
      req(input$seed)
      var <- input$variable
      var_list <- data_list[[var]]
      rangesdata$x <- c(min(var_list$data[,var_list$x]), max(var_list$data[,var_list$x]))
      rangesdata$y <- c(min(var_list$data[, var_list$y]), max(var_list$data[, var_list$y]))
      var_list$data <- var_list$data[var_list$data[,"gender"] %in% gender[[input$gender]],]
      set.seed(input$seed)
      if(var != 'No data'){
          samplesize <- input$sample.size
          n <- floor(sample.sizes[samplesize]*nrow(var_list$data))
          ind <- sample(1:nrow(var_list$data), n)
          var_list$data <- var_list$data[ind,]
      }
      data <- list("x" = var_list$data[,var_list$x], 
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
       
       values <- c('Response'='black')
       
       if(input$addloess){
            p <- p + geom_smooth(data = data, aes(x=x, y=y,  color = "LOESS smoother"),
                                 method = "loess", formula = "y~x", se=FALSE)
            values <- c(values, 'LOESS smoother' = loesscol)
       }
       if(input$addoptfit){
          optfit <- getoptfit()$fitted
          p <- p + geom_line(data = data, aes(x=x, y = optfit,
                                              color = "Optimal fit")
                             )
          values <- c(values, 'Optimal fit' = optfitcol)
       }
       if(any(input$showknots_bs, input$showknots_nsp)){
        pos <- switch(
          input$inputsindividual, 
          "B-Splines" = getpos.bs(), 
          "Natural Splines" = getpos.nsp() 
        )
        if(input$inputsindividual == 'Natural Splines'){
            pos <- c(pos, input$boundary1.nsp, input$boundary2.nsp)
        }
        
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
         scale_color_manual(values=values, name = " ", drop = TRUE) +
         theme_minimal() + 
         theme(legend.position = "bottom")
       return(p)
    })
    
    #intercept: 
    interceptslider <- sliderPL('interceptslider', number = reactive(1), ranges = c(0, 1),
                                     label = 'Intercept', values = reactiveValues('value1'=reactiveVal(0)))
    interceptslidervalue <- interceptslider$values
    
    observeEvent(input$adjustintercept, {
        data <- getdata()
        response <- getresponse()
        intercept <- opt.intercept(fitted=response, data=data$y, interval=c(0, max(data$y)))$minimum
        interceptslider$setRange(min = 0, max = ceiling(intercept/10)*10) 
        interceptslider$setValues(values = intercept)
        
        #updateSliderInput(session, 'interceptslider-interceptslider1-slider', value = intercept, max=ceiling(intercept/10)*10)
    })
    
    getintercept <- reactive({
      var <- as.character(input$variable)
      if(var == "No data"){
            return(0)
      } else {
            interceptslidervalue$value1()
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
      maxcoef <- ceiling(max(optcoefs)/10)*10
      mincoef <- floor(min(optcoefs)/10)*10
      
      interceptslider$setRange(min = 0, max = ceiling(intercept/10)*10) 
      updateSliderInput(session, 'interceptslider-interceptslider1-slider', value = intercept, max = ceiling(intercept/10)*10) 
      
      if(input$inputsindividual == "Fractional Polynomials"){
          updateSliderTextInput(session, "power1.fp", selected = getoptfit()$powers[1])
          updateSliderTextInput(session, "power2.fp", selected = getoptfit()$powers[2])
          opentab <- 'fp'
          fpslider$setRange(min = mincoef, max = maxcoef) 
      } else if(input$inputsindividual == "B-Splines"){
          opentab <- 'bs'
          bsslider$setRange(min = mincoef, max = maxcoef) 
      } else if(input$inputsindividual == "Natural Splines"){
          opentab <- 'nsp'
          nspslider$setRange(min = mincoef, max = maxcoef) 
      }
      
      for(i in 1:length(optcoefs)){
        updateSliderInput(session, paste0(opentab, '-', opentab,i,'-slider'),
                          min = mincoef, max = maxcoef,
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
        response <- getintercept() + getresponse()
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
      fpslider$reset()
      reset("inputs_bs") 
      bsslider$reset()
      reset("inputs_nsp") 
      nspslider$reset()
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
    
    fpslider <- sliderPL('fp', number = reactive(2), values = reactiveValues('value1'=reactiveVal(0), 
                                                                                   'value2'=reactiveVal(0)))
    fpslidervalues <- fpslider$values
    
    output$formula.fp <- renderUI({
        data <- getdata()
        coefs <- getcoef.fp()
        x <- data$x
        pT <- fp.scale(x)
        x <- paste0(
            "\\frac{ \\text{", data$names_vars[1] , "} + ", pT$shift, "}{", pT$scale ,"}"
        )
        pow1 <- as.numeric(input$power1.fp)
        trans1 <- paste0('\\left(',x,"\\right) ^{", pow1, "}")
        if(pow1 == 0) trans1 <- paste0("\\log \\left(",x,"\\right)")
        if(pow1 == 1) trans1 <- x
        coef1 <- as.numeric(coefs[1])
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
        coef2 <- as.numeric(coefs[2])
        if(coef2 >= 0) {
          coef2 <- paste0(" + ", coef2)
        } else {
          coef2 <- coef2
        }
        fp_fun <- paste(fp_fun, coef2, "\\cdot", trans2)
        return(withMathJax(HTML(paste0(
            "<div class='formula'>$$", fp_fun, "$$</div>"
        ))))
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
        c(fpslidervalues$value1(), fpslidervalues$value2())
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
      pos <- quantile(data$x, pos/100)
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
          need(is.numeric(input$degree.bs) & input$degree.bs <= 4, 'Please provide a valid degree.'),
          need(is.numeric(input$nknots.bs) & input$nknots.bs <= 10, 'Please provide a valid number of knots.')
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

    bsslider <- sliderPL('bs', number = reactive(input$nknots.bs + input$degree.bs), values = bsslidervalues)
    bsslidervalues <- bsslider$values
    
    getpos.bs <- reactive({
        req(input$nknots.bs)
        #get values of knot positions:
        names <- paste0("bs_pos", 1:input$nknots.bs, '-slider')
        pos <- c()
        for(i in names){
          pos <- c(pos, input[[i]])
        }
        pos <- sort(pos[!is.na(pos)])
        if(length(pos)!=input$nknots.bs){
          return(numeric(input$nknots.bs))
        } else {
          return(round(pos, 2))
        }
    })
    
    getcoef.bs <- reactive({
      req(input$nknots.bs, input$degree.bs)
      num <- input$degree.bs + input$nknots.bs
      ind <- paste0("value", 1:num)
      coef <- c()
      for(i in ind){
          coef <- c(coef, bsslidervalues[[i]]())
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
      pos <- quantile(data$x, pos/100)
      boundaries <- quantile(data$x, c(input$boundary1.nsp, input$boundary2.nsp)/100)
      b <- ns(data$x, knots = pos, Boundary.knots = boundaries)
      colnames(b) <- paste0("spline", 1:ncol(b))
      data <- data.frame("x" = data$x, "y" = data$y)
      data <- cbind(data,b)
      return(data)
    })

    # Internal Knot positions
    observeEvent(c(input$nknots.nsp, input$variable), {
        validate(
          need(is.numeric(input$nknots.nsp) & input$nknots.nsp <= 10, 'Please provide a valid number of internal knots.')
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
                                                  min=input$boundary1.nsp, max=input$boundary2.nsp, ticks = FALSE), id=id[i])
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
                                              min=input$boundary1.nsp, max=input$boundary2.nsp, ticks = FALSE), id=id[i])
                )
                inserted.pos.nsp <<- c(inserted.pos.nsp, id[i])
            }
        }
    }, priority = 100)

    nspslider <- sliderPL('nsp', number = reactive(input$nknots.nsp + 1), values = nspslidervalues)
    nspslidervalues <- nspslider$values

    getpos.nsp <- reactive({
        req(input$nknots.nsp)
        #get values of knot positions:
        names <- paste0("nsp_pos", 1:input$nknots.nsp, "-slider")
        pos <- c()
        for(i in names){
            pos <- c(pos, input[[i]])
        }
        pos <- sort(pos[!is.na(pos)])
         if(length(pos) != input$nknots.nsp){
           return(numeric(input$nknots.nsp))
         } else {
          return(round(sort(pos),2))
        }
    })

    getcoef.nsp <- reactive({
      req(input$nknots.nsp)
      num <- input$nknots.nsp + 1
      ind <- paste0("value", 1:num)
      coef <- c()
      for(i in ind){
          coef <- c(coef, nspslidervalues[[i]]())
      }
      return(coef)
    })
    
    #update max and min val of boundary knots slider according to second and second to last knot position
    bindEvent(
        observe({
            pos <- getpos.nsp()
            updateSliderInput(session, "boundary1.nsp", max=round(pos[1], 0)-1)
            updateSliderInput(session, "boundary2.nsp", min = round(max(pos), 0)+1)
        }), 
        getpos.nsp(), ignoreInit = TRUE
    )

    bindEvent(
      observe({
          num <- 1 + input$nknots.nsp
          ind <- paste0("nsp_pos", 1:num, '-slider')
          for(i in ind){
              updateSliderInput(session, i, min=input$boundary1.nsp+1, max=input$boundary2.nsp-1)
          }
      }),
      c(input$boundary1.nsp, input$boundary2.nsp)
    )
    
    
    #############################################
    #######      Exercises          #############
    #############################################
    
    exercisecounter <- reactiveVal(0)
    startedexercise <- reactiveVal(NA_character_)
    
    output$exerciseout <- renderUI({
      countervalue <- exercisecounter()
      if(countervalue==0){
        return(
          actionButton('exercisestart', 'Start')
        )
      } else {
          if(startedexercise()!=input$inputsindividual){
               return(HTML(paste0('<p>Please switch to ', startedexercise(), ' Input panel to continue.</p>')))
          }
        currentexercises <- exercises[[input$inputsindividual]][['Basic']]
        currentvalidation <- validationall[[input$inputsindividual]]
        if(countervalue <= length(currentexercises)){
          tagList(
            HTML(
              paste0(
                '<div id="exercisecounts">', countervalue, "/", length(currentexercises), '</div>
                <p>', currentexercises[countervalue], '</p>'
              )
            ), 
            if(currentvalidation[countervalue]){
              tagList(
              HTML('<svg class="checkmark" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 52 52">
                  <circle class="checkmark__circle" cx="26" cy="26" r="25" fill="none"/>
                  <path class="checkmark__check" fill="none" d="M14.1 27.2l7.1 7.2 16.7-16.8"/>
                </svg>
                 '), 
              actionButton('exercisenext', 'Next')
              )
            }
          )
        } else {
          if(input$exercisestart > 0){
              runjs("RestartConfetti();")
            } else {
              runjs("InitializeConfetti();")
            }
            runjs("$('#exercise_modal').modal().focus();")
            exercisecounter(0)
        }
      }
    })
    
    bindEvent(
      observe(exercisecounter(exercisecounter()+1)),
      input$exercisenext, ignoreInit = TRUE
    )
    bindEvent(
      observe({
          startedexercise(input$inputsindividual)
          exercisecounter(exercisecounter()+1)
      }),
      input$exercisestart, ignoreInit = TRUE
    )
    
    validationall <- reactiveValues()
    
    #for fractional polynomials:
    observe({
      req(input$exercisestart>0, input$inputsindividual == 'Fractional Polynomials')
      which <- 'Basic'
      response <- getresponse()
      coefs <- getcoef.fp()
      validationall[['Fractional Polynomials']] <- 
                c(input$variable == 'No data',
                  TRUE,
                  coefs[1] == 1,
                  input$power2.fp == 2 & coefs[1] == 1 & coefs[2] == 1,
                  input$power2.fp == 2 & coefs[1] == -1 & coefs[2] == 1,
                  input$power2.fp == 2 & coefs[1] == 1 & coefs[2] == -1,
                  TRUE, 
                  input$power2.fp == 2 & input$power1.fp == 2,
                  input$power2.fp == 2 & input$power1.fp == 2 & getshape(response)=="cup",
                  input$power2.fp == 2 & input$power1.fp == 2 & getshape(response)=="cap"
                )
    })
    
    #for B-Splines: 
    observe({
      req(input$exercisestart>0, input$inputsindividual == 'B-Splines')
      which <- 'Basic'
      response <- getresponse()
      knotpos <- getpos.bs()
      coefs <- getcoef.bs()
      validationall[['B-Splines']] <- 
                c(input$variable == 'No data',
                  input$degree.bs == 1 & input$nknots.bs == 2 & round(knotpos[1],0) == 33 & 
                    round(knotpos[2],0) == 67 & coefs[1] == 0 & coefs[2] == 0 & 
                    coefs[3] == 0,
                  TRUE, 
                  TRUE, 
                  TRUE, 
                  coefs[1] == 1 & coefs[2] == 1 & coefs[3] == 0, 
                  coefs[1] == 0 & coefs[2] == 1 & coefs[3] == 1,
                  coefs[1] == 1 & coefs[2] == 1 & coefs[3] == 1,
                  input$degree.bs == 1 & input$nknots.bs == 2 & getshape(response)=="cup", 
                  input$degree.bs >= 2
                )
    })
    
    #for Natural splines:
    observe({
      req(input$exercisestart>0, input$inputsindividual == 'Natural Splines')
      which <- 'Basic'
      response <- getresponse()
      knotpos <- getpos.nsp()
      coefs <- getcoef.nsp()
      validationall[['Natural Splines']] <- 
                c(input$variable == 'No data',
                  input$nknots.nsp == 2 & round(knotpos[1],0) == 33 & round(knotpos[2],0) == 67 & 
                  coefs[1] == 0 & coefs[2] == 0 & coefs[3] == 0,
                  TRUE, 
                  TRUE, 
                  TRUE, 
                  coefs[1] == 1 & coefs[2] == 1 & coefs[3] == 0, 
                  coefs[1] == 0 & coefs[2] == 1 & coefs[3] == 1,
                  coefs[1] == 1 & coefs[2] == 1 & coefs[3] == 1,
                  coefs[1] == -1& coefs[2] == 0 & coefs[3] == 0, 
                  coefs[1] %in% c(-1,-0.5,0, 0.5,1) & coefs[2] %in% c(-1,-0.5,0, 0.5,1) & 
                    coefs[3] %in% c(-1,-0.5,0, 0.5,1) & getshape(response)=="cup", 
                  coefs[1] %in% c(-1,-0.5,0, 0.5,1) & coefs[2] %in% c(-1,-0.5,0, 0.5,1) & 
                    coefs[3] %in% c(-1,-0.5,0, 0.5,1) & getshape(response)=="cap"
                )
    })
    
    session$onSessionEnded(stopApp) #automatically stop when closing browser
}