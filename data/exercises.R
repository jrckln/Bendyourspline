exercises <- list(
    "test"= list(
        "instructions" = 
        c(
        'Switch to Fractional polynomials in the methods section and use Bmi ~ Age as dataset. Make sure that response, optimal fit and loess smoother are turned off.',
        'Move the slider of coefficient 1 to 1 and see how the response curve reacts!', 
        'Set the second power to 2 and move the associated coefficient to 1!', 
        'Now move coefficient 1 to -1!', 
        'Now set coefficient 1 to +1 and coefficient 2 to -1!', 
        'How can you adjust the curve to get the peak of the response function at approximately 40 years?', 
        'Now change the second power to 3, then to 1, 0, -1 etc. See how the response curve reacts!', 
        'Set both powers to 2. The two powers are now set to equal numbers, which defines a <b> repeated power </b>, and in 
        this case the second term is also multiplied with log(x). (Check the formula)', 
        'With the powers at 2 and 2, try to adjust the coefficients such that you approximately get a U-shaped response function!',
        'Keeping the powers as they are, try to ‘invert’ this U into a ∩ only with the coefficient sliders!'
        ), 
        "validate" = c("input$tabsetmethods == 'Fractional Polynomials' & input$variable == 'Bmi ~ Age' & !input$add_y.fp & !input$add_loess_fp & !input$add_optfit_fp",
            "input[['val_coef1_fp-coef']] == 1", 
            "input$power2.fp == 2 & input[['val_coef2_fp-coef']] == 1", 
            "input[['val_coef1_fp-coef']] == -1",
            "input[['val_coef1_fp-coef']] == 1 & input[['val_coef2_fp-coef']] == -1", 
            "input[['val_coef2_fp-coef']] >= -(input[['val_coef1_fp-coef']]/(40/100))/2 - 0.05 & input[['val_coef2_fp-coef']] <= -(input[['val_coef1_fp-coef']]/(40/100))/2 + 0.05", 
            "TRUE", 
            "input$power2.fp == 2 & input$power1.fp == 2", 
            "TRUE", "TRUE")), 
    "test2" = list(
        "instructions" = c("hi", "whatever"), 
        "validate" = c("TRUE", "TRUE")
    )
)
