exercises <- list(
    "test"= list(
        "instructions" = 
        c(
        'Move the slider of coefficient 1 to 1 and see how the response curve reacts!', 
        'Set the second power to 2 and move the associated coefficient to 1!', 
        'Now move coefficient 1 to -1!', 
        'Now set coefficient 1 to +1 and coefficient 2 to -1!', 
        'How can you adjust the curve to get the peak of the response function at approximately 40 years? 
        (answer: move Coeff_1 to approx. 0.80, in general: Coeff_2 = - coeff_1/(40/100)/2, follows from setting 
        first derivative to 0)', 
        'Now change the second power to 3, then to 1, 0, -1 etc. See how the response curve reacts!', 
        '8)	Set both powers to 2. The two powers are now set to equal numbers, which defines a ‘repeated power’, and in 
        this case the second term is also multiplied with log(x). (Check the FP-formula!)', 
        'With the powers at 2 and 2, try to adjust the coefficients such that you approximately get a U-shaped response function!',
        ': keeping the powers as they are, try to ‘invert’ this U into a ∩ only with the coefficient sliders!: keeping the powers as they are,
        try to ‘invert’ this U into a ∩ only with the coefficient sliders!'
        ), 
        "validate" = c("TRUE", "input$power2.fp == 2", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE" ))
)
