exercises <- list(
    'fp' = list(
        "Basic"= 
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
            'Keeping the powers as they are, try to invert this U into a ∩ only with the coefficient sliders!'
            )
    ),
    'bs' = list(
        'Christine'= c(
            'Write something'
        )
    ), 
    'nsp' = list(
        'Advanced'= c(
            'In this exercise, we will fit a simple regression where we try to describe height as a function of age using natural splines. 
            In advanced settings, set sample size to 100%, sex to "both", and display the height values by adding and clicking at "Response". 
            The data set consists of 654 observations.', 
            'We want to fit a natural spline with 5 segments (two internal knots and two boundery knots). Set the knot positions to 
             approximately 12 and 15, and the boundary knots to 4 and 18.', 
            'The horizontal black line on the bottom is our current response function. First we adjust the intercept automatically. The black line will move to the average height.', 
            'Increase the coefficient range to -40 and +40, and set coefficient 1,  2 and 3 such that you receive a similar shape like the optimal fit (darkred). Adjust the intercept by hand to calibrate the response function. Can you find coefficients such that the adjusted R² is higher than 0.65?', 
            'Reduce the value of knot 1 from 12 to 10. Can you increase the R2 by adjusting the coefficients?', 
            'Look at your current intercept and memorize the number. Now click "Adjust intercept automatically". Did the number change or was your previous guess a good one?'
        )
    )
)
