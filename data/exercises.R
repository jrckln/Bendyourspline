colorize <- function(word, color){
    paste0("<span style='color: ", color, "'>", word, "</span>")
}


exercises <- list(
    'fp' = list(
        "Basic"= 
            c(
            paste0('Use BMI ~ Age as dataset. Make sure that response, ', colorize('optimal fit', optfitcol) , ' and ', colorize('loess smoother', loesscol) , ' are turned off.'),
            paste0('Move the slider of ', colorize('coefficient 1', col[1]) , ' to 1 and see how the response curve reacts!'), 
            paste0('Set the second power to 2 and move the ', colorize('associated coefficient', col[2]) ,' to 1!'), 
            paste0('Now move ', colorize('coefficient 1', col[1]) , ' to -1!'), 
            paste0('Now set ', colorize('coefficient 1', col[1]) , ' to +1 and ', colorize('coefficient 2', col[2]) , ' to -1!'), 
            'How can you adjust the curve to get the peak of the response function at approximately 40 years?', 
            'Now change the second power to 3, then to 1, 0, -1 etc. See how this affects the response curve.', 
            'Set both powers to 2. The two powers are now set to equal numbers, which defines a <b> repeated power </b>, and in 
            this case the second term is also multiplied with log(x). (Check the formula)', 
            'With the powers at 2 and 2, try to adjust the coefficients such that you obtain an approximately U-shaped response function!',
            'Keeping the powers as they are, try to invert this &cup; into a &caps; only with the coefficient sliders!'
            )
    ),
    'bs' = list(
        'Basic'= c(
            'Use  Diastolic blood pressure ~ Age as dataset. Set the degree to 3 and choose 4 knots.'
        )
    ), 
    'nsp' = list(
        'Advanced'= c(
            'In this exercise, we will fit a simple regression where we try to describe expected height as a function of age using natural splines. 
            In advanced settings, set sample size to 100%, sex to "both", and display the height values by adding and clicking at "Response". 
            The data set consists of 654 observations.', 
            'We want to fit a natural spline with 5 segments (i.e. two internal knots and two boundery knots). Set the knot positions to 
             approximately 12 and 15, and the boundary knots to 4 and 18.', 
            'The horizontal black line on the bottom is our current response function. To move the response function to the average height, click "Adjust intercept automatically".', 
            paste0('Increase the coefficient range to -40 and +40, and set ', colorize('coefficient 1', col[1]) , ',  ', colorize('coefficient 2', col[2]) , ' and ', colorize('coefficient 3', col[3]) , ' such that you receive a shape similar to the ', colorize('optimal fit', optfitcol) , '. Adjust the intercept manually to calibrate the response function. Can you find coefficients such that the adjusted R <sup>2</sup> exceeds 0.65?'), 
            'Reduce the value of knot 1 from 12 to 10. Can you increase the R <sup>2</sup> by adjusting the coefficients?', 
            'Read off the current value of the intercept and memorize this number. Now click "Adjust intercept automatically". Did the number change or was your previous guess a good one?'
        )
    )
)
