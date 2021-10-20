colorize <- function(word, color){
    paste0("<b> <font style='color: ", color, "'>", word, "</font> </b>")
}


exercises <- list(
    'fp' = list(
        "Basic"= 
            c(
            paste0('The aim of this exercise is to understand the impact of individual FP powers on the modeled response function. Reset the inputs. Use BMI ~ Age as variable pair. Make sure that data points, ', colorize('optimal fit', optfitcol) , ' and ', colorize('LOESS smoother', loesscol) , ' options are turned off.'),
            paste0('Move the slider of ', colorize('coefficient 1', col[1]) , ' to 1 and see how the response curve reacts!'), 
            paste0('Set the second power to 2 and move the ', colorize('associated coefficient', col[2]) ,' to 1!'), 
            paste0('Now move ', colorize('coefficient 1', col[1]) , ' to -1!'), 
            paste0('Now set ', colorize('coefficient 1', col[1]) , ' to +1 and ', colorize('coefficient 2', col[2]) , ' to -1!'), 
            'How can you adjust the curve to get the peak of the response function at approximately 40 years?', 
            'Now change the second power to 3, then to 1, 0, -1 etc. See how this affects the response curve.', 
            'Set both powers to 2. The two powers are now set to equal numbers, which defines a <b> repeated power </b>, and in 
            this case the second term is also multiplied with log(x). (Check the formula)', 
            'With the powers at 2 and 2, try to adjust the coefficients such that you obtain an approximately U-shaped response function!',
            'Keeping the powers as they are, try to invert this &Cup; into a &Cap; only with the coefficient sliders!'
            ), 
        "Advanced" = c(
            'In this exercise, we will try to fit a simple nonlinear regression line to describe expected diastolic blood pressure as a function of age. Reset the inputs. Select the variable pair Diastolic blood pressure ~ Age. In the advanced settings of data options, set sample size to "100%" and sex to "Both".', 
            paste0('Make sure that data points and ', colorize('LOESS smoother', loesscol) , ' options are turned on and the ', colorize('optimal fit', optfitcol) , ' option is turned off. The ', colorize('LOESS smoother', loesscol) , ' is a non-parametric function, which produces a smooth function fitting the data. However, it is not easily representable by a mathematical formula. In the plot, the variables exhibit a &Cap; -shaped association.'),
            paste0('The horizontal black line on the bottom is our current response function. Try to move it up to where the ', colorize('LOESS smoother', loesscol) , ' is. Move the slider of  the intercept such that the response is close to the ', colorize('LOESS smoother', loesscol) , ''), 
            paste0('By default, the two powers are set to 1. Increase the coefficient range, and move the slider of ', colorize('coefficient 1', col[1]) , ' and the intercept to improve the fit. Goodness of fit can be monitored with the R<sup>2</sup> and adjusted R<sup>2</sup> values. Move the sliders of ', colorize('coefficient 1', col[1]) , ' and the intercept such that you obtain an adjusted R<sup>2</sup> of at least 0.08.'),
            paste0('Obviously, there is curvature in the ', colorize('LOESS smoother', loesscol) , ', which cannot be nicely modelled with a linear response function as obtained in the previous step. Now, set the second power to 2. You can now increase the curvature by moving the slider of ', colorize('coefficient 2', col[2]) , ' into the negative range. Hint: enlarge the coefficient range ("Coefficient range: +"). There is also a feature to compute an optimal intercept automatically by clicking "Adjust Intercept automatically". Try to obtain an adjusted R<sup>2</sup> of at least 0.15 by varying both coefficients and the intercept.'),
            'Now play with the first power to change the curvature to better fit the data. When changing the power, you might have to adjust the sliders of the coefficients and the intercept to obtain a better adjusted R<sup>2</sup>.', 
            'Can you find a combination, which reaches the maximum possible adjusted R<sup>2</sup> of 0.26?'
            )
    ),
    'bs' = list(
        'Basic' = c(
            paste0('In this exercise, we examine the effect of individual linear B-spline basis functions  on the response function. Reset the inputs. Make sure that data points, ', colorize('optimal fit', optfitcol) , ' and ', colorize('LOESS smoother', loesscol) , ' options are turned off and that degree is set to 1. Initially two knots are used and set to the 33.3th and 66.7th percentiles from the distribution of x. Then, we can adjust 1 (degree) + 2 (number of knots) = 3 coefficients. Initially, two knots are used and set to the 33.3th and 66.7th percentiles from the distribution of x, the coefficients should be 0. Using two knots we have 3 coefficients for the natural spline to adjust.'), 
            paste0('In the panel "Spline Basis functions" the individual basis functions are visualized of which the response function in main panel is created. To weight the ', colorize('first spline basis', col[1]) , ' by 1, set ', colorize('coefficient 1', col[1]) , ' to 1 and see how this affects the response.'),
            paste0('Set the ', colorize('coefficient 1', col[1]) , ' back to 0 and repeat this step for ', colorize('coefficient 2', col[2]) , ' and for ', colorize('coefficient 3', col[3]) , '.'), 
            paste0('Now set ', colorize('coefficient 1', col[1]) , ' and ', colorize('coefficient 2', col[2]) , ' to 1, and ', colorize('coefficient 3', col[3]) , ' to 0. This adds up the ', colorize('spline basis 1', col[1]) , ' and ', colorize('spline basis 2', col[2]) , '.'), 
            paste0('Set ', colorize('coefficient 1', col[1]) , ' back to 0 and ', colorize('coefficient 3', col[3]) , ' to 1. You obtain the sum of ', colorize('spline basis 2', col[2]) , ' and ', colorize('spline basis 3', col[3]) , ' as response function.'), 
            'Now set all coefficients to 1. The response function is now exactly the sum of the three spline bases.', 
            'By adjusting the number of knots and the position of knot(s) create a V-shaped response curve.'
        )
    ), 
    'nsp' = list(
        'Basic' = c(
            paste0('In this exercise, we will examine the effect of individual spline basis functions on the response function. Reset the inputs. Make sure that data points, ', colorize('optimal fit', optfitcol) , ' and ', colorize('LOESS smoother', loesscol) , ' options are turned off.'),
            'Initially, two knots are used and set to the 33.3th and 66.7th percentiles from the distribution of x, the boundary knots are automatically set to the minimum and maximum values of x, and the coefficients should be 0. Using two internal knots we have 3 coefficients for the natural spline to adjust.', 
            paste0('Now move the slider of ', colorize('coefficient 1', col[1]) , ' to 1 and see how this affects the response curve. The ', colorize('spline basis 1', col[1]) , ' is now weighted by 1. The response function in the main output area should now be exactly equal to the ', colorize('spline basis 1', col[1]) , ' (spline basis functions panel). '),
            paste0('Set the ', colorize('coefficient 1', col[1]) , ' back to 0 and repeat the last step for ', colorize('coefficient 2', col[2]) , ' and for ', colorize('coefficient 3', col[3]) , '.'), 
            paste0('Now set ', colorize('coefficient 1', col[1]) , ' and ', colorize('coefficient 2', col[2]) , ' to 1, and ', colorize('coefficient 3', col[3]) , ' to 0. This adds up the ', colorize('spline basis 1', col[1]) , ' and ', colorize('spline basis 2', col[2]) , '.'), 
            paste0('Now also move ', colorize('coefficient 3', col[3]) , ' to 1. The response function is now exactly the sum of the three spline bases.'), 
            paste0('Reset the inputs with the button on the bottom and again turn off data points, ', colorize('optimal fit', optfitcol) , ' and ', colorize('LOESS smoother', loesscol) , '. Then move', colorize('coefficient 1', col[1]) , ' to -1. ', colorize('Spline basis 1', col[1]) , ' is then multiplied by -1, so the function is inverted.'),
            'Create a &Cup; -shaped response function by playing around with coefficient 1 to coefficient 3 only choosing values from {-1,-0.5,0, 0.5,1}.', 
            'Create a &Cap; -shaped response function by playing with coefficient 1 to coefficient 3 only choosing values from {-1,-0.5,0, 0.5,1}.'
        ),
        'Advanced'= c(
            'In this exercise, we will fit a simple regression where we try to describe expected height as a function of age using natural splines. Reset the inputs. Select the variable pair Height ~ Age. In advanced settings, set sample size to 100%, sex to "both", and display the height values by adding and clicking at "Response".', 
            'We want to fit a natural spline with 5 segments (i.e. two internal knots and two boundery knots). Set the knot positions to 
             approximately 12 and 15, and the boundary knots to 4 and 18.', 
            'The horizontal black line on the bottom is our current response function. To move the response function to the average height, click "Adjust intercept automatically".', 
            paste0('Increase the coefficient range to -40 and +40, and set ', colorize('coefficient 1', col[1]) , ',  ', colorize('coefficient 2', col[2]) , ' and ', colorize('coefficient 3', col[3]) , ' such that you receive a shape similar to the ', colorize('optimal fit', optfitcol) , '. Adjust the intercept manually to calibrate the response function. Can you find coefficients such that the adjusted  exceeds 0.65?'), 
            'Reduce the value of knot 1 from 12 to 10. Can you increase the adjusted R<sup>2</sup> by adjusting the coefficients?', 
            'Read off the current value of the intercept and memorize this number. Now click "Adjust intercept automatically". Did the number change or was your previous guess a good one?'
        )
    )
)
