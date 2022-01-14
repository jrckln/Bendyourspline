colorize <- function(word, color) {
  paste0("<b> <font style='color: ", color, "'>", word, "</font> </b>")
}

exercises <- list(
    'Fractional Polynomials' = list(
        "Basic"= 
            c(
            paste0('The aim of this exercise is to understand the impact of individual FP powers on the modeled response function. Reset the inputs. Use "No data" as variable pair.'),
            paste0('Move the slider of ', colorize('coefficient 1', col[1]) , ' to 1 and see how the response curve reacts!'), 
            paste0('Set the second power to 2 and move the ', colorize('associated coefficient', col[2]) ,' to 1!'), 
            paste0('Now move ', colorize('coefficient 1', col[1]) , ' to -1!'), 
            paste0('Now set ', colorize('coefficient 1', col[1]) , ' to +1 and ', colorize('coefficient 2', col[2]) , ' to -1!'), 
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
      'In this exercise, we examine the effect of individual linear B-spline basis functions  on the response function. Select "no data" in the data options and reset the inputs on the bottom.',
       'Initially two knots are used and set to 33 and 67. Then, we can adjust 1 (degree) + 2 (number of knots) = 3 coefficients. Initially, the coefficients should be 0. ',
      paste0(
        'In the panel "Spline Basis functions" the individual basis functions are visualized of which the response function in main panel is created. To weight the ',
        colorize('first spline basis', col[1]) ,
        ' by 1, set ',
        colorize('coefficient 1', col[1]) ,
        ' to 1 and see how this affects the response.'
      ),
      paste0(
        'Set the ',
        colorize('coefficient 1', col[1]) ,
        ' back to 0 and repeat this step for ',
        colorize('coefficient 2', col[2]) ,
        ' and for ',
        colorize('coefficient 3', col[3]) ,
        '.'
      ),
      paste0(
        'Now set ',
        colorize('coefficient 1', col[1]) ,
        ' and ',
        colorize('coefficient 2', col[2]) ,
        ' to 1, and ',
        colorize('coefficient 3', col[3]) ,
        ' to 0. This adds up the ',
        colorize('spline basis 1', col[1]) ,
        ' and ',
        colorize('spline basis 2', col[2]) ,
        '.'
      ),
      paste0(
        'Set ',
        colorize('coefficient 1', col[1]) ,
        ' back to 0 and ',
        colorize('coefficient 3', col[3]) ,
        ' to 1. You obtain the sum of ',
        colorize('spline basis 2', col[2]) ,
        ' and ',
        colorize('spline basis 3', col[3]) ,
        ' as response function.'
      ),
      'Now set all coefficients to 1. The response function is now exactly the sum of the three spline bases.',
      'By adjusting the number of knots and the position of knot(s) create a V-shaped response curve.'
    ),
    'Advanced I' = c(
      paste0(
        'In this exercise, we will fit a simple regression where we try to describe expected height as a function of age using B-splines. Reset the inputs. Select the variable pair Height ~ Age. In advanced settings, set sample size to 50%, sex to "both", and display the height values by enabling data points option.',
        'We want to fit a B-spline with 3 segments. Set the knot positions to 15th and 85th quantile.',
        'The horizontal black line on the bottom is our current response function. To move the response function to the average height, click "Adjust intercept automatically".'),
        paste0(
          'Show the optimal fit by enabling "optimal fit". Increase the coefficient range to -40 and +40, and set ',
          colorize('coefficient 1', col[1]) ,
          ',  ',
          colorize('coefficient 2', col[2]) ,
          ', ',
          colorize('coefficient 3', col[3]) ,
          ', and ',
          colorize('coefficient 4', col[4]) ,
          ' such that you receive a shape similar to the ',
          colorize('optimal fit', optfitcol) ,
          '. Adjust the intercept manually to calibrate the response function. Can you find coefficients such that the adjusted R<sup>2</sup> exceeds 0.65?'
        ),
        'Reduce the value of knot 1 from 12 to 10. Can you increase the adjusted R<sup>2</sup> by adjusting the coefficients?',
        'Read off the current value of the intercept and memorize this number. Now click "Adjust intercept automatically". Did the number change or was your previous guess a good one?'
        
      ),
    'Advanced II' = c(
      paste0(
        'In this exercise, we will fit a simple regression where we try to describe expected body mass index (BMI) as a function of age using B-splines. Reset the inputs. Select the variable pair Bmi ~ Age. In advanced settings, set sample size to 20%, sex to "both", the seed to 60, and display the values by enabling data points option.',
        'Set the input parameters such that a B-spline with 2 segments of degree 1 (linear splines) is fitted and show the optimal fit by enabling "optimal fit".',
        'The horizontal black line on the bottom is our current response function. To move the response function to the average height, click "Adjust intercept automatically".'),
      paste0(
        'Show the optimal fit by enabling "optimal fit". Increase the coefficient range to -20 and +20, and set ',
        colorize('coefficient 1', col[1]) ,
        ' and ',
        colorize('coefficient 2', col[2]) ,
        ' such that you receive a shape similar to the ',
        colorize('optimal fit', optfitcol) ,
        '. Adjust the intercept manually to calibrate the response function. Can you find coefficients such that the adjusted R<sup>2</sup> exceeds 0.25?'
      ),
      'Increase the number of knots to 2. A higher maximum adjusted R<sup>2</sup> can now be reached (given the defined knot positions).',
      'Can you find coefficients and an intercept such that R<sup>2</sup> is higher than 0.3?',
      'Try to vary the knot positions such that an even higher maximum adjusted R<sup>2</sup> is possible.', # pos at 40 & 70
      'Click on "set optimal fit" to display the coefficients of the optimal fit. Now, change the seed in data options and see if this fit is still good when a new sample is drawn.',
      'You probably found that your fit is quite robust. Can you detect differences between women and men (vary sex in data options)?.'
    )
  )
,
'nsp' = list(
  'Basic' = c(
    'In this exercise, we will examine the effect of individual spline basis functions on the response function. Select "no data" in the data options and reset the inputs on the bottom. ',
    'Initially, two knots are used and set to the 33 and 67, the boundary knots are automatically set to the 0 and 100 (minimum and maximum of the x-axis), and the coefficients should be 0. Using two internal knots we have 3 coefficients for the natural spline to adjust.',
    paste0(
      'Now move the slider of ',
      colorize('coefficient 1', col[1]) ,
      ' to 1 and see how this affects the response curve. The ',
      colorize('spline basis 1', col[1]) ,
      ' is now weighted by 1. The response function in the main output area should now be exactly equal to the ',
      colorize('spline basis 1', col[1]) ,
      ' (spline basis functions panel). '
    ),
    paste0(
      'Set the ',
      colorize('coefficient 1', col[1]) ,
      ' back to 0 and repeat the last step for ',
      colorize('coefficient 2', col[2]) ,
      ' and for ',
      colorize('coefficient 3', col[3]) ,
      '.'
    ),
    paste0(
      'Now set ',
      colorize('coefficient 1', col[1]) ,
      ' and ',
      colorize('coefficient 2', col[2]) ,
      ' to 1, and ',
      colorize('coefficient 3', col[3]) ,
      ' to 0. This adds up the ',
      colorize('spline basis 1', col[1]) ,
      ' and ',
      colorize('spline basis 2', col[2]) ,
      '.'
    ),
    paste0(
      'Now also move ',
      colorize('coefficient 3', col[3]) ,
      ' to 1. The response function is now exactly the sum of the three spline bases.'
    ),
    paste0(
      'Reset the inputs with the button on the bottom and again turn off data points, ',
      colorize('optimal fit', optfitcol) ,
      ' and ',
      colorize('LOESS smoother', loesscol) ,
      '. Then move',
      colorize('coefficient 1', col[1]) ,
      ' to -1. ',
      colorize('Spline basis 1', col[1]) ,
      ' is then multiplied by -1, so the function is inverted.'
    ),
    'Create a &Cup; -shaped response function by playing around with coefficient 1 to coefficient 3 only choosing values from {-1,-0.5,0, 0.5,1}.',
    'Create a &Cap; -shaped response function by playing with coefficient 1 to coefficient 3 only choosing values from {-1,-0.5,0, 0.5,1}.'
  ),
  'Advanced I' = c(
    'In this exercise, we will fit a simple regression where we try to describe expected height as a function of age using natural splines. Reset the inputs. Select the variable pair Height ~ Age. In advanced settings, set sample size to 100%, sex to "both", and display the height values by enabling data points option.',
    'We want to fit a natural spline with 5 segments (i.e. two internal knots and two boundery knots). Set the knot positions to
    approximately 12 and 15, and the boundary knots to 4 and 18.',
    'The horizontal black line on the bottom is our current response function. To move the response function to the average height, click "Adjust intercept automatically".',
    paste0(
      'Increase the coefficient range to -20 and +20, and set ',
      colorize('coefficient 1', col[1]) ,
      ',  ',
      colorize('coefficient 2', col[2]) ,
      ' and ',
      colorize('coefficient 3', col[3]) ,
      ' such that you receive a shape similar to the ',
      colorize('optimal fit', optfitcol) ,
      '. Adjust the intercept manually to calibrate the response function. Can you find coefficients such that the adjusted R<sup>2</sup> exceeds 0.62?'
    ),
    'Reduce the value of knot 2 from 85 to 95 Can you increase the adjusted R<sup>2</sup> by adjusting the coefficients?',
    'Read off the current values of the coefficients and memorize their numbers. Now click "Set optimal fit". Did the numbers change markedly or was your previous guess a good one?'
  ),
  'Advanced II' = c(
    'In this exercise, we will fit a simple regression where we try to describe expected average costs at an intensive care unit (TISS) as a function of mean blood pressure using natural splines. Reset the inputs. Select the variable pair Average TISS ~ Mean blood pressure. In advanced settings, set sample size to 50%, sex to "both", seed to 55, and display the values by enabling data points option.',
     'We want to fit a natural spline with 6 segments (i.e. 3 internal knots and 2 boundery knots). Increase the number of internal knot to 3, and set the internal knot positions to the 25th, 50th and 75th quantile of the data.',
    'Enable "optimal fit" and play around with the boundary knot positions. If you set the boundary knots closer to the edges, the functional form of the optimal fit flattens at the edges, whereas it shows a steeper angle when boundary knots are closer to the internal knot positions. What do you think fits the data better or is more plausible in the context?',
    'Set the boundary knots to the 5th and 95th quantile again, and click "set optimal fit". Then, go back to data options and set the sample size to 20% the data. Does the previous response function fit the data well?. ',
    'Click again on "set optimal fit" such that the fit is adapted to the smaller data set. Then change the seed in the data options to 100. Is your fit from the previous data sample still good? Compare the your previous fit (response) and the current optimal fit. Also look at the adjusted R<sup>2</sup>.',
    'You probably found that the optimal respone function given the number of knots and the knot position looks quite different depending on the sample, especially, when the sample size is small.'
)
)
)