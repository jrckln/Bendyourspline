colorize <- function(word, color) {
    paste0("<b> <font style='color: ", color, "'>", word, "</font> </b>")
}

exercises <- list(
    'Fractional Polynomials' = list(
        "Basic" = c(
            '<b>Goal:</b> In this exercise, you will examine the effect of individual fractional polynomial powers on the response function and familiarize yourself with the interface of the app.<br>
                <b>Suggested prerequisites:</b>  This exercise assumes no special background knowledge, but please feel free to read up on the basic aims of using fractional polynomials in the "Explanatory comments" tab [link]. <br>
                <b>Setup:</b>  Go to "Data options" and select "No data" from the dropdown menu. Make sure to reset the inputs at the bottom of the app.',
            'The panel "Basis Functions" visualizes the individual fractional polynomial functions. Each of the 2 functions has a corresponding coefficient. The panel "Response Function" shows the response function, which is a weighted sum of the individual basis functions, as you will see in the next few steps. At the moment, it is a simple horizontal line, as all the coefficients are set to 0.',
            paste0(
                    'To increase the weight of the ',
                    colorize('first fractional polynomial', col[1]) ,
                    ' by 1, set ',
                    colorize('coefficient 1', col[1]) ,
                    ' to 1 and observe the effect on the response function.'
                ),
            paste0(
                'Set the second power to 2 and set the ',
                colorize('associated coefficient', col[2]) ,
                ' to 1! You obtain x + x<sup>2</sup>, i.e. the first and the second fractional polynomial are summed-up.'
            ),
            paste0(
                'Now move ',
                colorize('coefficient 1', col[1]) ,
                ' to -1 and observe the effect on the response function.'
            ),
            paste0(
                'Set ',
                colorize('coefficient 1', col[1]) ,
                ' to +1 and ',
                colorize('coefficient 2', col[2]) ,
                ' to -1!'
            ),
            'Now change the second power to 3, then to 1, 0, -1 etc. See how this affects the response curve.',
            'Set both powers to 2. The two powers are now set to equal numbers, which defines a <b>repeated power</b>, and in this case the second term is also multiplied with log(x). (Check the formula in the "Formula" panel.)',
            'With the powers at 2, try to adjust the coefficients such that you obtain an approximately U-shaped response function!',
            'Keeping the powers as they are, try to invert this &cup; into a &cap; only with the coefficient sliders!'
        ),
        "Advanced I" = c(
            paste0(
                '<b>Goal:</b> In this exercise, you will use a nonlinear regression model to describe expected diastolic blood pressure as a function of age. <br>
          <b>Suggested prerequisites:</b>  You should know the basics about linear regression. Feel free to run through the Basic exercise to familiarize yourself with the app before starting this exercise. <br>
          <b>Setup:</b>  Go to "Data options" and select the variable pair "Diastolic blood pressure ~ Age" from the dropdown menu. In advanced settings, set sample size to 100% and sex to "both". Make sure to reset the inputs and enable data points and  ',
                colorize('LOESS smoother', loesscol) ,
                ' at the bottom of the app.'
            ),
            paste0(
                'The panel "Response function" shows the current response function, a simple horizontal line. Try to move the response function up to where the ',
                colorize('LOESS smoother', loesscol) ,
                ' is. Move the slider of  the intercept such that the response is close to the ',
                colorize('LOESS smoother', loesscol) ,
                '.'
            ),
            paste0(
                'By default, the two powers are set to 1. Increase the coefficient range, and move the slider of ',
                colorize('coefficient 1', col[1]) ,
                ' and the intercept to improve the fit. Goodness of fit can be monitored with the adjusted R<sup>2</sup> values displayed in the "Goodness of fit" panel. Move the sliders of ',
                colorize('coefficient 1', col[1]) ,
                ' and the intercept such that you obtain an adjusted R<sup>2</sup> of at least 0.08. <br>
                   Hint: You can use the + and - buttons to fine-tune the coefficient and intercept values.'
            ),
            paste0(
                'Obviously, there is curvature in the ',
                colorize('LOESS smoother', loesscol) ,
                ', which cannot be appropriately modelled with a linear response function as obtained in the previous step. Now, set the second power to 2. You can now increase the curvature by moving the slider of ',
                colorize('coefficient 2', col[2]) ,
                ' into the negative range. Hint: enlarge the coefficient range ("Coefficient range: +"). There is also a feature to compute an optimal intercept automatically by clicking "Adjust Intercept automatically". <br>
                   Try to obtain an adjusted R<sup>2</sup> of at least 0.15 by varying both coefficients and the intercept.'
            ),
            'Now play with the first power to change the curvature to better fit the data. When changing the power, you might have to adjust the sliders of the coefficients and the intercept to obtain a better adjusted R<sup>2</sup>.',
            'Can you find a combination, which reaches the maximum possible adjusted R<sup>2</sup> of 0.26?'
        ),
        'Advanced II' = c(
            '<b>Goal:</b> In this exercise, you will use a nonlinear regression model to describe expected body mass index (BMI) as a function of age using fractional polynomials. <br>
        <b>Suggested prerequisites:</b>  You should know the basics about linear regression. Feel free to run through the Basic and the Advanced I exercise to familiarize yourself with the app before starting this exercise. <br>
        <b>Setup:</b>  Go to "Data Options" and select the variable pair "Bmi ~ Age" from the dropdown menu. In advanced settings, set sample size to  10%, sex to "male" and the seed to 100. By doing so, a random sample with 10% of the full data set is drawn. Reset the inputs and display the values by enabling data points option at the bottom.',
            
            paste0(
                'Enable ',
                colorize('LOESS smoother', loesscol) ,
                ', and move the current response function to the average BMI by clicking "Adjust intercept automatically".'
            ),
            paste0(
                'Leave the first and the second power at 1. This means that age is log-transformed for the second fractional polynomial, the current formula is displayed in the "Formula" panel. Increase the coefficient range to -40 and +40, and set ',
                colorize('coefficient 1', col[1]) ,
                ' and ',
                colorize('coefficient 2', col[2]) ,
                ' such that you receive a shape similar to the ',
                colorize('LOESS smoother', loesscol) ,
                '. Adjust the intercept manually to calibrate the response function. Can you find coefficients such that the adjusted R<sup>2</sup> exceeds 0.30? <br>
            Hint: You can use the + and - buttons to fine-tune the coefficient and intercept values.'
            ),
            paste0(
                'Is it possible to receive a similar shape like the LOESS smoother? Enable ',
                colorize('optimal fit', optfitcol) ,
                '. Is your solution close to the optimal fit?'
            ),
            'Click on "set optimal fit" to display the coefficients of the optimal fit. Now, change the seed in "Data options" to 80 and see if the previous response function stil fits well to the new sample.',
            'Watch the power and the coefficient values while clicking on "set optimal fit" again. Did the powers change or did the coefficients change markedly?',
            paste(
                'Are the response functions for women and men very different? <br>
        Hint: The optimal response function for men is currently shown in the "Response function" panel. Select "female" in "Data options" and compare the ',
                colorize('optimal fit', optfitcol),
                ' of women to the response function, which was fitted on data from men.'
            )
            
        )
    ),
    'B-Splines' = list(
        'Basic' = c(
            '<b>Goal:</b> In this exercise, you examine the effect of individual linear B-spline basis functions on the response function and familiarize yourself with the interface of the app.<br>
      <b>Suggested prerequisites:</b>  This exercise assumes no special background knowledge, but please feel free to read up on the basic aims of using splines in the "Explanatory comments" tab [link]. <br>
      <b>Setup:</b>  Go to "Data options" and select "No data" from the dropdown menu. Make sure to reset the inputs at the bottom of the app.',
            'First, set degree to 1. Initially, this B-spline basis uses two knots, set to 33 and 67. All coefficients should be set to 0. There are 3 coefficients in total, which are given by the degree (1) and the number of knots (2) used for the B-spline basis.',
            'The panel "Basis Functions" visualizes the individual spline basis functions. Each of the 3 functions has a corresponding coefficient. The panel "Response Function" shows the response function, which is a weighted sum of the individual basis functions, as you will see in the next few steps. At the moment, it is a simple horizontal line, as all the coefficients are set to 0.',
            paste0(
                'To increase the weight of the ',
                colorize('first spline basis', col[1]) ,
                ' by 1, set ',
                colorize('coefficient 1', col[1]) ,
                ' to 1 and observe the effect on the response function. Afterwards, set ',
                colorize('coefficient 1', col[1]),
                ' back to 0.'
            ),
            paste0(
                'Now repeat the previous step for ',
                colorize('coefficient 2', col[2]) ,
                ' and for ',
                colorize('coefficient 3', col[3]) ,
                ' and observe their effects on the respone function.'
            ),
            paste0(
                'Now set ',
                colorize('coefficient 1', col[1]) ,
                ' and ',
                colorize('coefficient 2', col[2]) ,
                ' to 1, and leave',
                colorize('coefficient 3', col[3]) ,
                ' to 0. Observe that the respone function is now given as the sum of ',
                colorize('spline basis function 1', col[1]) ,
                ' and ',
                colorize('spline basis function 2', col[2]) ,
                '.'
            ),
            paste0(
                'Set ',
                colorize('coefficient 1', col[1]) ,
                ' back to 0 and ',
                colorize('coefficient 3', col[3]) ,
                ' to 1. You obtain the sum of ',
                colorize('spline basis function 2', col[2]) ,
                ' and ',
                colorize('spline basis function 3', col[3]) ,
                ' as response function.'
            ),
            'Now set all coefficients to 1. The response function is now the sum of all 3 spline basis functions.',
            'What kind of shapes can you obtain by twiddling with these controls? Try to create a V-shaped response function by adjusting the coefficients, but leave the knots and degree as they are.',
            'Now try increasing the degree to 2, and higher. Observe how the number of spline basis functions increases with higher degree, but also how they become smoother with higher degrees. <br>
      <b>Feel free to play around with the degree, the position of the knots and the coefficients to create all kinds of shapes using B-Splines!</b>'
        ),
        'Advanced I' = c(
            paste0(
                '<b>Goal:</b> In this exercise, you will use a  linear regression model to describe expected height as a function of age using B-splines. <br>
          <b>Suggested prerequisites:</b>  You should know the basics about linear regression. Feel free to run through the Basic exercise to familiarize yourself with the app before starting this exercise. <br>
          <b>Setup:</b>  Go to "Data options" and select the variable pair "Height ~ Age" from the dropdown menu. In advanced settings, set sample size to 50%, sex to "both". Make sure to reset the inputs and enable data points at the bottom of the app.',
                'You would like to fit a linear B-spline with 3 basis functions. Therefore, set degree to 1, select 2 knots, and set the knot positions to the 15th and 85th percentile. Enable "show knots" to visualize the positions in the response function panel.'
            ),
            'The panel "Response function" shows the current response function, a simple horizontal line. To move the response function to the average of all height values, go to "Intercept" and click "Adjust automatically".',
            paste0(
                'Show the optimal fit by enabling "optimal fit" at the bottom. Increase the coefficient range to -40 and +40, and set ',
                colorize('coefficient 1', col[1]) ,
                ',  ',
                colorize('coefficient 2', col[2]) ,
                ', ',
                colorize('coefficient 3', col[3]) ,
                ', and ',
                colorize('coefficient 4', col[4]) ,
                ' such that you receive a shape similar to the ',
                colorize('optimal fit', optfitcol) ,
                '. Adjust the intercept manually to calibrate the response function. Can you find coefficients such that the adjusted R<sup>2</sup> (in the goodness of fit panel) exceeds 0.60? <br>
          Hint: You can use the + and - buttons to fine-tune the coefficient and intercept values.'
            ),
            'Reduce the value of knot 2 from the 85th to the 60th percentile. Can you increase the adjusted R<sup>2</sup> to 0.65 by adjusting the coefficients and the intercept?',
            'Read off the current value of the intercept and memorize this number. Now click "Adjust intercept automatically" for perfect calibration of the response function given the current coefficient senttings. Did the intercept value change or was your previous guess a good one?'
            
        ),
        'Advanced II' = c(
            '<b>Goal:</b> In this exercise, you will use a  linear regression model to describe expected body mass index (BMI) as a function of age using B-splines. <br>
        <b>Suggested prerequisites:</b>  You should know the basics about linear regression. Feel free to run through the Basic and the Advanced I exercise to familiarize yourself with the app before starting this exercise. <br>
        <b>Setup:</b>  Go to  "Data options" and select the variable pair "Bmi ~ Age" from the dropdown menu. In advanced settings, set sample size to 20%, sex to "both", and seed to 60.By doing so, a random sample with 20% of the full data set is drawn. Reset the inputs and display the values by enabling data points option at the bottom.',
            'You would like to fit a linear B-spline with 3 basis functions. Therefore, set degree to 1 and select 1 knot. Enable "show knots" to visualize its position in the response function panel.',
            'The panel "Response function" shows the current response function, a simple horizontal line. To move the response function to the average of all BMI values, go to "Intercept" and click "Adjust automatically".',
            paste0(
                'Show the optimal fit (based on the current settions of degree and knots) by enabling "optimal fit" on the bottom. Increase the coefficient range to -20 and +20, and set ',
                colorize('coefficient 1', col[1]) ,
                ' and ',
                colorize('coefficient 2', col[2]) ,
                ' such that you receive a shape similar to the ',
                colorize('optimal fit', optfitcol) ,
                '. Adjust the intercept manually to calibrate the response function. Can you find coefficients such that the adjusted R<sup>2</sup> exceeds 0.25? <br>
        Hint: You can use the + and – buttons to fine-tune the coefficient and intercept values.'
            ),
            'Increase the number of knots to 2. A higher maximum adjusted R<sup>2</sup> can now be reached (given the defined knot positions).',
            'Can you find coefficients and an intercept such that R<sup>2</sup> is higher than 0.3?',
            'Try to vary the knot positions such that an even higher maximum adjusted R<sup>2</sup> is possible.',
            # pos at 40 & 70
            'Click on "set optimal fit" to display the coefficients of the optimal fit. Now, change the seed in "Data options" and see if the current response function still fits well when a new sample is drawn.',
            paste(
                'You probably found that your fit is quite robust. Are the response functions for women and men very different? <br>
      Hint: Select "male" in "Data options" and click "set optimal fit" to receive the response function for men. Then select "female" in "Data options" and compare the ',
                colorize('optimal fit', optfitcol),
                ' of women to the response function, which was fitted on data from men.'
            )
        )
    )
    ,
    'Natural Splines' = list(
        'Basic' = c(
            '<b>Goal:</b> In this exercise, you examine the effect of individual natural spline basis functions on the response function and familiarize yourself with the interface of the app.<br>
      <b>Suggested prerequisites:</b>  This exercise assumes no special background knowledge, but please feel free to read up on the basic aims of using splines in the "Explanatory comments" tab [link]. <br>
      <b>Setup:</b>  Go to "Data options" and select "No data" from the dropdown menu. Make sure to reset the inputs at the bottom of the app.',
            'Initially, two knots are used and set to 33 and 67, the boundary knots are automatically set to 5 and 95, and all coefficients should be 0. There are 3 coefficients in total, which can be adjusted.',
            'The panel "Basis Functions" visualizes the individual spline basis functions. Each of the 3 functions has a corresponding coefficient. The panel "Response Function" shows the response function, which is a weighted sum of the individual basis functions, as you will see in the next few steps. At the moment, it is a simple horizontal line, as all the coefficients are set to 0.',
            paste0(
                'To increase the weight of the ',
                colorize('first spline basis', col[1]) ,
                ' by 1, set ',
                colorize('coefficient 1', col[1]) ,
                ' to 1 and observe the effect on the response function. Afterwards, set ',
                colorize('coefficient 1', col[1]),
                ' back to 0.'
            ),
            paste0(
                'Now repeat the previous step for ',
                colorize('coefficient 2', col[2]) ,
                ' and for ',
                colorize('coefficient 3', col[3]) ,
                ' and observe their effects on the respone function.'
            ),
            paste0(
                'Now set ',
                colorize('coefficient 1', col[1]) ,
                ' and ',
                colorize('coefficient 2', col[2]) ,
                ' to 1, and leave',
                colorize('coefficient 3', col[3]) ,
                ' to 0. Observe that the respone function is now given as the sum of ',
                colorize('spline basis function 1', col[1]) ,
                ' and ',
                colorize('spline basis function 2', col[2]) ,
                '.'
            ),
            paste0(
                'Set ',
                colorize('coefficient 1', col[1]) ,
                ' back to 0 and ',
                colorize('coefficient 3', col[3]) ,
                ' to 1. You obtain the sum of ',
                colorize('spline basis function 2', col[2]) ,
                ' and ',
                colorize('spline basis function 3', col[3]) ,
                ' as response function.'
            ),
            'Now set all coefficients to 1. The response function is now the sum of all 3 spline basis functions.',
            paste0(
                'Reset the inputs with the button on the bottom. Then set',
                colorize('coefficient 1', col[1]) ,
                ' to -1. ',
                colorize('Spline basis 1', col[1]) ,
                ' is then multiplied by -1, so the response function is inverted.'
            ),
            paste(
                'Create a &cup; -shaped response function by playing around with ',
                colorize('coefficient 1', col[1]),
                ', ',
                colorize('coefficient 2', col[2]),
                ' and ',
                colorize('coefficient 3', col[3]) ,
                ' only choosing values from {-1,-0.5,0, 0.5,1}.'
            ),
            paste(
                'Create a &cap; -shaped response function by playing with ',
                colorize('coefficient 1', col[1]),
                ', ',
                colorize('coefficient 2', col[2]),
                ' and ',
                colorize('coefficient 3', col[3]) ,
                'only choosing values from {-1,-0.5,0, 0.5,1}.'
            )
        ),
        'Advanced I' = c(
            paste0(
                '<b>Goal:</b> In this exercise, you will use a linear regression model to describe expected height as a function of age using natural splines. <br>
          <b>Suggested prerequisites:</b>  You should know the basics about linear regression. Feel free to run through the Basic exercise to familiarize yourself with the app before starting this exercise. <br>
          <b>Setup:</b>  Go to "Data options" and select the variable pair "Height ~ Age" from the dropdown menu. In advanced settings, set sample size to 50% and sex to "both". Make sure to reset the inputs and enable data points at the bottom of the app.'
            ),
            'You would like to fit a linear B-spline with 3 basis functions. Therefore, set degree to 1, select 2 knots,  set the internal knot positions to the 15th and 85th percentile and the boundary knots to the 1st and 99th percentile. Enable "show knots" to visualize the positions in the response function panel.',
            'The panel "Response function" shows the current response function, a simple horizontal line. To move the response function to the average of all height values, go to "Intercept" and click "Adjust automatically".',
            paste0(
                'Increase the coefficient range to -20 and +20, and set ',
                colorize('coefficient 1', col[1]) ,
                ',  ',
                colorize('coefficient 2', col[2]) ,
                ' and ',
                colorize('coefficient 3', col[3]) ,
                ' such that you receive a shape similar to the ',
                colorize('optimal fit', optfitcol) ,
                '. Then, adjust the intercept manually to calibrate the response function. Can you find coefficients and an intercept such that the adjusted R<sup>2</sup> (in the goodness of fit panel) exceeds 0.60? <br>
      Hint: You can use the + and - buttons to fine-tune the coefficient and intercept values.'
            ),
            'Change the values of the internal knots to the 25th to the 75th percentile. Can you increase the adjusted R<sup>2</sup> by adjusting the coefficients?',
            'Read off the current values of the coefficients and memorize their numbers. Now click "Set optimal fit". Did the numbers change markedly or was your previous guess a good one?'
        ),
        'Advanced II' = c(
            '<b>Goal:</b> In this exercise, you will use a  linear regression model to describe expected average costs at an intensive care unit (TISS) as a function of mean blood pressure using natural splines. <br>
        <b>Suggested prerequisites:</b>  You should know the basics about linear regression. Feel free to run through the Basic and the Advanced I exercise to familiarize yourself with the app before starting this exercise. <br>
        <b>Setup:</b>  Go to "Data Options" and select the variable pair "Average TISS ~ Age" from the dropdown menu. In advanced settings, set sample size to 50%, sex to "both", and seed to 55.  By doing so, a random sample with 50% of the full data set is drawn. Reset the inputs and display the values by enabling data points option at the bottom.',
            'You would like to fit a natural spline with 3 internal knots and 2 boundary knots. Increase the number of internal knots to 3, and set the internal knot positions to the 25th, 50th and 75th quantile of the data. Enable "show knots" to visualize its position in the response function panel.',
            'The panel "Response function" shows the current response function, a simple horizontal line. To move the response function to the average of all TISS values, go to "Intercept" and click "Adjust automatically".',
            'Enable "optimal fit" and play around with the boundary knot positions. If you set the boundary knots closer to the edges, the functional form of the optimal fit flattens at the edges, whereas it shows a steeper angle when boundary knots are closer to the internal knot positions. What do you think fits the data better or is more plausible in the context?',
            'Set the boundary knots to the 5th and 95th quantile again, and click "set optimal fit". Then, go back to "Data options" and set the sample size to 20% the data. Does the previous response function fit the data well? ',
            paste0(
                'Click again on "set optimal fit" such that the fit is adapted to the smaller data set. Then change the seed in "Data options" to 100. Does your response function based on the previous data sample still fits well? Compare your previous response function to the current ',
                colorize('optimal fit', optfitcol) ,
                '. Also look at the adjusted R<sup>2</sup> and the maximum adjusted R<sup>2</sup>.'
            ),
            'You probably found that the optimal response function (given the number of knots and the knot position) looks quite different depending on the sample, especially, when the sample size is small.'
        )
    )
)