## Input Parameters - Fractional Polynomials

Specify inputs here

### Power 

Use the sliders to change the exponents of the fractional polynomials. For FP1 and FP2 a set with 8 values was proposed: $S=−2,−1,−0.5,\\log(x),0.5,1,2,3$
For $p1=p2=p$ ('repeated powers') it is defined as $FP2=\\beta_1x^p+\\beta_2x^p⋅\\log(x)$.
This defines 8 FP1 and 36 FP2 models.
See link: (https://mfp.imbi.uni-freiburg.de/fp)

### Coefficients 

Use the sliders to change the coefficients $\\beta_1$ and $\\beta_2$ of the fractional polynomials. Set the first coefficient to zero to obtain FP1.

For a better control of the coefficients use the + and - buttons near the sliders. Additionally you can increase the coefficient range at the top of the panel. 

### Intercept 

You can either manually adjust the intercept, or let it adjust automatically in which case the sum of the squared residuals is minimized.

### Data points 

This option plots the individual data points of the chosen variable pair in the 'Response function' panel.

### LOESS Smoother 

This option plots the LOESS Smoother in the 'Response function' panel.
The LOESS Smoother creates a non-linear smooth curve through the data points without fitting a parametric model and can therefore be used here as a benchmark.

### Optimal fit 

This option plots the optimal fit in the 'Response function' panel. For the optimal fit, the coefficients (and the intercept) are optimised given the currently defined powers of the fractional polynomials. 

### Set optimal fit

This button sets the response function to the optimal fit. It updates the coefficient sliders and the intercept slider to the optimised values given the currently defined powers of the fractional polynomials.

### Reset inputs 

This button resets the input values except for the coefficient range, which has to be decreased manually. 
