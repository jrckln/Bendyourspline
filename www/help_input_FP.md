---
title: "Input Parameters - Fractional Polynomials"
---

### Power 

Use the sliders to change the exponents of the fractional polynomials. For FP1 and FP2 a set with 8 values was proposed: $S=−2,−1,−0.5,\log(x),0.5,1,2,3$
For $p1=p2=p$ ('repeated powers') it is defined as $FP2=\beta_1x^p+\beta_2x^p⋅\log(x)$.
This defines 8 FP1 and 36 FP2 models.
See link: (https://mfp.imbi.uni-freiburg.de/fp)

### Coefficients 

Use the sliders to change the coefficients $\beta_1$ and $\beta_2$ of the fractional polynomials. Set the first coefficient to zero to obtain FP1.

You can adjust the step size and the range of values in "..." at the top-right of the panel.

### Intercept 

You can either manually adjust the intercept, or let it adjust automatically in which case the sum of the squared residuals is minimized.
You can adjust the step size and the range of values in "..." at the top-right of the panel.

### Data points 

This option plots the individual data points of the chosen variable pair in the 'Response function' panel.

### LOESS Smoother 

This option plots the LOESS Smoother in the 'Response function' panel.
The LOESS Smoother creates a non-linear smooth curve through the data points without fitting a parametric model and can therefore be used here as a benchmark.

### Optimal fit 

This option plots the optimal fit in the 'Response function' panel. The optimal fit is obtained from ```mfp::mfp()```.

### Set optimal fit

This button sets the response function to the optimal fit. It updates the power sliders, coefficient sliders and the intercept slider to the optimised values obtained from ```mfp::mfp()```.

### Reset inputs 

This button resets the input values except for the coefficient range, which has to be decreased manually. 
