---
title: 'Input Parameters - Natural Splines'
---

Specify inputs here

### Number and position of knots 

The domain of the independent variable $x$, $[\\min(x), \\max(x)]$, is divided into $k+3$ disjunct intervals. $k$ denotes the number of internal knots. In each of the $k+1$ inner intervals (within the boundary knots) a polynomial function of degree 3 is defined. The spline in the interval from $\\min(x)$ to boundary knot 1 as well as the interval from boundary knot 2 to $\\max(x)$ is linearly extrapolated.

At default, the knots are set to the percentiles of the independent variable $x$ which are equidistantly distributed across 0 and 1. The boundary knots are set to $\\min(x) + 0.01$ and $\\max(x) - 0.01$.

### Coefficients 

Use the sliders to change the coefficients $1$ to $k+1$ of the spline basis functions. 
You can adjust the step size and the range of values in "..." at the top-right of the panel. 

### Intercept 

You can either manually adjust the intercept, or let it adjust automatically in which case the sum of the squared residuals is minimized.
You can adjust the step size and the range of values in "..." at the top-right of the panel.

### Data points 

This option plots the individual data points of the chosen variable pair in the 'Response function' panel.

### LOESS Smoother 

This option plots the LOESS Smoother in the 'Response function' panel.
The LOESS Smoother creates a non-linear smooth curve through the data points without fitting a parametric model and can therefore be used here as a benchmark.

### Knot position 

Shows vertical lines in the 'response function' and 'spline basis functions' panel at the positions of the knots and prints the respective approximate percentiles. 

### Optimal fit 

This option plots the optimal fit in the 'Response function' panel. For the optimal fit, the coefficients (and the intercept) are optimised given the currently defined number and position of knots.

### Set optimal fit

This button sets the response function to the optimal fit. It updates the coefficient sliders and the intercept slider to the optimised values given the currently defined number and position of knots.

### Reset inputs 

This button resets the input values except for the coefficient range, which has to be decreased manually.