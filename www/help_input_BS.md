---
title: 'Input Parameters - B-Splines'
---

Specify inputs here

### Degree $d$

This input defines the degree of the individual polynomials of which the spline is combined. Degrees of 1, 2 and 3 correspond to linear, quadratic and cubic splines, respectively.

### Number and position of knots 

The domain of the independent variable $x$, $[\\min(x), \\max(x)]$, is divided into $k+1$ disjunct intervals. The boundaries of the intervals are called knots and $k$ denotes the number of knots. In each of these intervals a polynomial function of degree $d$ is defined. A spline is a piecewise polynomial function which has continuous derivatives up to order $(d-1)$.

At default, the knots are set to the percentiles of the independent variable $x$ which are equidistantly distributed across 0 and 1. 

### Coefficients 

Use the sliders to change the coefficients $1$ to $d+k$ of the spline basis functions. 

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

This option plots the optimal fit in the 'Response function' panel. For the optimal fit, the coefficients (and the intercept) are optimised given the currently defined number and position of knots and degree.

### Set optimal fit

This button sets the response function to the optimal fit. It updates the coefficient sliders and the intercept slider to the optimised values given the currently defined number and position of knots and degree.

### Reset inputs 

This button resets the input values except for the coefficient range, which has to be decreased manually. 