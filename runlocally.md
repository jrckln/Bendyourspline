---
layout: page
title: Run locally
permalink: /runlocal/
---

### To run **Bend your (sp)line** locally, you first need to install [R](https://www.r-project.org). 

#### Install R
##### For Windows

1. Go to [https://cran.rstudio.com/](https://cran.rstudio.com/)
2. Under **Download and Install R** click on **Download R for Windows**
3. Choose **base**
4. Click on **Download R x.x.x for Windows**
5. Save and run the .exe file and follow the installation instructions

##### For Mac OS

1. Go to [https://cran.rstudio.com/](https://cran.rstudio.com/)
2. Under **Download and Install R** click on **Download R for macOS**
3. Under **Latest release** click on the file **R-x.x.x.pkg** which contains the latest version of R
5. Save and open the .pkg file and follow the installation instructions

#### Run **Bend your (sp)line**

Start R and install the `shiny` package with

```
install.packages('shiny')
```

To run the app locally, you can use the function `runGitHub()`: 

``` 
shiny::runGitHub('Bendyourspline', 'jrckln')
```

Another way is to clone or download this repository and run

``` 
shiny::runApp()
```
