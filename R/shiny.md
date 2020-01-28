# Shiny

Good resource: https://kellobri.github.io/shiny-prod-book/

* an R package
* lets you build web apps in R
* created by rstudio
    * who also have a package manager
* can output to:
    * host standalone apps on a webpage
    * embed them in _R markdown_ documents
    * build dashboards
* extend them with
    * CSS themes
    * htmlwidgets
    * JS actions

```r
install.packages("shiny")

# puts 'shiny' (on my machine) in:
# /usr/local/lib/R/3.6/site-library

# also installs the R dependencies
#     ‘Rcpp’, ‘BH’, ‘magrittr’, ‘httpuv’, ‘mime’, ‘jsonlite’, ‘xtable’,
#     ‘digest’, ‘htmltools’, ‘R6’, ‘sourcetools’, ‘later’, ‘promises’,
#     ‘crayon’, ‘rlang’, ‘fastmap’
```

* it compiles a bunch of C/C++ when you install shiny
* R is single threaded and R is the web server when you run shiny
* R uses CSV files for data out of the box but can be persuaded to use a SQL DB
* shiny has a bunch of compiled dependencies so a docker deploy might be a good idea
* it seems like interactive elements on the page send requests back ot the R process to get it to re-render data and send it back
* there is a testing framework looks a bit rudimentary - uses phantomjs https://rstudio.github.io/shinytest/

Usage Stories

https://medium.com/inlocotech/r-shiny-in-production-e2fb6a577fe0
https://appsilon.com/why-you-should-use-r-shiny-for-enterprise-application-development/


Examples

https://statisticsnz.shinyapps.io/trade_dashboard/


## JSON

> all objects that are sent from R to JavaScript, or from JavaScript to R,
> are tacitly passed and processed by Shiny via jsonlite, using the functions
> toJSON and fromJSON