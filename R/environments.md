# Set up R on macOS

```bash
your@your-computer $ brew install r

# Install renv
r> install.packages("renv")
# Packages are installed to /usr/local/lib/R/4.0/site-library by default

# list installed packages:
# notice the distinction between "site library" and the built-in "library"
r> installed.packages()
# =>            Package      LibPath                                     Version
# => renv       "renv"       "/usr/local/lib/R/4.0/site-library"         "0.10.0"
# => base       "base"       "/usr/local/Cellar/r/4.0.0_1/lib/R/library" "4.0.0"
# => boot       "boot"       "/usr/local/Cellar/r/4.0.0_1/lib/R/library" "1.3-24"
# => class      "class"      "/usr/local/Cellar/r/4.0.0_1/lib/R/library" "7.3-16"
# => cluster    "cluster"    "/usr/local/Cellar/r/4.0.0_1/lib/R/library" "2.1.0"
# => codetools  "codetools"  "/usr/local/Cellar/r/4.0.0_1/lib/R/library" "0.2-16"
# => compiler   "compiler"   "/usr/local/Cellar/r/4.0.0_1/lib/R/library" "4.0.0"
# => datasets   "datasets"   "/usr/local/Cellar/r/4.0.0_1/lib/R/library" "4.0.0"
# => foreign    "foreign"    "/usr/local/Cellar/r/4.0.0_1/lib/R/library" "0.8-78"
# => graphics   "graphics"   "/usr/local/Cellar/r/4.0.0_1/lib/R/library" "4.0.0"
# => grDevices  "grDevices"  "/usr/local/Cellar/r/4.0.0_1/lib/R/library" "4.0.0"
# => grid       "grid"       "/usr/local/Cellar/r/4.0.0_1/lib/R/library" "4.0.0"
# => KernSmooth "KernSmooth" "/usr/local/Cellar/r/4.0.0_1/lib/R/library" "2.23-16"
# => lattice    "lattice"    "/usr/local/Cellar/r/4.0.0_1/lib/R/library" "0.20-41"
# => MASS       "MASS"       "/usr/local/Cellar/r/4.0.0_1/lib/R/library" "7.3-51.5"
# => Matrix     "Matrix"     "/usr/local/Cellar/r/4.0.0_1/lib/R/library" "1.2-18"
# => methods    "methods"    "/usr/local/Cellar/r/4.0.0_1/lib/R/library" "4.0.0"
# => mgcv       "mgcv"       "/usr/local/Cellar/r/4.0.0_1/lib/R/library" "1.8-31"
# => nlme       "nlme"       "/usr/local/Cellar/r/4.0.0_1/lib/R/library" "3.1-147"
# => nnet       "nnet"       "/usr/local/Cellar/r/4.0.0_1/lib/R/library" "7.3-13"
# => parallel   "parallel"   "/usr/local/Cellar/r/4.0.0_1/lib/R/library" "4.0.0"
# => rpart      "rpart"      "/usr/local/Cellar/r/4.0.0_1/lib/R/library" "4.1-15"
# => spatial    "spatial"    "/usr/local/Cellar/r/4.0.0_1/lib/R/library" "7.3-11"
# => splines    "splines"    "/usr/local/Cellar/r/4.0.0_1/lib/R/library" "4.0.0"
# => stats      "stats"      "/usr/local/Cellar/r/4.0.0_1/lib/R/library" "4.0.0"
# => stats4     "stats4"     "/usr/local/Cellar/r/4.0.0_1/lib/R/library" "4.0.0"
# => survival   "survival"   "/usr/local/Cellar/r/4.0.0_1/lib/R/library" "3.1-12"
# => tcltk      "tcltk"      "/usr/local/Cellar/r/4.0.0_1/lib/R/library" "4.0.0"
# => tools      "tools"      "/usr/local/Cellar/r/4.0.0_1/lib/R/library" "4.0.0"
# => utils      "utils"      "/usr/local/Cellar/r/4.0.0_1/lib/R/library" "4.0.0"
#
# =>            Priority      Depends
# => renv       NA            NA
# => base       "base"        NA
# => boot       "recommended" "R (>= 3.0.0), graphics, stats"
# => class      "recommended" "R (>= 3.0.0), stats, utils"
# => cluster    "recommended" "R (>= 3.3.0)"
# => codetools  "recommended" "R (>= 2.1)"
# => compiler   "base"        NA
# => datasets   "base"        NA
# => foreign    "recommended" "R (>= 4.0.0)"
# => graphics   "base"        NA
# => grDevices  "base"        NA
# => grid       "base"        NA
# => KernSmooth "recommended" "R (>= 2.5.0), stats"
# => lattice    "recommended" "R (>= 3.0.0)"
# => MASS       "recommended" "R (>= 3.1.0), grDevices, graphics, stats, utils"
# => Matrix     "recommended" "R (>= 3.2.0)"
# => methods    "base"        NA
# => mgcv       "recommended" "R (>= 2.14.0), nlme (>= 3.1-64)"
# => nlme       "recommended" "R (>= 3.4.0)"
# => nnet       "recommended" "R (>= 3.0.0), stats, utils"
# => parallel   "base"        NA
# => rpart      "recommended" "R (>= 2.15.0), graphics, stats, grDevices"
# => spatial    "recommended" "R (>= 3.0.0), graphics, stats, utils"
# => splines    "base"        NA
# => stats      "base"        NA
# => stats4     "base"        NA
# => survival   "recommended" "R (>= 3.4.0)"
# => tcltk      "base"        NA
# => tools      "base"        NA
# => utils      "base"        NA
#
# =>            Imports                                            LinkingTo
# => renv       "utils"                                            NA
# => base       NA                                                 NA
# => boot       NA                                                 NA
# => class      "MASS"                                             NA
# => cluster    "graphics, grDevices, stats, utils"                NA
# => codetools  NA                                                 NA
# => compiler   NA                                                 NA
# => datasets   NA                                                 NA
# => foreign    "methods, utils, stats"                            NA
# => graphics   "grDevices"                                        NA
# => grDevices  NA                                                 NA
# => grid       "grDevices, utils"                                 NA
# => KernSmooth NA                                                 NA
# => lattice    "grid, grDevices, graphics, stats, utils"          NA
# => MASS       "methods"                                          NA
# => Matrix     "methods, graphics, grid, stats, utils, lattice"   NA
# => methods    "utils, stats"                                     NA
# => mgcv       "methods, stats, graphics, Matrix, splines, utils" NA
# => nlme       "graphics, stats, utils, lattice"                  NA
# => nnet       NA                                                 NA
# => parallel   "tools, compiler"                                  NA
# => rpart      NA                                                 NA
# => spatial    NA                                                 NA
# => splines    "graphics, stats"                                  NA
# => stats      "utils, grDevices, graphics"                       NA
# => stats4     "graphics, methods, stats"                         NA
# => survival   "graphics, Matrix, methods, splines, stats, utils" NA
# => tcltk      "utils"                                            NA
# => tools      NA                                                 NA
# => utils      NA                                                 NA
#
# =>            Suggests
# => renv       "covr, knitr, packrat, remotes, reticulate, rmarkdown,\nrstudioapi, testthat, uuid, yaml"
# => base       "methods"
# => boot       "MASS, survival"
# => class      NA
# => cluster    "MASS, Matrix"
# => codetools  NA
# => compiler   NA
# => datasets   NA
# => foreign    NA
# => graphics   NA
# => grDevices  "KernSmooth"
# => grid       "lattice"
# => KernSmooth "MASS"
# => lattice    "KernSmooth, MASS, latticeExtra"
# => MASS       "lattice, nlme, nnet, survival"
# => Matrix     "expm, MASS"
# => methods    "codetools"
# => mgcv       "parallel, survival, MASS"
# => nlme       "Hmisc, MASS"
# => nnet       "MASS"
# => parallel   "methods"
# => rpart      "survival"
# => spatial    "MASS"
# => splines    "Matrix, methods"
# => stats      "MASS, Matrix, SuppDists, methods, stats4"
# => stats4     NA
# => survival   NA
# => tcltk      NA
# => tools      "codetools, methods, xml2, curl, commonmark"
# => utils      "methods, xml2, commonmark"
#
# =>            Enhances                                License
# => renv       NA                                      "MIT + file LICENSE"
# => base       NA                                      "Part of R 4.0.0"
# => boot       NA                                      "Unlimited"
# => class      NA                                      "GPL-2 | GPL-3"
# => cluster    NA                                      "GPL (>= 2)"
# => codetools  NA                                      "GPL"
# => compiler   NA                                      "Part of R 4.0.0"
# => datasets   NA                                      "Part of R 4.0.0"
# => foreign    NA                                      "GPL (>= 2)"
# => graphics   NA                                      "Part of R 4.0.0"
# => grDevices  NA                                      "Part of R 4.0.0"
# => grid       NA                                      "Part of R 4.0.0"
# => KernSmooth NA                                      "Unlimited"
# => lattice    "chron"                                 "GPL (>= 2)"
# => MASS       NA                                      "GPL-2 | GPL-3"
# => Matrix     "MatrixModels, graph, SparseM, sfsmisc" "GPL (>= 2) | file LICENCE"
# => methods    NA                                      "Part of R 4.0.0"
# => mgcv       NA                                      "GPL (>= 2)"
# => nlme       NA                                      "GPL (>= 2) | file LICENCE"
# => nnet       NA                                      "GPL-2 | GPL-3"
# => parallel   "snow, nws, Rmpi"                       "Part of R 4.0.0"
# => rpart      NA                                      "GPL-2 | GPL-3"
# => spatial    NA                                      "GPL-2 | GPL-3"
# => splines    NA                                      "Part of R 4.0.0"
# => stats      NA                                      "Part of R 4.0.0"
# => stats4     NA                                      "Part of R 4.0.0"
# => survival   NA                                      "LGPL (>= 2)"
# => tcltk      NA                                      "Part of R 4.0.0"
# => tools      NA                                      "Part of R 4.0.0"
# => utils      NA                                      "Part of R 4.0.0"
#
# =>            License_is_FOSS License_restricts_use OS_type MD5sum
# => renv       NA              NA                    NA      NA
# => base       NA              NA                    NA      NA
# => boot       NA              NA                    NA      NA
# => class      NA              NA                    NA      NA
# => cluster    NA              NA                    NA      NA
# => codetools  NA              NA                    NA      NA
# => compiler   NA              NA                    NA      NA
# => datasets   NA              NA                    NA      NA
# => foreign    NA              NA                    NA      NA
# => graphics   NA              NA                    NA      NA
# => grDevices  NA              NA                    NA      NA
# => grid       NA              NA                    NA      NA
# => KernSmooth NA              NA                    NA      NA
# => lattice    NA              NA                    NA      NA
# => MASS       NA              NA                    NA      NA
# => Matrix     NA              NA                    NA      NA
# => methods    NA              NA                    NA      NA
# => mgcv       NA              NA                    NA      NA
# => nlme       NA              NA                    NA      NA
# => nnet       NA              NA                    NA      NA
# => parallel   NA              NA                    NA      NA
# => rpart      NA              NA                    NA      NA
# => spatial    NA              NA                    NA      NA
# => splines    NA              NA                    NA      NA
# => stats      NA              NA                    NA      NA
# => stats4     NA              NA                    NA      NA
# => survival   NA              NA                    NA      NA
# => tcltk      NA              NA                    NA      NA
# => tools      NA              NA                    NA      NA
# => utils      NA              NA                    NA      NA
#
# =>            NeedsCompilation Built
# => renv       "no"             "4.0.0"
# => base       NA               "4.0.0"
# => boot       "no"             "4.0.0"
# => class      "yes"            "4.0.0"
# => cluster    "yes"            "4.0.0"
# => codetools  "no"             "4.0.0"
# => compiler   NA               "4.0.0"
# => datasets   NA               "4.0.0"
# => foreign    "yes"            "4.0.0"
# => graphics   "yes"            "4.0.0"
# => grDevices  "yes"            "4.0.0"
# => grid       "yes"            "4.0.0"
# => KernSmooth "yes"            "4.0.0"
# => lattice    "yes"            "4.0.0"
# => MASS       "yes"            "4.0.0"
# => Matrix     "yes"            "4.0.0"
# => methods    "yes"            "4.0.0"
# => mgcv       "yes"            "4.0.0"
# => nlme       "yes"            "4.0.0"
# => nnet       "yes"            "4.0.0"
# => parallel   "yes"            "4.0.0"
# => rpart      "yes"            "4.0.0"
# => spatial    "yes"            "4.0.0"
# => splines    "yes"            "4.0.0"
# => stats      "yes"            "4.0.0"
# => stats4     NA               "4.0.0"
# => survival   "yes"            "4.0.0"
# => tcltk      "yes"            "4.0.0"
# => tools      "yes"            "4.0.0"
# => utils      "yes"            "4.0.0"
```

Setting up renv

```r
> renv::init()
#
# Welcome to renv! It looks like this is your first time using renv.
# This is a one-time message, briefly describing some of renv's functionality.
#
# This package maintains a local cache of data on the filesystem at:
#
#   - '~/Library/Application Support/renv'
#
# This path can be customized -- see the documentation in `?paths`.
#
# renv will also write to files within the active project folder, including:
#
#   - A folder 'renv' in the project directory, and
#   - A lockfile called 'renv.lock' in the project directory.
#
# In particular, projects using renv will normally use a private, per-project
# R library, in which new packages will be installed. This project library is
# isolated from other R libraries on your system.
#
# In addition, renv will attempt to update files within your project, including:
#
#   - .gitignore
#   - .Rbuildignore
#   - .Rprofile
#
# Please read the introduction vignette with `vignette("renv")` for more information.
# You can also browse the package documentation online at http://rstudio.github.io/renv.
#
# By providing consent, you will allow renv to write and update these files.
```
