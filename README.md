# Warming decreases trophic transfer efficiency in a long-term field experiment

[![license](https://img.shields.io/badge/license-MIT%20+%20file%20LICENSE-lightgrey.svg)](https://choosealicense.com/)
[![Ask Us Anything
\!](https://img.shields.io/badge/Ask%20us-anything-1abc9c.svg)](https://github.com/dbarneche/nature20200508666/issues/new)
![Open Source
Love](https://badges.frapsoft.com/os/v2/open-source.svg?v=103)

This repository contains code and data needed to reproduce the article:

**Barneche DR, Hulatt CJ, Dossena M, Padfield D, Woodward G, Trimmer M, Yvon-Durocher G**, Warming decreases trophic transfer efficiency in a long-term field experiment (in review)

## Instructions

All processing was done in `R`. This routine uses the [drake R package](https://github.com/ropensci/drake) to compile the output time table. First install `drake`:

```r
install.packages("drake")
```

Next you need to open an R session with working directory set to the root of the project.

This routine loads multiple packages which are found in `R/packages.R`, **so make sure to successfully install and load them before running drake** with the code below.

To reproduce particular targets outlined in `R/plan.R`, do e.g.:

```r
source("_drake.R")
drake::make(plan, targets = c("fig_1_pdf", "fig_2_pdf", "ed_table_2"), lock_envir = FALSE)
```

This will create Figures 1 and 2, and Extended Data Table 2 as presented in the manuscript along with all its dependencies. All output will be automatically placed in a directory called `output` (it is going to be automatically created for you).

The main Bayesian model needed to create Figures 1 and 2 is called `stan_output` in `R/plan.R`, and to access it within an R session, simply run:

```r
source("_drake.R")
drake::make(plan, targets = "stan_output", lock_envir = FALSE)
drake::loadd(stan_output)
```

This may take 20-30 min depending on your computing power. Alternatively, to reproduce all data analyses / figures / tables, and then make them available within an R session, do:

```r
source("make.R")
drake::loadd()
```

Again, all output will be automatically placed in a directory called `output` (it is going to be automatically created for you).

Also notice that all the combined Bayesian models in this paper may take a few hours to run on a regular computer.

### This paper was produced using the following software and associated packages:
```
R version 4.0.2 (2020-06-22)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: macOS Catalina 10.15.7

Matrix products: default
BLAS:   /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRblas.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib

locale:
[1] en_AU.UTF-8/en_AU.UTF-8/en_AU.UTF-8/C/en_AU.UTF-8/en_AU.UTF-8

attached base packages:
[1] grid      parallel  stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] ggdist_2.2.0         tidybayes_2.1.1      vegan_2.5-6          permute_0.9-5        cowplot_1.1.0        ggnewscale_0.4.3     png_0.1-7            ggpubr_0.4.0        
 [9] Hmisc_4.4-1          Formula_1.2-3        survival_3.1-12      lattice_0.20-41      gridExtra_2.3        LoLinR_0.0.0.9000    ggsci_2.9            bayesplot_1.7.2     
[17] rstan_2.21.3         ggplot2_3.3.2        StanHeaders_2.21.0-6 brms_2.13.5          Rcpp_1.0.5           MuMIn_1.43.17        gamm4_0.2-6          mgcv_1.8-31         
[25] nlme_3.1-148         lme4_1.1-23          Matrix_1.2-18        emmeans_1.5.0        HDInterval_0.2.2     tidyselect_1.1.0     scales_1.1.1         lubridate_1.7.9     
[33] tibble_3.0.3         tidyr_1.1.2          dplyr_1.0.2          plyr_1.8.6           knitr_1.29           kableExtra_1.2.1     tinytex_0.25         rmarkdown_2.3       
[41] drake_7.12.5        

loaded via a namespace (and not attached):
  [1] htmlwidgets_1.5.1    munsell_0.5.0        base64url_1.4        codetools_0.2-16     statmod_1.4.34       DT_0.15              miniUI_0.1.1.1       withr_2.3.0         
  [9] Brobdingnag_1.2-6    colorspace_1.4-1     filelock_1.0.2       rstudioapi_0.11      stats4_4.0.2         ggsignif_0.6.0       farver_2.0.3         bridgesampling_1.0-0
 [17] txtq_0.2.3           coda_0.19-3          vctrs_0.3.4          generics_0.0.2       xfun_0.17            R6_2.4.1             markdown_1.1         assertthat_0.2.1    
 [25] promises_1.1.1       nnet_7.3-14          gtable_0.3.0         processx_3.4.4       rlang_0.4.7          splines_4.0.2        rstatix_0.6.0        broom_0.7.0         
 [33] checkmate_2.0.0      inline_0.3.16        reshape2_1.4.4       abind_1.4-5          threejs_0.3.3        crosstalk_1.1.0.1    backports_1.1.10     httpuv_1.5.4        
 [41] rsconnect_0.8.16     tools_4.0.2          ellipsis_0.3.1       RColorBrewer_1.1-2   ggridges_0.5.2       base64enc_0.1-3      progress_1.2.2       purrr_0.3.4         
 [49] ps_1.3.4             prettyunits_1.1.1    rpart_4.1-15         zoo_1.8-8            haven_2.3.1          cluster_2.1.0        magrittr_1.5         data.table_1.13.1   
 [57] openxlsx_4.2.2       colourpicker_1.1.0   lmtest_0.9-38        mvtnorm_1.1-1        storr_1.2.1          matrixStats_0.56.0   hms_0.5.3            shinyjs_2.0.0       
 [65] mime_0.9             evaluate_0.14        arrayhelpers_1.1-0   xtable_1.8-4         shinystan_2.5.0      rio_0.5.16           jpeg_0.1-8.1         readxl_1.3.1        
 [73] rstantools_2.1.1     compiler_4.0.2       V8_3.2.0             crayon_1.3.4         minqa_1.2.4          htmltools_0.5.0      later_1.1.0.1        RcppParallel_5.0.2  
 [81] MASS_7.3-51.6        boot_1.3-25          car_3.0-9            cli_2.0.2            igraph_1.2.5         forcats_0.5.0        pkgconfig_2.0.3      foreign_0.8-80      
 [89] xml2_1.3.2           svUnit_1.0.3         dygraphs_1.1.1.6     webshot_0.5.2        estimability_1.3     rvest_0.3.6          stringr_1.4.0        distributional_0.2.0
 [97] callr_3.4.4          digest_0.6.25        cellranger_1.1.0     htmlTable_2.1.0      curl_4.3             shiny_1.5.0          gtools_3.8.2         nloptr_1.2.2.2      
[105] lifecycle_0.2.0      jsonlite_1.7.1       carData_3.0-4        viridisLite_0.3.0    fansi_0.4.1          pillar_1.4.6         loo_2.3.1            fastmap_1.0.1       
[113] httr_1.4.2           pkgbuild_1.1.0       glue_1.4.2           xts_0.12.1           zip_2.1.1            shinythemes_1.1.2    stringi_1.5.3        latticeExtra_0.6-29 
```

## License

This repository is provided by the authors under the MIT License ([MIT](http://opensource.org/licenses/MIT)).

### How to download this project for people not familiar with GitHub:  
* on the project main page on GitHub, click on the green button `clone or download` and then click on `Download ZIP`  

## Bug reporting
* Please [report any issues or bugs](https://github.com/dbarneche/nature20200508666/issues).
