README
======

This directory contains the files necessary to reproduce the analyses for the 
article:

Montero-Melis, G., Eisenbeiss, S., Narasimhan, B., Ibarretxe-Antuñano, I., Kita,
S., Kopecka, A., Lüpke, F., Nikitina, T., Tragel, I., Jaeger, T.F., &
Bohnemeyer, J. (accepted). Talmy’s framing typology underpredicts nonverbal 
motion categorization: Insights from a large language sample and simulations. 
Cognitive Semantics.


The directory consists of data files and R scripts necessary to replicate
the analyses reported in the paper (to be uploaded to Dataverse.org).


Content
-------

### `data_tirads.csv`

Data set used for analyses. The columns/variables mean:

- `LanguageType`
- `Language`
- `Participant`
- `ListUnique`
- `ListOrder`
- `TargetTrial`
- `Item`
- `ItemWithinScene`
- `ItemScene`
- `MannerVariant`
- `SameMannerResponse`
- `FirstTrial.SameMannerResponse`





Last tested and system specifications
-------------------------------------

Below details on when the R scripts were last tested by the first author (with
success) and the corresponding session info:


-----------------------------------------------------------
"analysis.R"
24 Nov 2016

> sessionInfo()
R version 3.1.2 (2014-10-31)
Platform: i386-w64-mingw32/i386 (32-bit)

locale:
[1] LC_COLLATE=Swedish_Sweden.1252  LC_CTYPE=Swedish_Sweden.1252    LC_MONETARY=Swedish_Sweden.1252 LC_NUMERIC=C                   
[5] LC_TIME=Swedish_Sweden.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] ggplot2_2.1.0 dplyr_0.4.3   lme4_1.1-8    Matrix_1.1-4 

loaded via a namespace (and not attached):
 [1] acepack_1.3-3.3     assertthat_0.1      cluster_1.15.3      colorspace_1.2-4    DBI_0.3.1           digest_0.6.9        foreign_0.8-61     
 [8] Formula_1.1-2       grid_3.1.2          gtable_0.1.2        Hmisc_3.14-6        labeling_0.3        lattice_0.20-29     latticeExtra_0.6-26
[15] lazyeval_0.1.10     magrittr_1.5        MASS_7.3-35         minqa_1.2.4         munsell_0.4.2       nlme_3.1-118        nloptr_1.0.4       
[22] nnet_7.3-8          parallel_3.1.2      plyr_1.8.3          R6_2.1.1            RColorBrewer_1.1-2  Rcpp_0.11.3         rpart_4.1-8        
[29] scales_0.4.0        splines_3.1.2       survival_2.37-7     tools_3.1.2  
-----------------------------------------------------------


-----------------------------------------------------------
"scenarios_intro.R"
24 Nov 2016

> sessionInfo()
R version 3.1.2 (2014-10-31)
Platform: i386-w64-mingw32/i386 (32-bit)

locale:
[1] LC_COLLATE=Swedish_Sweden.1252  LC_CTYPE=Swedish_Sweden.1252    LC_MONETARY=Swedish_Sweden.1252 LC_NUMERIC=C                   
[5] LC_TIME=Swedish_Sweden.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] mvtnorm_1.0-2 boot_1.3-13   dplyr_0.4.3   lme4_1.1-8    Matrix_1.1-4  ggplot2_2.1.0

loaded via a namespace (and not attached):
 [1] acepack_1.3-3.3     assertthat_0.1      cluster_1.15.3      colorspace_1.2-4    DBI_0.3.1           digest_0.6.9        foreign_0.8-61     
 [8] Formula_1.1-2       grid_3.1.2          gtable_0.1.2        Hmisc_3.14-6        labeling_0.3        lattice_0.20-29     latticeExtra_0.6-26
[15] lazyeval_0.1.10     magrittr_1.5        MASS_7.3-35         minqa_1.2.4         munsell_0.4.2       nlme_3.1-118        nloptr_1.0.4       
[22] nnet_7.3-8          parallel_3.1.2      plyr_1.8.3          R6_2.1.1            RColorBrewer_1.1-2  Rcpp_0.11.3         rpart_4.1-8        
[29] scales_0.4.0        splines_3.1.2       survival_2.37-7     tools_3.1.2        
-----------------------------------------------------------



-----------------------------------------------------------
"simulations.R"
24 Nov 2016

> sessionInfo()
R version 3.1.2 (2014-10-31)
Platform: i386-w64-mingw32/i386 (32-bit)

locale:
[1] LC_COLLATE=Swedish_Sweden.1252  LC_CTYPE=Swedish_Sweden.1252    LC_MONETARY=Swedish_Sweden.1252
[4] LC_NUMERIC=C                    LC_TIME=Swedish_Sweden.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] boot_1.3-13   purrr_0.2.1   mvtnorm_1.0-2 dplyr_0.4.3   lme4_1.1-8    Matrix_1.1-4  ggplot2_2.1.0

loaded via a namespace (and not attached):
 [1] assertthat_0.1   colorspace_1.2-4 DBI_0.3.1        digest_0.6.9     grid_3.1.2       gtable_0.1.2     labeling_0.3    
 [8] lattice_0.20-29  lazyeval_0.1.10  magrittr_1.5     MASS_7.3-35      minqa_1.2.4      munsell_0.4.2    nlme_3.1-118    
[15] nloptr_1.0.4     parallel_3.1.2   plyr_1.8.3       R6_2.1.1         Rcpp_0.11.3      reshape2_1.4.1   scales_0.4.0    
[22] splines_3.1.2    stringr_0.6.2    tools_3.1.2 
-----------------------------------------------------------
