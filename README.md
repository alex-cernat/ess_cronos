# Comparing face to face and web answers in the ESS CRONOS project

This is the code used for the analysis done in the peer-reviewed paper:

Alexandru Cernat, Melanie Revilla, Moving from Face-to-Face to a Web Panel: Impacts on Measurement Quality, *Journal of Survey Statistics and Methodology*, Volume 9, Issue 4, September 2021, Pages 745–763, https://doi.org/10.1093/jssam/smaa007


The analysis is based on the European Social Survey and they were downloaded from (https://www.europeansocialsurvey.org/)[https://www.europeansocialsurvey.org/]. The names and versions of the data used are:

- CRONOS Administrative data, edition 1.1 (published 15.03.19)
- CRONOS Integrated data, edition 1.0 (published 16.05.18)
- CRONOS Paradata, edition 1.0 (published 16.05.18)
- ESS8 test data from main questionnaire, edition 1.0 (published 30.05.18)


The "Master.R" code does the data cleaning, analysis, exporting of the data to Mplus and importing the results. The "output" folder includes the main findings from the analysis. 

For the SEM analysis we use Mplus 8.3 while the R version used was 3.6.0 and the packages were:

```r
sessionInfo()
```
```
R version 3.6.0 (2019-04-26)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19042)

Matrix products: default

locale:
[1] LC_COLLATE=English_United Kingdom.1252 
[2] LC_CTYPE=English_United Kingdom.1252   
[3] LC_MONETARY=English_United Kingdom.1252
[4] LC_NUMERIC=C                           
[5] LC_TIME=English_United Kingdom.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] semTools_0.5-1        lavaan_0.6-3          MplusAutomation_0.7-3
 [4] reshape2_1.4.3        haven_2.1.0           ggthemes_4.1.1       
 [7] forcats_0.4.0         stringr_1.4.0         dplyr_0.8.0.1        
[10] purrr_0.3.2           readr_1.3.1           tidyr_0.8.3          
[13] tibble_2.1.1          ggplot2_3.1.1         tidyverse_1.2.1      

loaded via a namespace (and not attached):
 [1] tidyselect_0.2.5  pander_0.6.3      lattice_0.20-38   colorspace_1.4-1 
 [5] generics_0.0.2    stats4_3.6.0      rlang_0.3.4       pillar_1.3.1     
 [9] glue_1.3.1        withr_2.1.2       gsubfn_0.7        modelr_0.1.4     
[13] readxl_1.3.1      plyr_1.8.4        munsell_0.5.0     gtable_0.3.0     
[17] cellranger_1.1.0  rvest_0.3.3       coda_0.19-2       parallel_3.6.0   
[21] broom_0.5.2       proto_1.0.0       Rcpp_1.0.1        xtable_1.8-4     
[25] scales_1.0.0      backports_1.1.4   jsonlite_1.6      texreg_1.36.23   
[29] mnormt_1.5-5      hms_0.4.2         packrat_0.5.0     digest_0.6.18    
[33] stringi_1.4.3     grid_3.6.0        cli_1.1.0         tools_3.6.0      
[37] magrittr_1.5      lazyeval_0.2.2    pbivnorm_0.6.0    crayon_1.3.4     
[41] pkgconfig_2.0.2   data.table_1.12.2 xml2_1.2.0        lubridate_1.7.4  
[45] assertthat_0.2.1  httr_1.4.0        rstudioapi_0.10   R6_2.4.0         
[49] boot_1.3-22       nlme_3.1-139      compiler_3.6.0   
```
