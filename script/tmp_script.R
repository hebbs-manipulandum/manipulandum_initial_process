tmp_fn <- list.files("data/rawdata/")

## This experiment project has 3 different versions of data because the operation system for the manipulandum has changed in the midway
## Specifically, the version 2 gives much more down-sampled data than the version 1 due to a switch from LabView to C++
## Then, time stamp was calculated in python in version 2 but changed to C++ timestamp in version 3.

# use_max_dt <-  T # set this to false up to n534 or n415 (n416)

for (unique_sub_tag in tmp_fn) {
  
  tmp_id <- as.numeric(gsub("\\D", "", unique_sub_tag))
  
  if ( (tmp_id > 500 & tmp_id <= 534) | (tmp_id < 500 & tmp_id <= 416) ) {
    data_version <- 1
  } else if ( (tmp_id >= 535) | (tmp_id < 500 & tmp_id >= 417) ) { # THIS NEED TO BE CHECKED
    data_version <- 2
  } else {
    data_version <- 3
  }
  
  source("script/process_rawdata_script_multiple.R")
  
}
  
  

# 
# tmp <- output_list$point
# tmp2 <- dplyr::filter(tmp, row_number() <=10)
# 
# tmp3 <- dplyr::filter(tmp, row_number() >= 71)
# mean(tmp3$error_deg, na.rm=T)
# sd(tmp3$error_deg, na.rm=T)
