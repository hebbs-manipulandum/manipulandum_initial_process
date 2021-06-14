tmp_fn <- list.files("data/rawdata/")

for (unique_sub_tag in tmp_fn)
  source("script/process_rawdata_script.R")

# 
# tmp <- output_list$point
# tmp2 <- dplyr::filter(tmp, row_number() <=10)
# 
# tmp3 <- dplyr::filter(tmp, row_number() >= 71)
# mean(tmp3$error_deg, na.rm=T)
# sd(tmp3$error_deg, na.rm=T)
