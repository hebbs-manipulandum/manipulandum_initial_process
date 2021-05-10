# This processes calibration data to calculate calibration matrix
# Suppose Xt be task-space coordinate points and Xv be visual coordinate points, we want calibration matrix A where
# Xr*A = Xv
# Hence,
# A = Xr+*Xv where Xr+ is pseudo-inverse of Xr
# We consider both interaction and bias term. Hence, Xr is an nx3 matrix whose last column is just 1, and A is a 3x2 matrix: [a11, a12; a21, a22; b1, b2]
#
# # How to use
# 1) Put your folder with raw text data file saved from a manipulandum script into "data/rawdata"
# 2) Set key parameters. (File name and location)
# 3) Run this script.
# 4) Processed files (calculated matrix) are output in "data/processed".
#
# Author: Taisei Sugiyama

#### Set key parameters ####
# sub_identifier <- "S" # subject identifier (tagging) 
# sub_id <- 1 # subject id
# add_zero <- F # add "0" to subject ID with a single digit. May be used for old data set where initial subject ids were "S01", "S02", and so on
unique_sub_tag <- "calib_20210428" # you can set your own specific subject id/tag in string. Set NA if you don't need this feature
rawdata_dir <- "data/rawdata" # the directory where raw data folders (directories) are stored
save_main_dir <- "data"
save_sub_dir <- "processed"

# fname_col_list <- "script/miscellaneous/rawdata_col_list_default.R"
# plot_kinematics <- T # whether you want to output plots for basic kinematics. 
# plot_points <- T # boolean. whether you want to output plots for basic trial-by-trial point data (e.g., mt, rt, error). 
# output_rdata <- T # boolean. whether you want output formatted as rdata
# output_csv <- T # boolean.  whether you want output formatted as csv
# 
# align_window <- 500 # In how long (ms) do you want kinematic data after it's aligned to movement initiation. This automatically considers downsampling, so set as time, not # of data points. 
# align_time_back <- 100 # How long (ms) do you want to include kinematic data before movement initiation. Note that this "pre-movement" period is included in align_window. 
# 
# #### Preparation ####
# ## Load packages and subscripts
library(dplyr)
library(pracma)

tgt_dir <- unique_sub_tag

fpath <- sprintf("%s/%s",rawdata_dir,tgt_dir) # File path
fname_all <- list.files(path = fpath, full.names = T) # all filenames in the directory. There should be only one file

# # stop if no data are found in the specified directory
if (length(fname_all) ==0)
  stop("No file is found in the directory you specified. Make sure that you have put data folder specified correct file path and name.")


# #### Read and organize data ### 
calib_data_raw <- data.frame(read.table(fname_all, header = F, row.names = NULL)) 

colnames(calib_data_raw) <- c("vis_x","vis_y","task_x","task_y")

vis_data <- data.matrix(subset(calib_data_raw, select = c("vis_x","vis_y"))) 
task_data <- data.matrix(subset(calib_data_raw, select = c("task_x","task_y"))) %>% 
  cbind(1)

calib_matrix <- pinv(task_data)%*%vis_data

# #### Saving ####
save_dir <-  sprintf("%s/%s",save_main_dir,save_sub_dir)
# 
# # Create folders if not exist
dir.create(file.path(save_main_dir, save_sub_dir), showWarnings = FALSE)
dir.create(file.path(save_dir, tgt_dir), showWarnings = FALSE)
# 
fpath = sprintf("%s/%s",save_dir,tgt_dir)
write.csv(calib_matrix, file=sprintf("%s/calib_matrix.csv",fpath), row.names = FALSE)
