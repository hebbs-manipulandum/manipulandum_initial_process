# This processes raw output data file from experiment script
#
# Author: Taisei Sugiyama

## Load packages and subscripts
library(dplyr)
library(purrr)
library(signal)
library(iemisc)
library(ggplot2)
library(ggforce)
library(gridExtra)
source("script/sub_script/process_kin.R")

sub_identifier <- "S" # subject identifier (tagging) 
sub_id <- 1 # subject id
add_zero <- F # add "0" to subject ID with a single digit. May be used for old data set where initial subject ids were "S01", "S02", and so on
# unique_sub_tag <- "test_pretrain_ts" # you can set your own specific subject id/tag in string. Set NA if you don't need this feature
unique_sub_tag <- "sample_data" # you can set your own specific subject id/tag in string. Set NA if you don't need this feature
rawdata_dir <- "data/rawdata" # the directory where raw data folders (directories) are stored

save_main_dir <- "data"
save_sub_dir <- "processed"

kin_col <- c("state","x","y","vx","vy","t_a","t_b","fs_x","fs_y","fx","fy") # columns of kinematic data 
point_col <- c("cross_x_rbt","cross_y_rbt","cross_deg_rbt","peak_vel","mt","rt","score","retry","error_deg") # columns of point data
tgt_col <- c("field","apply_field","trad","tgt","wait_time", "bval", " chan_k11", "chan_b11", "spring_gain","rot","show_arc",
             "show_cur","show_score","train_type","min_score","max_score","difficulty")

plot_kinematics <- T # whether you want to output plots for basic kinematics. 
plot_points <- T # whether you want to output plots for basic trial-by-trial point data (e.g., mt, rt, error). 


#### Preparation ####

if (!is.na(unique_sub_tag)){
  tgt_dir <- unique_sub_tag
} else {
  if (add_zero & sub_id < 10){
    tgt_dir <- sprintf("%s0%d",sub_identifier,sub_id)
  } else {
    tgt_dir <- sprintf("%s%d",sub_identifier,sub_id)
  }
}

fpath <- sprintf("%s/%s",rawdata_dir,tgt_dir) # File path
fname_all <- list.files(path = fpath, full.names = T) # all filenames in the directory

# stop if no data are found in the specified directory
if (length(fname_all) ==0)
  stop("No file is found in the directory you specified. Make sure that you have put data folder specified correct file path and name.")

#### Read and organize data ### 
# This reads data files and organize them into 4 different dataframes
source("script/sub_script/read_files.R")
# point_raw: "point" trial data (e.g., movement time, hand error)
# kin_raw: kinematic data
# tgt_raw: target sequence file data 
# param_raw: block-wide parameters(e.g., sequence file name used, target distance)

#### Plotting ####
if (plot_kinematics){
  source("script/sub_script/plot_pos.R")
  source("script/sub_script/plot_vel.R")
}

if (plot_points){
  # some editing
  point_edit <- point_raw %>% 
    mutate(hand_error = -error_deg) %>% # original value is (hand - tgt), so flip this
    dplyr::select(blk_tri,cross_deg_rbt, hand_error, mt, rt, peak_vel, score, retry)
  
  source("script/sub_script/plot_mtrt.R")
  source("script/sub_script/plot_err.R")
  source("script/sub_script/plot_retry.R")
}

#### Saving ####
save_dir <-  sprintf("%s/%s",save_main_dir,save_sub_dir)

# Create folders if not exist
dir.create(file.path(save_main_dir, save_sub_dir), showWarnings = FALSE)
dir.create(file.path(save_dir, tgt_dir), showWarnings = FALSE)

fpath = sprintf("%s/%s",save_dir,tgt_dir)

# put everything in list and save as Rdata
output_list <- list(point = point_raw, tgt = tgt_raw, kin = kin_raw, param = param_raw)
save(output_list, file=sprintf("%s/exp_data.RData",fpath))

# save as csv
write.csv(point_raw, file=sprintf("%s/point_data.csv",fpath), row.names = FALSE)
write.csv(tgt_raw, file=sprintf("%s/tgt_data.csv",fpath), row.names = FALSE)
write.csv(kin_raw, file=sprintf("%s/kin_data.csv",fpath), row.names = FALSE)
write.csv(param_raw, file=sprintf("%s/param_data.csv",fpath), row.names = FALSE)
