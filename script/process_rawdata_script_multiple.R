# This processes raw output data file from experiment script
#
# # How to use
# 1) Put your folder with raw text data file saved from a manipulandum script into "data/rawdata"
# 2) Set key parameters. The minimum edits to make are change your subject identifier (sub_identifier & sub_id, or unique_sub_tag).
# 3) Run this script. It make take a while if you have a large number of trials
# 4) Processed files (middata) are output in "data/processed". You can also check basic data plots (e.g., trajectory, trial-by-trial movement time) unless you turn them off
#
# Author: Taisei Sugiyama

#### Set key parameters ####
sub_identifier <- "S" # subject identifier (tagging) 
sub_id <- 1 # subject id
add_zero <- F # add "0" to subject ID with a single digit. May be used for old data set where initial subject ids were "S01", "S02", and so on
# unique_sub_tag <- "sample_data" # you can set your own specific subject id/tag in string. Set NA if you don't need this feature
rawdata_dir <- "data/rawdata" # the directory where raw data folders (directories) are stored
save_main_dir <- "data"
save_sub_dir <- "processed"
fname_col_list <- "script/miscellaneous/rawdata_col_list_default.R"

# unique_sub_tag <- "S41" # you can set your own specific subject id/tag in string. Set NA if you don't need this feature

plot_kinematics <- T # whether you want to output plots for basic kinematics. 
plot_points <- T # boolean. whether you want to output plots for basic trial-by-trial point data (e.g., mt, rt, error). 
output_rdata <- T # boolean. whether you want output formatted as rdata
output_csv <- F # boolean.  whether you want output formatted as csv

plot_bias <- F

align_window <- 500 # In how long (ms) do you want kinematic data after it's aligned to movement initiation. This automatically considers downsampling, so set as time, not # of data points. 
align_time_back <- 100 # How long (ms) do you want to include kinematic data before movement initiation. Note that this "pre-movement" period is included in align_window. 

#### Preparation ####
## Load packages and subscripts
library(dplyr)
library(purrr)
library(signal)
# library(iemisc)
library(ggplot2)
library(ggforce)
library(gridExtra)
source("script/sub_script/process_kin.R")
source("script/miscellaneous/param_sg_filt.R")
source("script/miscellaneous/state_list.R")

options(dplyr.summarise.inform = FALSE) # suppress annoying summary message from dplyr

## miscellaneous processing
if (!is.na(unique_sub_tag)){
  tgt_dir <- unique_sub_tag
} else {
  if (add_zero & sub_id < 10){
    tgt_dir <- sprintf("%s0%d",sub_identifier,sub_id)
  } else {
    tgt_dir <- sprintf("%s%d",sub_identifier,sub_id)
  }
}


tmp_date <- list.files(sprintf("%s/%s",rawdata_dir,tgt_dir)) # date of experiment used as directory name

runs <- list.files(sprintf("%s/%s/%s/",rawdata_dir,tgt_dir,tmp_date[1]))

for (run in runs){ 
  
  fpath <- sprintf("%s/%s/%s/%s",rawdata_dir,tgt_dir,tmp_date[1],run) # File path
  fname_all <- list.files(path = fpath, full.names = T) # all filenames in the directory
  
  # stop if no data are found in the specified directory
  if (length(fname_all) ==0)
    stop("No file is found in the directory you specified. Make sure that you have put data folder specified correct file path and name.")
  
  #### Read and organize data ### 
  # This reads data files and organize them into 4 different dataframes
  source("script/sub_script/read_files.R")
  # point_raw: "point" trial data (e.g., movement time, hand error)
  # kin_raw: kinematic data (Downsampling is applied if you set so in options, even though it's tagged "raw")
  # tgt_raw: target sequence file data 
  # param_raw: block-wide parameters(e.g., sequence file name used, target distance)
  
  # data alignment
  # This has been changed from old scripts where movement initiation is defined as 10% of peak velocity.
  # Currently, it uses the event defined in an experiment script, such as distance or target velocity.
  # kin_align <- dplyr::filter(kin_raw, lead(state, (align_time_back%/%reduce_hz_rate))>= state_moving) %>% 
  #   group_by(blk_tri) %>% 
  #   dplyr::filter(row_number() <= ((align_window/reduce_hz_rate)+1)) %>% 
  #   mutate(tstep_align = tstep - tstep[1]+1) %>% # add time step with respect to movement initiation
  #   ungroup()
  
  kin_align <- kin_raw %>%
    group_by(blk_tri) %>%
    mutate(start_t = time_t[which(state == state_moving)[1]]) %>% 
    dplyr::filter(time_t >= (start_t-align_time_back/1000), time_t <= (start_t+align_window/1000)) %>% 
    mutate(tstep_align = tstep - tstep[1]+1) %>% # add time step with respect to movement initiation
    ungroup()
  
  
  
  
  #### Plotting ####
  if (plot_kinematics){
    source("script/sub_script/plot_pos.R")
    source("script/sub_script/plot_vel.R")
  }
  
  
  
  if (plot_points){
    # some editing
    point_edit <- point_raw %>% 
      mutate(hand_error = -error_deg) %>% # original value is (hand - tgt), so flip this
      dplyr::select(blk_tri,cross_deg_rbt, hand_error, mt, rt, score, fail)
    
    source("script/sub_script/plot_mtrt.R")
    source("script/sub_script/plot_err.R")
    source("script/sub_script/plot_retry.R")
  }
  
  
  # if (plot_bias){
  #   source("script/sub_script/plot_pos_bias.R")
  #   source("script/sub_script/plot_err_bias.R")
  #   
  #   
  # }
  
  #### Saving ####
  save_dir <-  sprintf("%s/%s",save_main_dir,save_sub_dir)
  
  # Create folders if not exist
  dir.create(file.path(save_main_dir, save_sub_dir), showWarnings = FALSE)
  dir.create(file.path(save_dir, sprintf("%s_%s",tgt_dir, run)), showWarnings = FALSE)
  
  fpath = sprintf("%s/%s_%s",save_dir,tgt_dir,run)
  
  # put everything in list and save as Rdata
  
  if (output_rdata){
    output_list <- list(point = point_raw, tgt = tgt_raw, kin = kin_raw, param = param_raw)
    save(output_list, file=sprintf("%s/exp_data.RData",fpath))
  }
  
  # save as csv
  if (output_csv){
    write.csv(point_raw, file=sprintf("%s/point_data.csv",fpath), row.names = FALSE)
    write.csv(tgt_raw, file=sprintf("%s/tgt_data.csv",fpath), row.names = FALSE)
    write.csv(kin_raw, file=sprintf("%s/kin_data.csv",fpath), row.names = FALSE)
    write.csv(kin_align, file=sprintf("%s/kin_data_align.csv",fpath), row.names = FALSE)
    write.csv(param_raw, file=sprintf("%s/param_data.csv",fpath), row.names = FALSE)
  }
  
  
  
}
