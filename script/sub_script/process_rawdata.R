# This processes raw output data file from experiment script
#
# Author: Taisei Sugiyama


# source("unload_all_lib.R") # Clear all loaded library first to avoid bugs by masking functions

## Load packages
library(dplyr)
library(purrr)
library(signal)
library(iemisc)
library(ggplot2)
library(ggforce)
library(gridExtra)
source("script/process_rawdata/process_kin.R")

# library(stringr)


sub_identifier <- "S" # subject identifier (tagging) 
sub_id <- 1 # subject id
add_zero <- F # add "0" to subject ID with a single digit. May be used for old data set where initial subject ids were "S01", "S02", and so on
unique_sub_tag <- "test_pretrain_ts" # you can set your own specific subject id/tag in string. Set NA if you don't need this feature
# unique_sub_tag <- "test_pretrain_ts" # you can set your own specific subject id/tag in string. Set NA if you don't need this feature
rawdata_dir <- "data/rawdata" # the directory where raw data folders (directories) are stored

kin_col <- c("state","x","y","vx","vy","t_a","t_b","fs_x","fs_y","fx","fy") # columns of kinematic data 
point_col <- c("cross_x_rbt","cross_y_rbt","cross_deg_rbt","peak_vel","mt","rt","score","retry","error_deg") # columns of point data
tgt_col <- c("field","apply_field","trad","tgt","wait_time", "bval", " chan_k11", "chan_b11", "spring_gain","rot","show_arc",
             "show_cur","show_score","train_type","min_score","max_score","difficulty")


plot_kinematics <- T # whether you want to output plots for basic kinematics. 
plot_points <- T # whether you want to output plots for basic trial-by-trial point data (e.g., mt, rt, error). 
# SG filter parameter
# While not well cited, this gives some reference value for the parameters
# Crenna 2015, Filtering signals for movement analysis in biomechanics
# https://www.imeko.org/publications/wc-2015/IMEKO-WC-2015-TC18-350.pdf
order <- 4 # degree of polynomial
framelen <- 201 # window size
reduce_hz <- T # reduce sample frequency of kinematic data. Filtering is done BEFORE reduction, so keep this True unless you want "raw" data.
reduce_hz_rate <- 5 # factor for reduction of frequency.  
sample_rate <-  1000 # sampling rate in Hz. NOTE: Current script has close to but not exactly 1000 Hz sampling (it fluctuates). Change this number after fixing the samling rate problem somehow


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

# categorize filenames
fname_kin <- fname_all[grepl('kin', fname_all) & grepl('.txt', fname_all)] # kinematic data filenames
fname_point <- fname_all[grepl('point', fname_all) & grepl('.txt', fname_all)] # point (trial-by-trial) data filenames
fname_tgt <-  fname_all[grepl('data_load.txt', fname_all)]  # loaded target sequence data. pre-defined name
fname_param <-  fname_all[grepl('data_params.txt', fname_all)] # key block parameters. pre-defined name


## Now read data
tgt_raw <- data.frame(read.table(fname_tgt, header = T, row.names = NULL)) %>%  # target sequence
  mutate(blk_tri = row_number())

names(tgt_raw) <- c(tgt_col,"blk_tri")

param_raw <- data.frame(read.table(fname_param, header = T, row.names = NULL)) # block-wide parameters

# single-point data 
point_raw <- map(fname_point,function(fname){
  tri_filename <- gsub("^.*/","",fname) # remove file path
  tri_num <-   as.numeric(gsub("[^0-9]","",tri_filename))+1 # extract trial number (starting from 0, so add 1)
  return_df <- data.frame(read.table(fname, header = F, row.names = NULL)) %>% 
    mutate(blk_tri = tri_num) # get block trial number, which is in filename (starting from 0, so add 1)
}) %>% 
  reduce(rbind)

colnames(point_raw) <- c(point_col,"blk_tri")

point_edit <- point_raw %>% 
  mutate(hand_error = -error_deg) %>% # original value is (hand - tgt), so flip this
  dplyr::select(blk_tri,cross_deg_rbt, hand_error, mt, rt, peak_vel, score, retry)

# kinematic data
task_center_x <- param_raw$center_pos_x
task_center_y <- param_raw$center_pos_y

kin_raw <- map(fname_kin,function(fname){
  
  tri_filename <- gsub("^.*/","",fname) # remove file path
  tri_num <-   as.numeric(gsub("[^0-9]","",tri_filename))+1 # extract trial number (starting from 0, so add 1)
  return_df_raw <- data.frame(read.table(fname, header = F, row.names = NULL)) # %>% 
    
  colnames(return_df_raw) <- kin_col
  
  return_df_raw2 <- return_df_raw %>% 
    mutate(x = -(x - task_center_x), y = -(y - task_center_y)) # zero-ing with respect to the center of task space. Also, flip with respect to x & y axis, as each is defined opposite in labview (and so is in recorded data)
  
  return_df <- process_kin(return_df_raw2,reduce_hz,reduce_hz_rate) %>% 
    mutate(blk_tri = tri_num)
  
}) %>% 
  reduce(rbind)



if (plot_kinematics){
  source("script/process_rawdata/plot_pos.R")
  source("script/process_rawdata/plot_vel.R")
}

if (plot_points){
  source("script/process_rawdata/plot_mtrt.R")
  source("script/process_rawdata/plot_err.R")
}


# 
# tmp_plot_vx <- ggplot(tmp_data, aes(x = time, y = vx, color = tri)) +
# # tmp_plot_vx <- ggplot(tmp_data, aes(x = time, y = vx, color = tri, group = NA)) +
#   geom_path(size = 1) +
#   xlab("Time") +
#   ylab("X-velocity") +
#   xlim(0,1500) +
#   ylim(-1, 1)
# 
# # tmp_plot_vy <- ggplot(tmp_data, aes(x = time, y = vy, color = tri, group = NA)) +
# tmp_plot_vy <- ggplot(tmp_data, aes(x = time, y = vy, color = tri)) +
#   geom_path(size = 1) +
#   xlab("Time") +
#   ylab("Y-velocity") +
#   xlim(0,1500) +
#   ylim(-1, 1) # +
#   # scale_color_manual(values = c("red","green","blue"), name="State", labels = c("Go","Move","Feedback"))
# 
# tmp_plot_vx
# tmp_plot_vy
#   
# 
# 
# tmp_plot_pos <- ggplot(tmp_data, aes(x = x, y = y, color = tri)) +
#   # tmp_plot_vx <- ggplot(tmp_data, aes(x = time, y = vx, color = tri, group = NA)) +
#   geom_path(size = 1) +
#   xlab("xpos") +
#   ylab("ypos") +
#   xlim(0,.3) +
#   ylim(.3, .6)
# 
# 
# 
# 
# 
# # source("function/get_grp_soi.R")
# source("function/set_exp_param.R")
# # subs = get_grp_soi(all_subs,"rd")
# 
# subs <- 404:406
# 
# for (sub in subs){
# 
# sid_num <- sub
# 
# message(sprintf("Running S%d",sub))
# 
# #### Basic processing 
# # source("script/ind_process/p1_load_rawdata.R")
# # source("script/ind_process/p2_clean_data.R")
# # source("script/ind_process/p3_organize_data.R")
# # source("script/ind_process/p4_fitting_data_lsse.R")
# # # source("script/ind_process/p5_fitting_data_em.R")
# # source("script/ind_process/p6_save_ind_data.R")
# # source("script/ind_process/ex1_additional_kin.R")
# # source("script/ind_process/ex2_switch_point.R")
# 
# 
# }
# 




