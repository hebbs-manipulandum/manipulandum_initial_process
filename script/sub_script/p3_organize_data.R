# This script organizes data.
# Make sure to run the p2_clean_data before you run this (or have the environment ready by loading it) 
#
##### Edit log #####
# 2020/12/24
# Following edits are made.
# 1) data_kin_endpt is processed from data_kin_filt, meaning that it uses data that are not temporally filtered. old data are still calculated for comparison and testing
# 2) points and directions at 100ms, 150ms, 200ms are calculated
#
# Author: Taisei Sugiyama

## Load packages
library(dplyr)
library(iemisc)

## Organize hand and error data 
# Get kinematic data for end-point (i.e., Just when the hand is crossing)
data_kin_endpt_old = data_kin_align %>%
  select(total_tri,blk_tri,blk,state,x_mm_adj,y_mm_adj,dist) %>%
  group_by(total_tri) %>%
  mutate(tstep = row_number()) %>% 
  dplyr::filter(dist >= data_para[[1,"trad"]]*1000) %>% # Find endpoint
  slice(1) %>% # Get the first data point
  ungroup() %>%
  mutate(hand = atan2d(y = .$y_mm_adj, x = .$x_mm_adj)) # calculate the angle from xy

data_kin_endpt = data_kin_filt %>%
  select(total_tri,blk_tri,blk,state,x_mm_adj,y_mm_adj,dist) %>%
  group_by(total_tri) %>%
  mutate(tstep = row_number()) %>% 
  dplyr::filter(dist >= data_para[[1,"trad"]]*1000) %>% # Find endpoint
  slice(1) %>% # Get the first data point
  ungroup() %>%
  mutate(hand = atan2d(y = .$y_mm_adj, x = .$x_mm_adj)) # calculate the angle from xy

data_endpt_comp <- dplyr::select(data_kin_endpt, total_tri, hand) %>% 
  dplyr::rename(hand_new = hand) %>% 
  left_join(dplyr::select(data_kin_endpt_old, total_tri, hand), by ="total_tri")

# Organize trial data frame
data_tri_clean = data_tri %>%
  mutate(tgt = data_tgt$tgt[total_tri]) %>%
  left_join(data_kin_endpt, by = c("total_tri", "blk_tri", "blk")) %>%
  mutate(cur = hand + data_tgt$shift[total_tri]) %>%
  mutate(error_hand = tgt - hand) %>%
  mutate(error_cur = tgt - cur) %>%
  select(total_tri,blk_tri,blk,tgt,hand,cur,error_hand,error_cur,gain,mt,rt,rc_xpass,rc_ypass)

# Create another dataframe for comparing time at different points

data_pts_raw <- data_kin_align %>% 
  select(total_tri,blk_tri,blk,x_mm_adj,y_mm_adj,dist) %>% 
  group_by(total_tri) %>% 
  mutate(tstep = row_number()) %>% 
  ungroup()



data_pt_100ms <- data_pts_raw %>% 
  dplyr::filter(tstep == 100) %>% 
  mutate(type = "100") 

data_pt_150ms <- data_pts_raw %>% 
  dplyr::filter(tstep == 150) %>% 
  mutate(type = "150") 

data_pt_200ms <- data_pts_raw %>% 
  dplyr::filter(tstep == 200) %>% 
  mutate(type = "200") 

data_pt_250ms <- data_pts_raw %>% 
  dplyr::filter(tstep == 250) %>% 
  mutate(type = "250") 

data_pts <- rbind(data_pt_100ms,data_pt_150ms,data_pt_200ms,data_pt_250ms) %>% 
  dplyr::select(-tstep) %>%
  mutate(hand = atan2d(y = .$y_mm_adj, x = .$x_mm_adj)) %>%  # calculate the angle from xy
  rbind(dplyr::select(data_kin_endpt, total_tri,blk_tri,blk,x_mm_adj,y_mm_adj,dist,hand) %>% mutate(type = "end")) %>% 
  mutate(type = factor(type, levels =c("100","150","200","250","end")))


