## This script cleans and organizes "raw" data.
## Make sure to run the p1_load_rawdata before you run this (or have the environment ready by loading it) 
## Author: Taisei Sugiyama

## Load packages
library(dplyr)
library(iemisc)
library(signal)

## Organize data. Do some conversions and adjustments
data_kin_adj = data_kin %>%
  mutate(x_mm = x*1000, y_mm = y*1000, dx_mm =  dx*1000, dy_mm = dy*1000) %>% # Convert from m to mm
  mutate(x_mm_adj = x_mm + data_para$xoffset[1] - data_para$cx[1], # Adjust according to the start position (x)
         y_mm_adj = y_mm + data_para$yoffset[1] - data_para$cy[1],  # Adjust according to the start position (y)
         speed_rbt = sqrt(dx_mm^2 + dy_mm^2),  # Speed measured by the robot
         dist = sqrt(x_mm_adj^2 + y_mm_adj^2)) %>%   # Distance from the start
  mutate(x_mm_adj_vis = x_mm_adj*cosd(data_tgt$shift[total_tri]/1000) - y_mm_adj*sind(data_tgt$shift[total_tri]/1000), # visual feedback (apply rotation)
         y_mm_adj_vis = x_mm_adj*sind(data_tgt$shift[total_tri]/1000) + y_mm_adj*cosd(data_tgt$shift[total_tri]/1000)) %>% # visual feedback (apply rotation)
  group_by(total_tri) %>% 
  mutate(time_pt = row_number()) %>% 
  ungroup()

## Align target direction and apply Savitzky-Golay filtering

# Set param
order = 3
framelen = 11
ref_rate = 1000 # refresh rate of the system (frequency, Hz) 
speed_thresh = 1000 # velocity threshold (mm/s). Any value above this is considered unsuccessful filtering 

# Aligning and Filtering
data_kin_filt <- data_kin_adj %>%
  mutate(x_mm_adj_tgt = x_mm_adj*cosd(90-data_tgt$tgt[total_tri])-y_mm_adj*sind(90-data_tgt$tgt[total_tri]),  # align the target to 90 degree (up straight)
         y_mm_adj_tgt = x_mm_adj*sind(90-data_tgt$tgt[total_tri])+y_mm_adj*cosd(90-data_tgt$tgt[total_tri])) %>% # align the target to 90 degree (up straight)
  mutate(dx_sgolay_temp = sgolayfilt(c(diff(x_mm_adj_tgt,1),0), p = order, n = framelen)*1000, # sg filter on velocity
         dy_sgolay_temp = sgolayfilt(c(diff(y_mm_adj_tgt,1),0), p = order, n = framelen)*1000) %>% # sg filter on velocity
  mutate(temp_last = c(diff(total_tri,1),0)) %>% # Temporary indexing to flag the last data point for each trial
  mutate(dx_sgolay = ifelse(temp_last==1,speed_rbt,
                            ifelse(abs(dx_sgolay_temp) > speed_thresh,speed_rbt,dx_sgolay_temp))) %>% # trim "invalid" filtered value
  mutate(dy_sgolay = ifelse(temp_last==1,speed_rbt,
                            ifelse(abs(dy_sgolay_temp) > speed_thresh,speed_rbt,dy_sgolay_temp))) %>% # trim "invalid" filtered value
  mutate(speed_sgolay = sqrt(dx_sgolay^2 + dy_sgolay^2)) %>%
  mutate(fx_adj_tgt = fx*cosd(90-data_tgt$tgt[total_tri])-fy*sind(90-data_tgt$tgt[total_tri]),  # align the target to 90 degree (up straight)
         fy_adj_tgt = fx*sind(90-data_tgt$tgt[total_tri])+fy*cosd(90-data_tgt$tgt[total_tri])) %>% # align the target to 90 degree (up straight)
  mutate(fx_sgolay = sgolayfilt(fx_adj_tgt, p = order, n = framelen),  # sg filter on force
         fy_sgolay = sgolayfilt(fy_adj_tgt, p = order, n = framelen)) # %>% # sg filter on force
  

# Get max speed for each trial
data_kin_sum = data_kin_filt %>%
  group_by(total_tri) %>%
  summarise(max_speed = max(speed_sgolay)) %>%
  ungroup()

# data_tri = mutate(data_tri, max_speed = data_kin_sum$max_speed)

## Alignment with respect to movement initiation
move_thresh = 0.1 # "threshold" (ratio to max speed) to identify movement
num_step_back = 50 # From the 10% max speed (movement initiation), step back by this number to align 
align_size = 500 # alignment window size

data_kin_align = data_kin_filt %>%
  mutate(max_speed = data_kin_sum$max_speed[total_tri]) %>%
  group_by(total_tri) %>% # Process trial-by-trial
  mutate(moving = ifelse(row_number() < num_step_back, 0,
                         ifelse(speed_sgolay >= data_kin_sum$max_speed[total_tri]*move_thresh,1,0))) %>% # Detect movement
  mutate(include = ifelse(row_number() < match(1,moving) - num_step_back, 0, 
                          ifelse(row_number()<=match(1,moving)+align_size- num_step_back-1, 1, 0))) %>% # Set a flag for filtering
  dplyr::filter(include == 1) %>% # Now filter it
  ungroup() # Check if this doesn't cause any bug 


data_kin_align_clean <- data_kin_align %>% 
  dplyr::select(total_tri, blk_tri, time_pt, state, x_mm_adj, y_mm_adj, x_mm_adj_tgt, y_mm_adj_tgt, 
                dx_mm, dy_mm, dx_sgolay, dy_sgolay, speed_rbt, speed_sgolay, 
                fx_adj_tgt, fy_adj_tgt, dist, max_speed, moving)



## Save the figure ## 

# Create a folder if not exist
# main_dir = "int"
# 
# if (rm_bias == 0){
#   sub_dir = "prb_grp_hand_halfsegavg_phase_noval"
# } else if (rm_bias == 1){
#   sub_dir = "prb_grp_hand_halfsegavg_phase_noval_nb"
# }
# main_dir_path = sprintf("figure/%s",main_dir)
# dir.create(file.path("figure", main_dir), showWarnings = FALSE)
# dir.create(file.path(main_dir_path, sub_dir), showWarnings = FALSE)
# 
# # Set filename
# fname_plot = sprintf("figure/%s/%s/%s_P%d_sh%d.pdf",main_dir,sub_dir,sub_dir,phoi,shoi)
# fname_readme = sprintf("figure/%s/%s/%s.txt",main_dir,sub_dir,sub_dir)
# # Save
# pdf(fname_plot)
# print(post_int_prb.plot)
# dev.off()
# 
# # Spit out readme 
# readme = file(fname_readme)
# writeLines(c("These are group average and se plots of Probe hand profile (two segment average)",
#              "Hands in positive rotation and following wo are flipped so that ideal hand is always plotted upward.",
#              "Also, this manipulation is necessary when averaging across sequence and/or block"),
#            readme)
# close(readme)
