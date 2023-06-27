# Set a function to perform SG filter on position data (smoothing and derivatives) to
# filter data as well as calculate velocity, acceleration, and jerk
# Modified function from Fujita
# Further modified for fMRI experiment (Use Pygame to record, so sampling is slower than the manipulandum)
#
# Author: Taisei Sugiyama
# Date created: 04/13/2021
# Date last modified: 06/26/2023

process_kin <- function(df, reduce_hz, reduce_hz_rate){

# pos_data_tri <- subset(pos_data, trial == tri) 

  
  
time_g <- df$time_global
time_t <- df$time_trial
x <- df$x # raw x position
y <- df$y # raw y position

## SG filtering (smoothing & derivatives).
# x
sx <- sgolayfilt(x, p = order, n = framelen, m = 0, ts = 1/sample_rate) # smooth
svx <- sgolayfilt(x, p = order, n = framelen, m = 1, ts = 1/sample_rate) # velocity
sax <- sgolayfilt(x, p = order, n = framelen, m = 2, ts = 1/sample_rate) # acceleration
sjx <- sgolayfilt(x, p = order, n = framelen, m = 3, ts = 1/sample_rate) # jerk

# y
sy <- sgolayfilt(y, p = order, n = framelen, m = 0, ts = 1/sample_rate) # smooth
svy <- sgolayfilt(y, p = order, n = framelen, m = 1, ts = 1/sample_rate) # velocity
say <- sgolayfilt(y, p = order, n = framelen, m = 2, ts = 1/sample_rate) # acceleration
sjy <- sgolayfilt(y, p = order, n = framelen, m = 3, ts = 1/sample_rate) # jerk

# vx <- df$vx # robot x-velocity
# vy <- df$vy # robot y-velocity
state <- df$state # trial state


if (reduce_hz) {
  
  # copy trigger info so that this can be passed even with down-sampling
  trigger <- select(df, trigger) %>% 
    rename(trigger_original = trigger) %>% 
    mutate(bin = ((row_number()-1)%/%reduce_hz_rate+1), tstep = row_number()) %>% 
    group_by(bin) %>% 
    summarise(trigger = sum(trigger_original, na.rm = T), tstep = tstep[1]) %>% 
    ungroup() %>%
    mutate(trigger = ifelse(trigger != 0, 1, 0)) %>% 
    select(-bin)
    
  return_df <- data.frame(tstep = 1:length(x), state, x, y, sx, sy, svx, svy, sax, say, sjx, sjy, time_g, time_t)  %>% 
    # dplyr::filter(tstep %% red_rate == 1) # reduce sample Hz
    dplyr::filter(tstep %% reduce_hz_rate == 1) %>% # reduce sample Hz
    left_join(trigger, by="tstep")
} else{
  return_df <- data.frame(tstep = 1:length(x), state, x, y, sx, sy, svx, svy, sax, say, sjx, sjy, time_g, time_t)
}



# ## Checking
# # trajectory
# 
# tmp_trajx <- ggplot(return_df, aes(x = tstep)) +
#   geom_hline(yintercept = 0, linetype="dashed") +
#   geom_path(aes(y = sx), color = "red") +
#   geom_path(aes(y = x), color = "blue", alpha = .5)
# #
# tmp_trajy <- ggplot(return_df, aes(x = tstep)) +
#   geom_hline(yintercept = 0, linetype="dashed") +
#   geom_path(aes(y = sy), color = "red") +
#   geom_path(aes(y = y), color = "blue", alpha = .5)
# # # 
# # # # velocity
# tmp_vel <- ggplot(return_df, aes(x = tstep)) +
#   geom_hline(yintercept = 0, linetype="dashed") +
#   geom_path(aes(y = svy), color = "red")
# # # 
# # # # acceleration
# tmp_acc <- ggplot(return_df, aes(x = tstep)) +
#   geom_hline(yintercept = 0, linetype="dashed") +
#   geom_path(aes(y = say), color = "red")
# # # 
# # # # jerk
# tmp_jerk <- ggplot(return_df, aes(x = tstep)) +
#   geom_hline(yintercept = 0, linetype="dashed") +
#   geom_path(aes(y = sjy), color = "red")
# # 
# grid.arrange(tmp_trajy,tmp_vel,tmp_acc, tmp_jerk, ncol = 1)
}
