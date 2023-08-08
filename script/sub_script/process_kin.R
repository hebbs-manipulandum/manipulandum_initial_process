# Set a function to perform SG filter on position data (smoothing and derivatives) to
# filter data as well as calculate velocity, acceleration, and jerk
# Modified function from Fujita
#
# Author: Taisei Sugiyama
# Date created: 04/13/2021

process_kin <- function(df, reduce_hz, reduce_hz_rate, data_version = 1, add_dt = F){
  
  x <- df$x # raw x position
  y <- df$y # raw y position
  
  ## SG filtering (smoothing & derivatives).

  if (length(x)< framelen){
    framelen_used <- round(length(x)/2)
    
    if (framelen_used %% 2 == 0){
      framelen_used <- framelen_used + 1 # n needs to be an odd number
    }
  } else {
    framelen_used <- framelen
  }

  if (data_version > 1) {
    
    # Somehow, meandt will make SG filter overestimate sv (and the rest), whereas max dt does better job (Unknown why it works, but it works...)
    # mean_dt <- mean(df$dt, na.rm = T) # mean_delta time
    max_dt <- max(df$dt, na.rm = T) # max time
    
    sx <- sgolayfilt(x, p = order, n = framelen_used, m = 0, ts = max_dt) # smooth
    svx <- sgolayfilt(x, p = order, n = framelen_used, m = 1, ts = max_dt) # velocity
    sax <- sgolayfilt(x, p = order, n = framelen_used, m = 2, ts = max_dt) # acceleration
    sjx <- sgolayfilt(x, p = order, n = framelen_used, m = 3, ts = max_dt) # jerk
    
    # y
    sy <- sgolayfilt(y, p = order, n = framelen_used, m = 0, ts = max_dt) # smooth
    svy <- sgolayfilt(y, p = order, n = framelen_used, m = 1, ts = max_dt) # velocity
    say <- sgolayfilt(y, p = order, n = framelen_used, m = 2, ts = max_dt) # acceleration
    sjy <- sgolayfilt(y, p = order, n = framelen_used, m = 3, ts = max_dt) # jerk
    
    
    
  } else {
    
    sx <- sgolayfilt(x, p = order, n = framelen_used, m = 0, ts = 1/sample_rate) # smooth
    svx <- sgolayfilt(x, p = order, n = framelen_used, m = 1, ts = 1/sample_rate) # velocity
    sax <- sgolayfilt(x, p = order, n = framelen_used, m = 2, ts = 1/sample_rate) # acceleration
    sjx <- sgolayfilt(x, p = order, n = framelen_used, m = 3, ts = 1/sample_rate) # jerk
    
    # y
    sy <- sgolayfilt(y, p = order, n = framelen_used, m = 0, ts = 1/sample_rate) # smooth
    svy <- sgolayfilt(y, p = order, n = framelen_used, m = 1, ts = 1/sample_rate) # velocity
    say <- sgolayfilt(y, p = order, n = framelen_used, m = 2, ts = 1/sample_rate) # acceleration
    sjy <- sgolayfilt(y, p = order, n = framelen_used, m = 3, ts = 1/sample_rate) # jerk
    
    
  }
  
  
  
  vx <- df$vx # robot x-velocity
  vy <- df$vy # robot y-velocity
  state <- df$state # trial state
  
  
  if (reduce_hz) {
    return_df <- data.frame(tstep = 1:length(x), state, x, y, vx, vy, sx, sy, svx, svy, sax, say, sjx, sjy, fx = df$fs_x, fy = df$fs_y)  %>% 
      dplyr::filter(tstep %% reduce_hz_rate == 1) # reduce sample Hz
  } else{
    return_df <- data.frame(tstep = 1:length(x), state, x, y, vx, vy, sx, sy, svx, svy, sax, say, sjx, sjy, fx = df$fs_x, fy = df$fs_y)
  }
  
  
  if (add_dt){
    return_df <- return_df %>% 
      mutate(dt = df$dt, t_trial = df$t_trial)
  }


  # # ## Checking
  # # trajectory
  # tmp_trajx <- ggplot(return_df, aes(x = tstep)) +
  #   geom_hline(yintercept = 0, linetype="dashed") +
  #   geom_path(aes(y = sx), color = "red") +
  #   geom_path(aes(y = x), color = "blue", alpha = .5)
  # 
  # tmp_trajy <- ggplot(return_df, aes(x = tstep)) +
  #   geom_hline(yintercept = 0, linetype="dashed") +
  #   geom_path(aes(y = sy), color = "red") +
  #   geom_path(aes(y = y), color = "blue", alpha = .5)
  # 
  # 
  # tmp_traj <- ggplot(return_df, aes(x = x)) +
  #   geom_hline(yintercept = 0, linetype="dashed") +
  #   geom_path(aes(y = sy), color = "red") +
  #   geom_path(aes(y = y), color = "blue", alpha = .5) +
  #   xlim(-0.07,0.07) +
  #   ylim(-0.01,0.13)
  # 
  # # #
  # # # velocity
  # tmp_vel <- ggplot(return_df, aes(x = tstep)) +
  #   geom_hline(yintercept = 0, linetype="dashed") +
  #   geom_path(aes(y = svy), color = "red") +
  #   geom_path(aes(y = vy), color = "blue", alpha = .5)
  # #
  # # # acceleration
  # tmp_acc <- ggplot(return_df, aes(x = tstep)) +
  #   geom_hline(yintercept = 0, linetype="dashed") +
  #   geom_path(aes(y = say), color = "red")
  # #
  # # # jerk
  # tmp_jerk <- ggplot(return_df, aes(x = tstep)) +
  #   geom_hline(yintercept = 0, linetype="dashed") +
  #   geom_path(aes(y = sjy), color = "red")
  # 
  # grid.arrange(tmp_traj,tmp_vel,tmp_acc, tmp_jerk, ncol = 1)
  
}
