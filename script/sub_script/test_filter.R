# This is a temporary script that tests filtering for kinematic data after the operation system for the manipulandum has changed.
# Although this name has "filter", it is more like pre-processing and/or smoothing
#
# Author: Taisei Sugiyama
# Date created: Aug. 3, 2023

source("function/format_gg.R")
source("function/save_plots.R")

main_dir <-  "basic_kin"
sub_dir  <-  "test_filter"

# tmp_df <- kin_raw %>% 
#   rename(delta_t = dt)

## distribution of dt
plot1.pre <- ggplot(kin_raw, aes(x = dt)) +
  geom_histogram(binwidth = 0.001)

plot1 <- format_gg(plot1.pre, "dt (time between recording)", "Count", "Distribution of time stamp intervals")


save_plots(fname = "dt_dist", tgt_plot = plot1, pdf_only = T)

## Kinematic data

library(kza)



for (toi in seq(10,min(c(200,max(kin_raw$blk_tri))),10)) {
  
  
  
  tmp_df <- subset(kin_raw, blk_tri == toi) 
  
  kz_m <- 5 # average from how many points in kz filter
  kz_k <- 3 # how many iterations of averaging in kz filter (default is 3 in the package function)
  
  kzx <- kz(tmp_df$x, kz_m, kz_k)
  kzy <- kz(tmp_df$y, kz_m, kz_k)
  kzvx <- kz(tmp_df$vx, kz_m, kz_k)
  kzvy <- kz(tmp_df$vy, kz_m, kz_k)
  
  
  df_plot <- tmp_df %>% 
    mutate(kzx, kzy,kzvx, kzvy)
  
  
  # col_point <- "#777777"
  alpha_point <- 0.5
  
  # trajectory
  plot2.pre <- ggplot(tmp_df, aes(x = x*100, y = y*100)) +
    geom_point(alpha = alpha_point) + 
    geom_point(aes(x = sx*100, y = sy*100), color = "red", alpha = alpha_point) +
    geom_point(aes(x = kzx*100, y = kzy*100), color = "blue", alpha = alpha_point) 
  
  
  plot2 <- format_gg(plot2.pre, "x (cm)", "y (cm)", "Traj",
                     xlimit = c(-7,7), ylimit=c(-2,12))
  
  
  # velocity
  xrange <- c(0,.8)
  
  # vx
  plot3_1.pre <- ggplot(tmp_df, aes(x = t_trial, y = vx*100)) +
    # geom_point(alpha = .5) +
    geom_point(alpha = alpha_point) +
    geom_point(aes(y = svx*100), color = "red", alpha = alpha_point) +
    geom_point(aes(y = kzvx*100), color = "blue", alpha = alpha_point) 
  
  plot3_1 <- format_gg(plot3_1.pre, "time (s)", "vx (cm/s)", sprintf("vx"),
                       xlimit = xrange, ylimit=c(-30,30))
  
  # vy
  plot3_2.pre <- ggplot(tmp_df, aes(x = t_trial, y = vy*100)) +
    # geom_point(alpha = .5) +
    geom_point(alpha = alpha_point) +
    geom_point(aes(y = svy*100), color = "red", alpha = alpha_point) +
    geom_point(aes(y = kzvy*100), color = "blue", alpha = alpha_point) 
  
  plot3_2 <- format_gg(plot3_2.pre, "time (s)", "vy (cm/s)", "vy",
                       xlimit = xrange, ylimit=c(0,50))
  
  
  nr = 2 # number of rows
  nc = 2 # number of columns
  
  

  
  plot_list_save <- marrangeGrob(list(plot2, plot3_1, plot3_2), nrow = nr, ncol = nc, layout_matrix = matrix(1:(nr*nc),nr,nc,T),
                                 top = sprintf("Trial %d. Gray: Record, Red: SG, Blue: KZ",toi)) # convert the list of plots
  
  fname_list <-  sprintf("%s_%s_t%d",sub_dir,tgt_dir, toi)
  save_plots(fname = fname_list, tgt_plot = plot_list_save, pdf_only = T)
  
  
}




