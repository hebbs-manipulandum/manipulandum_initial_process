# plot trial-by-trial kinematic data

cosd <- function(deg){
  return <- cos(deg*pi/180)
}

sind <- function(deg){
  return <- sin(deg*pi/180)
}

##### Data processing #####
data_plot <- kin_raw %>%
  dplyr::select(blk_tri,state, tstep, x, y, sx, sy, svx, svy) %>% 
  left_join(dplyr::select(tgt_raw,blk_tri,tgt, rot), by = "blk_tri") %>%
  mutate(tgt = factor(tgt)) %>% 
  mutate(curx = (x*cosd(rot) - y*sind(rot)), cury = (x*sind(rot) + y*cosd(rot)))

# ##### Plotting #####
## Preparation
theme_update(plot.title = element_text(hjust = .5)) # Set default alignment of plot title to be centered
source("function/save_plots.R")
source("function/format_gg.R")
source("function/gg_def_col.R")
source("script/miscellaneous/state_list.R")

main_dir = "basic_kin"
sub_dir = "plot_pos"
desc_note <-  c("This plots trial-by-trial trajectory for all the trials in a single block")
tois <- point_raw$blk_tri # trials of interest. Plot all trials 


pix_to_mm <-  px_to_mm # .2451 * 2 # hard coding.
tsize_m <- 0.005 # 10*pix_to_mm/1000 # 10 px to mm to m

plot_list <- lapply(tois, function(toi){
  
  
  tgt_raw_tri <- subset(tgt_raw, blk_tri == toi)
  tgt_pos <- data.frame(x = tgt_raw_tri$trad*cosd(tgt_raw_tri$tgt)/1000, y =  tgt_raw_tri$trad*sind(tgt_raw_tri$tgt)/1000)

  tmp_plot.pre <- ggplot(subset(data_plot, blk_tri == toi)) +
    geom_circle(data = tgt_pos, aes(x0=x, y0=y, r = tsize_m), size = .5) +
    geom_path(size = .5, aes(x = curx, y = cury, color = "red")) +
    geom_path(size = .5, aes(x=x, y=y), color = "gray", linetype = "31") + 
    guides(shape = F, color = F) +
    theme(plot.margin=unit(c(1,1,1,1)*0,"pt"))
  
  
  # prange <- c(-.15, .15)
  
  tmp_plot <- format_gg(tmp_plot.pre, xlabel = "", ylabel = "", 
                        fsize_axis_text = 8,
                        xlimit = c(-.08, .08), ylimit = c(-.02,.14), xticks = NA, yticks = NA,  show.leg = F)
})

nr = 4 # number of rows
nc = 5 # number of columns


plot_list_save = marrangeGrob(plot_list, nrow = nr, ncol = nc, layout_matrix = matrix(1:(nr*nc),nr,nc,T),
                              top = sprintf("Trajectory [Hand: gray, Cursor: red, Target: black]"),
                              left = "y [m]", bottom = "x [m]") # convert the list of plots

fname_list = sprintf("%s_%s",sub_dir,tgt_dir)
save_plots(fname = fname_list, tgt_plot = plot_list_save, pdf_only = T)

# superimposed


tgt_poss <- data.frame(x = tgt_raw$trad[1]*cosd(unique(tgt_raw$tgt)), y =  tgt_raw$trad[1]*sind(unique(tgt_raw$tgt)))

plot1.pre <- ggplot(data_plot) +
  geom_path(aes(x=x, y=y, group = blk_tri, color = tgt)) +
  geom_circle(data = tgt_poss, aes(x0=x, y0=y, r = tsize_m, group = NA), size = .5, color = "black")

plot1 <- format_gg(plot1.pre, xlabel = "x [m]", ylabel = "y [m]", 
                      fsize_axis_text = 12,
                      # xlimit = c(-.15,.15), ylimit =c(-.15,.15), xticks = NA, yticks = NA,  show.leg = F)
                      # xlimit = c(-1,1), ylimit =c(-1,1), xticks = NA, yticks = NA,  show.leg = F)
                      ylimit = c(-.01,.15), xlimit =c(-.08,.08), xticks = NA, yticks = NA,  show.leg = F)

save_plots(fname = sprintf("%s_%s_blk",sub_dir,tgt_dir), tgt_plot = plot1, pdf_only = T)


# 
# plot2.pre <- ggplot(data_plot, aes(x = tstep, group = blk_tri)) +
#   geom_path(aes(y = x), color = "blue") +
#   geom_path(aes(y = y), color = "red") +  
#   geom_point(aes(y = x), color = "blue", size = .4) +
#   geom_point(aes(y = y), color = "red", size = .4)
# 
# plot2 <- format_gg(plot2.pre, xlabel = "Time [ms]", ylabel = "Position from Start (x: Blue, y:Red) [m]", 
#                    fsize_axis_text = 12,
#                    # xlimit = c(-.15,.15), ylimit =c(-.15,.15), xticks = NA, yticks = NA,  show.leg = F)
#                    ylimit = c(-.01,.15), xlimit = c(0,800), show.leg = F)
# 
# save_plots(fname = sprintf("%s_%s_time",sub_dir,tgt_dir), tgt_plot = plot2, pdf_only = T)
# 
# plot3 <- format_gg(plot2.pre, xlabel = "Time [ms]", ylabel = "Position from Start (x: Blue, y:Red) [m]", 
#                    fsize_axis_text = 12,
#                    # xlimit = c(-.15,.15), ylimit =c(-.15,.15), xticks = NA, yticks = NA,  show.leg = F)
#                    ylimit = c(.05,.10), xlimit = c(300,600), show.leg = F)
# 
# save_plots(fname = sprintf("%s_%s_time_zoomed",sub_dir,tgt_dir), tgt_plot = plot3, pdf_only = T)

# tmp <- subset(data_plot, blk_tri == 62)

