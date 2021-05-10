# plot 1) trial-by-trial kinematic data and 2) temporally aligned data

##### Data processing #####
data_plot <- kin_raw %>%
  dplyr::select(blk_tri, state, tstep, x, y, vx, vy, sx, sy, svx, svy) %>% 
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
sub_dir = "plot_vel"

# Set filename
desc_note <-  c("This plots trial-by-trial velocity for all the trials in a single block")
tois <- point_raw$blk_tri # trials of interest. Plot all trials 

# pix_to_mm <-  .2451 * 2 # hard coding.
# tsize_m <- 10*pix_to_mm/1000 # 10 px to mm to m

plot_list <- lapply(tois, function(toi){
  
  
  tgt_raw_tri <- subset(tgt_raw, blk_tri == toi)
  tgt_pos <- data.frame(x = tgt_raw_tri$trad*cosd(tgt_raw_tri$tgt), y =  tgt_raw_tri$trad*sind(tgt_raw_tri$tgt))

  # cross point (last time point in the "moving" state)
  cross_pt <- subset(data_plot, blk_tri == toi & state == state_moving) %>% 
    tail(1) %>% 
    .$tstep
  
  tmp_plot.pre <- ggplot(subset(data_plot, blk_tri == toi), aes(x = tstep/1000)) +
    geom_vline(xintercept = cross_pt/1000, color = "gray") +
    geom_path(size = .5, aes(y = svx),  color = "blue") +
    geom_path(size = .5, aes(y = svy), color = "red") +
    theme(plot.margin=unit(c(1,1,1,1)*0,"pt"))
  
  tmp_plot <- format_gg(tmp_plot.pre, xlabel = "", ylabel = "", 
                        fsize_axis_text = 8,
                        xlimit = c(0,1.2), ylimit =c(-1.5,1.5), xticks = c(0,.5,1), yticks = c(-1,0,1),  show.leg = F)
})

nr = 4  # number of rows
nc = 5 # number of columns

plot_list_save = marrangeGrob(plot_list, nrow = nr, ncol = nc, layout_matrix = matrix(1:(nr*nc),nr,nc,T),
                              top = sprintf("SG-filtered Velocity [Blue: x, Red: y, Gray: Time of Crossing]"),
                              left = "velocity [m/s]", bottom = "Time [s]") # convert the list of plots

fname_plot1 = sprintf("%s_%s",sub_dir,tgt_dir)
save_plots(fname = fname_plot1, tgt_plot = plot_list_save, pdf_only = T)


## Aligned data

tmp_plot2.pre <- ggplot(kin_align, aes(x=tstep_align/1000, y=svy, color= blk_tri, group = blk_tri)) +
  geom_hline(yintercept = 0, color = "gray", linetype="31") +
  geom_path() +
  scale_color_gradientn(colours = rainbow(8), name="Trial")

tmp_plot2 <- format_gg(tmp_plot2.pre, xlabel = "Time [s]", ylabel = "Y-Velocity [m/s]", 
                      xlimit = c(0,(align_window)/1000), ylimit =c(-1.5,1.5), xticks = c(0,(align_window)/2000, (align_window)/1000), yticks = seq(-1.5,1.5,.5),  show.leg = T, pos.leg = "tr")

fname_plot2 = sprintf("%s_%s_align_y",sub_dir,tgt_dir)
save_plots(fname = fname_plot2, tgt_plot = tmp_plot2, pdf_only = T)

