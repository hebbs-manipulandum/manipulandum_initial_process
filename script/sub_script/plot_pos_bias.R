# plot trial-by-trial kinematic data
# special (temporary) script that plot 1) data with 90 degree only, and 2) coloring by previous tgt direction

##### Data processing #####
data_plot <- kin_raw %>%
  dplyr::select(blk_tri,state, tstep, x, y, vx, vy, sx, sy, svx, svy) %>% 
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
sub_dir = "plot_pos_bias"
desc_note <-  c("This plots trial-by-trial trajectory for all the trials in a single block",
                "Temporary plot with 1) data with 90 degree only, and 2) coloring by previous tgt direction")
tois <- point_raw$blk_tri # trials of interest. Plot all trials 


pix_to_mm <-  .2451 * 2 # hard coding.
tsize_m <- 10*pix_to_mm/1000 # 10 px to mm to m

# superimposed
prev_tgt <- subset(tgt_raw, select=c("blk_tri","tgt")) %>% 
  mutate(prev_tgt = lag(tgt,1)) %>% 
  dplyr::filter(tgt == 90) %>% 
  dplyr::select(-tgt)

tgt_poss <- data.frame(x = tgt_raw$trad[1]*cosd(90), y =  tgt_raw$trad[1]*sind(90))

dp_subset <- subset(data_plot, blk_tri %in% prev_tgt$blk_tri) %>% 
  left_join(prev_tgt, by="blk_tri") %>% 
  mutate(prev_tgt = factor(prev_tgt))

plot1.pre <- ggplot(dp_subset) +
  geom_path(aes(x=x, y=y, group = blk_tri, color = prev_tgt), alpha = .6) +
  geom_circle(data = tgt_poss, aes(x0=x, y0=y, r = tsize_m, group = NA), size = .5, color = "black") 

plot1 <- format_gg(plot1.pre, xlabel = "x [m]", ylabel = "y [m]", 
                      fsize_axis_text = 12,
                      # xlimit = c(-.15,.15), ylimit =c(-.15,.15), xticks = NA, yticks = NA,  show.leg = F)
                      ylimit = c(-.01,.15), xlimit =c(-.08,.08), xticks = NA, yticks = NA,  show.leg = T)

save_plots(fname = sprintf("%s_%s_blk",sub_dir,tgt_dir), tgt_plot = plot1, pdf_only = T)
