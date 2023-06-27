## plot trial-by-trial movement & reaction time
##
## Author: Taisei Sugiyama

## Set variables
main_dir = "basic_point"
sub_dir = "plot_mtrt"
desc_note <-  c("This plots trial-by-trial movement time for all the trials in a single block")

## Preparation
theme_update(plot.title = element_text(hjust = .5)) # Set default alignment of plot title to be centered
source("function/save_plots.R")
source("function/format_gg.R")
source("function/gg_def_col.R")
source("script/miscellaneous/state_list.R")

## Data processing
data_plot <- point_raw %>%
  dplyr::select(blk_tri, mt, rt, error_deg) %>% 
  left_join(dplyr::select(tgt_raw,blk_tri, rot, show_cur, show_score), by = "blk_tri")


tmp_plot.pre <- ggplot(data_plot, aes(x=blk_tri)) +
  geom_path(aes(y=mt), color = gg_def_col(2)[1]) +
  geom_point(aes(y=mt), color = gg_def_col(2)[1]) +
  geom_path(aes(y=rt), color = gg_def_col(2)[2]) +
  geom_point(aes(y=rt), color = gg_def_col(2)[2])
  
tmp_plot <- format_gg(tmp_plot.pre, xlabel = "Trial", ylabel = "Time [s] (Red: MT, Blue: RT)", ptitle = "Movement & Reaction Time", 
                      xlimit = c(0,max(data_plot$blk_tri)), ylimit =c(0,1.2), xticks = c(seq(1,max(data_plot$blk_tri),20),max(data_plot$blk_tri)) , yticks = c(0,.5,1),  show.leg = F)

fname_plot = sprintf("%s_%s",sub_dir,tgt_dir)
save_plots(fname = fname_plot, tgt_plot = tmp_plot, pdf_only = T)

