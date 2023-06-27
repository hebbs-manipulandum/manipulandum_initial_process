## plot trial-by-trial hand error
## 
## Author: Taisei Sugiyama

## Set variables
main_dir = "basic_point"
sub_dir = "plot_err"
desc_note <-  c("This plots trial-by-trial hand error (tgt - hand) for all the trials in a single block",
                "Note that error in 'raw' data file is defined as (hand - error), so it's flipped when processed")

## Preparation
theme_update(plot.title = element_text(hjust = .5)) # Set default alignment of plot title to be centered
source("function/save_plots.R")
source("function/format_gg.R")
source("function/gg_def_col.R")
source("script/miscellaneous/state_list.R")

## Data processing
data_plot <- point_edit %>%
  dplyr::select(blk_tri, mt, rt, hand_error) %>% 
  left_join(dplyr::select(tgt_raw,blk_tri, rot, show_cur, show_score), by = "blk_tri")


tmp_plot.pre <- ggplot(data_plot, aes(x=blk_tri)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_path(aes(y=rot), color = "orange") +
  geom_path(aes(y=hand_error)) +
  geom_point(aes(y=hand_error)) 

tmp_plot <- format_gg(tmp_plot.pre, xlabel = "Trial", ylabel = "Hand Error (Tgt - Hand) [deg]", ptitle = "Hand Error (Black) & Rotation (Orange)", 
                      xlimit = c(0,max(data_plot$blk_tri)), ylimit =c(-25,25), xticks = c(seq(1,max(data_plot$blk_tri),20),max(data_plot$blk_tri)) , yticks = seq(-15,15,5),  show.leg = F)

fname_plot = sprintf("%s_%s",sub_dir,tgt_dir)
save_plots(fname = fname_plot, tgt_plot = tmp_plot, pdf_only = T)

