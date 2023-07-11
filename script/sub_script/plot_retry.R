## plot trial-by-trial number of retry (per trial, not total)
## 
## Author: Taisei Sugiyama

## Set variables
main_dir = "basic_point"
sub_dir = "plot_retry"
desc_note <-  c("This plots trial-by-trial number of retry (per trial, not total number)")

## Preparation
theme_update(plot.title = element_text(hjust = .5)) # Set default alignment of plot title to be centered
source("function/save_plots.R")
source("function/format_gg.R")
source("function/gg_def_col.R")
source("script/miscellaneous/state_list.R")

## Data processing
data_plot <- point_edit %>%
  dplyr::select(blk_tri, mt, rt, fail, hand_error) %>% 
  left_join(dplyr::select(tgt_raw,blk_tri, rot, show_cur, show_score), by = "blk_tri")


tmp_plot.pre <- ggplot(data_plot, aes(x=blk_tri)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_path(aes(y=fail)) +
  geom_point(aes(y=fail)) 

tmp_plot <- format_gg(tmp_plot.pre, xlabel = "Trial", ylabel = "Fail (code)", ptitle = "Failed trial", 
                      xlimit = c(0,max(data_plot$blk_tri)), ylimit =c(0,5), yticks =seq(0,5,1), xticks = c(seq(1,max(data_plot$blk_tri),20),max(data_plot$blk_tri)) ,  show.leg = F)

if (exists("save_tag")){
  fname_plot <- sprintf("%s_%s_blk",sub_dir,save_tag)
} else {
  fname_plot <- sprintf("%s_%s_blk",sub_dir,tgt_dir)
}

save_plots(fname = fname_plot, tgt_plot = tmp_plot, pdf_only = T)

