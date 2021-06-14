## plot trial-by-trial hand error
## 
## Author: Taisei Sugiyama

## Set variables
main_dir = "basic_point"
sub_dir = "plot_err_bias"
desc_note <-  c("Temporary plots that examine the average hand error in data with 90 degree, separated by previous tgt directions")

## Preparation
theme_update(plot.title = element_text(hjust = .5)) # Set default alignment of plot title to be centered
source("function/save_plots.R")
source("function/format_gg.R")
source("function/gg_def_col.R")
source("script/miscellaneous/state_list.R")

## Data processing

prev_tgt <- subset(tgt_raw, select=c("blk_tri","tgt")) %>% 
  mutate(prev_tgt = lag(tgt,1)) %>% 
  dplyr::filter(tgt == 90) %>% 
  dplyr::select(-tgt)

data_plot <- point_edit %>%
  dplyr::select(blk_tri, peak_vel, mt, rt, retry, hand_error) %>% 
  left_join(dplyr::select(tgt_raw,blk_tri, rot, show_arc, show_cur, show_score), by = "blk_tri") %>% 
  dplyr::filter(blk_tri %in% prev_tgt$blk_tri) %>% 
  left_join(prev_tgt, by="blk_tri")

sdp <- data_plot %>% 
  group_by(prev_tgt) %>% 
  summarise(m = mean(hand_error, na.rm = T), sd = sd(hand_error, na.rm = T)) %>% 
  ungroup()


tmp_plot.pre <- ggplot(data_plot, aes(x=prev_tgt, color = as.factor(prev_tgt))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_errorbar(data = sdp, aes(ymin = m-sd, ymax = m+sd), size = 1, width = 2) +
  geom_point(data = sdp, aes(y = m), size = 3) +
  geom_point(aes(y=hand_error), alpha = .7, size = 1.5) 

tmp_plot <- format_gg(tmp_plot.pre, xlabel = "Previous Target Direction", ylabel = "Hand Error (Tgt - Hand) [deg]", ptitle = "Mean & SD Hand Error at 90-degree Target", 
                      ylimit =c(-5,5), xticks = sort(unique(prev_tgt$prev_tgt)), show.leg = F)

fname_plot = sprintf("%s_%s",sub_dir,tgt_dir)
save_plots(fname = fname_plot, tgt_plot = tmp_plot, pdf_only = T)

