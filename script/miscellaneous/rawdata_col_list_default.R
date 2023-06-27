# set columns of raw data files. 
# You can specify your own columns according to your raw data files, but script may fail if "key" variables are missing or renamed (e.g., x)
#
# Author: Taisei Sugiyama

# kin_col <- c("state","x","y","vx","vy","t_a","t_b","fs_x","fs_y","fx","fy") # columns of kinematic data. used in old version
# kin_col <- c("state","x","y","vx","vy","fs_x","fs_y","unused1","unused2","unused3","unused4") # columns of kinematic data. changed in version 11 of labview program
kin_col <- c("time_global","time_trial","state","x","y","trigger") # columns of kinematic data. changed in version 11 of labview program
point_col <- c("cross_time_global","cross_time_trial","cross_x_rbt","cross_y_rbt","cross_deg_rbt","tgt_deg","error_deg","error_deg_cur","rt","mt","score","fail") # columns of point data
# tgt_col <- c("field","apply_field","trad","tgt","wait_time", "bval", " chan_k11", "chan_b11", "spring_gain","rot","show_arc",
#              "show_cur","show_score","train_type","min_score","max_score","difficulty")
# 
# tgt_col <- c("field","apply_field","trad","tgt","wait_time", "bval", " chan_k11", "chan_b11", "spring_gain","rot","show_arc",
             # "show_cur","show_score","train_type","min_score","max_score","difficulty", "trial_type", "block_phase")#
tgt_col <- c("trad","tgt","tgt_type", "tgt_manip",  "show_cur",  "show_cur_return",
             "show_dist_circle",  "show_dist_circle_return", "cursor_manip",
             "rot","show_score","train_type","min_score","max_score","difficulty", "time_trial", "block_phase")
