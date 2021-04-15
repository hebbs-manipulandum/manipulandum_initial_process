# This saves individual data. Make sure to do all processing before you run this.
# Author: Taisei Sugiyama

# sid_num = 25 # Subject ID # If you want to run this script alone, uncomment this. 

# Convert to string
if (sid_num < 10){
  sid_str = sprintf("S0%d",sid_num) # Convert sub ID to string (and add an identifier "S")
} else {
  sid_str = sprintf("S%d",sid_num) # Convert sub ID to string (and add an identifier "S")
}

# Create folders if not exist
main_dir = "data"
sub_main_dir =  "processed"

sub_dir = sprintf("%s/%s",main_dir,sub_main_dir)
save_dir = sid_str
dir.create(file.path(main_dir, sub_main_dir), showWarnings = FALSE)
dir.create(file.path(sub_dir, save_dir), showWarnings = FALSE)

fpath = sprintf("%s/%s",sub_dir,save_dir)

# Now save them
# template write.csv(, file=sprintf("%s/.csv",fpath), row.names = FALSE)
# write.csv(data_kin_align, file=sprintf("%s/data_kin_align.csv",fpath), row.names = FALSE)
write.csv(data_kin_align_clean, file=sprintf("%s/data_kin_align_clean.csv",fpath), row.names = FALSE)
# write.csv(data_kin_filt, file=sprintf("%s/data_kin_align_filt.csv",fpath), row.names = FALSE)
write.csv(data_para, file=sprintf("%s/data_para.csv",fpath), row.names = FALSE)
# write.csv(data_prb, file=sprintf("%s/data_prb.csv",fpath), row.names = FALSE)
write.csv(data_tgt, file=sprintf("%s/data_tgt.csv",fpath), row.names = FALSE)
write.csv(data_tri_clean, file=sprintf("%s/data_tri_clean.csv",fpath), row.names = FALSE)
write.csv(data_pts, file=sprintf("%s/data_pts.csv",fpath), row.names = FALSE)
# write.csv(estpara_em, file=sprintf("%s/estpara_em.csv",fpath), row.names = FALSE)
# write.csv(estpara_lsse, file=sprintf("%s/estpara_lsse.csv",fpath), row.names = FALSE)
# write.csv(fit_em_res_whole, file=sprintf("%s/fit_em_res_whole.csv",fpath), row.names = FALSE)
# write.csv(fit_em_res_half, file=sprintf("%s/fit_em_res_half.csv",fpath), row.names = FALSE)
# write.csv(fit_em_res_init, file=sprintf("%s/fit_em_res_init.csv",fpath), row.names = FALSE)
# write.csv(fit_em_xs_whole, file=sprintf("%s/fit_em_xs_whole.csv",fpath), row.names = FALSE)
# write.csv(fit_em_xs_half_first, file=sprintf("%s/fit_em_xs_half_first.csv",fpath), row.names = FALSE)
# write.csv(fit_em_xs_half_second, file=sprintf("%s/fit_em_xs_half_second.csv",fpath), row.names = FALSE)
# write.csv(fit_em_xs_init, file=sprintf("%s/fit_em_xs_init.csv",fpath), row.names = FALSE)
# write.csv(fit_res_whole, file=sprintf("%s/fit_res_whole.csv",fpath), row.names = FALSE)
# write.csv(fit_res_half, file=sprintf("%s/fit_res_half.csv",fpath), row.names = FALSE)
# write.csv(fit_res_init, file=sprintf("%s/fit_res_init.csv",fpath), row.names = FALSE)


