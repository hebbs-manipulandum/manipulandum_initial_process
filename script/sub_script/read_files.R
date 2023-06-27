# This reads the 4 following different types of files and create a dataframe for each.
# point_raw: "point" trial data (e.g., movement time, hand error)
# kin_raw: kinematic data
# tgt_raw: target sequence file data 
# param_raw: block-wide parameters(e.g., sequence file name used, target distance)


## Preparation
source(fname_col_list)

# categorize filenames
fname_kin <- fname_all[grepl('kin', fname_all) & grepl('.txt', fname_all)] # kinematic data filenames
fname_point <- fname_all[grepl('point', fname_all) & grepl('.txt', fname_all)] # point (trial-by-trial) data filenames
fname_tgt <-  fname_all[grepl('data_load.txt', fname_all)]  # loaded target sequence data. pre-defined name
fname_param <-  fname_all[grepl('data_params.txt', fname_all)] # key block parameters. pre-defined name

## Now read data
tgt_raw <- data.frame(read.table(fname_tgt, header = T, row.names = NULL)) %>%  # target sequence
  mutate(blk_tri = row_number())


# correct a minor bug from experiment script

if (colnames(tgt_raw)[11] == "Not_defined_state")
  colnames(tgt_raw)[11] <- "SHOW_SCORE"
  
  
  
names(tgt_raw) <- c(tgt_col,"blk_tri")

param_raw <- data.frame(read.table(fname_param, header = T, row.names = NULL)) # block-wide parameters

# single-point data 
point_raw <- map(fname_point,function(fname){
  tri_filename <- gsub("^.*/","",fname) # remove file path
  tri_num <-   as.numeric(gsub("[^0-9]","",tri_filename))
  return_df <- suppressWarnings(data.frame(read.table(fname, header = F, row.names = NULL))) %>% 
    mutate(blk_tri = tri_num) # get block trial number, which is in filename (starting from 0, so add 1)
}) %>% 
  reduce(rbind)

colnames(point_raw) <- c(point_col,"blk_tri")

# kinematic data
task_center_x <- param_raw$center_pos_x
task_center_y <- param_raw$center_pos_y


# This was not implemented for f101, so manually define the conversion factor
if (tgt_dir == "f101") {
  px_to_mm <- 311/1920
  
} else {

  px_to_mm <- param_raw$display_width/param_raw$resolution_width
  
}

kin_raw <- map(fname_kin,function(fname){
  
  tri_filename <- gsub("^.*/","",fname) # remove file path
  tri_num <-   as.numeric(gsub("[^0-9]","",tri_filename))
  return_df_raw <- data.frame(read.table(fname, header = F, row.names = NULL)) # %>% 
  
  colnames(return_df_raw) <- kin_col
  
  return_df_raw2 <- return_df_raw %>% 
    mutate(x = (x - task_center_x*px_to_mm)/1000, y = (y - task_center_y*px_to_mm)/1000) # zero-ing with respect to the center of task space. also convert from mm to m
  
  # for checking frequency
  # temp <- return_df_raw2 %>% 
  #   mutate(dt = time_trial - lag(time_trial, 1), dtg = time_global - lag(time_global, 1))
  # summary(temp)
  
  return_df <- process_kin(return_df_raw2,reduce_hz,reduce_hz_rate) %>% 
    mutate(blk_tri = tri_num)
  
}) %>% 
  reduce(rbind)



# temp line for checking
# tmp.plot <- ggplot(return_df_raw2, aes(x = x, y = y)) +
#   geom_path() +
#   ylim(300,450) +
#   xlim(-95,55)




