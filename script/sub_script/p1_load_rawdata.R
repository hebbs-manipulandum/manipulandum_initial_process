## This script reads and organizes raw data text file (sequence file, param file, and raw data file) for RMA Project 
## Author: Taisei Sugiyama

## Define key parameters
# sid_num = 25 # Subject ID # If you want to run this script alone, uncomment this. 

## Load packages
library(dplyr)
library(purrr)
library(stringr)

## Read in data
# Nothing to read in because this is the first processing script

# Get the names of the all files
if (sid_num < 10){
  sid_str <- sprintf("S0%d",sid_num) # Convert sub ID to string (and add an identifier "S")
  } else {
  sid_str <- sprintf("S%d",sid_num) # Convert sub ID to string (and add an identifier "S")
  }

fpath <- sprintf("data/rawdata/%s",sid_str) # File path
fname_all <- list.files(path = fpath, full.names = T) 

# Convert parameter file name with wildcard in reg. expression
grx_para <- glob2rx("*B*Para.dat") 
grx_tri <- glob2rx("*B*T*.dat") # data file names

# Read parameter files

para_filename = grep(grx_para,fname_all) %>%
  fname_all[.] 

num_blk = length(para_filename)

blk_num_tag = vector(mode = "integer",num_blk) # store block number

for (blk in 1:num_blk){
  blk_num_tag[blk] = as.numeric(str_extract_all(para_filename[blk], "[0-9]+")[[1]][[2]]) # extract 2nd number from the filename, which is block number (1st one is subject ID)
}

data_para = grep(grx_para,fname_all) %>%
  fname_all[.] %>%
  map(read.table, header = T, sep= ",") %>%
  reduce(rbind) %>%
  dplyr::rename(trad = Trad, max_mt = MaxMT, min_mt = MinMT, sxpos = Sxpos, sypos = Sypos,
                tgt_fname = Tgt.Filename, rwd_zone = RwdZone, learn_to_max_outcome = L2Maxpay) %>% 
  mutate(blk = blk_num_tag) %>%
  arrange(blk) # sort by block

# Read trial data files 
data_raw <- grep(grx_tri,fname_all) %>%
  fname_all[.] %>%
  map(function(fname){
    return_df <- read.table(fname, header = F, sep= ",") %>% 
      dplyr::filter(V7 != 0) # Trim "tails" from each trial recording. V7 represents state.
  })
      

## Calculate some basic parameters from data 
num_tri <- length(data_raw) # number of total trials
num_blk <- dim(data_para)[1] # number of blocks
num_tri_blk <- rep(0,num_blk) # initialize

# Get the number of trials for each block
for (blk in 1:num_blk){
  num_tri_blk[blk] = sprintf("*B%d_T*.dat",blk) %>% # file name for a block
    glob2rx(.) %>% # Convert to regular expression
    grep(.,fname_all) %>%  # extract filenames for only this block
    length(.)
}

num_cumtri_blk <- vector(mode="integer", length = num_blk+1) # cumulative number of trials across block. Initialize

for (blk in 1:num_blk){
  num_cumtri_blk[blk+1:num_blk] <- num_cumtri_blk[blk+1:num_blk] + num_tri_blk[blk]
}

fpath_tgt <- sprintf("script/target/") # File path for tgt files

# Read target (sequence) file data 
data_tgt <- data_para$tgt_fname %>% # Get tgt file names as string 
  sprintf("%s%s",fpath_tgt,.) %>% # Add "pre-fix"
  map(read.table, header=F, sep="") 

# Some editing
for (blk in 1:num_blk){
  colnames(data_tgt[[blk]]) = c("tgt","iti","bval","field","shift","maxpay","basepen","showcur","mf","tsize","phase","task_demand","minpay")
  data_tgt[[blk]] = mutate(data_tgt[[blk]],blk = blk)
}
  
# More editing
data_tgt = data_tgt %>%
  reduce(rbind) %>%
  mutate(total_tri = row_number()) %>%
  mutate(task_demand_str = ifelse(task_demand == 0, "NoMF",
                                  ifelse(task_demand == 1, "Go","Nogo"))) %>%
  group_by(blk) %>%
  mutate(blk_tri = row_number()) %>%
  ungroup; 

## Editing and some cleaning on raw data

# Get file names for trial data
fname_tri = grep(grx_tri,fname_all) %>%
  fname_all[.]

for (tri in 1:num_tri){
  
  tri_ids = as.numeric(str_extract_all(fname_tri[tri], "[0-9]+")[[1]]) # Get trial "id" (sub#, block, trial) 

  data_raw[[tri]] = mutate(data_raw[[tri]], blk = tri_ids[2]) %>% # add total trial number
    mutate(blk_tri = tri_ids[3] + 1) # Since tri_ids comes from file name, which starts from 0, add 1 to it. 
  
}

data_raw_edited <- data_raw %>%
  reduce(rbind) %>% # Combine the list
  arrange(blk, blk_tri) %>%
  mutate(total_tri = num_cumtri_blk[blk]+blk_tri)


# A new data column has been added to additional subjects (S110~), but it creates a problem when you combine it to the old data set, so trim it for a moment

colnames(data_raw_edited) <- c("x","y","dx","dy","fx","fy","state","mt","rt","vc_xpass","vc_ypass","rc_xpass","rc_ypass","gain","num_retry","blk","blk_tri","total_tri")

data_tri_ret <- data_raw_edited %>%
  select(mt, rt, vc_xpass, vc_ypass, rc_xpass, rc_ypass, gain, total_tri, blk_tri, blk, num_retry) %>%
  group_by(total_tri) %>%
  dplyr::filter(row_number() == 1) %>%
  ungroup()

data_tri <- data_tri_ret %>%
  select(-num_retry) 

dir.create(file.path("data", "processed"), showWarnings = FALSE)
dir.create(file.path("data/processed", sid_str), showWarnings = FALSE)

write.csv(data_tri_ret, file=sprintf("data/processed/%s/data_tri_ret.csv",sid_str), row.names = FALSE)

data_raw_edited <- dplyr::select(data_raw_edited, -num_retry)
  
## Organize kinematic data
data_kin <- data_raw_edited %>%
  select(x,y,dx,dy,fx,fy,state,total_tri,blk_tri,blk) %>% # Extract
  dplyr::filter(state != 0) # Trim "tails" from each trial recording
  
## Done!
