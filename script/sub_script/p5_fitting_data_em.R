## This script conducts EM fitting.
## This does fitting on both whole and part (1st half, 2nd half, and initial segment) of Probe data set
## Make sure to run the p4_fitting_data_lsse before you run this (or have the environment ready by loading it) 
## Author: Taisei Sugiyama

## Define key parameter values
## Make sure to set them correctly when you adapt this script to a different version
# phase_pre = 2 # Pre-Intervention Probe phase
# phase_pos = 6 # Post-Intervention Probe phase
# 
# init_boi = 2 # Initial block of interest (correspond to the 1st BL block)
# 
# num_wo_inc = 3 # Number of Washout trials to be included in initial segment
# num_prb_inc = 5 # Number of initial Probe trials to be included in initial segment
# num_init = num_wo_inc + num_prb_inc # number of trials in the initial data set

## Load packages
library(dplyr)
library(purrr)
library(nloptr)
library(Biobase) # Be careful about using this package, as it will mask many functions
library(astsa)

source("function/em.ssm.R")
source("function/EM1.input.R")

## Fitting
# Whole
fit_em_res_whole = data_prb %>%
  dplyr::filter(half == "first" | half == "second") %>% # Filter 
  with(split(.,list(prepos,blk))) %>% # Convert to list
  purrr::discard(function(x) nrow(x) ==0)  %>% # Remove list elements with zero rows
  map(~ em.ssm(data = ., max_iter = 500, tol = 1.0e-3, fix_alpha = TRUE)) # Fitting

estpara_em_whole_raw = subListExtract(L=fit_em_res_whole, name = "solution") %>% # Get estimated parameters
  reduce(rbind) %>% # Combine the list
  as.data.frame() # Convert to data frame

# Rename row and column
rownames(estpara_em_whole_raw) = names(fit_em_res_whole) 
colnames(estpara_em_whole_raw) = c("alpha","beta","x0")

# Add tags
estpara_em_whole = estpara_em_whole_raw %>%
  mutate(split = row.names(.)) %>% # Preserve the splitting condition, as mutate will change the row name to numbers
  mutate(prepos = ifelse(grepl("pre", split),"pre","pos")) %>% # Get Pre vs Pos
  mutate(blk = as.numeric(gsub(".*([0-9]+).*$", "\\1", split))) %>% # Get block number
  mutate(part = "whole") 
  
# Save estimated x
fit_em_ks_whole = subListExtract(L=fit_em_res_whole, name = "ks") %>% # Get estimated parameters
  reduce(rbind) %>% # Combine the list
  as.data.frame() # Convert to data frame

fit_em_xs_whole = fit_em_ks_whole$xs %>%
  reduce(rbind) %>%
  t()

colnames(fit_em_xs_whole) = c("pos.2","pre.2","pos.3","pos.4","pos.5","pos.6") # Hard-coding now. Fix this later.



# Half
fit_em_res_half = data_prb %>%
  dplyr::filter(half == "first" | half == "second") %>% # Filter 
  with(split(.,list(prepos,blk,half))) %>% # Convert to list
  purrr::discard(function(x) nrow(x) ==0)  %>% # Remove list elements with zero rows
  map(~ em.ssm(data = ., max_iter = 500, tol = 1.0e-3, fix_alpha = TRUE)) # Fitting

estpara_em_half_raw = subListExtract(L=fit_em_res_half, name = "solution") %>% # Get estimated parameters
  reduce(rbind) %>% # Combine the list
  as.data.frame() # Convert to data frame  

# Rename row and column
rownames(estpara_em_half_raw) = names(fit_em_res_half)
colnames(estpara_em_half_raw) = c("alpha","beta","x0")

estpara_em_half = estpara_em_half_raw %>%
  mutate(split = row.names(.)) %>% # Preserve the splitting condition, as mutate will change the row name to numbers
  mutate(prepos = ifelse(grepl("pre", split),"pre","pos")) %>% # Get Pre vs Pos
  mutate(blk = as.numeric(gsub(".*([0-9]+).*$", "\\1", split))) %>% # Get block number
  mutate(part = ifelse(grepl("first", split),"first","second"))

fit_em_ks_half = subListExtract(L=fit_em_res_half, name = "ks") %>% # Get estimated parameters
  reduce(rbind) %>% # Combine the list
  as.data.frame() # Convert to data frame

fit_em_xs_half_first = fit_em_ks_half$xs[1:6] %>%
  reduce(rbind) %>%
  t()

fit_em_xs_half_second = fit_em_ks_half$xs[7:12] %>%
  reduce(rbind) %>%
  t()

colnames(fit_em_xs_half_first) = c("pos.2-1","pre.2-1","pos.3-1","pos.4-1","pos.5-1","pos.6-1")
colnames(fit_em_xs_half_second) = c("pos.2-2","pre.2-2","pos.3-2","pos.4-2","pos.5-2","pos.6-2")

# Initial 
fit_em_res_init = data_prb %>%
  dplyr::filter(initial == 1) %>% # Filter
  with(split(.,list(prepos,blk))) %>% # Convert to list
  purrr::discard(function(x) nrow(x) ==0)  %>% # Remove list elements with zero rows
  map(~ em.ssm(data = ., max_iter = 500, tol = 1.0e-3, fix_alpha = TRUE)) # Fitting

estpara_em_init_raw = subListExtract(L=fit_em_res_init, name = "solution") %>% # Get estimated parameters
  reduce(rbind) %>% # Combine the list 
  as.data.frame() # Convert to data frame

# Rename
rownames(estpara_em_init_raw) = names(fit_em_res_init)
colnames(estpara_em_init_raw) = c("alpha","beta","x0")

# Add tags
estpara_em_init = estpara_em_init_raw %>%
  mutate(split = row.names(.)) %>% # Preserve the splitting condition, as mutate will change the row name to numbers
  mutate(prepos = ifelse(grepl("pre", split),"pre","pos")) %>% # Get Pre vs Pos
  mutate(blk = as.numeric(gsub(".*([0-9]+).*$", "\\1", split))) %>% # Get block number
  mutate(part = "init") %>% # Get part data
  dplyr::filter(blk != 7)

# Save estimated x
fit_em_ks_init = subListExtract(L=fit_em_res_init, name = "ks") %>% # Get estimated parameters
  reduce(rbind) %>% # Combine the list
  as.data.frame() # Convert to data frame

fit_em_xs_init = fit_em_ks_init$xs %>%
  reduce(rbind) %>%
  t()

colnames(fit_em_xs_init) = c("pos.2","pre.2","pos.3","pos.4","pos.5","pos.6")

# Combine all together
estpara_em = do.call(rbind, list(estpara_em_whole,estpara_em_half,estpara_em_init)) %>%
  select(-"split")


