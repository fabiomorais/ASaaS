library(dplyr)
library(doMC)
library(foreach)

args = commandArgs(trailingOnly = TRUE)

flavors        = c("c4.large", "m4.large", "r3.large", "c3.large", "m3.medium")
filename       = "data/prices.csv"
df_inst        = read.csv(file = filename, header = T)
df_inst        = select(df_inst, NAME, VCPU, ECU, MEM, GHZ, PRICE)
df_flavor      = filter(df_inst, NAME %in% flavors) %>% mutate(RATIO = ECU / MEM)

trace_ghz      = 1
cluster        = as.integer(args[1])
ncores         = as.integer(args[2])
exp_factor     = as.numeric(args[3])
ecu_factor     = 0.7602
viol_limit     = 1
target         = viol_limit - exp_factor
tmp            = '27042016T212529'
filename       = paste("data/job/", tmp, "/conf/", cluster, "_sub_jobs.txt", sep = "")
jobs           = read.table(file = filename, header = F)

select_flavor = function(d, dfl){
     
     fsel1  = dfl %>% group_by(NAME) %>% mutate(cap_ecu = ceiling((d$ECU_UTIL / (viol_limit - exp_factor)) / ECU), cap_mem =  ceiling((d$MEM_UTIL / (viol_limit - exp_factor)) / MEM)) %>% mutate(cap = max(cap_ecu, cap_mem)) %>% mutate(val = cap * PRICE) %>% ungroup() %>% filter(val == min(val))
     fsel1[1, ]
}

registerDoMC(cores = ncores)
x = foreach(tt = 1:nrow(jobs), .combine = rbind) %dopar% {
     
     id = jobs[tt, 1]
     
     print(paste(tt, "/", nrow(jobs)))
     
     filename       = paste("data/job/", tmp, "/pred/normal/", id, "-estimated-trace-AR-168-normal.dat", sep = "")
     df_scaling     = read.table(file = filename, header = T)
     df_scaling     = df_scaling %>% mutate(ECU_UTIL = (CPU_UTIL * trace_ghz) / ecu_factor)
     
     dfx            = df_scaling %>% group_by(scenario, slot) %>% do(fv = select_flavor(. , df_flavor))
     dfx            = dfx %>% rename(index = slot)
     dfx            = dfx %>% group_by(scenario, index) %>% mutate(flavor = fv[[1]]$NAME, cost = fv[[1]]$val, scaling = fv[[1]]$cap, vcpu = fv[[1]]$VCPU * fv[[1]]$cap, ecu = fv[[1]]$ECU * fv[[1]]$cap, mem = fv[[1]]$MEM * fv[[1]]$cap ) %>% select(-fv)
     
     filename       = paste("data/job/", tmp, "/scaling/normal/", id, "-scaling-pred-max-index-", target, ".dat", sep = "")
     write.table(dfx, file = filename, row.names = F)
     
     data.frame(ID = id)
}