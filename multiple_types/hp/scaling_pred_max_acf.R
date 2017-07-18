library(dplyr)
library(doMC)
library(foreach)

args = commandArgs(trailingOnly = TRUE)

flavors        = c("c4.large", "m4.large", "r3.large", "c3.large", "m3.medium")
filename       = "prices.csv"
df_inst        = read.csv(file = filename, header = T)
df_inst        = select(df_inst, NAME, VCPU, ECU, MEM, GHZ, PRICE)
df_flavor      = filter(df_inst, NAME %in% flavors) %>% mutate(RATIO = ECU / MEM)

df_ghz         = read.table(file = "../../data/normal/machine_data.dat", header = T)

ncores         = as.integer(args[1])
exp_factor     = as.numeric(args[2])
ecu_factor     = 0.7602
viol_limit     = 1
target         = viol_limit - exp_factor
jobs           = c(1, 2, 3, 4, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 30, 31, 32, 33, 35, 38, 40)

select_flavor = function(d, dfl){
     
     fsel1  = dfl %>% group_by(NAME) %>% mutate(cap_ecu = ceiling((d$ECU_UTIL / (viol_limit - exp_factor)) / ECU), cap_mem =  ceiling((d$MEM_UTIL / (viol_limit - exp_factor)) / MEM)) %>% mutate(cap = max(cap_ecu, cap_mem)) %>% mutate(val = cap * PRICE) %>% ungroup() %>% filter(val == min(val))
     fsel1[1, ]
}

for (adj in c(42, 84, 168)) {
    
     print(adj) 
     x = foreach(tt = 1:length(jobs), .combine = rbind) %dopar% {
          
          id = jobs[tt]
          
          trace_ghz      = filter(df_ghz, TRACE == id)$GHZ
          
          print(paste(tt, "/", length(jobs)))
          
          filename       = paste("data/pred/normal/", id, "-estimated-trace-AR-168-normal-acf-", adj, ".dat", sep = "")
          df_scaling     = read.table(file = filename, header = T)
          df_scaling     = df_scaling %>% mutate(ECU_UTIL = (CPU_UTIL * trace_ghz) / ecu_factor)

          dfx            = df_scaling %>% group_by(slot) %>% do(fv = select_flavor(. , df_flavor))
          dfx            = dfx %>% rename(index = slot)
          dfx            = dfx %>% group_by(index) %>% mutate(flavor = fv[[1]]$NAME, cost = fv[[1]]$val, scaling = fv[[1]]$cap, vcpu = fv[[1]]$VCPU * fv[[1]]$cap, ecu = fv[[1]]$ECU * fv[[1]]$cap, mem = fv[[1]]$MEM * fv[[1]]$cap ) %>% select(-fv)
          
          filename       = paste("data/scaling/normal/", id, "-scaling-pred-max-index-", target, "-acf-", adj, ".dat", sep = "")
          write.table(dfx, file = filename, row.names = F)
          
          data.frame(ID = id)
     }
}