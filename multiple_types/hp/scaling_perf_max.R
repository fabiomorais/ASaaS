library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(foreach)
library(doMC)

flavors        = c("c4.large", "m4.large", "r3.large", "c3.large", "m3.medium")
filename       = "data/prices.csv"
df_inst        = read.csv(file = filename, header = T)
df_inst        = select(df_inst, NAME, VCPU, ECU, MEM, GHZ, PRICE)
df_flavor      = filter(df_inst, NAME %in% flavors) %>% mutate(RATIO = ECU / MEM)

ecu_factor     = 0.7602
jobs           = c(1, 2, 3, 4, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 30, 31, 32, 33, 35, 38, 40)

df_ghz         = read.table(file = "../../data/normal/machine_data.dat", header = T)

instance_time   = 0
grain           = 12
time_decision   = 12
selected_flavor = NA 
selected_cap    = NA

select_flavor = function(d, dfl){
     
     dftmp1 = d %>% summarise(ecu_max = max(ecu, na.rm = T), mem_max = max(mem, na.rm = T))
     fsel1  = dfl %>% group_by(NAME) %>% mutate(cap_ecu = ceiling( dftmp1$ecu_max / ECU), cap_mem =  ceiling(dftmp1$mem_max / MEM)) %>% mutate(cap = max(cap_ecu, cap_mem)) %>% mutate(val = cap * PRICE) %>% ungroup() %>% filter(val == min(val))
     fsel1[1, ]
}

registerDoMC(cores = 8)
df_final = foreach(tindex = 1:length(jobs), .combine = rbind) %dopar% {
     
     print(paste(tindex, "/", length(jobs)))
     id = jobs[tindex]
     
     trace_ghz      = filter(df_ghz, TRACE == id)$GHZ
     
     filename       = paste("data/usage/usage-prod-server-", id, "-normal-index.dat", sep = "")
     df_usage     = read.table(file = filename, header = T)
     
     df_demand     = df_usage %>% mutate(ecu = (CPU_USAGE * trace_ghz) / ecu_factor, mem = MEM_USAGE)
     
     dfx           = df_demand %>% mutate(index = ((1:nrow(df_demand) - 1) %/% 12 ) + 1) %>% group_by(index) %>% do(fv = select_flavor(. , df_flavor)) #%>% 
     dfx           = dfx %>% group_by(index) %>% mutate(flavor = fv[[1]]$NAME, cost = fv[[1]]$val, scaling = fv[[1]]$cap, vcpu = fv[[1]]$VCPU * fv[[1]]$cap, ecu = fv[[1]]$ECU * fv[[1]]$cap, mem = fv[[1]]$MEM * fv[[1]]$cap ) %>% select(-fv)
          
     filename       = paste("data/scaling/perfect/", id, "-scaling-perf-max-index.dat", sep = "")
     write.table(dfx, file = filename, row.names = F)
     
     data.frame(ID = id)
}


df_final = c()

for (tindex in 1:length(jobs)) {
     
     print(paste(tindex, "/", length(jobs)))
     id = jobs[tindex]
     
     filename       = paste("data/scaling/perfect/", id, "-scaling-perf-max-index.dat", sep = "")
     dff = read.table(file = filename, header = T)
     
     df_tmp         = dff %>% filter(index != 1) %>% mutate(len = n()) %>% group_by(flavor) %>% summarise(jobId = id, n = n(), len = nth(len, 1)) 
     
     df_final = rbind(df_final, df_tmp)
}

write.table(df_final, file = "data/scaling_perf_max.dat", row.names = F)
