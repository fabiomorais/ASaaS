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

df_scaling     = c()

for (tindex in 1:length(jobs)) {
     
     print(paste(tindex, "/", length(jobs)))
     id = jobs[tindex]
     
     trace_ghz      = filter(df_ghz, TRACE == id)$GHZ
     
     filename       = paste("data/usage/usage-prod-server-", id, "-normal-index.dat", sep = "")
     df_usage       = read.table(file = filename, header = T)
     
     df_usage       = df_usage %>% group_by(indexslot) %>% summarise(cpu_usage = sum(CPU_USAGE), mem_usage = sum(MEM_USAGE)) %>% ungroup() %>% arrange(indexslot)
     
     df_usage       = df_usage %>% ungroup() %>% mutate(index = ((1:nrow(df_usage) - 1) %/% 12 ) + 1)
     df_demand      = df_usage %>% filter(index != 1) %>% mutate(ecu = (cpu_usage * trace_ghz) / ecu_factor, mem = mem_usage)
     
     billing_slots  = length(unique(df_demand$index))
     
     dftmp1         = df_demand %>% summarise(ecu_max = max(ecu, na.rm = T), mem_max = max(mem, na.rm = T))
     fv             = df_flavor %>% group_by(NAME) %>% mutate(cap_ecu = ceiling( dftmp1$ecu_max / ECU), cap_mem =  ceiling(dftmp1$mem_max / MEM)) %>% mutate(cap = max(cap_ecu, cap_mem)) %>% mutate(val = cap * PRICE) %>% ungroup() %>% filter(val == min(val))
     
     dfx            = data.frame(jobId = id, flavor = fv$NAME, cost = fv$val * billing_slots, scaling = fv$cap, vcpu = fv$VCPU * fv$cap, ecu = fv$ECU * fv$cap, mem = fv$MEM * fv$cap )
     
     df_scaling     = rbind(df_scaling, dfx)   
}

write.table(df_scaling, file = "data/scaling_perf_max_single.dat", row.names = F)

