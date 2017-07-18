library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(foreach)
library(doMC)

flavors        = c("c4.large", "m4.large", "r3.large", "c3.large", "m3.medium")
filename       = "../../../multiple_types/hp/data/prices.csv"
df_inst        = read.csv(file = filename, header = T)
df_inst        = select(df_inst, NAME, VCPU, ECU, MEM, GHZ, PRICE)
df_flavor      = filter(df_inst, NAME %in% flavors) %>% mutate(RATIO = ECU / MEM)

ecu_factor     = 0.7602

jobs           = c(1, 2, 3, 4, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 30, 31, 32, 33, 35, 38, 40)

df_ghz         = read.table(file = "../../../data/normal/machine_data.dat", header = T)

instance_time   = 0
grain           = 12
time_decision   = 12
selected_flavor = NA 
selected_cap    = NA


registerDoMC(cores = 8)
var_over = c(-0.8, -0.6, -0.7, -0.5, -0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3)

#for(tindex in 1:length(jobs)) {

df_result      = foreach(tindex = 1:length(jobs), .combine = rbind) %dopar% {      
     
     print(paste(tindex, "/", length(jobs)))
     id = jobs[tindex]
     
     trace_ghz      = filter(df_ghz, TRACE == id)$GHZ
     
     filename       = paste("../../../multiple_types/hp/data/usage/usage-prod-server-", id, "-normal-index.dat", sep = "")
     df_usage       = read.table(file = filename, header = T)
     
     df_usage       = df_usage %>% group_by(indexslot) %>% summarise(cpu_usage = sum(CPU_USAGE), mem_usage = sum(MEM_USAGE)) %>% ungroup() %>% arrange(indexslot)
     df_usage       = df_usage %>% ungroup() %>% mutate(index = ((1:nrow(df_usage) - 1) %/% 12 ) + 1)
     
     df_result_tmp  = c()
     
     for (over_v in var_over) {
          
          billing   = length(unique(df_usage$index)) - 1 # desconsidera o primeiro slot
          
          df_over   = df_usage %>% filter(index != 1) %>% mutate(ecu = (cpu_usage * trace_ghz) / ecu_factor, mem = mem_usage)
          df_over   = df_over %>% ungroup() %>% summarise(ecu_max = max(ecu, na.rm = T), mem_max = max(mem, na.rm = T))
          df_over   = df_over %>% mutate(ecu_max = ecu_max + (ecu_max * over_v), mem_max = mem_max + (mem_max * over_v))
          
          df_over_res = df_flavor %>% group_by(NAME) %>% mutate(cap_ecu = ceiling(df_over$ecu_max / ECU), cap_mem = ceiling(df_over$mem_max / MEM), cap = max(cap_ecu, cap_mem), cost = cap * PRICE)
          df_over_res = df_over_res %>% mutate(over_cost = cost * billing, ecu_alloc = cap * ECU, mem_alloc = cap * MEM)
          df_over_res = df_over_res %>% select(over_cap = cap, ecu_alloc, mem_alloc, over_flavor = NAME, over_cost)
          df_over_res = df_over_res %>% mutate(jobId = id, factor = over_v)
          
          df_demand      = df_usage %>% filter(index != 1) %>% mutate(ecu = (cpu_usage * trace_ghz) / ecu_factor, mem = mem_usage)
          df_demand      = df_demand %>% group_by(index) %>% summarise(ecu_max = max(ecu, na.rm = T), mem_max = max(mem, na.rm = T))
          
          df_over_res = df_over_res %>% group_by(over_flavor) %>% mutate(viol = sum((df_demand$ecu_max > ecu_alloc) | (df_demand$mem_max > mem_alloc)), viol_ecu = sum(df_demand$ecu_max > ecu_alloc), viol_mem = sum(df_demand$mem_max > mem_alloc))
          df_over_res = df_over_res %>% mutate(p_viol = viol / billing, p_viol_ecu = viol_ecu / billing, p_viol_mem = viol_mem /billing)                          
          
          df_result_tmp  = rbind(df_result_tmp, df_over_res)
     }
     
     df_result_tmp
}

write.table(df_result, file = "../data/hp/over-provisioning-data.dat", row.names = F)