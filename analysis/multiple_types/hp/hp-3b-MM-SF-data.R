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

df_result      = foreach(tindex = 1:length(jobs), .combine = rbind) %dopar% {      
     
     print(paste(tindex, "/", length(jobs)))
     id = jobs[tindex]
     
     trace_ghz      = filter(df_ghz, TRACE == id)$GHZ
     
     filename       = paste("../../../multiple_types/hp/data/usage/usage-prod-server-", id, "-normal-index.dat", sep = "")
     df_usage       = read.table(file = filename, header = T)
     
     df_usage       = df_usage %>% group_by(indexslot) %>% summarise(cpu_usage = sum(CPU_USAGE), mem_usage = sum(MEM_USAGE)) %>% ungroup() %>% arrange(indexslot)
     df_usage       = df_usage %>% ungroup() %>% mutate(index = ((1:nrow(df_usage) - 1) %/% 12 ) + 1)
     
     df_result_tmp  = c()
     df_scaling     = c()
     
     billing   = length(unique(df_usage$index)) - 1 # desconsidera o primeiro slot
     
     df_over   = df_usage %>% filter(index != 1) %>% mutate(ecu = (cpu_usage * trace_ghz) / ecu_factor, mem = mem_usage)
     df_over   = df_over %>% ungroup() %>% summarise(ecu_max = max(ecu, na.rm = T), mem_max = max(mem, na.rm = T))
     
     df_over_res = df_flavor %>% group_by(NAME) %>% mutate(cap_ecu = ceiling(df_over$ecu_max / ECU), cap_mem = ceiling(df_over$mem_max / MEM), cap = max(cap_ecu, cap_mem), cost = cap * PRICE) #%>% ungroup() %>% filter(cost == min(cost)) %>% sample_n(1)
     df_over_res = df_over_res %>% mutate(over_cost = cost * billing)
     df_over_res = df_over_res %>% select(over_cap = cap, over_flavor = NAME, over_cost)
     
     for (flv in 1:nrow(df_flavor)) {
          
          flavor    = df_flavor[flv, ]
          
          df_demand      = df_usage %>% filter(index != 1) %>% mutate(ecu = (cpu_usage * trace_ghz) / ecu_factor, mem = mem_usage)
          df_demand      = df_demand %>% group_by(index) %>% summarise(ecu_max = max(ecu, na.rm = T), mem_max = max(mem, na.rm = T))
          df_demand      = df_demand %>% group_by(index) %>% mutate(cap_ecu = ceiling(ecu_max / flavor$ECU), cap_mem = ceiling(mem_max / flavor$MEM), cap = max(cap_ecu, cap_mem), price = cap * flavor$PRICE)
          
          df_tmp         = df_demand %>% mutate(ecu_alloc = cap * flavor$ECU, mem_alloc = cap * flavor$MEM) 
          df_viol_all    = df_tmp %>% ungroup() %>% filter((ecu_max > ecu_alloc) | (mem_max > mem_alloc)) %>% summarise(viol = length(unique(index)))
          df_res         = df_tmp %>% ungroup() %>% mutate(len = n()) %>% summarise(cost_total = sum(price), ecu_viol = sum(ecu_max > ecu_alloc), mem_viol = sum(mem_max > mem_alloc), len = nth(len, 1))
          df_res         = df_res %>% mutate(tviol = df_viol_all$viol, p_ecu_viol = ecu_viol / len, p_mem_viol = mem_viol / len, p_viol = tviol / len, jobId = id, flavor = flavor$NAME, metric_base = "multi") 
          df_res         = cbind(data.frame(df_res), data.frame(df_over_res))
          df_res         = df_res %>% mutate(saving = (over_cost - cost_total) / over_cost)
          
          df_result_tmp  = rbind(df_result_tmp, df_res)
          
          df_demand$jobId     = id
          df_demand$flavor    = as.character(flavor$NAME)
          
          df_scaling          = rbind(df_scaling, df_demand)
          
     }     
     
     filename       = paste("../../../proactive/scaling/2m/data/scaling/single/", id, "-scaling-perd-max-single-index-2m.dat", sep = "") 
     write.table(df_scaling, file = filename, row.names = F)
     
     df_result_tmp
}

write.table(df_result, file = "../data/hp/scaling-analysis-MM-SF.dat", row.names = F)

