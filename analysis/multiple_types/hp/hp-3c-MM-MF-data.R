library(dplyr)
library(tidyr)
library(foreach)
library(doMC)

ecu_factor     = 0.7602

jobs           = c(1, 2, 3, 4, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 30, 31, 32, 33, 35, 38, 40)

df_ghz         = read.table(file = "../../../data/normal/machine_data.dat", header = T)

registerDoMC(cores = 8)

df_result      = foreach(tindex = 1:length(jobs), .combine = rbind) %dopar% {      
     
     print(paste(tindex, "/", length(jobs)))
     id = jobs[tindex]
     
     trace_ghz      = filter(df_ghz, TRACE == id)$GHZ
     
     filename       = paste("../../../multiple_types/hp/data/scaling/perfect/", id, "-scaling-perf-max-index.dat", sep = "")
     df_scaling     = read.table(file = filename, header = T) %>% filter(index != 1)
     
     filename       = paste("../../../multiple_types/hp/data/usage/usage-prod-server-", id, "-normal-index.dat", sep = "")
     df_usage       = read.table(file = filename, header = T)
     
     df_usage       = df_usage %>% group_by(indexslot) %>% summarise(cpu_usage = sum(CPU_USAGE), mem_usage = sum(MEM_USAGE)) %>% ungroup() %>% arrange(indexslot)
     df_usage       = df_usage %>% ungroup() %>% mutate(index = ((1:nrow(df_usage) - 1) %/% 12 ) + 1)
     
     df_demand      = df_usage %>% filter(index != 1) %>% mutate(ecu = (cpu_usage * trace_ghz) / ecu_factor, mem = mem_usage)
     df_demand      = df_demand %>% group_by(index) %>% summarise(ecu_max = max(ecu, na.rm = T), mem_max = max(mem, na.rm = T))
     
     df_util        = df_demand
     
     df_tmp         = inner_join(df_util, df_scaling, by = c("index")) %>% select(-vcpu) %>% rename(ecu_alloc = ecu, mem_alloc = mem)
     df_res         = df_tmp %>% mutate(len = n()) %>% summarise(cost_total = sum(cost), ecu_viol = sum(ecu_max > ecu_alloc), mem_viol = sum(mem_max > mem_alloc), len = nth(len, 1))
     df_res         = df_res %>% mutate(p_ecu_viol = ecu_viol / len, p_mem_viol = mem_viol / len, jobId = id, metric_base = "multi") 
     
     df_res
}

write.table(df_result, file = "../data/hp/scaling-analysis-MM-MF.dat", row.names = F)