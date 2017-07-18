library(dplyr)
library(stringr)
library(ggplot2)
library(scales)

ecu_factor     = 0.7602

jobs           = c(1, 2, 3, 4, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 30, 31, 32, 33, 35, 38, 40)

df_ghz         = read.table(file = "../../data/normal/machine_data.dat", header = T)

df_final       = c()
df_single      = read.table(file = "data/scaling_perf_max_single.dat", header =  T)
df_single      = df_single %>% select(over_cost = cost, jobid = jobId) %>% distinct() 

for (tindex in 1:length(jobs)) {
     
     print(paste(tindex, "/", length(jobs)))
     
     id = jobs[tindex]
     
     trace_ghz      = filter(df_ghz, TRACE == id)$GHZ
     
     #read original trace
     filename       = paste("data/usage/usage-prod-server-", id, "-normal-index.dat", sep = "")
     df_usage       = read.table(file = filename, header = T)
     
     df_usage            = df_usage %>% group_by(indexslot) %>% summarise(cpu_usage = sum(CPU_USAGE), mem_usage = sum(MEM_USAGE)) %>% ungroup() %>% arrange(indexslot)
     df_demand           = df_usage %>% mutate(index = ((1:nrow(df_usage) - 1) %/% 12 ) + 1) %>% group_by(index) %>% summarise(cpu_max = max(cpu_usage, na.rm = T), mem_max = max(mem_usage, na.rm = T)) 
     df_demand           = df_demand %>% filter(index != 1)


     #read pred scaling com target de 100
     filename            = paste("data/scaling/normal/", id, "-scaling-pred-max-index-1.dat", sep = "")
     df_scaling_pred     = read.table(file = filename, header = T)
     
     df_all              = inner_join(df_scaling_pred, df_demand, by = "index") %>% mutate(ecu_demand = (cpu_max * trace_ghz) / ecu_factor, mem_demand = mem_max)
     
     df_viol_pred        = df_all %>% ungroup() %>% filter((ecu_demand > ecu) | (mem_demand > mem)) %>% summarise(viol = length(unique(index)))
     df_result_pred      = df_all %>% ungroup() %>% mutate(len = n()) %>% summarise(cost_total = sum(cost), ecu_viol = sum(ecu_demand > ecu), mem_viol = sum(mem_demand > mem), len = nth(len, 1))
     df_result_pred      = df_result_pred %>% mutate(p_ecu_viol = ecu_viol / len, p_mem_viol = mem_viol / len, jobid = id)
     df_result_pred      = df_result_pred %>% cbind(df_viol_pred) %>% mutate(tviol = min(viol, sum(ecu_viol, mem_viol), na.rm = T)) %>% mutate(pviol = tviol / len) %>% select(-viol)
     
     df_result_pred$type = "ar"
     df_result_pred$tgt  = 1
     
     df_final            = rbind(df_final, df_result_pred)
     
     #read pred scaling com target de 100 e acf 168
     filename            = paste("data/scaling/normal/", id, "-scaling-pred-max-index-1-acf-168.dat", sep = "")
     df_scaling_pred     = read.table(file = filename, header = T)
     
     df_all              = inner_join(df_scaling_pred, df_demand, by = "index") %>% mutate(ecu_demand = (cpu_max * trace_ghz) / ecu_factor, mem_demand = mem_max)
     
     df_viol_pred        = df_all %>% ungroup() %>% filter((ecu_demand > ecu) | (mem_demand > mem)) %>% summarise(viol = length(unique(index)))
     df_result_pred      = df_all %>% ungroup() %>% mutate(len = n()) %>% summarise(cost_total = sum(cost), ecu_viol = sum(ecu_demand > ecu), mem_viol = sum(mem_demand > mem), len = nth(len, 1))
     df_result_pred      = df_result_pred %>% mutate(p_ecu_viol = ecu_viol / len, p_mem_viol = mem_viol / len, jobid = id)
     df_result_pred      = df_result_pred %>% cbind(df_viol_pred) %>% mutate(tviol = min(viol, sum(ecu_viol, mem_viol), na.rm = T)) %>% mutate(pviol = tviol / len) %>% select(-viol)

     df_result_pred$type = "ar_acf_168"
     df_result_pred$tgt  = 1

     df_final            = rbind(df_final, df_result_pred)
     
     #read pred scaling com target de 100 e acf 84
     filename            = paste("data/scaling/normal/", id, "-scaling-pred-max-index-1-acf-84.dat", sep = "")
     df_scaling_pred     = read.table(file = filename, header = T)
     
     df_all              = inner_join(df_scaling_pred, df_demand, by = "index") %>% mutate(ecu_demand = (cpu_max * trace_ghz) / ecu_factor, mem_demand = mem_max)
     
     df_viol_pred        = df_all %>% ungroup() %>% filter((ecu_demand > ecu) | (mem_demand > mem)) %>% summarise(viol = length(unique(index)))
     df_result_pred      = df_all %>% ungroup() %>% mutate(len = n()) %>% summarise(cost_total = sum(cost), ecu_viol = sum(ecu_demand > ecu), mem_viol = sum(mem_demand > mem), len = nth(len, 1))
     df_result_pred      = df_result_pred %>% mutate(p_ecu_viol = ecu_viol / len, p_mem_viol = mem_viol / len, jobid = id)
     df_result_pred      = df_result_pred %>% cbind(df_viol_pred) %>% mutate(tviol = min(viol, sum(ecu_viol, mem_viol), na.rm = T)) %>% mutate(pviol = tviol / len) %>% select(-viol)
     
     df_result_pred$type = "ar_acf_84"
     df_result_pred$tgt  = 1
     
     df_final            = rbind(df_final, df_result_pred)
     
     #read pred scaling com target de 100 e acf 42
     filename            = paste("data/scaling/normal/", id, "-scaling-pred-max-index-1-acf-42.dat", sep = "")
     df_scaling_pred     = read.table(file = filename, header = T)
     
     df_all              = inner_join(df_scaling_pred, df_demand, by = "index") %>% mutate(ecu_demand = (cpu_max * trace_ghz) / ecu_factor, mem_demand = mem_max)
     
     df_viol_pred        = df_all %>% ungroup() %>% filter((ecu_demand > ecu) | (mem_demand > mem)) %>% summarise(viol = length(unique(index)))
     df_result_pred      = df_all %>% ungroup() %>% mutate(len = n()) %>% summarise(cost_total = sum(cost), ecu_viol = sum(ecu_demand > ecu), mem_viol = sum(mem_demand > mem), len = nth(len, 1))
     df_result_pred      = df_result_pred %>% mutate(p_ecu_viol = ecu_viol / len, p_mem_viol = mem_viol / len, jobid = id)
     df_result_pred      = df_result_pred %>% cbind(df_viol_pred) %>% mutate(tviol = min(viol, sum(ecu_viol, mem_viol), na.rm = T)) %>% mutate(pviol = tviol / len) %>% select(-viol)
     
     df_result_pred$type = "ar_acf_42"
     df_result_pred$tgt  = 1
     
     df_final            = rbind(df_final, df_result_pred)
     
}

df_correct     = df_final %>% inner_join(df_single, by = c("jobid"))
df_analysis    = df_correct %>% mutate(saving = (over_cost - cost_total) / over_cost)

write.table(df_analysis, file = "data/scaling-general-data.dat", row.names = F)
