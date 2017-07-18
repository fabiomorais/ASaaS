library(dplyr)
library(stringr)
library(ggplot2)
library(scales)

trace_ghz      = 1
ecu_factor     = 0.7602
tmp            = '27042016T212529'
filename       = paste("data/job/", tmp, "/selected_jobs.dat", sep = "")
jobs           = read.table(file = filename, header = T) 

job_error      = read.table(file = "data/job_without_util.dat", header = T) 

df_final       = c()
df_single      = read.table(file = "data/scaling_perf_max_single.dat", header =  T)
df_single      = df_single %>% select(scenario, over_cost = cost, jobid = jobId) %>% distinct() %>% filter(!jobid %in% job_error$jobId)
df_single      = df_single %>% mutate(scenario = as.character(scenario))

for (tindex in 1:1) { #nrow(jobs)) {
     
     print(paste(tindex, "/", nrow(jobs)))
     id = jobs[tindex, ]$jobId
     
     #read original trace
     filename            = paste("data/job/", tmp, "/", id, "_task_usage_index.csv", sep = "")
     task_usage          = read.csv(file = filename, header = T)
     
     df_usage            = task_usage %>% group_by(indexslot) %>% summarise(cpu_usage = sum(maxCpuRate), mem_usage = sum(maxMemory)) %>% ungroup() %>% arrange(indexslot)
     df_demand           = df_usage %>% mutate(index = ((1:nrow(df_usage) - 1) %/% 12 ) + 1) %>% group_by(index) %>% summarise(cpu_max = max(cpu_usage, na.rm = T), mem_max = max(mem_usage, na.rm = T)) 
     df_demand           = df_demand %>% filter(index != 1)
     
     # read perf scaling
     filename            = paste("data/job/", tmp, "/scaling/perfect/", id, "-scaling-perf-max-index.dat", sep = "")
     df_scaling_perf     = read.table(file = filename, header = T)
     df_scaling_perf     = df_scaling_perf %>% mutate(str_index = regexpr("_", scenario)[1]) %>% mutate(cpuRef = as.numeric(substr(scenario, 1, (str_index - 1))), memRef = as.numeric(substr(scenario, (str_index + 1), nchar(as.character(scenario)))))
     df_scaling_perf     = df_scaling_perf %>% select(-str_index)
     
     df_all              = inner_join(df_scaling_perf, df_demand, by = "index") %>% mutate(ecu_demand = (cpu_max * cpuRef * trace_ghz) / ecu_factor, mem_demand = mem_max * memRef)
     df_result_perf      = df_all %>% ungroup() %>% group_by(scenario) %>% mutate(len = n()) %>% summarise(cost_total = sum(cost), ecu_viol = sum(ecu_demand > ecu), mem_viol = sum(mem_demand > mem), len = nth(len, 1))
     df_result_perf      = df_result_perf %>% group_by(scenario) %>% mutate(p_ecu_viol = ecu_viol / len, p_mem_viol = mem_viol / len, jobid = id, tviol = sum(ecu_viol , mem_viol), pviol = tviol / len)
     df_result_perf$type = "perfect"
     df_result_perf$tgt = 1
     df_final            = rbind(df_final, df_result_perf)
     
     # read pred scaling
     filename            = paste("data/job/", tmp, "/scaling/normal/", id, "-scaling-pred-max-index.dat", sep = "")
     df_scaling_pred     = read.table(file = filename, header = T)
     df_scaling_pred$scenario = str_replace(df_scaling_pred$scenario, ":", "_")
     df_scaling_pred     = df_scaling_pred %>% mutate(str_index = regexpr("_", scenario)[1]) %>% mutate(cpuRef = as.numeric(substr(scenario, 1, (str_index - 1))), memRef = as.numeric(substr(scenario, (str_index + 1), nchar(as.character(scenario)))))
     df_scaling_pred     = df_scaling_pred %>% select(-str_index)
     
     df_all              = inner_join(df_scaling_pred, df_demand, by = "index") %>% mutate(ecu_demand = (cpu_max * cpuRef * trace_ghz) / ecu_factor, mem_demand = mem_max * memRef)
     df_viol_pred        = df_all %>% ungroup() %>% group_by(scenario) %>% filter((ecu_demand > ecu) | (mem_demand > mem)) %>% summarise(viol = length(unique(index)))
     df_result_pred      = df_all %>% ungroup() %>% group_by(scenario) %>% mutate(len = n()) %>% summarise(cost_total = sum(cost), ecu_viol = sum(ecu_demand > ecu), mem_viol = sum(mem_demand > mem), len = nth(len, 1))
     df_result_pred      = df_result_pred %>% mutate(p_ecu_viol = ecu_viol / len, p_mem_viol = mem_viol / len, jobid = id)
     df_result_pred      = df_result_pred %>% full_join(df_viol_pred, by = "scenario") %>% group_by(scenario) %>% mutate(tviol = min(viol, sum(ecu_viol, mem_viol), na.rm = T)) %>% mutate(pviol = tviol / len) %>% select(-viol)
     
     df_result_pred$type = "ar"
     df_result_pred$tgt  = 1
     df_final            = rbind(df_final, df_result_pred)
     
     #read pred scaling com target de 100 e acf 168
     filename            = paste("data/job/", tmp, "/scaling/normal/", id, "-scaling-pred-max-index-1-acf-168.dat", sep = "")
     df_scaling_pred     = read.table(file = filename, header = T)
     df_scaling_pred$scenario = str_replace(df_scaling_pred$scenario, ":", "_")
     df_scaling_pred     = df_scaling_pred %>% mutate(str_index = regexpr("_", scenario)[1]) %>% mutate(cpuRef = as.numeric(substr(scenario, 1, (str_index - 1))), memRef = as.numeric(substr(scenario, (str_index + 1), nchar(as.character(scenario)))))
     df_scaling_pred     = df_scaling_pred %>% select(-str_index)
     
     df_all              = inner_join(df_scaling_pred, df_demand, by = "index") %>% mutate(ecu_demand = (cpu_max * cpuRef * trace_ghz) / ecu_factor, mem_demand = mem_max * memRef)
     df_viol_pred        = df_all %>% ungroup() %>% group_by(scenario) %>% filter((ecu_demand > ecu) | (mem_demand > mem)) %>% summarise(viol = length(unique(index)))
     df_result_pred      = df_all %>% ungroup() %>% group_by(scenario) %>% mutate(len = n()) %>% summarise(cost_total = sum(cost), ecu_viol = sum(ecu_demand > ecu), mem_viol = sum(mem_demand > mem), len = nth(len, 1))
     df_result_pred      = df_result_pred %>% mutate(p_ecu_viol = ecu_viol / len, p_mem_viol = mem_viol / len, jobid = id)
     df_result_pred      = df_result_pred %>% full_join(df_viol_pred, by = "scenario") %>% group_by(scenario) %>% mutate(tviol = min(viol, sum(ecu_viol, mem_viol), na.rm = T)) %>% mutate(pviol = tviol / len) %>% select(-viol)
     
     df_result_pred$type = "ar_acf_168"
     df_result_pred$tgt  = 1
     df_final            = rbind(df_final, df_result_pred)
     
     #read pred scaling com target de 100 e acf 84
     filename            = paste("data/job/", tmp, "/scaling/normal/", id, "-scaling-pred-max-index-1-acf-84.dat", sep = "")
     df_scaling_pred     = read.table(file = filename, header = T)
     df_scaling_pred$scenario = str_replace(df_scaling_pred$scenario, ":", "_")
     df_scaling_pred     = df_scaling_pred %>% mutate(str_index = regexpr("_", scenario)[1]) %>% mutate(cpuRef = as.numeric(substr(scenario, 1, (str_index - 1))), memRef = as.numeric(substr(scenario, (str_index + 1), nchar(as.character(scenario)))))
     df_scaling_pred     = df_scaling_pred %>% select(-str_index)
     
     df_all              = inner_join(df_scaling_pred, df_demand, by = "index") %>% mutate(ecu_demand = (cpu_max * cpuRef * trace_ghz) / ecu_factor, mem_demand = mem_max * memRef)
     df_viol_pred        = df_all %>% ungroup() %>% group_by(scenario) %>% filter((ecu_demand > ecu) | (mem_demand > mem)) %>% summarise(viol = length(unique(index)))
     df_result_pred      = df_all %>% ungroup() %>% group_by(scenario) %>% mutate(len = n()) %>% summarise(cost_total = sum(cost), ecu_viol = sum(ecu_demand > ecu), mem_viol = sum(mem_demand > mem), len = nth(len, 1))
     df_result_pred      = df_result_pred %>% mutate(p_ecu_viol = ecu_viol / len, p_mem_viol = mem_viol / len, jobid = id)
     df_result_pred      = df_result_pred %>% full_join(df_viol_pred, by = "scenario") %>% group_by(scenario) %>% mutate(tviol = min(viol, sum(ecu_viol, mem_viol), na.rm = T)) %>% mutate(pviol = tviol / len) %>% select(-viol)
     
     df_result_pred$type = "ar_acf_84"
     df_result_pred$tgt  = 1
     df_final            = rbind(df_final, df_result_pred)
     
     #read pred scaling com target de 100 e acf 42
     filename            = paste("data/job/", tmp, "/scaling/normal/", id, "-scaling-pred-max-index-1-acf-42.dat", sep = "")
     df_scaling_pred     = read.table(file = filename, header = T)
     df_scaling_pred$scenario = str_replace(df_scaling_pred$scenario, ":", "_")
     df_scaling_pred     = df_scaling_pred %>% mutate(str_index = regexpr("_", scenario)[1]) %>% mutate(cpuRef = as.numeric(substr(scenario, 1, (str_index - 1))), memRef = as.numeric(substr(scenario, (str_index + 1), nchar(as.character(scenario)))))
     df_scaling_pred     = df_scaling_pred %>% select(-str_index)
     
     df_all              = inner_join(df_scaling_pred, df_demand, by = "index") %>% mutate(ecu_demand = (cpu_max * cpuRef * trace_ghz) / ecu_factor, mem_demand = mem_max * memRef)
     df_viol_pred        = df_all %>% ungroup() %>% group_by(scenario) %>% filter((ecu_demand > ecu) | (mem_demand > mem)) %>% summarise(viol = length(unique(index)))
     df_result_pred      = df_all %>% ungroup() %>% group_by(scenario) %>% mutate(len = n()) %>% summarise(cost_total = sum(cost), ecu_viol = sum(ecu_demand > ecu), mem_viol = sum(mem_demand > mem), len = nth(len, 1))
     df_result_pred      = df_result_pred %>% mutate(p_ecu_viol = ecu_viol / len, p_mem_viol = mem_viol / len, jobid = id) 
     df_result_pred      = df_result_pred %>% full_join(df_viol_pred, by = "scenario") %>% group_by(scenario) %>% mutate(tviol = min(viol, sum(ecu_viol, mem_viol), na.rm = T)) %>% mutate(pviol = tviol / len) %>% select(-viol)
     
     df_result_pred$type = "ar_acf_42"
     df_result_pred$tgt  = 1
     
     df_final            = rbind(df_final, df_result_pred)
}

df_correct     = df_final %>% ungroup() %>% filter(!jobid %in% job_error$jobId)
df_correct     = df_correct %>% inner_join(df_single, by = c("jobid", "scenario"))
df_analysis    = df_correct %>% mutate(saving = (over_cost - cost_total) / over_cost)

write.table(df_analysis, file = "data/scaling-general-data.dat", row.names = F)