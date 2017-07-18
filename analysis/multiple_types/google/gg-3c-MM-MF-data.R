library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(foreach)
library(doMC)

job_error      = read.table(file = "../../../multiple_types/google/data/job_without_util.dat", header = T) 


trace_ghz      = 1
ecu_factor     = 0.7602
tmp            = '27042016T212529'
filename       = paste("../../../multiple_types/google/data/job/", tmp, "/selected_jobs.dat", sep = "")
jobs           = read.table(file = filename, header = T) 
jobs           = jobs %>% filter(!jobId %in% job_error$jobId)

ref            = data.frame(cpuRef = c(32, 32, 32, 32), memRef = c(32, 64, 128, 256))

registerDoMC(cores = 7)

# for(tindex in 1:nrow(jobs)) {
df_result      = foreach(tindex = 1:nrow(jobs), .combine = rbind) %dopar% {      
     
     print(paste(tindex, "/", nrow(jobs)))
     id = jobs[tindex, ]$jobId
     
     filename       = paste("../../../multiple_types/google/data/job/", tmp, "/scaling/perfect/", id, "-scaling-perf-max-index.dat", sep = "")
     df_scaling     = read.table(file = filename, header = T) %>% filter(index != 1)
     
     filename       = paste("../../../multiple_types/google/data/job/", tmp, "/", id, "_task_usage_index.csv", sep = "")
     task_usage     = read.csv(file = filename, header = T)
     
     df_usage       = task_usage %>% group_by(indexslot) %>% summarise(cpu_usage = sum(maxCpuRate), mem_usage = sum(maxMemory)) %>% ungroup() %>% arrange(indexslot)
     df_usage       = df_usage %>% ungroup() %>% mutate(index = ((1:nrow(df_usage) - 1) %/% 12 ) + 1)
     
     df_util        = c()
     for (r in 1:nrow(ref)) {
          
          rmachine = ref[r, ]
          sc       = paste(rmachine$cpuRef, "_", rmachine$memRef, sep = "")
          
          df_demand      = df_usage %>% filter(index != 1) %>% mutate(ecu = (cpu_usage * rmachine$cpuRef * trace_ghz) / ecu_factor, mem = mem_usage * rmachine$memRef)
          df_demand      = df_demand %>% group_by(index) %>% summarise(ecu_max = max(ecu, na.rm = T), mem_max = max(mem, na.rm = T))
          df_demand$scenario = sc
          
          df_util = rbind(df_util, df_demand)
          
     }
     
     df_tmp         = inner_join(df_util, df_scaling, by = c("scenario", "index")) %>% select(-vcpu) %>% rename(ecu_alloc = ecu, mem_alloc = mem)
     df_res         = df_tmp %>% group_by(scenario) %>% mutate(len = n()) %>% summarise(cost_total = sum(cost), ecu_viol = sum(ecu_max > ecu_alloc), mem_viol = sum(mem_max > mem_alloc), len = nth(len, 1))
     df_res         = df_res %>% mutate(p_ecu_viol = ecu_viol / len, p_mem_viol = mem_viol / len, jobId = id, metric_base = "multi") 
     
     df_res
}

write.table(df_result, file = "../data/google/scaling-analysis-MM-MF.dat", row.names = F)