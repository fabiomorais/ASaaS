library(dplyr)
library(foreach)
library(doMC)

job_error      = read.table(file = "../../multiple_types/google/data/job_without_util.dat", header = T) 

trace_ghz      = 1
ecu_factor     = 0.7602
tmp            = '27042016T212529'
filename       = paste("../../multiple_types/google/data/job/", tmp, "/selected_jobs.dat", sep = "")
jobs           = read.table(file = filename, header = T) 
jobs           = jobs %>% filter(!jobId %in% job_error$jobId)

ref            = data.frame(cpuRef = c(32, 32, 32, 32), memRef = c(32, 64, 128, 256))

registerDoMC(cores = 8)
df_result      = foreach(tindex = 1:nrow(jobs), .combine = rbind) %dopar% {      
  
  print(paste(tindex, "/", nrow(jobs)))
  id = jobs[tindex, ]$jobId
  
  filename       = paste("../../multiple_types/google/data/job/", tmp, "/", id, "_task_usage_index.csv", sep = "")
  task_usage     = read.csv(file = filename, header = T)
  
  df_usage       = task_usage %>% group_by(indexslot) %>% summarise(cpu_usage = sum(maxCpuRate), mem_usage = sum(maxMemory)) %>% ungroup() %>% arrange(indexslot)
  df_usage       = df_usage %>% ungroup() %>% mutate(index = ((1:nrow(df_usage) - 1) %/% 12 ) + 1)
  
  head(df_usage)
  
  df_util        = c()
  for (r in 1:nrow(ref)) {
    
    rmachine       = ref[r, ]
    sc             = paste(rmachine$cpuRef, "_", rmachine$memRef, sep = "")
    
    df_demand      = df_usage %>% mutate(cpu = cpu_usage * rmachine$cpuRef, mem = mem_usage * rmachine$memRef) %>% 
                     group_by(index) %>% summarise(cpu_max = max(cpu, na.rm = T), mem_max = max(mem, na.rm = T)) %>%
                     mutate(ratio = cpu_max / mem_max) %>% 
                     select(index, ratio) %>% mutate(scenario = sc, jobId = id)

    df_util = rbind(df_util, df_demand)
    
  }
  
  df_util
  
}

predominance = df_result %>% inner_join(ref, by = "scenario") %>% filter(!jobId %in% job_error$jobId)
predominance = predominance %>% filter(scP %in% c("1:1", "1:4"))
predominance$scP = factor(predominance$scP, levels = ref$scP)

write.table(predominance, file = "data/google/2a_predominance_ratio-google-data.dat", row.names = F)