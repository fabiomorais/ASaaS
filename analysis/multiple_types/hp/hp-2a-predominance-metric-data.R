library(dplyr)
library(foreach)
library(doMC)

ecu_factor     = 0.7602
jobs           = c(1, 2, 3, 4, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 30, 31, 32, 33, 35, 38, 40)

registerDoMC(cores = 8)
df_result      = foreach(tindex = 1:length(jobs), .combine = rbind) %dopar% {      
     
     print(paste(tindex, "/", length(jobs)))
     id = jobs[tindex]
     
     filename       = paste("../../multiple_types/hp/data/usage/usage-prod-server-", id, "-normal-index.dat", sep = "")
     df_usage       = read.table(file = filename, header = T)
     
     df_usage       = df_usage %>% group_by(indexslot) %>% summarise(cpu_usage = sum(CPU_USAGE), mem_usage = sum(MEM_USAGE)) %>% ungroup() %>% arrange(indexslot)
     df_usage       = df_usage %>% ungroup() %>% mutate(index = ((1:nrow(df_usage) - 1) %/% 12 ) + 1)
     
     df_demand      = df_usage %>% mutate(cpu = cpu_usage, mem = mem_usage) %>% 
          group_by(index) %>% summarise(cpu_max = max(cpu, na.rm = T), mem_max = max(mem, na.rm = T)) %>%
          mutate(ratio = cpu_max / mem_max) %>% 
          select(index, ratio) %>% mutate(jobId = id)
     
     df_demand
     
}

write.table(df_result, file = "data/hp/2a_predominance_ratio-hp-data.dat", row.names = F)