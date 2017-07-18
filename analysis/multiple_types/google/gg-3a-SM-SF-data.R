library(dplyr)
library(tidyr)
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

instance_time   = 0
grain           = 12
time_decision   = 12
selected_flavor = NA 
selected_cap    = NA

metrics         = c("ecu", "mem")

registerDoMC(cores = 7)

df_result      = foreach(tindex = 1:nrow(jobs), .combine = rbind) %dopar% {      
     
     print(paste(tindex, "/", nrow(jobs)))
     id = jobs[tindex, ]$jobId

     filename       = paste("../../../multiple_types/google/data/job/", tmp, "/", id, "_task_usage_index.csv", sep = "")
     task_usage     = read.csv(file = filename, header = T)
     
     df_usage       = task_usage %>% group_by(indexslot) %>% summarise(cpu_usage = sum(maxCpuRate), mem_usage = sum(maxMemory)) %>% ungroup() %>% arrange(indexslot)
     df_usage       = df_usage %>% ungroup() %>% mutate(index = ((1:nrow(df_usage) - 1) %/% 12 ) + 1)
     
     df_result_tmp  = c()
     df_scaling     = c()
     
     for (r in 1:nrow(ref)) {
          
          rmachine = ref[r, ]
          sc       = paste(rmachine$cpuRef, "_", rmachine$memRef, sep = "")
          
          billing   = length(unique(df_usage$index)) - 1 # desconsidera o primeiro slot
          
          df_over   = df_usage %>% filter(index != 1) %>% mutate(ecu = (cpu_usage * rmachine$cpuRef * trace_ghz) / ecu_factor, mem = mem_usage * rmachine$memRef)
          df_over   = df_over %>% ungroup() %>% summarise(ecu_max = max(ecu, na.rm = T), mem_max = max(mem, na.rm = T))
          
          df_over_res = df_flavor %>% group_by(NAME) %>% mutate(cap_ecu = ceiling(df_over$ecu_max / ECU), cap_mem = ceiling(df_over$mem_max / MEM), cap = max(cap_ecu, cap_mem), cost = cap * PRICE) #%>% ungroup() %>% filter(cost == min(cost)) %>% sample_n(1)
          df_over_res = df_over_res %>% mutate(over_cost = cost * billing)
          df_over_res = df_over_res %>% select(over_cap = cap, over_flavor = NAME, over_cost)
          
          for (flv in 1:nrow(df_flavor)) {
               
               flavor    = df_flavor[flv, ]

               df_demand      = df_usage %>% filter(index != 1) %>% mutate(ecu = (cpu_usage * rmachine$cpuRef * trace_ghz) / ecu_factor, mem = mem_usage * rmachine$memRef)
               df_demand      = df_demand %>% group_by(index) %>% summarise(ecu_max = max(ecu, na.rm = T), mem_max = max(mem, na.rm = T))
               df_demand      = df_demand %>% group_by(index) %>% mutate(cap_ecu = ceiling(ecu_max / flavor$ECU), cap_mem = ceiling(mem_max / flavor$MEM), price_ecu = cap_ecu * flavor$PRICE, price_mem = cap_mem * flavor$PRICE)
               
               
               for (mt in metrics) {
               
                    df_tmp = if (mt == "ecu") {
                         df_demand %>% select(index, ecu_max, mem_max, cap = cap_ecu, price = price_ecu) %>% mutate(ecu_alloc = cap * flavor$ECU, mem_alloc = cap * flavor$MEM) 
                    } else {
                         df_demand %>% select(index, ecu_max, mem_max, cap = cap_mem, price = price_mem) %>% mutate(ecu_alloc = cap * flavor$ECU, mem_alloc = cap * flavor$MEM) 
                    }
                    
                    df_viol_all    = df_tmp %>% ungroup() %>% filter((ecu_max > ecu_alloc) | (mem_max > mem_alloc)) %>% summarise(viol = length(unique(index)))
                    df_res         = df_tmp %>% ungroup() %>% mutate(len = n()) %>% summarise(cost_total = sum(price), ecu_viol = sum(ecu_max > ecu_alloc), mem_viol = sum(mem_max > mem_alloc), len = nth(len, 1))
                    df_res         = df_res %>% mutate(tviol = df_viol_all$viol, p_ecu_viol = ecu_viol / len, p_mem_viol = mem_viol / len, p_viol = tviol / len, jobId = id, scenario = sc, flavor = flavor$NAME, metric_base = mt) 
                    df_res         = cbind(df_res, df_over_res)
                    df_res         = df_res %>% mutate(saving = (over_cost - cost_total) / over_cost)

                    df_result_tmp  = rbind(df_result_tmp, df_res)
               }
               
               df_demand$jobId     = id
               df_demand$scenario  = sc
               df_demand$flavor    = as.character(flavor$NAME)
               
               df_scaling          = rbind(df_scaling, df_demand)
               
          }     
     }
     
     filename       = paste("../../../multiple_types/google/data/job/", tmp, "/scaling/single/", id, "-scaling-perd-max-single-index.dat", sep = "") 
     write.table(df_scaling, file = filename, row.names = F)
     
     df_result_tmp
}

write.table(df_result, file = "data/google/scaling-analysis-SM-SF.dat", row.names = F)

