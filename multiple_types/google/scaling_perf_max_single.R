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

trace_ghz      = 1
ecu_factor     = 0.7602
tmp            = '27042016T212529'
filename       = paste("data/job/", tmp, "/selected_jobs.dat", sep = "")
jobs           = read.table(file = filename, header = T) 

ref            = data.frame(cpuRef = c(32, 32, 32, 32), memRef = c(32, 64, 128, 256))

instance_time   = 0
grain           = 12
time_decision   = 12
selected_flavor = NA 
selected_cap    = NA

df_single       = read.table(file = "data/scaling_perf_max_single.dat", header =  T)

df_scaling     = c()

for (tindex in 1:nrow(jobs)) {
     
     print(paste(tindex, "/", nrow(jobs)))
     id = jobs[tindex, ]$jobId
     
     filename       = paste("data/job/", tmp, "/", id, "_task_usage_index.csv", sep = "")
     task_usage     = read.csv(file = filename, header = T)
     
     df_usage       = task_usage %>% group_by(indexslot) %>% summarise(cpu_usage = sum(maxCpuRate), mem_usage = sum(maxMemory)) %>% ungroup() %>% arrange(indexslot)
     
     for (r in 1:nrow(ref)) {
          
          rmachine = ref[r, ]
          
          scenario       = paste(rmachine$cpuRef, "_", rmachine$memRef, sep = "")
          df_usage       = df_usage %>% ungroup() %>% mutate(index = ((1:nrow(df_usage) - 1) %/% 12 ) + 1)
          df_demand      = df_usage %>% filter(index != 1) %>% mutate(ecu = (cpu_usage * rmachine$cpuRef * trace_ghz) / ecu_factor, mem = mem_usage * rmachine$memRef)
          
          billing_slots  = length(unique(df_demand$index))
          
          dftmp1         = df_demand %>% summarise(ecu_max = max(ecu, na.rm = T), mem_max = max(mem, na.rm = T))
          fv             = df_flavor %>% group_by(NAME) %>% mutate(cap_ecu = ceiling( dftmp1$ecu_max / ECU), cap_mem =  ceiling(dftmp1$mem_max / MEM)) %>% mutate(cap = max(cap_ecu, cap_mem)) %>% mutate(val = cap * PRICE) %>% ungroup() %>% filter(val == min(val))
          
          dfx            = data.frame(scenario = scenario, jobId = id, flavor = fv$NAME, cost = fv$val * billing_slots, scaling = fv$cap, vcpu = fv$VCPU * fv$cap, ecu = fv$ECU * fv$cap, mem = fv$MEM * fv$cap )
          dfx$scenario   = scenario
          
          df_scaling     = rbind(df_scaling, dfx)   
     }
}

write.table(df_scaling, file = "data/scaling_perf_max_single.dat", row.names = F)