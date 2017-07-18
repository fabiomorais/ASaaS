begin	<- Sys.time()

library(foreach, quietly = TRUE)
library(doMC, quietly = TRUE)
library(dplyr, quietly = TRUE)

args = commandArgs(trailingOnly = TRUE)

job_id	     = as.character(args[1])
num_cores      = as.integer(args[2])
pred_type      = "normal"
tmp            = '27042016T212529'
num_period     = 168 #1 semana 2016 / 12
pred_ahead     = 1
trace_ghz      = 1
diff_value     = 300000000

df_ref         = data.frame(cpuRef = c(32, 32, 32, 32), memRef = c(32, 64, 128, 256))

filename       = paste("data/job/", tmp, "/", job_id, "_task_usage_index.csv", sep = "")
task_usage     = read.csv(file = filename, header = T)

df_usage       = task_usage %>% mutate(index = ((indexslot - 1) %/% 12 ) + 1) %>% select(startTime, maxCpuRate, maxMemory, indexslot, index)

outpur_dir     = paste("data/job/", tmp, "/pred/", pred_type, "/", sep = "")

df_final       = c()

for (rt in 1:nrow(df_ref)) {
     
     cpuRef    = df_ref[rt, ]$cpuRef
     memRef    = df_ref[rt, ]$memRef
     scenario  = paste(cpuRef, memRef, sep = ":")
     
     df_demand = df_usage %>% mutate(cpu_usage = maxCpuRate * cpuRef, mem_usage = maxMemory * memRef)
     df_demand = df_demand %>% group_by(index, indexslot) %>% summarise(startTime = nth(startTime, 1), cpu_usage = sum(cpu_usage), mem_usage = sum(mem_usage))
     
     init_it   = min(df_demand$index) 
     num_it    = max(df_demand$index)
     
     registerDoMC(cores = num_cores)
     estimated_trace <- foreach(i = init_it:num_it, .combine = rbind) %dopar% {
          
          h_index    = seq(max(init_it, i - (num_period - 1)), i)
          f_index    = i + 1
          
          if (f_index <= num_it) {
               
               tmp_df_trace   = filter(df_demand, index %in% h_index) %>% group_by(index) %>% summarise(startTime = nth(startTime, 1), cpu = max(cpu_usage), mem = max(mem_usage))
               
               tmp_ref_time   = filter(df_demand, index == f_index)$startTime[1] 
               ref_timestamp  = format(tmp_ref_time, scientific = F)
               
               ts_util_core	= ts(tmp_df_trace$cpu)
               ts_util_mem	= ts(tmp_df_trace$mem)
               
               util_core_prediction  = NA
               util_mem_prediction   = NA
               
               if (!is.na(var(ts_util_core)) & var(ts_util_core) != 0) {
                    
                    order_max		          =  max(1, min(num_period, length(ts_util_core) - 1))
                    ar_util_core		     =  ar(ts_util_core, aic = T, order.max = order_max)
                    util_core_prediction	=  predict(ar_util_core, n.ahead = pred_ahead)$pred
                    
               } else {
                    
                    print("zero variance")
                    util_core_prediction 	= tmp_df_trace$cpu[length(tmp_df_trace$cpu)]
                    
               }
               
               if (!is.na(var(ts_util_mem)) & var(ts_util_mem) != 0) {
                    
                    order_max                =  max(1, min(num_period, length(ts_util_mem) - 1))
                    ar_util_mem              =  ar(ts_util_mem, aic = T, order.max = order_max)
                    util_mem_prediction      =  predict(ar_util_mem, n.ahead = pred_ahead)$pred
                    
               } else {
                    print("zero variance")
                    util_mem_prediction 	= tmp_df_trace$mem[length(tmp_df_trace$mem)]
               }
               
               prediction_result	= data.frame(slot = f_index, timestamp_ref = ref_timestamp,  CPU_UTIL = util_core_prediction, MEM_UTIL = util_mem_prediction)
               prediction_result
          }
     }
     
     estimated_trace$scenario = scenario
     
     df_final = rbind(df_final, estimated_trace)
     
}

file_name	     = paste(outpur_dir, job_id, "-estimated-trace-AR-", num_period, "-", pred_type, ".dat", sep = "")
write.table(df_final, file = file_name, row.names = FALSE)

diff_mins	<- difftime(Sys.time(), begin, units = 'secs')
print(paste("Seconds:", diff_mins))