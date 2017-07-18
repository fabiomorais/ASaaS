begin	<- Sys.time()

library(foreach, quietly = TRUE)
library(doMC, quietly = TRUE)
library(dplyr, quietly = TRUE)

args = commandArgs(trailingOnly = TRUE)

job_id	     = as.character(args[1])
num_cores      = as.integer(args[2])
wcorr          = as.integer(args[3])
pred_type      = "normal"
tmp            = '27042016T212529'
num_period     = 168 #1 semana 2016 / 12
pred_ahead     = 1
trace_ghz      = 1
diff_value     = 300000000

df_ref         = data.frame(cpuRef = c(32, 32, 32, 32), memRef = c(32, 64, 128, 256))

filename       = paste("data/job/", tmp, "/", job_id, "_task_usage_index.csv", sep = "")
task_usage     = read.csv(file = filename, header = T)

# predição sem correção
filename       = paste("data/job/", tmp, "/pred/normal/", job_id, "-estimated-trace-AR-168-normal.dat", sep = "")
df_pred        = read.table(file = filename, header = T)
df_pred$scenario = as.character(df_pred$scenario)

df_usage       = task_usage %>% mutate(index = ((indexslot - 1) %/% 12 ) + 1) %>% select(startTime, maxCpuRate, maxMemory, indexslot, index)
outpur_dir     = paste("data/job/", tmp, "/pred/", pred_type, "/", sep = "")

df_final       = c()

calculate_adj = function(wval, error) {
     
     result = if (wval > 1) { 
                    acf_result     = acf(error, lag.max = length(error) / 2, plot = FALSE)$acf
                    acf_values	= acf_result[2:length(acf_result)]
                    
                    acf_pos	     = acf_values > 0
                    
                    ival		     = which(acf_pos == FALSE, arr.ind = TRUE)[1]
                    
                    init           = if (is.na(ival)) {
                         1
                    }else{
                         ival
                    }
                    
                    acf_value      = acf_values[init:length(acf_values)]
                    acf_index      = which.max(acf_value)
                    
                    acf_index 	= acf_index + init - pred_ahead
                    
                    we			= floor(wval/2)
                    first_index	= max(1, length(error) - acf_index - we)
                    last_index	= min(length(error), length(error) - acf_index + we)
                    
                    adjustment_value = abs(min(0, error[first_index:last_index]))
                    adjustment_value
          }else{
               0
          }
     result     
}

for (rt in 1:nrow(df_ref)) {

     cpuRef    = df_ref[rt, ]$cpuRef
     memRef    = df_ref[rt, ]$memRef
     sc        = paste(cpuRef, memRef, sep = ":")
     
     df_demand = df_usage %>% mutate(cpu_usage = maxCpuRate * cpuRef, mem_usage = maxMemory * memRef)
     df_demand = df_demand %>% group_by(index, indexslot) %>% summarise(startTime = nth(startTime, 1), cpu_usage = sum(cpu_usage), mem_usage = sum(mem_usage))
     
     df_pred_tmp = filter(df_pred, scenario == sc)
     
     init_it   = min(df_demand$index) 
     num_it    = max(df_demand$index)
     
     registerDoMC(cores = num_cores)
     estimated_trace <- foreach(i = init_it:num_it, .combine = rbind) %dopar% {
          
          h_index    = seq(max(init_it, i - (num_period - 1)), i)
          f_index    = i + 1
          
          if (f_index <= num_it) {
               
               tmp_df_trace   = filter(df_demand, index %in% h_index) %>% group_by(index) %>% summarise(startTime = nth(startTime, 1), cpu = max(cpu_usage), mem = max(mem_usage))
               tmp_df_pred    = filter(df_pred_tmp, slot %in% h_index)
               
               tmp_ref_time   = filter(df_demand, index == f_index)$startTime[1] 
               ref_timestamp  = format(tmp_ref_time, scientific = F)
               
               df_pred_raw    = filter(df_pred_tmp, slot == f_index)
               
               df_work        = inner_join(tmp_df_trace, tmp_df_pred, by = c("index" = "slot", "startTime" = "timestamp_ref"))
               df_error       = df_work %>% group_by(index) %>% mutate(diff_cpu = CPU_UTIL - cpu, diff_mem = MEM_UTIL - mem) %>% select(index, timestamp_ref = startTime, scenario, diff_cpu, diff_mem)
               
               df_error$diff_cpu[df_error$diff_cpu >= 0] = 0
               df_error$diff_mem[df_error$diff_mem >= 0] = 0
               
               w              = min(wcorr, nrow(df_error))
               
               adj_cpu        = calculate_adj(w, df_error$diff_cpu)
               adj_mem        = calculate_adj(w, df_error$diff_mem)

               util_core_prediction     = df_pred_raw$CPU_UTIL[1] + adj_cpu
               util_mem_prediction      = df_pred_raw$MEM_UTIL[1] + adj_mem
                    
               prediction_result	= data.frame(slot = f_index, timestamp_ref = ref_timestamp, CPU_UTIL = util_core_prediction, MEM_UTIL = util_mem_prediction)
               prediction_result
          }
     }
     
     estimated_trace$scenario = sc
     
     df_final = rbind(df_final, estimated_trace)
}

file_name	     = paste(outpur_dir, job_id, "-estimated-trace-AR-", num_period, "-", pred_type, "-acf-", wcorr, ".dat", sep = "")
write.table(df_final, file = file_name, row.names = FALSE)

diff_mins	<- difftime(Sys.time(), begin, units = 'secs')
print(paste("Seconds:", diff_mins))
