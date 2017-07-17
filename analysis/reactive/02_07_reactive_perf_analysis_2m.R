library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(plotly)
library(foreach)
library(doMC)

calculate_cost_value = function(scaling_trace){
     
     tmp     <- scaling_trace$SCALING[2:nrow(scaling_trace)] - scaling_trace$SCALING[1:(nrow(scaling_trace) - 1)]
     tmp     <- tmp[tmp < 0]
     
     cost_value <- sum(scaling_trace$SCALING) + sum(abs(tmp), na.rm = T)	
     cost_value / (60 / 5)	
}


df_scenarios = read.table(file = "data/02_reactive_configurations-2m.dat", header = T)

traces       = unique(df_scenarios$TRACE)
horizon      = 2

df_result    = c()
df_error     = c()

for (trace in traces) {
     
     print(trace)
     
     # CPU data
     metric         = "cpu"
     file_name      = paste("../../data/normal/", metric, "_util-prod-server-", trace, "-normal.dat", sep = "")
     df_trace       = read.table(file_name, header = T)
     
     file_name      = paste("../../data/normal/", metric, "_alloc-prod-server-", trace, "-normal.dat", sep = "")
     df_alloc       = read.table(file_name, header = T)
     
     df_util_cpu    = data.frame(TIMESTAMP = df_trace$TIMESTAMP, USAGE_CPU = (df_trace$CPU_UTIL * df_alloc$CPU_ALLOC / 100))
     df_util_cpu$TIMESTAMP_STR = as.POSIXct(df_util_cpu$TIMESTAMP,  origin = "1970-01-01 00:00:00", tz = "GMT")
     
     # MEM data
     metric         = "mem"
     file_name      = paste("../../data/normal/", metric, "_util-prod-server-", trace, "-normal.dat", sep = "")
     df_trace       = read.table(file_name, header = T)
     
     file_name      = paste("../../data/normal/", metric, "_alloc-prod-server-", trace, "-normal.dat", sep = "")
     df_alloc       = read.table(file_name, header = T)
     
     df_util_mem    = data.frame(TIMESTAMP = df_trace$TIMESTAMP, USAGE_MEM = (df_trace$MEM_UTIL * df_alloc$MEM_ALLOC / 100))
     df_util_mem$TIMESTAMP_STR = as.POSIXct(df_util_mem$TIMESTAMP,  origin = "1970-01-01 00:00:00", tz = "GMT")
     
     df_util        = inner_join(df_util_cpu, df_util_mem, by = c("TIMESTAMP", "TIMESTAMP_STR")) %>% select(TIMESTAMP, TIMESTAMP_STR, USAGE_CPU, USAGE_MEM)
     
     # Dados do perfeito
     filename	     = paste("../../reactive/data/perfect/scaling-perfect-", trace, "-1-2m.dat", sep = "")
     df_perf        = read.table(file = filename, header = T)  
     
     df_util_tmp    = filter(df_util, TIMESTAMP >= df_perf$TIMESTAMP[1])
     
     perf_value     = calculate_cost_value(df_perf)
     over_value     = (ceiling(max(df_util_tmp$USAGE_CPU, df_util_tmp$USAGE_MEM)) * nrow(df_util_tmp))/ (60 / 5)
     
     df_scn_tmp     = df_scenarios %>% filter(TRACE == trace)
     
     indexes        = 1:nrow(df_scn_tmp)
     
     registerDoMC(cores = 8)
     
     x = foreach(index = 1:nrow(df_scn_tmp), .combine = "rbind") %dopar% {
          
          scn            = df_scn_tmp[index, ]  
          
          metric         = scn$METRIC
          trace          = scn$TRACE
          
          filename	  = paste("../../reactive/data/scaling/server-", trace, "-scaling-threshold-", horizon, "-", scn$ADD_CPU, "-", scn$ADD_VM_CPU, "-", scn$ADD_MEM, "-", scn$ADD_VM_MEM, "-", 
                             scn$RM_CPU, "-", scn$RM_VM_CPU, "-", scn$RM_MEM, "-", scn$RM_VM_MEM, "-2m.dat", sep = "")
          df_reactive = read.table(file = filename, header = T)  
          
          head(df_util)
          head(df_reactive)
          
          trace_len           = nrow(df_util_tmp)

          # analise do reativo
          timestamp_viol_cpu  = df_util_tmp$TIMESTAMP[df_util_tmp$USAGE_CPU > (df_reactive$SCALING * 0.99)]
          timestamp_viol_mem  = df_util_tmp$TIMESTAMP[df_util_tmp$USAGE_MEM > (df_reactive$SCALING * 0.99)]
          
          timestamp_viol      = unique(c(timestamp_viol_cpu, timestamp_viol_mem))
          hv_value            = length(timestamp_viol)
          hvp_value           = hv_value / nrow(df_util_tmp)

          cost_value          = calculate_cost_value(df_reactive)
          saving_rel          = ((cost_value - over_value) / over_value)
          cost_rel            = ((cost_value - perf_value) / perf_value)
          
          tmp                 = scn %>% mutate(TYPE = "reactive", TRACE_LEN = trace_len, 
                                               VIOL = hv_value, VIOLP = hvp_value, 
                                               COST = cost_value, PERF_COST = perf_value, 
                                               OVER_COST = over_value, PERF_REL = cost_rel, 
                                               OVER_REL = saving_rel)
          tmp
     }
     
     df_result = rbind(df_result, x)
}

write.table(df_result, file = "data/trace_conf_sweep-2m.dat", row.names = F)