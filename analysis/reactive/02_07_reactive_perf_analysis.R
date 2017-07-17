library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(plotly)

calculate_cost_value = function(scaling_trace){
     
     tmp     <- scaling_trace$SCALING[2:nrow(scaling_trace)] - scaling_trace$SCALING[1:(nrow(scaling_trace) - 1)]
     tmp     <- tmp[tmp < 0]
     
     cost_value <- sum(scaling_trace$SCALING) + sum(abs(tmp), na.rm = T)	
     cost_value / (60 / 5)	
}


df_scenarios = read.table(file = "data/02_reactive_configurations-reduced.dat", header = T)

horizon      = 2

df_result    = c()
for (index in 1:nrow(df_scenarios)) {
     
     print(paste(index, "/", nrow(df_scenarios)))
     
     scn   = df_scenarios[index, ]  
     
     metric         = scn$METRIC
     trace          = scn$TRACE
     
     file_name      = paste("../../data/normal/", metric, "_util-prod-server-", trace, "-normal.dat", sep = "")
     df_trace       = read.table(file_name, header = T)
     
     file_name      = paste("../../data/normal/", metric, "_alloc-prod-server-", trace, "-normal.dat", sep = "")
     df_alloc       = read.table(file_name, header = T)
     
     metric_util    = paste(toupper(metric), "_UTIL", sep = "") 
     metric_alloc   = paste(toupper(metric), "_ALLOC", sep = "") 
     
     df_util        = data.frame(TIMESTAMP = df_trace$TIMESTAMP, UTIL = (df_trace[ , metric_util] * df_alloc[ , metric_alloc] / 100))
     df_util$TIMESTAMP_STR = as.POSIXct(df_util$TIMESTAMP,  origin = "1970-01-01 00:00:00", tz = "GMT")
     
     filename	  = paste("../../reactive/data/scaling/server-", trace, "-scaling-threshold-", horizon, "-", scn$ADD, "_", scn$VM_ADD, "-", scn$RM, "_",  scn$VM_RM, "-", metric, ".dat", sep = "")
     df_reactive = read.table(file = filename, header = T)  
     
     if (!file.exists(filename)) {
          print(filename)
     }
      
     filename	  = paste("../../reactive/data/perfect/scaling-perfect-", trace, "-1-", metric, ".dat", sep = "")
     df_perf     = read.table(file = filename, header = T)  
     
     df_util_tmp         = filter(df_util, TIMESTAMP >= df_perf$TIMESTAMP[1])
     
     perf_value          = calculate_cost_value(df_perf)
     over_value          = (ceiling(max(df_util_tmp$UTIL)) * nrow(df_util_tmp)) / (60 / 5)
     trace_len           = nrow(df_reactive)
     
     # analise do reativo
     timestamp_viol      = df_util_tmp$TIMESTAMP[df_util_tmp$UTIL > (df_reactive$SCALING * 0.99)]
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
     
     df_result           = rbind(df_result, tmp)     

}

write.table(df_result, file = "data/02_reactive_perf_data.dat", row.names = F)