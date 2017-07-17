library(dplyr)

calculate_cost_value = function(scaling_trace){
     
     tmp     <- scaling_trace$SCALING[2:nrow(scaling_trace)] - scaling_trace$SCALING[1:(nrow(scaling_trace) - 1)]
     tmp     <- tmp[tmp < 0]
     
     cost_value <- sum(scaling_trace$SCALING) + sum(abs(tmp), na.rm = T)	
     cost_value / (60 / 5)	
}

traces = c(1, 2, 3, 4, 8, 10, 11, 12 ,13 ,14 ,15 ,16 ,17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 30, 31, 32, 33, 35, 38, 40)
df_result = c()

for (metric in c("cpu", "mem")) { 
     
     for (trace in traces) {
          
          print(paste(trace, metric))
          
          file_name      = paste("../../data/normal/", metric, "_util-prod-server-", trace, "-normal.dat", sep = "")
          df_trace       = read.table(file_name, header = T)
          
          file_name      = paste("../../data/normal/", metric, "_alloc-prod-server-", trace, "-normal.dat", sep = "")
          df_alloc       = read.table(file_name, header = T)
          
          metric_util    = paste(toupper(metric), "_UTIL", sep = "") 
          metric_alloc   = paste(toupper(metric), "_ALLOC", sep = "") 
          
          df_util        = data.frame(TIMESTAMP = df_trace$TIMESTAMP, UTIL = (df_trace[ , metric_util] * df_alloc[ , metric_alloc] / 100))
          df_util$TIMESTAMP_STR = as.POSIXct(df_util$TIMESTAMP,  origin = "1970-01-01 00:00:00", tz = "GMT")
          
          filename	  = paste("../../reactive/data/perfect/scaling-perfect-", trace, "-1-", metric, ".dat", sep = "")
          df_perf     = read.table(file = filename, header = T)  
          
          perf_value          = calculate_cost_value(df_perf)
          over_value          = (ceiling(max(df_util$UTIL)) * nrow(df_util))/ (60 / 5)
          
          saving_value        = ((over_value - perf_value) / over_value)
          
          
          df_result           = rbind(df_result, data.frame(TRACE = trace, METRIC = metric, OVER_COST = over_value, PERF_COST = perf_value, SAVING = saving_value)) 
     }
     
}

write.table(df_result, file = "data/01_data_saving.dat", row.names = F)
