library(dplyr)

calculate_cost_value = function(scaling_trace){
     
     tmp     <- scaling_trace$SCALING[2:nrow(scaling_trace)] - scaling_trace$SCALING[1:(nrow(scaling_trace) - 1)]
     tmp     <- tmp[tmp < 0]
     
     cost_value <- sum(scaling_trace$SCALING) + sum(abs(tmp), na.rm = T)	
     cost_value / (60 / 5)	
}

scenarios      = c("LW", "AR")
traces         = c(1, 2, 3, 4, 8, seq(10, 27), 30, 31, 32, 33, 35, 38, 40)
filters        = c("normal", "ceiling_max")
corrections    = c(0, 41, 1009, 2017, 4031) # 0% 1% 25% 50% 100%
safety_ms      = c(0, 0.1, 0.2, 0.3, 0.4, 0.5)
df_result      = c()

for (t in traces) {
     
     print(t)     
     # ---- CPU ---------------
     filename = paste("../../proactive/scaling/2m/data/scaling/normal/server-", t, "-scaling-0-PERFECT-60-55-FALSE-0-0-1-NA-1-1.dat", sep = "")          
     df_perf        = read.table(file = filename, header = T)
     
     # ---- CPU ---------------
     m = "cpu"
     
     file_name      = paste("../../data/normal/", m, "_util-prod-server-", t, "-normal.dat", sep = "")
     df_trace       = read.table(file_name, header = T)
     
     file_name      = paste("../../data/normal/", m, "_alloc-prod-server-", t, "-normal.dat", sep = "")
     df_alloc       = read.table(file_name, header = T)
     
     metric_util    = paste(toupper(m), "_UTIL", sep = "") 
     metric_alloc   = paste(toupper(m), "_ALLOC", sep = "") 
     
     df_util_cpu    = data.frame(TIMESTAMP = df_trace$TIMESTAMP, UTIL_CPU = (df_trace[ , metric_util] * df_alloc[ , metric_alloc] / 100))
     df_util_cpu$TIMESTAMP_STR = as.POSIXct(df_util_cpu$TIMESTAMP,  origin = "1970-01-01 00:00:00", tz = "GMT")
     
     # ---- MEM ---------------
     m = "mem"
     file_name      = paste("../../data/normal/", m, "_util-prod-server-", t, "-normal.dat", sep = "")
     df_trace       = read.table(file_name, header = T)
     
     file_name      = paste("../../data/normal/", m, "_alloc-prod-server-", t, "-normal.dat", sep = "")
     df_alloc       = read.table(file_name, header = T)
     
     metric_util    = paste(toupper(m), "_UTIL", sep = "") 
     metric_alloc   = paste(toupper(m), "_ALLOC", sep = "") 
     
     df_util_mem    = data.frame(TIMESTAMP = df_trace$TIMESTAMP, UTIL_MEM = (df_trace[ , metric_util] * df_alloc[ , metric_alloc] / 100))
     df_util_mem$TIMESTAMP_STR = as.POSIXct(df_util_mem$TIMESTAMP,  origin = "1970-01-01 00:00:00", tz = "GMT")
     
     df_util = dplyr::full_join(df_util_cpu, df_util_mem, by = c("TIMESTAMP", "TIMESTAMP_STR"))
     
     for (sc in scenarios) {
          
          for (ff in filters) {
               
               for (cc in corrections) {
                    
                    for (sm in safety_ms) {
                         
                         case = paste(t, m, sc, ff, cc, sm, sep = "-")
                         
                         print(case)
                         
                         filename = paste("../../proactive/scaling/2m/data/scaling/", ff, "/server-", t, "-scaling-", cc, "_", cc ,"-", sc, "-60-55-TRUE-acf_neg_i-1-", sm, "-", sm, "-1-NA-1-1.dat", sep = "")
                         df_scaling = read.table(file = filename, header = T) %>% mutate(TRACE = t, SC = sc, FILTER = ff, CORR = cc, SM = sm)

                         df_util_tmp         = filter(df_util, TIMESTAMP >= df_scaling$TIMESTAMP[1])

                         perf_value          = calculate_cost_value(df_perf)
                         over_value          = (ceiling(max(df_util_tmp$UTIL_CPU, df_util_tmp$UTIL_MEM, na.rm = T)) * nrow(df_util_tmp)) / (60 / 5)
                         trace_len           = nrow(df_scaling)

                         timestamp_viol_cpu  = df_util_tmp$TIMESTAMP[df_util_tmp$UTIL_CPU > (df_scaling$SCALING * 0.99)]
                         timestamp_viol_mem  = df_util_tmp$TIMESTAMP[df_util_tmp$UTIL_MEM > (df_scaling$SCALING * 0.99)]
                         
                         hv_value            = length(unique(c(timestamp_viol_cpu, timestamp_viol_mem)))
                         hvp_value           = hv_value / nrow(df_util_tmp)

                         cost_value          = calculate_cost_value(df_scaling)
                         saving_rel          = ((cost_value - over_value) / over_value)
                         cost_rel            = ((cost_value - perf_value) / perf_value)

                         tmp                 = data.frame(TRACE = t, TRACE_LEN = trace_len, SCENARIO = sc, FILTER = ff, CORR = cc, SAFEM = sm,
                                                          VIOL = hv_value, VIOLP = hvp_value,
                                                          COST = cost_value, PERF_COST = perf_value,
                                                          OVER_COST = over_value, PERF_REL = cost_rel,
                                                          OVER_REL = saving_rel)

                         df_result           = rbind(df_result, tmp)
                    }
               }
          }
     }
}

write.table(df_result, file = "data/general-data-analysis-2m.dat", row.names = F)
