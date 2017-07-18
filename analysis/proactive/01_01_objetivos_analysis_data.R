library(dplyr)

calculate_cost_value = function(scaling_trace){
     
     tmp     <- scaling_trace$SCALING[2:nrow(scaling_trace)] - scaling_trace$SCALING[1:(nrow(scaling_trace) - 1)]
     tmp     <- tmp[tmp < 0]
     
     cost_value <- sum(scaling_trace$SCALING) + sum(abs(tmp), na.rm = T)	
     cost_value / (60 / 5)	
}

scenarios      = c("LW", "AR", "Dynamic")
traces         = c(1, 2, 3, 4, 8, seq(10, 27), 30, 31, 32, 33, 35, 38, 40)
metrics        = c("cpu", "mem")
filters        = c("normal", "ceiling_max")
corrections    = c(0, 41, 1009, 2017, 4031) # 0% 1% 25% 50% 100%
safety_ms      = c(0, 0.1, 0.2, 0.3, 0.4, 0.5)

df_result      = c()

for (t in traces) {
     
     print(t)     
     
     for (m in metrics) {
          
          filename       = paste("../../proactive/scaling/", m, "/data/scaling/normal/server-", t, "-scaling-0-PERFECT-60-55-FALSE-0-linear-1.dat", sep = "")          
          df_perf        = read.table(file = filename, header = T)
          
          file_name      = paste("../../data/normal/", m, "_util-prod-server-", t, "-normal.dat", sep = "")
          df_trace       = read.table(file_name, header = T)
          
          file_name      = paste("../../data/normal/", m, "_alloc-prod-server-", t, "-normal.dat", sep = "")
          df_alloc       = read.table(file_name, header = T)
          
          metric_util    = paste(toupper(m), "_UTIL", sep = "") 
          metric_alloc   = paste(toupper(m), "_ALLOC", sep = "") 
          
          df_util        = data.frame(TIMESTAMP = df_trace$TIMESTAMP, UTIL = (df_trace[ , metric_util] * df_alloc[ , metric_alloc] / 100))
          df_util$TIMESTAMP_STR = as.POSIXct(df_util$TIMESTAMP,  origin = "1970-01-01 00:00:00", tz = "GMT")
          
          for (sc in scenarios) {
               
               for (ff in filters) {
                    
                    for (cc in corrections) {
                         
                         flag = if (cc == 0) { "F" } else {"T" }
                         
                         for (sm in safety_ms) {
                              
                              case = paste(t, m, sc, ff, cc, sm, sep = "-")
                              print(case)
                              
                              df_scaling = if (sc == "Dynamic") {
                                   filename = paste("../../proactive/scaling/", m, "/data/analysis/", ff, "/period/dynamic/acf_neg_i/server-", t, "-global-dynamic-scaling-288-1-NA-reactive-F-NA-", flag, "-", cc, "-", sm, "-linear-linear-NA-0.99.dat", sep = "")
                                   read.table(file = filename, header = T) %>% select(TIMESTAMP, SCALING) %>% mutate(TRACE = t, SC = sc, FILTER = ff, CORR = cc, SM = sm)
                              } else {
                                   filename = paste("../../proactive/scaling/", m, "/data/scaling/", ff, "/server-", t, "-scaling-", cc, "-", sc, "-60-55-TRUE-acf_neg_i-1-", sm, "-linear-1.dat", sep = "")          
                                   read.table(file = filename, header = T) %>% select(TIMESTAMP, SCALING) %>% mutate(TRACE = t, SC = sc, FILTER = ff, CORR = cc, SM = sm)
                              }
                              
                              df_util_tmp         = filter(df_util, TIMESTAMP >= df_scaling$TIMESTAMP[1])

                              perf_value          = calculate_cost_value(df_perf)
                              over_value          = (ceiling(max(df_util_tmp$UTIL)) * nrow(df_util_tmp)) / (60 / 5)
                              trace_len           = nrow(df_scaling)

                              timestamp_viol      = df_util_tmp$TIMESTAMP[df_util_tmp$UTIL > (df_scaling$SCALING * 0.99)]
                              hv_value            = length(timestamp_viol)
                              hvp_value           = hv_value / nrow(df_util_tmp)

                              cost_value          = calculate_cost_value(df_scaling)
                              saving_rel          = ((cost_value - over_value) / over_value)
                              cost_rel            = ((cost_value - perf_value) / perf_value)

                              tmp                 = data.frame(TRACE = t, METRIC = m, TRACE_LEN = trace_len, SCENARIO = sc, FILTER = ff, CORR = cc, SAFEM = sm,
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
}

write.table(df_result, file = "data/general-data-analysis.dat", row.names = F)