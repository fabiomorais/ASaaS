library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

calculate_cost_value = function(scaling_trace){
     
     tmp     <- scaling_trace$SCALING[2:nrow(scaling_trace)] - scaling_trace$SCALING[1:(nrow(scaling_trace) - 1)]
     tmp     <- tmp[tmp < 0]
     
     cost_value <- sum(scaling_trace$SCALING) + sum(abs(tmp), na.rm = T)	
     cost_value / (60 / 5)	
}

for (metric in c("cpu", "mem")) { 
     
     df_scenarios   = read.table(file = paste("data/reactive_scenarios_", metric, ".dat", sep = ""), header = F)
     colnames(df_scenarios)  = c("TRACE", "TH_ADD", "VM_ADD", "FREQ_ADD_CONF", "FREQ_ADD_THR", "TH_RM", "VM_RM", "FREQ_RM_CONF", "FREQ_RM_THR", "NSCALING", "TRACE_LEN", "SCALING_FREQ")
     horizon      = 2
     df_result    = c()
     
     for (index in 1:nrow(df_scenarios)) {
          
          print(index)
          
          scn   = df_scenarios[index, ]  
          
          file_name      = paste("../data/normal/", metric, "_util-prod-server-", scn$TRACE, "-normal.dat", sep = "")
          df_trace       = read.table(file_name, header = T)
          
          file_name      = paste("../data/normal/", metric, "_alloc-prod-server-", scn$TRACE, "-normal.dat", sep = "")
          df_alloc       = read.table(file_name, header = T)
          
          metric_util    = paste(toupper(metric), "_UTIL", sep = "") 
          metric_alloc   = paste(toupper(metric), "_ALLOC", sep = "") 
          
          df_util        = data.frame(TIMESTAMP = df_trace$TIMESTAMP, UTIL = (df_trace[ , metric_util] * df_alloc[ , metric_alloc] / 100))
          df_util$TIMESTAMP_STR = as.POSIXct(df_util$TIMESTAMP,  origin = "1970-01-01 00:00:00", tz = "GMT")
          
          filename	  = paste("data/scaling/server-", scn$TRACE, "-scaling-threshold-", horizon, "-", scn$TH_ADD, "_", scn$VM_ADD, "-", scn$TH_RM, "_",  scn$VM_RM, "-", metric, ".dat", sep = "")
          df_reactive = read.table(file = filename, header = T)  
          
          filename	  = paste("data/perfect/scaling-perfect-", scn$TRACE, "-1-", metric, ".dat", sep = "")
          df_perf     = read.table(file = filename, header = T)  
          
          df_util_tmp         = filter(df_util, TIMESTAMP >= df_reactive$TIMESTAMP[1])
          
          perf_value          = calculate_cost_value(df_perf)
          over_value          = (ceiling(max(df_util_tmp$UTIL)) * nrow(df_util_tmp))/ (60 / 5)
          trace_len           = nrow(df_reactive)
          
          # analise do reativo
          timestamp_viol      = df_util_tmp$TIMESTAMP[df_util_tmp$UTIL > (df_reactive$SCALING * 0.99)]
          hv_value            = length(timestamp_viol)
          hvp_value           = hv_value / nrow(df_util_tmp)
          
          cost_value          = calculate_cost_value(df_reactive)
          saving_rel          = ((over_value - cost_value) / over_value)
          cost_rel            = ((cost_value - perf_value) / perf_value)

          tmp                 = scn %>% mutate(TYPE = "reactive", METRIC = metric, TRACE_LEN = trace_len, 
                                               VIOL = hv_value, VIOLP = hvp_value, 
                                               COST = cost_value, PERF_COST = perf_value, 
                                               OVER_COST = over_value, PERF_REL = cost_rel, 
                                               OVER_REL = saving_rel)
          
          df_result           = rbind(df_result, tmp)     
     }
     
     head(df_result)
     dplot = df_result %>% select(TRACE, TH_ADD, TH_RM, VM_ADD, VM_RM, VIOLP, PERF_REL, OVER_REL, SCALING_FREQ) %>% gather("METRIC", "VALUE", 6:9) %>% group_by(TRACE, TH_ADD, TH_RM, VM_ADD, VM_RM) %>% mutate(VM = paste("+", VM_ADD, " | -", VM_RM, sep = ""))
     
     dplot$METRIC[dplot$METRIC == "VIOLP"] = "Violation"
     dplot$METRIC[dplot$METRIC == "PERF_REL"] = "Costs (Perf)"
     dplot$METRIC[dplot$METRIC == "OVER_REL"] = "Costs (Over)"
     dplot$METRIC[dplot$METRIC == "SCALING_FREQ"] = "Scaling frequency"
     
     write.table(dplot, file = paste("data/reactive_", metric, "_performance_all.dat", sep = ""), row.names = T)
}