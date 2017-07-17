read_usage_metric <- function(trace, metric) {

     filename	= paste("../data/normal/", metric, "_util-prod-server-", trace, "-normal.dat", sep = "")
     df_util	= read.table(filename, header = T)
     
     filename	= paste("../data/normal/", metric, "_alloc-prod-server-", trace, "-normal.dat", sep = "")
     df_alloc	= read.table(filename, header = T)
     
     df_usage  = inner_join(df_util, df_alloc, by = "TIMESTAMP") 
     df_usage[, paste(toupper(metric), "_USAGE", sep = "")] = df_usage[, paste(toupper(metric), "_UTIL", sep = "")] * df_usage[, paste(toupper(metric), "_ALLOC", sep = "")] / 100
     
     return(df_usage)
}

library("dplyr")

traces    = c(1, 2, 3, 4, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 30, 31, 32, 33, 35, 38, 40)
prov_time = 1

for (trace in traces) {
     
     print(trace)
     
     usage_cpu      = read_usage_metric(trace, "cpu")
     usage_mem      = read_usage_metric(trace, "mem")
          
     df_usage_2m    = inner_join(usage_cpu, usage_mem)
     scaling_final  = read.table(file = paste("data/perfect/scaling-perfect-", trace, "-", prov_time, "-2m.dat", sep = ""), header = T)
     
     capacity_final = read.table(file = paste("data/perfect/capacity-perfect-", trace, "-", prov_time, "-2m.dat", sep = ""), header = T)
     capacity_final = capacity_final %>% select(TIMESTAMP, METRIC)
     
     df_usage_ref   = inner_join(scaling_final, df_usage_2m, by = "TIMESTAMP")
     df_usage_ref   = df_usage_ref %>% mutate(SCALING_CPU_UTIL_REF = CPU_USAGE / SCALING, SCALING_MEM_UTIL_REF = MEM_USAGE / SCALING)
     df_usage_ref   = df_usage_ref %>% select(TIMESTAMP, SCALING_CPU_UTIL_REF, SCALING_MEM_UTIL_REF) #%>% inner_join(capacity_final)
     
     df_reference   = df_usage_2m %>% mutate(TMSP = TIMESTAMP + ((1 + prov_time) * 300)) %>% filter(TMSP <= scaling_final$TIMESTAMP[nrow(scaling_final)]) %>% inner_join(df_usage_ref, by = "TIMESTAMP")

     df_diff_ref    = inner_join(scaling_final, df_reference, by = c("TIMESTAMP" = "TMSP")) %>% rename(TIMESTAMP_REF = TIMESTAMP.y) 
     
     df_analysis    = df_diff_ref %>% mutate(DIFF = diff(scaling_final$SCALING)[2:(nrow(scaling_final) - prov_time)]) %>% filter(DIFF != 0, !is.na(DIFF)) 
     df_analysis    = df_analysis %>% mutate(THRESHOLD_CPU = floor((round(df_analysis$SCALING_CPU_UTIL_REF, digits = 2) * 10) * 2) / (2 * 10))
     df_analysis    = df_analysis %>% mutate(THRESHOLD_MEM = floor((round(df_analysis$SCALING_MEM_UTIL_REF, digits = 2) * 10) * 2) / (2 * 10))
     
     df_analysis    = df_analysis %>% inner_join(capacity_final, by = "TIMESTAMP")

     df_analysis$ACT_DONE[df_analysis$DIFF > 0] = "ADD"
     df_analysis$ACT_DONE[df_analysis$DIFF < 0] = "RM"

     df_analysis    = df_analysis %>% mutate(THRESHOLD = ifelse(METRIC == "cpu", THRESHOLD_CPU, THRESHOLD_MEM))

     df_out   = df_analysis %>% group_by(ACT_DONE) %>% mutate(TOTAL = n(), DIFF = abs(DIFF)) %>% group_by(ACT_DONE, METRIC, THRESHOLD, DIFF) %>% summarise(OCC = n())
     write.table(df_out, file = paste("data/threshold/threshold-perfect-", trace, "-", prov_time, "-2m-summary.dat", sep = ""), row.names = F)

     df_out   = df_analysis %>% group_by(ACT_DONE) %>% mutate(TOTAL = n(), DIFF = abs(DIFF)) %>% group_by(ACT_DONE, METRIC, THRESHOLD, DIFF) %>% mutate(OCC = n())
     write.table(df_out, file = paste("data/threshold/threshold-perfect-", trace, "-", prov_time, "-2m.dat", sep = ""), row.names = F)
}