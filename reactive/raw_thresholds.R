library("dplyr")

traces    = c(1, 2, 3, 4, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 30, 31, 32, 33, 35, 38, 40)
metrics   = c("cpu", "mem")
range     = c(1, 5)

for (prov_time in c(1)) { 
     
     for (metric in metrics) {
          
          for (trace in traces) {
               
               for (r in range) {
                    
                    filename	= paste("../data/normal/", metric, "_util-prod-server-", trace, "-normal.dat", sep = "")
                    df_util	= read.table(filename, header = T)
                    
                    filename	= paste("../data/normal/", metric, "_alloc-prod-server-", trace, "-normal.dat", sep = "")
                    df_alloc	= read.table(filename, header = T)
                    
                    df_usage  = inner_join(df_util, df_alloc, by = "TIMESTAMP") 
                    df_usage[, paste(toupper(metric), "_USAGE", sep = "")] = df_usage[, paste(toupper(metric), "_UTIL", sep = "")] * df_usage[, paste(toupper(metric), "_ALLOC", sep = "")] / 100
                    
                    
                    scaling_final = read.table(file = paste("data/perfect/scaling-perfect-", trace, "-", prov_time, "-", metric, ".dat", sep = ""), header = T)
                    
                    df_usage_ref   = inner_join(scaling_final, df_usage, by = "TIMESTAMP")
                    
                    df_usage_ref[, "SCALING_UTIL_REF"] =  df_usage_ref[, paste(toupper(metric), "_USAGE", sep = "")] / df_usage_ref$SCALING
                    df_usage_ref   = df_usage_ref %>% select(TIMESTAMP, SCALING_UTIL_REF)
                    
                    df_reference   = df_usage %>% mutate(TMSP = TIMESTAMP + ((1 + prov_time) * 300)) %>% filter(TMSP <= scaling_final$TIMESTAMP[nrow(scaling_final)]) %>% inner_join(df_usage_ref, by = "TIMESTAMP")
                    
                    df_diff_ref    = inner_join(scaling_final, df_reference, by = c("TIMESTAMP" = "TMSP")) %>% rename(TIMESTAMP_REF = TIMESTAMP.y) 
                    df_analysis    = df_diff_ref %>% mutate(DIFF = diff(scaling_final$SCALING)[2:(nrow(scaling_final) - prov_time)]) %>% filter(DIFF != 0, !is.na(DIFF)) 
                    
                    if (r == 1) {
                         df_analysis    = df_analysis %>% mutate(THRESHOLD = round(df_analysis$SCALING_UTIL_REF, digits = 2))
                    } else {
                         df_analysis    = df_analysis %>% mutate(THRESHOLD = floor((round(df_analysis$SCALING_UTIL_REF, digits = 2) * 10) * 2) / (2 * 10))
                    }
                    
                    print(paste(trace, prov_time, metric))
                    
                    df_analysis$ACT_DONE[df_analysis$DIFF > 0] = "ADD"
                    df_analysis$ACT_DONE[df_analysis$DIFF < 0] = "RM"
                    
                    df_out   = df_analysis %>% group_by(ACT_DONE) %>% mutate(TOTAL = n(), DIFF = abs(DIFF)) %>% group_by(ACT_DONE, THRESHOLD, DIFF) %>% summarise(OCC = n()) %>% mutate(METRIC = metric)
                    write.table(df_out, file = paste("data/threshold/threshold-perfect-", trace, "-", prov_time, "-", metric, "-", r, "-summary.dat", sep = ""), row.names = F)

                    df_out   = df_analysis %>% group_by(ACT_DONE) %>% mutate(TOTAL = n(), DIFF = abs(DIFF)) %>% group_by(ACT_DONE, THRESHOLD, DIFF) %>% mutate(OCC = n(), METRIC = metric)
                    write.table(df_out, file = paste("data/threshold/threshold-perfect-", trace, "-", prov_time, "-", metric, "-", r, ".dat", sep = ""), row.names = F)
               }
          }
     }
}