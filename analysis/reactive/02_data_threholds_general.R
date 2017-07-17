library(dplyr)

traces    = c(1, 2, 3, 4, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 30, 31, 32, 33, 35, 38, 40)
pred_time = c(0, 1)
df_result = c()
metrics   = c("cpu", "mem")
range_size = 5
prov_time = 1

for (trace in traces) {
     
     print(trace)
     
     for (metric in metrics) {
          
          df_threshold   = read.table(paste("../../reactive/data/threshold/threshold-perfect-", trace, "-", prov_time, "-", metric, "-", range_size, ".dat", sep = ""), header = T)

          if (metric == "cpu") {
               df_threshold = df_threshold %>% ungroup() %>% mutate(METRIC_UTIL = CPU_UTIL, METRIC_ALLOC = CPU_ALLOC, METRIC_USAGE = CPU_USAGE) %>%
                    data.frame() %>% select(-CPU_UTIL, -CPU_ALLOC, -CPU_USAGE)
          } else {
               df_threshold = df_threshold %>% ungroup() %>% mutate(METRIC_UTIL = MEM_UTIL, METRIC_ALLOC = MEM_ALLOC, METRIC_USAGE = MEM_USAGE) %>%
                    data.frame() %>% select(-MEM_UTIL, -MEM_ALLOC, -MEM_USAGE)
          }
          
          num_tmp        = df_threshold %>% mutate(TRACE = trace, PROV_TIME = prov_time, RANGE_SIZE = range_size)
          df_result      = rbind(df_result, num_tmp)
     }
}   

write.table(df_result, file = paste("data/02_data_thresholds-", range_size, ".dat", sep = ""), row.names = F)