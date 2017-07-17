library(dplyr)
library(doMC)
library(foreach)

traces    = c(1, 2, 3, 4, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 30, 31, 32, 33, 35, 38, 40)

metrics   = c("cpu", "mem")

registerDoMC(cores = 8)

x = foreach(index = 1:length(traces), .combine = 'c') %dopar% {
     
     prov_time = 1     
     trace     = traces[index]
     filename  = paste("data/perfect/scaling-perfect-", trace, "-", prov_time, "-2m.dat-", sep = "")
     
     print(paste(trace, prov_time))
     
     if (!file.exists(filename)) { 
          
          begin	<- Sys.time()
          
          load_util_files = function(){
               
               source("scaling_server.R", local = T)
               
               return(list(calculate_scaling = calculate_scaling))
          }
          
          util      = load_util_files()
          trace     = traces[index]
          
          df_usage_2m = c()
          
          for (metric in metrics) {
               
               filename	= paste("../data/normal/", metric, "_util-prod-server-", trace, "-normal.dat", sep = "")
               df_util	= read.table(filename, header = T)
               
               filename	= paste("../data/normal/", metric, "_alloc-prod-server-", trace, "-normal.dat", sep = "")
               df_alloc	= read.table(filename, header = T)
               
               df_usage  = inner_join(df_util, df_alloc, by = "TIMESTAMP")
               colnames(df_usage) = c("TIMESTAMP", "UTIL", "ALLOC")
               
               df_usage       = df_usage %>% mutate(METRIC = metric, USAGE = UTIL * ALLOC / 100)
               
               df_usage_2m    = rbind(df_usage_2m, df_usage)
          }
          
          
          df_usage_2m    = df_usage_2m %>% group_by(TIMESTAMP) %>% arrange(TIMESTAMP) %>% ungroup()
          
          start_time     = (4032) * 300
          
          timestamp      = as.numeric(as.POSIXct(df_usage_2m$TIMESTAMP[1], origin = "1970-01-01", tz = "GMT") + start_time)
          
          capacity       = filter(df_usage_2m, TIMESTAMP > timestamp) %>% mutate(FLAVOR_NAME = "vmbase")
          capacity       = capacity %>% group_by(TIMESTAMP) %>% mutate(CAP = ceiling(USAGE)) %>% filter(CAP == max(CAP))
          
          filename  = paste("data/perfect/capacity-perfect-", trace, "-", prov_time, "-2m.dat", sep = "")
          write.table(capacity, file = filename, row.names = F)
          
          timestamps          = unique(capacity$TIMESTAMP)
          scaling_status      = c()
          scaling_final       = c()
          
          for (tmps in timestamps) {
               
               i = which(timestamps == tmps)
               
               try(system(paste("echo ", i / length(timestamps) * 100, "% > output/result-", trace, "-", prov_time, ".out", sep = ""), intern = TRUE))
               
               estimated_capacity  = filter(capacity, TIMESTAMP == tmps) %>% sample_n(1)
               
               scaling_result      = util$calculate_scaling(estimated_capacity, 55, 60, 1, scaling_status)
               scaling_status      = scaling_result[["status"]]
               scaling_tmp         = scaling_result[["result"]]
               scaling_final       = rbind(scaling_final, scaling_tmp)
               
          }
          
          filename  = paste("data/perfect/scaling-perfect-", trace, "-", prov_time, "-2m.dat", sep = "")
          write.table(scaling_final, file = filename, row.names = F)
     }
}
