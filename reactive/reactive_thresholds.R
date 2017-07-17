library(dplyr)
library(doMC)
library(foreach)

traces    = c(1, 2, 3, 4, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 30, 31, 32, 33, 35, 38, 40)
metrics   = c("cpu", "mem")

registerDoMC(cores = 8)

for (prov_time in c(0, 1)) { 
     
     for (metric in metrics) {
          
          x = foreach(index = 1:length(traces), .combine = 'c') %dopar% {

               trace     = traces[index]
               filename  = paste("data/perfect/scaling-perfect-", trace, "-", prov_time, "-", metric, ".dat", sep = "")
               
               print(paste(trace, metric, prov_time))
               
               if (!file.exists(filename)) { 
               
                    
                    begin	<- Sys.time()
                    
                    load_util_files = function(){
                         
                         source("scaling_server.R", local = T)
                         
                         return(list(calculate_scaling = calculate_scaling))
                    }
                    
                    util      = load_util_files()
                    trace     = traces[index]
                    
                    filename	= paste("../data/normal/", metric, "_util-prod-server-", trace, "-normal.dat", sep = "")
                    df_util	= read.table(filename, header = T)
                    
                    filename	= paste("../data/normal/", metric, "_alloc-prod-server-", trace, "-normal.dat", sep = "")
                    df_alloc	= read.table(filename, header = T)
                    
                    df_usage  = inner_join(df_util, df_alloc, by = "TIMESTAMP")
                    
                    dtmp           = data.frame(df_usage[, paste(toupper(metric), "_UTIL", sep = "")] *  df_usage[, paste(toupper(metric), "_ALLOC", sep = "")] / 100)
                    colnames(dtmp) = c(paste(toupper(metric), "_USAGE", sep = ""))
                    df_usage       = cbind(df_usage, dtmp)
                    
                    start_time     = (4032) * 300
                    timestamp      = as.numeric(as.POSIXct(df_usage[,1][1], origin = "1970-01-01", tz = "GMT") + start_time)
                    
                    capacity       = filter(df_usage, TIMESTAMP > timestamp) %>% mutate(FLAVOR_NAME = "vmbase")
                    dtmp           = data.frame(ceiling(capacity[, paste(toupper(metric), "_USAGE", sep = "")]))
                    colnames(dtmp) = c("CAP")
                    capacity       = cbind(capacity, dtmp)
                    
                    timestamps          = capacity$TIMESTAMP
                    scaling_status      = c()
                    scaling_final       = c()
                    
                    for (tmps in timestamps) {
                         
                         i = which(timestamps == tmps)
                         
                         try(system(paste("echo ", i / length(timestamps) * 100, "% > output/result-", trace, "-", prov_time, ".out", sep = ""), intern = TRUE))
                         
                         estimated_capacity  = filter(capacity, TIMESTAMP == tmps)
                         scaling_result      = util$calculate_scaling(estimated_capacity, 55, 60, 1, scaling_status)
                         scaling_status      = scaling_result[["status"]]
                         scaling_tmp         = scaling_result[["result"]]
                         scaling_final       = rbind(scaling_final, scaling_tmp)
                         
                    }
                    
                    filename  = paste("data/perfect/scaling-perfect-", trace, "-", prov_time, "-", metric, ".dat", sep = "")
                    write.table(scaling_final, file = filename, row.names = F)
                    
               }
          }
     }
}