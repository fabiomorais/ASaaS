library("dplyr")

traces    = c(1, 2, 3, 4, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 30, 31, 32, 33, 35, 38, 40)
metrics   = c("cpu", "mem")

df_final  = c()

for (metric in metrics) {
     
     for (trace in traces) {
          
          print(trace)
          
          filename	= paste("../data/normal/", metric, "_util-prod-server-", trace, "-normal.dat", sep = "")
          df_util	= read.table(filename, header = T)
          
          filename	= paste("../data/normal/", metric, "_alloc-prod-server-", trace, "-normal.dat", sep = "")
          df_alloc	= read.table(filename, header = T)
          
          df_usage  = inner_join(df_util, df_alloc, by = "TIMESTAMP")
          
          metric_name    = paste(toupper(metric), "_USAGE", sep = "")
          dtmp           = data.frame(df_usage[, paste(toupper(metric), "_UTIL", sep = "")] *  df_usage[, paste(toupper(metric), "_ALLOC", sep = "")] / 100)
          colnames(dtmp) = c(metric_name)
          df_usage       = cbind(df_usage, dtmp)         
          
          dff_val   = diff(df_usage[, metric_name])
          df_result = data.frame(TRACE = trace, METRIC = metric, SD = sd(dff_val), VAR = var(dff_val), MEAN_USAGE = mean(df_usage[ , metric_name]), MEDIAN_USAGE = median(df_usage[, metric_name]), MIN_USAGE = min(df_usage[ , metric_name]), MAX_USAGE = max(df_usage[, metric_name]))
          
          df_final  = rbind(df_final, df_result)
     }
}

filename  = "data/trace_analysis.dat"
write.table(df_final, file = filename, row.names = F)
