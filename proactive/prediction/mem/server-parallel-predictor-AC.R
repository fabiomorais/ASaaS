begin	<- Sys.time()

suppressMessages(library(foreach, quietly = TRUE))
suppressMessages(library(doMC, quietly = TRUE))
suppressMessages(library(plyr, quietly = TRUE))
suppressMessages(library(signal, quietly = TRUE))

args  <- commandArgs(trailingOnly = TRUE)

var_1m			<- 60
var_5m			<- 5 * var_1m
var_1h			<- 60 * var_1m
var_24h			<- 24 * var_1h
og			     <- "1970-01-01"
timez			<- "GMT"
policy_name		<- "auto_correlation"
policy_label		<- "Auto-correlation (AC)"
policy_abr		<- "AC"
grain_unit		<- list("5" = var_5m, "10" = 10 * var_1m, "60" = var_1h)

trace_number		<- as.integer(args[1])
num_cores		     <- as.integer(args[2])
trace_type		<- as.character(args[3])
pred_type		     <- as.character(args[4])

input_dir		     <- paste("../../../data/", trace_type, "/", sep = "")
outpur_dir		<- paste("data/pred/", pred_type, "/", sep = "")

num_period		<- 0
pred_ahead		<- 2
pred_grain		<- 5

filename_utilization	<- paste(input_dir, "mem_util-prod-server-", trace_number, "-", trace_type, ".dat", sep = "")
filename_alloc			<- paste(input_dir, "mem_alloc-prod-server-", trace_number, "-", trace_type, ".dat", sep = "")

file_name	<- paste(outpur_dir, "server-", trace_number, "-estimated-trace-", policy_abr, "-", num_period, "-", pred_type, ".dat", sep = "")
if (!file.exists(file_name)) {
     
     df_util			<- read.table(filename_utilization, header = T)
     df_alloc		     <- read.table(filename_alloc, header = T)
     
     timestamps		<- df_util$TIMESTAMP
     trace_util		<- df_util$MEM_UTIL
     trace_alloc		<- df_alloc$MEM_ALLOC
     
     trace_util_cores	<- trace_util * trace_alloc / 100 
     
     df_trace			<- data.frame(cbind(timestamps, trace_util_cores),0, "Original")
     names(df_trace)	<- c("TIMESTAMP", "MEM_UTIL", "EXEC_TIME", "POLICY")
     
     start_time	<- as.POSIXct(df_trace[,1][1],  origin = og, tz = timez)
     end_time	     <- as.POSIXct(df_trace[,1][nrow(df_trace)],  origin = og, tz = timez)
     
     num_it		<- nrow(df_trace) 
     init_it		<- (2016 * 2) + 1	#3
     
     registerDoMC(cores = num_cores)
     x <- foreach(i = init_it:num_it) %dopar% {
          
          begin_pred	<- Sys.time()
          
          try(system(paste("echo ", i, " > result-", trace_number, "-", policy_abr, ".out", sep = ""), intern = TRUE))
          
          V			<- 1
          H			<- i
          var			<- grain_unit[[as.character(pred_grain)]]
          
          present_start_time	<- (H - 1) * var
          
          past_start_time		<- max(0, (H - 4033)) * var
          
          workload_start_time		<- as.POSIXct(df_trace[,1][1], origin = og, tz = timez)
          
          past_win_start_time		<- as.numeric(workload_start_time + past_start_time)
          present_win_start_time	<- as.numeric(workload_start_time + present_start_time)
          
          tmp_df_trace	<- subset(df_trace, TIMESTAMP < present_win_start_time)
          tmp_df_trace	<- subset(tmp_df_trace, TIMESTAMP >= past_win_start_time)
          
          tmp_df_trace$MEM_UTIL	<- if (pred_type == "ceiling") {
               ceiling(tmp_df_trace$MEM_UTIL)
          }else if (pred_type == "ceiling_fft") {
               fftfilt(rep(1, 10)/10, ceiling(tmp_df_trace$MEM_UTIL))
          }else if (pred_type == "fftfilt") {
               fftfilt(rep(1, 10)/10, tmp_df_trace$MEM_UTIL)
          }else if (pred_type == "ceiling_max") {
               df_new_trace <- c()
               for (z in seq(tmp_df_trace$TIMESTAMP[1], tmp_df_trace$TIMESTAMP[nrow(tmp_df_trace)], 6*300)) {
                    
                    df_max 	<- subset(tmp_df_trace, TIMESTAMP >= (z - (6 * 300)))
                    df_max 	<- subset(df_max, TIMESTAMP < (z + (6 * 300)))
                    
                    max_util 	<- ceiling(max(df_max$MEM_UTIL))
                    
                    df_tmpp 	<- subset(tmp_df_trace, TIMESTAMP >= z)
                    df_tmpp 	<- subset(df_tmpp, TIMESTAMP < (z + (6*300)))
                    
                    df_tmpp$MEM_UTIL <- max_util
                    
                    df_new_trace <- rbind(df_new_trace, df_tmpp)
                    
               }
               df_new_trace$MEM_UTIL
          }else{
               tmp_df_trace$MEM_UTIL
          }
          acf_result		<- acf(tmp_df_trace$MEM_UTIL, lag.max = length(tmp_df_trace$MEM_UTIL)/2, plot = FALSE)
          
          acf_pos			<- (acf_result$acf > 0)
          j		<- 2
          
          if (!is.na(sum(acf_pos))) {
               while (acf_pos[j] && j <= length(acf_pos)) {
                    j <- j + 1
               }
          }
          
          #Verificar se sao duas fases de auto-correlacao forte
          if (j > length(acf_pos)) init	  <- 2
          else init <- j
          
          acf_max	  <- -Inf
          acf_index <- 1
          
          acf_value <- acf_result$acf[init:length(acf_result$acf)]
          acf_index <- which(acf_value == max(acf_value), arr.ind = TRUE)[1] + init - 1
          
          if (length(acf_index) == 0) {
               acf_index	<- pred_ahead
          }
          
          if (is.na(acf_index)) {
               acf_index	<- pred_ahead
          }	
          
          V	<- acf_index
          
          present_start_time	<- (H - V) * var
          present_end_time	<- ((H - V + (pred_grain / 5)) * var) - var_5m
          
          future_start_time	<- (H - 1) * var
          future_end_time	<- ((H + pred_ahead - 1) * var) - var_5m
          
          present_win_start_time	<- as.numeric(workload_start_time + present_start_time)
          present_win_end_time	<- as.numeric(workload_start_time + present_end_time)

          future_win_start_time	<- as.numeric(workload_start_time + future_start_time)
          future_win_end_time		<- as.numeric(workload_start_time + future_end_time)

          fut_timestamp	<- seq(future_win_start_time, future_win_end_time, var_5m)[pred_ahead]
          
          window_trace	<- subset(tmp_df_trace, TIMESTAMP >= present_win_start_time)
          window_trace	<- subset(window_trace, TIMESTAMP <= present_win_end_time)

          exec_time	<- as.numeric(difftime(Sys.time(), begin_pred, units = 'secs'))
          
          prediction_result			<- data.frame(fut_timestamp, window_trace[,2], exec_time)
          names(prediction_result)	<- c("TIMESTAMP","MEM_UTIL", "EXEC_TIME")
     
          prediction_result
     }
     
     estimated_trace		<- data.frame(matrix(unlist(x), ncol = 3 , byrow = T))
     names(estimated_trace)   <- c("TIMESTAMP","MEM_UTIL", "EXEC_TIME")
     
     file_name	<- paste(outpur_dir, "server-", trace_number, "-estimated-trace-", policy_abr, "-", num_period, "-", pred_type, ".dat_bk", sep = "")
     write.table(estimated_trace, file = file_name, row.names = FALSE)
     
     start_time_new_trace	<- max(estimated_trace[,1][1], df_trace[,1][1])
     end_time_new_trace		<- min(estimated_trace[,1][nrow(estimated_trace)], df_trace[,1][nrow(df_trace)])
     
     estimated_trace		<- subset(estimated_trace, TIMESTAMP >= start_time_new_trace)
     estimated_trace		<- subset(estimated_trace, TIMESTAMP <= end_time_new_trace)
     
     complete_trace_frac		<- subset(df_trace, TIMESTAMP >= start_time_new_trace)
     complete_trace_frac		<- subset(complete_trace_frac, TIMESTAMP <= end_time_new_trace)
     
     diff_abs			<- (estimated_trace[,2] - complete_trace_frac[,2])
     diff_rel			<- ((estimated_trace[,2] - complete_trace_frac[,2]) / complete_trace_frac[,2]) * 100
     
     diff_trace			<- data.frame(complete_trace_frac[,1], diff_abs, diff_rel)
     
     diff_trace[,(ncol(diff_trace) + 1)]	<- paste("SERVER", trace_number, sep = "")
     diff_trace[,(ncol(diff_trace) + 1)]	<- policy_abr
     
     names(diff_trace)	<- c("TIMESTAMP", "ERROR_ABS", "ERROR_REL", "TRACE", "POLICY")
     
     diff_trace[,ncol(diff_trace) + 1] <- num_period
     names(diff_trace)	<- c(names(diff_trace)[1:(ncol(diff_trace) - 1)], "PERIOD")
     
     estimated_trace[,ncol(estimated_trace) + 1] <- policy_abr
     names(estimated_trace)	<- c(names(estimated_trace)[1:(ncol(estimated_trace) - 1)], "POLICY")
     estimated_trace[,ncol(estimated_trace) + 1] <- num_period
     names(estimated_trace)	<- c(names(estimated_trace)[1:(ncol(estimated_trace) - 1)], "PERIOD")
     
     file_name	<- paste(outpur_dir, "server-", trace_number, "-estimated-trace-", policy_abr, "-", num_period, "-", pred_type, ".dat", sep = "")
     write.table(estimated_trace, file = file_name, row.names = FALSE)
     
     file_name	<- paste(outpur_dir, "server-", trace_number, "-diff-trace-", policy_abr, "-", num_period, "-", pred_type, ".dat", sep = "")
     write.table(diff_trace, file = file_name, row.names = FALSE)
     
}
diff_mins	<- difftime(Sys.time(), begin, units = 'mins')

print(paste("Minutes:", diff_mins))
print(paste("Hours:", (diff_mins / 60)))