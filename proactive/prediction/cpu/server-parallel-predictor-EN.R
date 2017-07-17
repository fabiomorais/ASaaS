begin	<- Sys.time()

args  <- commandArgs(trailingOnly = TRUE)

var_1m			<- 60
var_5m			<- 5 * var_1m
var_1h			<- 60 * var_1m
var_24h			<- 24 * var_1h
og				<- "1970-01-01"
timez			<- "GMT"
policy_name		<- "ensemble"
policy_label	     <- "Ensemble (EN)"
policy_abr		<- "EN"
grain_unit		<- list("5" = var_5m, "10" = 10 * var_1m, "60" = var_1h)

trace_number	<- as.integer(args[1])
num_cores		<- as.integer(args[2])
trace_type	<- as.character(args[3])
pred_type		<- as.character(args[4])

input_dir		<- paste("../../../data/", trace_type, "/", sep = "")
outpur_dir	<- paste("data/pred/", pred_type, "/", sep = "")

num_period		<- 0
pred_ahead		<- 2
pred_grain		<- 5

filename_utilization	<- paste(input_dir, "cpu_util-prod-server-", trace_number, "-", trace_type, ".dat", sep = "")
filename_alloc			<- paste(input_dir, "cpu_alloc-prod-server-", trace_number, "-", trace_type, ".dat", sep = "")

pred_names <- c("LW", "LR", "AC", "ARIMA", "AR")

list_of_periods <- list("LW" = 0, "LR" = 0, "AC" = 0, "ARIMA" = 0, "AR" = 2016)

list_of_df <- sapply(pred_names, function(x) read.table(paste(outpur_dir, "server-", trace_number, "-estimated-trace-", x, "-", list_of_periods[[x]], "-", pred_type, ".dat", sep = ""), header = TRUE), simplify = FALSE)

list_of_df <- lapply(list_of_df, function(x) { x$CPU_UTIL <- pmax(x$CPU_UTIL, 0); x})

start_timestamp <- max(sapply(list_of_df, function(x) x[1,1]))

finish_timestamp <- min(sapply(list_of_df, function(x) x[nrow(x),1]))

list_of_df <- sapply(list_of_df, function(x) subset(x, TIMESTAMP >= start_timestamp & TIMESTAMP <= finish_timestamp), simplify = FALSE)


file_name	<- paste(outpur_dir, "server-", trace_number, "-estimated-trace-", policy_abr, "-", num_period, "-", pred_type, ".dat", sep = "")
weight_file_name	<- paste(outpur_dir, "server-", trace_number, "-weight-trace-", policy_abr, "-", num_period, "-", pred_type, ".dat", sep = "")
if (!file.exists(file_name)) {
	
	df_util		<- read.table(filename_utilization, header = T)

	df_alloc		<- if (file.exists(filename_alloc)) {
					read.table(filename_alloc, header = T)
				   }else{
					data.frame(CPU_ALLOC = rep(16, nrow(df_util)))
				   }
	
	timestamps		<- df_util$TIMESTAMP
	trace_util		<- df_util$CPU_UTIL
	trace_alloc		<- df_alloc$CPU_ALLOC
	
	trace_util_cores	<- trace_util * trace_alloc / 100 
	
	df_trace			<- data.frame(cbind(timestamps, trace_util_cores),0, "Original")
	names(df_trace)		<- c("TIMESTAMP", "CPU_UTIL", "EXEC_TIME", "POLICY")
	
	begin_pred	<- Sys.time()

	weight <- data.frame(start_timestamp, t(rep(1/length(pred_names), length(pred_names))))
	
	names(weight) <- c("TIMESTAMP", pred_names)
	
	ensemble_pred <- sum(weight[1,2:ncol(weight)] * sapply(list_of_df, function(x) max(0, x[1,2])))
	
	exec_time	<- as.numeric(difftime(Sys.time(), begin_pred, units = 'secs'))
	
	prediction_result		<- data.frame(start_timestamp, ensemble_pred, exec_time)
	
	names(prediction_result)	<- c("TIMESTAMP","CPU_UTIL", "EXEC_TIME")
	
	
	calc_cost <- function(prediction,raw_data) {
		T_hit <- 5
		T_miss <- 1040
		R_vm <- 400
		BETA <- 0.5
		cost <- if (raw_data < prediction) {
			(BETA * raw_data * T_hit) + ((1 - BETA)*(prediction - raw_data) * R_vm)
		}else{
			(BETA * prediction * T_hit) + ((raw_data - prediction) * T_miss)
		}
		cost
	}
	
	cost <-  as.matrix(sapply(list_of_df, function(x) calc_cost(x[1,2], subset(df_trace, TIMESTAMP == start_timestamp)$CPU_UTIL), simplify = TRUE))
	
	error <- (sum(cost)/cost) * as.vector(weight[1,2:ncol(weight)])  
	
	for (t in 2:nrow(list_of_df[["LW"]])) {
			
		begin_pred	<- Sys.time()
		
		timestamp <- list_of_df[["LW"]][t,1]
		
		tmp_weight <- data.frame(timestamp, error/sum(error))
		
		names(tmp_weight) <- c("TIMESTAMP", pred_names)
		row.names(tmp_weight) <- t

		weight <- rbind(weight, tmp_weight)
				
		ensemble_pred <- sum(weight[t,2:ncol(weight)] * sapply(list_of_df, function(x) x[t,2]))
		
		exec_time <- as.numeric(difftime(Sys.time(), begin_pred, units = 'secs'))
		
		tmp_result <- data.frame(timestamp, ensemble_pred, exec_time)
		names(tmp_result)	<- c("TIMESTAMP","CPU_UTIL", "EXEC_TIME")
		prediction_result <- rbind(prediction_result, tmp_result)
		
		cost <-  as.matrix(sapply(list_of_df, function(x) calc_cost(x[t,2], subset(df_trace, TIMESTAMP == timestamp)$CPU_UTIL), simplify = TRUE))
		
		error <- (sum(cost)/cost) * as.vector(weight[t,2:ncol(weight)])  
	}
		
	estimated_trace		<- prediction_result
	names(estimated_trace)   <- c("TIMESTAMP","CPU_UTIL", "EXEC_TIME")
	
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
	write.table(estimated_trace, file = File_name, row.names = FALSE)
	
	file_name	<- paste(outpur_dir, "server-", trace_number, "-diff-trace-", policy_abr, "-", num_period, "-", pred_type, ".dat", sep = "")
	write.table(diff_trace, file = File_name, row.names = FALSE)
	
	write.table(weight, file = weight_file_name, row.names = FALSE)
	
}
diff_mins	<- difftime(Sys.time(), begin, units = 'mins')

print(paste("Minutes:", diff_mins))
print(paste("Hours:", (diff_mins / 60)))
