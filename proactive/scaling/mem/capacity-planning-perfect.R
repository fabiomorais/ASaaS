generate_capacity_planning = function(error_adjustment, 
										ad_w, 
										diff_trace, 
										original_trace, 
										complete_trace, 
										estimated_trace,
										alloc_trace,
										trace_number, 
										util_target, 
										policy_name, 
										policy_abr, 
										pred_grain, 
										cap_grain, 
										adjustment_type,
										slo_limit) {

	pred_ahead			<- 2
	og					<- "1970-01-01"
	timez				<- "GMT"
	var_1m				<- 60
	var_5m				<- 5 * var_1m
	var_1h				<- 60 * var_1m
	grain_unit			<- list("5" = var_5m, "10" = 10 * var_1m, "60" = var_1h)
	
	trace_start_time	<- as.POSIXct(estimated_trace[,1][1],  origin = og, tz = timez)
	trace_end_time		<- as.POSIXct(estimated_trace[,1][nrow(estimated_trace)],  origin = og, tz = timez)
	
	num_it				<- (as.integer(as.numeric(difftime(trace_end_time, trace_start_time, units = 'mins')) / pred_grain) + 1)
	init_it				<- as.integer(cap_grain / pred_grain)
	var					<- grain_unit[[as.character(cap_grain)]]
	
	df_result			<- c()	
	ad_w_tmp 			<- ad_w

	if (0 %in% ad_w_tmp) {

		capacity_real   	<- complete_trace$MEM_UTIL
		capacity_est		<- estimated_trace$MEM_UTIL
		capacity_est_tg		<- NA


		for (safety_margin in util_target) {

			capacity_est_tg		<- capacity_est / as.double(slo_limit - safety_margin)
		
			df_tmp				<- data.frame(complete_trace$TIMESTAMP, capacity_real, capacity_est, capacity_est_tg, 0, trace_number, policy_name, slo_limit, safety_margin, 0)
			names(df_tmp)		     <- c("TIMESTAMP", "CAP_REAL", "CAP_EST", "CAP_EST_TG", "ADJUSTMENT", "TRACE", "POLICY", "SLO_LIMIT", "SAFETY_MARGIN", "WINDOW")
		

			df_result 			<- rbind(df_result, df_tmp)
		}

		ad_w_tmp 			<- ad_w_tmp[ad_w_tmp != 0]
	}

	df_result <- df_result %>% ungroup() %>% arrange(WINDOW, TIMESTAMP, SAFETY_MARGIN)
	df_result
}

run_analysis = function(filename_utilization, 
						filename_allocation, 
						trace_number, 
						trace_type, 
						pred_type, 
						num_periods, 
						policies, 
						input_dir, 
						outpur_dir, 
						util_target, 
						pred_grain, 
						cap_grain, 
						error_adjustment, 
						adjustment_type, 
						slo_limit) {
	
	pred.labels	<- list(last_h	= "Last window (LW)",
			ets 				= "ETS",
			auto_regressive	= "Auto-regressive (AR)",
			sar 				= "Sazonal auto-regressive (AR)",
			arima			= "Auto-regressive integrated moving average (ARIMA)",
			sarima			= "Sazonal auto-regressive integrated moving average (ARIMA)",
			auto_correlation	= "Auto-correlation (AC)",
			linear_regression	= "Linear regression (LR)",
			ensemble			= "Ensemble (EN)"
	)
	
	pred.abr.labels	<- list(last_h		= "LW",
			ets 				= "ETS",
			auto_regressive	= "AR",
			sar 				= "SAR",
			arima			= "ARIMA",
			sarima			= "SARIMA",
			auto_correlation	= "AC",
			linear_regression	= "LR",
			ensemble		     = "EN"
	)
	
	dff			<- read.table(filename_utilization, header = T)
	dff_alloc	     <- if (file.exists(filename_allocation)) {
					read.table(filename_allocation, header = T)
				}else{
					data.frame(TIMESTAMP = dff$TIMESTAMP, MEM_ALLOC = rep(16, nrow(dff)))
				}
	
	timestamps	<- dff$TIMESTAMP
	trace_util	<- dff$MEM_UTIL
	trace_alloc	<- dff_alloc$MEM_ALLOC
	
	df_trace		     <- data.frame(cbind(timestamps, trace_util), 0, "Original")
	names(df_trace)	<- c("TIMESTAMP", "MEM_UTIL", "EXEC_TIME", "POLICY")
	
	df_alloc		     <- data.frame(cbind(timestamps, trace_alloc), 0, "Alloc")
	names(df_alloc)	<- c("TIMESTAMP", "MEM_ALLOC", "EXEC_TIME", "POLICY")
			
		
	policy				<- policies[1]
	num_period			<- num_periods[1]
			
	estimated_trace		<- read.table(paste(input_dir, "server-", trace_number, "-estimated-trace-", pred.abr.labels[[policy]], "-", num_period, "-", pred_type,".dat", sep = ""), header = T)
	diff_trace			<- read.table(paste(input_dir, "server-", trace_number, "-diff-trace-", pred.abr.labels[[policy]], "-", num_period, "-", pred_type, ".dat", sep = ""), header = T)	
			
	tmp_df_trace		<- subset(df_trace, TIMESTAMP >= estimated_trace$TIMESTAMP[1])
	tmp_df_trace		<- subset(tmp_df_trace, TIMESTAMP <= estimated_trace$TIMESTAMP[nrow(estimated_trace)])
			
	tmp_df_alloc		<- subset(df_alloc, TIMESTAMP >= estimated_trace$TIMESTAMP[1])
	tmp_df_alloc		<- subset(tmp_df_alloc, TIMESTAMP <= estimated_trace$TIMESTAMP[nrow(estimated_trace)])
			
	tmp_df_trace[,5] 	<- num_period
	names(tmp_df_trace)	<- c("TIMESTAMP", "MEM_UTIL", "EXEC_TIME", "POLICY", "PERIOD")
			
	new_df_trace		<- data.frame(tmp_df_trace$TIMESTAMP, tmp_df_trace$MEM_UTIL * tmp_df_alloc$MEM_ALLOC / 100, tmp_df_trace$EXEC_TIME, tmp_df_trace$POLICY, tmp_df_trace$PERIOD)
	names(new_df_trace)	<- c("TIMESTAMP", "MEM_UTIL", "EXEC_TIME", "POLICY", "PERIOD")
			
	original_df_alloc	<- subset(df_alloc, TIMESTAMP >= df_trace$TIMESTAMP[1])
	original_df_alloc	<- subset(original_df_alloc, TIMESTAMP <= df_trace$TIMESTAMP[nrow(df_trace)])

	original_df_trace		<- data.frame(df_trace$TIMESTAMP, df_trace$MEM_UTIL * original_df_alloc$MEM_ALLOC / 100, df_trace$EXEC_TIME, df_trace$POLICY, num_period)
	names(original_df_trace)	<- c("TIMESTAMP", "MEM_UTIL", "EXEC_TIME", "POLICY", "PERIOD")
			
	print("")
	print(paste("Server: ", trace_number, "- Perfect scaling"))
				 	
	original_capacity_trace	  	<- generate_capacity_planning(FALSE, 
													0, 
													diff_trace, 
													original_df_trace, 
													new_df_trace, 
													new_df_trace, 
													tmp_df_alloc, 
													trace_number, 
													util_target, 
													"perfect", 
													"PERF", 
													pred_grain, 
													cap_grain, 
													adjustment_type,
													slo_limit)

	for (safety_margin in util_target) {

		partial_result 		<- original_capacity_trace %>% filter(SAFETY_MARGIN == safety_margin)

		file_name_capacity	<- paste(outpur_dir, "capacity/", pred_type, "/server-", trace_number, "-capacity-planning-0-PERFECT-FALSE-", as.character(safety_margin), "-linear.dat", sep = "")

		write.table(partial_result, file = File_name_capacity, row.names = FALSE)
	}
}

begin	<- Sys.time()

suppressMessages(library(ggplot2, quietly = TRUE))
suppressMessages(library(scales, quietly = TRUE))
suppressMessages(library(Hmisc, quietly = TRUE))
suppressMessages(library(foreach, quietly = TRUE))
suppressMessages(library(doMC, quietly = TRUE))
suppressMessages(library(TTR, quietly = TRUE))
suppressMessages(library(dplyr, quietly = TRUE))

args  <- commandArgs(trailingOnly = TRUE)

pred_grain			<- 5
cap_grain				<- 5
error_adjustment		<- T

trace_number			<- as.integer(args[1])
num_cores				<- as.integer(args[2])
trace_type			<- as.character(args[3])
pred_type				<- as.character(args[4])
adjustment_type		<- as.character(args[5])
slo_limit 			<- as.numeric(args[6])

policies				<- c("auto_regressive") 
num_periods			<- c(2016) 

capacity_target		<- c(0.5, 0.4, 0.3, 0.2, 0.1, 0)

input_dir				<- paste("../../prediction/mem/data/pred/", pred_type, "/", sep = "")
outpur_dir			<- "data/"

filename_utilization	<- paste("../../../data/", trace_type, "/mem_util-prod-server-", trace_number, "-", trace_type, ".dat", sep = "")
filename_allocation		<- paste("../../../data/", trace_type, "/mem_alloc-prod-server-", trace_number, "-", trace_type, ".dat", sep = "")
	
registerDoMC(cores = num_cores)
run_analysis(	filename_utilization, 
				filename_allocation, 
				trace_number, 
				trace_type, 
				pred_type, 
				num_periods, 
				policies, 
				input_dir, 
				outpur_dir, 
				capacity_target, 
				pred_grain, 
				cap_grain, 
				error_adjustment, 
				adjustment_type, 
				slo_limit)

diff_mins	<- difftime(Sys.time(), begin, units = 'mins')
print(paste("Minutes:", diff_mins))
print(paste("Hours:", (diff_mins / 60)))