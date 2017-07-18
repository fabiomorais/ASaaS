generate_capacity_planning = function(	error_adjustment, 
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
	grain_unit			<- list("5"= var_5m, "10"= 10 * var_1m, "60"= var_1h)
	
	trace_start_time	<- as.POSIXct(estimated_trace[,1][1],  origin = og, tz = timez)
	trace_end_time		<- as.POSIXct(estimated_trace[,1][nrow(estimated_trace)],  origin = og, tz = timez)
	
	num_it				<- (as.integer(as.numeric(difftime(trace_end_time, trace_start_time, units='mins')) / pred_grain) + 1)
	init_it				<- as.integer(cap_grain / pred_grain)
	var					<- grain_unit[[as.character(cap_grain)]]
	
	df_result			<- c()	
	ad_w_tmp 			<- ad_w

	if (0 %in% ad_w_tmp) {

		capacity_real   	<- complete_trace$MEM_UTIL
		capacity_est		<- estimated_trace$MEM_UTIL
		capacity_est_tg		<- NA


		for(safety_margin in util_target) {

			capacity_est_tg		<- capacity_est / as.double(slo_limit - safety_margin)
		
			df_tmp				<- data.frame(complete_trace$TIMESTAMP, capacity_real, capacity_est, capacity_est_tg, 0, trace_number, policy_name, slo_limit, safety_margin, 0)
			names(df_tmp)		<- c("TIMESTAMP", "CAP_REAL", "CAP_EST", "CAP_EST_TG", "ADJUSTMENT", "TRACE", "POLICY", "SLO_LIMIT", "SAFETY_MARGIN", "WINDOW")
		

			df_result 			<- rbind(df_result, df_tmp)
		}

		ad_w_tmp 			<- ad_w_tmp[ad_w_tmp != 0]
	}

	if (length(ad_w_tmp) != 0) {

		#num_it
		df_correction <- foreach(i = init_it:num_it, .combine = "rbind") %dopar% {

			try(system(paste("echo ", (i / num_it * 100), "% > output/result-", trace_number, "-", policy_abr, "-", adjustment_type, "-", capacity_target, ".out", sep = ""), intern = TRUE))
			
			start_win			<- as.numeric(trace_start_time + ((i-1) * var))
			end_win				<- min(as.numeric(trace_start_time + ((i * var) - var_5m)), trace_end_time)
			
			tmp_df_trace_real	<- subset(complete_trace, TIMESTAMP >= start_win)
			tmp_df_trace_real	<- subset(tmp_df_trace_real, TIMESTAMP <= end_win)
			
			tmp_df_trace_est	<- subset(estimated_trace, TIMESTAMP >= start_win)
			tmp_df_trace_est	<- subset(tmp_df_trace_est, TIMESTAMP <= end_win)

			tmp_df_alloc		<- subset(alloc_trace, TIMESTAMP >= start_win)
			tmp_df_alloc		<- subset(tmp_df_alloc, TIMESTAMP <= end_win)
			
			capacity_real   	<- 0
			capacity_est    	<- 0
			
			capacity_real   	<- tmp_df_trace_real$MEM_UTIL
			capacity_est		<- tmp_df_trace_est$MEM_UTIL
			total_cap			<- tmp_df_alloc$MEM_ALLOC
			
			error_adjustment_value	<- if ( adjustment_type == "padding") {
						
						burst_based_padding <- function(data) { 
							fft_data <- fft(data)

							number_of_freq	<- floor((length(data) * 0.5 + 1) * 0.2)

							fft_data[c(1:number_of_freq, ((length(data) - number_of_freq) + 2):length(data))] = 0
							
							burst_pattern <- fft(fft_data, inverse = TRUE)/length(fft_data)
							burst_pattern <- Re(burst_pattern)
							if ( length(burst_pattern[burst_pattern >= 0])/length(burst_pattern) > 0.5 ) {
								max(burst_pattern)
							}else{
								quantile(burst_pattern[burst_pattern >= 0], probs = .8)
							}
						}
						
						remedial_padding <- function(data) { 
							if (length(data) == 0) {
								0
							}else{
								data[data > 0] = 0
								max(0, abs(WMA(x = data, n = length(data), wts = 1:length(data))[length(data)]), na.rm  = TRUE)
							}
						}
						
						adjustment_value_result	<- c()

						for (aw in ad_w_tmp) {

							base_win			<- as.numeric(start_win - (pred_ahead * var ))
							
							past_start_time		<- max(0, (i - (aw + pred_ahead))) * var
							
							past_win_start_time	<- as.numeric(trace_start_time + past_start_time)
							
							df_error			<- subset(diff_trace, TIMESTAMP <= base_win)
							df_error			<- subset(df_error, TIMESTAMP >= past_win_start_time)
							
							tmp_df_error		<- df_error$ERROR_ABS
							
							df_util				<- subset(original_trace, TIMESTAMP <= base_win)
							df_util				<- subset(df_util, TIMESTAMP >= past_win_start_time)
							
							tmp_df_util			<- df_util$MEM_UTIL
							
							adjustment_value 	<- data.frame(ADJ_VAL = max(burst_based_padding(tmp_df_util), remedial_padding(tmp_df_error)), W = aw)
							adjustment_value_result <- rbind(adjustment_value_result, adjustment_value)
						
						}
						adjustment_value_result

					} else {

						base_win		<- as.numeric(start_win - (pred_ahead * var ))
						
						past_start_time		<- max(0, (i - (4032 + pred_ahead))) * var
							
						past_win_start_time	<- as.numeric(trace_start_time + past_start_time)
							
						df_error		<- subset(diff_trace, TIMESTAMP <= base_win)
						df_error		<- subset(df_error, TIMESTAMP >= past_win_start_time)
							
						if ((adjustment_type == "acf_neg") | (adjustment_type == "acf_neg_i")) {
							df_error$ERROR_ABS[df_error$ERROR_ABS >= 0] <- 0
						}
						
						tmp_df_error	<- df_error$ERROR_ABS

						adjustment_value_result <- c()				
	
						if (error_adjustment && (length(tmp_df_error) > 0)) {
							
							
							acf_result		<- acf(tmp_df_error, lag.max = length(tmp_df_error)/2, plot = FALSE)$acf
							acf_values		<- acf_result[2:length(acf_result)]
							
							acf_pos	<- acf_values > 0
							
							i		<- which(acf_pos == FALSE, arr.ind = TRUE)[1]
							
							if (is.na(i)) 	init	<- 1
							else 			init	<- i
							
							acf_value <- acf_values[init:length(acf_values)]
							
							acf_index <- which(acf_value == max(acf_value), arr.ind = TRUE)

							if ((adjustment_type == "acf_i") | (adjustment_type == "acf_neg_i")) {
								acf_index 	<- acf_index + init - pred_ahead
							}

							for (aw in ad_w_tmp) {

								we			<- floor(aw/2)
								first_index	<- max(1, length(tmp_df_error) - acf_index - we)
								last_index	<- min(length(tmp_df_error), length(tmp_df_error) - acf_index + we)
								
								adjustment_value	<- data.frame(ADJ_VAL = abs(min(0, tmp_df_error[first_index:last_index])), W = aw)
								adjustment_value_result <- rbind(adjustment_value_result, adjustment_value)
		
							}

						}

						adjustment_value_result
					}

			df_tmp				<- c()

			if (is.null(error_adjustment_value)) {
				error_adjustment_value = data.frame(ADJ_VAL = 0, W = ad_w_tmp)
			}

			timestamp_slot		<- tmp_df_trace_real$TIMESTAMP[1]
			tmp_value 			<- error_adjustment_value %>% mutate(TIMESTAMP = timestamp_slot, CAP_REAL = capacity_real, CAP_EST = capacity_est,  ADJUSTMENT = ADJ_VAL, WINDOW = W, TRACE = trace_number, POLICY = policy_name, CAP_FINAL = CAP_EST + ADJUSTMENT)
	
			for (safety_margin in util_target) {

				df_tgt 		<- tmp_value %>% mutate(CAP_EST_TG = CAP_FINAL / as.double(slo_limit - safety_margin), SLO_LIMIT = slo_limit, SAFETY_MARGIN = safety_margin)
				df_tmp 		<- rbind(df_tmp, df_tgt)
			}
				
			df_tmp 	<- df_tmp %>% select(TIMESTAMP, CAP_REAL, CAP_EST, CAP_EST_TG, ADJUSTMENT, TRACE, POLICY, SLO_LIMIT, SAFETY_MARGIN, WINDOW)	

			df_tmp
			
		}	
		
		df_result 	<- rbind(df_result, df_correction)
	}

	df_result <- df_result %>% ungroup() %>% arrange(WINDOW, TIMESTAMP, SAFETY_MARGIN)
	df_result
}

run_analysis = function(filename_utilization, filename_allocation, trace_number, trace_type, pred_type, num_periods, policies, input_dir, outpur_dir, util_target, pred_grain, cap_grain, error_adjustment, adjustment_type, adjustment_window, slo_limit) {
	
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
	
	total_cap	<- 16

	dff			<- read.table(filename_utilization, header = T)
	dff_alloc	<- if (file.exists(filename_allocation)) {
					read.table(filename_allocation, header = T)
				}else{
					data.frame(TIMESTAMP = dff$TIMESTAMP, MEM_ALLOC = rep(total_cap, nrow(dff)))
				}
	
	timestamps	<- dff$TIMESTAMP
	trace_util	<- dff$MEM_UTIL
	trace_alloc	<- dff_alloc$MEM_ALLOC
	
	df_trace		<- data.frame(cbind(timestamps, trace_util), 0, "Original")
	names(df_trace)		<- c("TIMESTAMP", "MEM_UTIL", "EXEC_TIME", "POLICY")
	
	df_alloc		<- data.frame(cbind(timestamps, trace_alloc), 0, "Alloc")
	names(df_alloc)		<- c("TIMESTAMP", "MEM_ALLOC", "EXEC_TIME", "POLICY")
			
	for (p in 1:length(policies)) { 
			
		policy				<- policies[p]
		num_period			<- num_periods[p]
			
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
		print(paste("Server: ", trace_number, "-", pred.abr.labels[[policy]], "-", adjustment_window))
				
		capacity_trace		<- generate_capacity_planning(error_adjustment, adjustment_window, diff_trace, original_df_trace, new_df_trace, estimated_trace, tmp_df_alloc, trace_number, util_target, policy, pred.abr.labels[[policy]], pred_grain, cap_grain, adjustment_type, slo_limit)
			
		for (wa in adjustment_window) {

			for (safety_margin in util_target) {

				partial_result 		<- capacity_trace %>% filter(SAFETY_MARGIN == safety_margin, WINDOW == wa)

				file_name_capacity	<- paste(outpur_dir, "capacity/", pred_type, "/server-", trace_number, "-capacity-planning-", wa, "-", pred.abr.labels[[policy]], "-", error_adjustment, "-", adjustment_type, "-", slo_limit, "-",  as.character(safety_margin), "-linear.dat", sep = "")
				write.table(partial_result, file = file_name_capacity, row.names = FALSE)
			}
		}
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

policies				<- c("last_h", "linear_regression", "auto_correlation", "arima", "auto_regressive", "ensemble")
num_periods			<- c(0, 0, 0, 0, 2016, 0)


adjustment_window		<- c(0, 41, 201, 403, 1009, 2017, 4031)

capacity_target		<- c(0.5, 0.4, 0.3, 0.2, 0.1, 0)

input_dir				<- paste("../../prediction/mem/data/pred/", pred_type, "/", sep = "")
outpur_dir			<- "data/"

filename_utilization	<- paste("../../../data/", trace_type, "/mem_util-prod-server-", trace_number, "-", trace_type, ".dat", sep = "")
filename_allocation		<- paste("../../../data/", trace_type, "/mem_alloc-prod-server-", trace_number, "-", trace_type, ".dat", sep = "")
	
registerDoMC(cores = num_cores)
run_analysis(filename_utilization, filename_allocation, trace_number, trace_type, pred_type, num_periods, policies, input_dir, outpur_dir, capacity_target, pred_grain, cap_grain, error_adjustment, adjustment_type, adjustment_window, slo_limit)

diff_mins	<- difftime(Sys.time(), begin, units = 'mins')
print(paste("Minutes:", diff_mins))
print(paste("Hours:", (diff_mins / 60)))
