calculate_scaling = function(capacity_trace_cpu, 
                             capacity_trace_mem, 
                             time_decision, 
                             instance_size_ecu, 
                             instance_size_ghz, 
                             instance_size_mem, 
                             instance_grain, 
                             instance_base, 
                             trace_number, 
                             policy_abr, 
                             adj_w_cpu, 
                             adj_w_mem, 
                             trace_ghz, 
                             ecu_factor) {
	
	timestamps		<- capacity_trace_cpu$TIMESTAMP
	
	inst_number		<- max(instance_base, max(ceiling((capacity_trace_cpu$CAP_EST_TG[1] * trace_ghz) / ecu_factor / instance_size_ecu), ceiling(capacity_trace_mem$CAP_EST_TG[1] / instance_size_mem)))	
	inst_alloc		<- c()
	
	for (j in 1:inst_number) {
		inst_alloc[[j]] <- c(j, 0)
	}
	
	inst_alloc_list	<- c(inst_number)
	
	for (i in 2:nrow(capacity_trace_cpu)) {
	# for (i in 2:10) {     
		
		try(system(paste("echo ", i, " > output/result-", trace_number, "-", policy_abr, "-", adj_w_cpu, "_", adj_w_mem, ".out", sep = ""), intern = TRUE))
		
		cap <- max(ceiling((capacity_trace_cpu$CAP_EST_TG[i] * trace_ghz) / ecu_factor / instance_size_ecu), ceiling(capacity_trace_mem$CAP_EST_TG[i] / instance_size_mem))
		
		#update lifetime
		for (j in 1:length(inst_alloc)) {
			inst_alloc[[j]][2] <- inst_alloc[[j]][2] + 5
		}	
		
		#remove instances
		if ((cap - inst_number) < 0) {
		     
			inst.kill <- min(abs(cap - inst_number), (inst_number - instance_base)) 
			
			for (k in 1:length(inst_alloc)) {
				if (inst.kill != 0 && !is.na(inst_alloc[[k]][2])) {
					
					if (((inst_alloc[[k]][2] - (as.integer(inst_alloc[[k]][2] / instance_grain) * instance_grain)) %% time_decision) == 0) {
						
						inst_alloc[[k]] <- NA
						inst.kill <- inst.kill - 1
						inst_number <- inst_number - 1
					}
				}
			}
			
		} else if ((cap - inst_number) > 0) { #add instances
			
			inst.add <- abs(cap - inst_number)
			
			for (n in 1:inst.add) {
				inst_alloc[[length(inst_alloc) + 1]] <- c(length(inst_alloc) + 1, 0)
			}
			
			inst_number	<- cap
		} 
		
		inst_alloc_list	<- c(inst_alloc_list, inst_number)
	}
	
	df_scaling	     <- data.frame(timestamps[1:length(inst_alloc_list)], inst_alloc_list)
	names(df_scaling)   <- c("TIMESTAMP", "SCALING")
     
	return(df_scaling)
}

run_analysis = function(filename_utilization_cpu, 
                        filename_utilization_mem, 
                        filename_allocation_cpu, 
                        filename_allocation_mem, 
                        trace_number, 
                        trace_type, 
                        pred_type, 
                        num_period, 
                        policy, 
                        input_dir_cpu, 
                        input_dir_mem, 
                        outpur_dir, 
                        instance_size_ecu, 
                        instance_size_ghz, 
                        instance_size_mem, 
                        time_decision, 
                        instance_grain, 
                        instance_base, 
                        pred_grain, 
                        cap_grain, 
                        error_adjustment, 
                        adjustment_type, 
                        ad_w_value_cpu, 
                        ad_w_value_mem, 
                        capacity_target_cpu, 
                        capacity_target_mem, 
                        trace_ghz, 
                        ecu_factor,
                        slo_limit) {
	
	
	pred.labels	<- list(last_h	= "Last window (LW)",
			auto_regressive	= "Auto-regressive (AR)",
			arima			= "Auto-regressive integrated moving average (ARIMA)",
			auto_correlation	= "Auto-correlation (AC)",
			linear_regression	= "Linear regression (LR)",
			ensemble		     = "Ensemble"
	)
	
	pred.abr.labels	<- list(last_h	= "LW",
			auto_regressive		= "AR",
			arima				= "ARIMA",
			auto_correlation		= "AC",
			linear_regression		= "LR",
			ensemble			     = "EN"
	)
	
	dff_cpu	     <- read.table(filename_utilization_cpu, header = T)
	dff_alloc_cpu	<- if (file.exists(filename_allocation_cpu)) {
				     read.table(filename_allocation_cpu, header = T)
     			}else{
     				data.frame(CPU_ALLOC = rep(16, nrow(dff_cpu)))
     			}

	dff_mem	     <- read.table(filename_utilization_mem, header = T)
	dff_alloc_mem	<- if (file.exists(filename_allocation_mem)) {
				     read.table(filename_allocation_mem, header = T)
     			}else{
     				data.frame(MEM_ALLOC = rep(16, nrow(dff_mem)))
     			}
	
	timestamps_cpu	     <- dff_cpu$TIMESTAMP
	trace_util_cpu	     <- dff_cpu$CPU_UTIL
	trace_alloc_cpu     <- dff_alloc_cpu$CPU_ALLOC
	
	timestamps_mem	     <- dff_mem$TIMESTAMP
	trace_util_mem	     <- dff_mem$MEM_UTIL
	trace_alloc_mem	<- dff_alloc_mem$MEM_ALLOC

	df_trace_cpu		<- data.frame(cbind(timestamps_cpu, trace_util_cpu), 0, "Original")
	names(df_trace_cpu)	<- c("TIMESTAMP", "CPU_UTIL", "EXEC_TIME", "POLICY")
	
	df_alloc_cpu		<- data.frame(cbind(timestamps_cpu, trace_alloc_cpu), 0, "Alloc")
	names(df_alloc_cpu)	<- c("TIMESTAMP", "CPU_ALLOC", "EXEC_TIME", "POLICY")

	df_trace_mem		<- data.frame(cbind(timestamps_mem, trace_util_mem), 0, "Original")
	names(df_trace_mem)	<- c("TIMESTAMP", "MEM_UTIL", "EXEC_TIME", "POLICY")
	
	df_alloc_mem		<- data.frame(cbind(timestamps_mem, trace_alloc_mem), 0, "Alloc")
	names(df_alloc_mem)	<- c("TIMESTAMP", "MEM_ALLOC", "EXEC_TIME", "POLICY")
	
	#CPU			
	estimated_trace_cpu	<- read.table(paste(input_dir_cpu, "server-", trace_number, "-estimated-trace-", pred.abr.labels[[policy]], "-", num_period, "-", pred_type, ".dat", sep = ""), header = T)
	diff_trace_cpu		<- read.table(paste(input_dir_cpu, "server-", trace_number, "-diff-trace-", pred.abr.labels[[policy]], "-", num_period, "-", pred_type, ".dat", sep = ""), header = T)	
			
	tmp_df_trace_cpu	<- subset(df_trace_cpu, TIMESTAMP >= estimated_trace_cpu$TIMESTAMP[1])
	tmp_df_trace_cpu	<- subset(tmp_df_trace_cpu, TIMESTAMP <= estimated_trace_cpu$TIMESTAMP[nrow(estimated_trace_cpu)])
			
	tmp_df_alloc_cpu	<- subset(df_alloc_cpu, TIMESTAMP >= estimated_trace_cpu$TIMESTAMP[1])
	tmp_df_alloc_cpu	<- subset(tmp_df_alloc_cpu, TIMESTAMP <= estimated_trace_cpu$TIMESTAMP[nrow(estimated_trace_cpu)])

	tmp_df_trace_cpu[,5] <- num_period
	names(tmp_df_trace_cpu)	<- c("TIMESTAMP", "CPU_UTIL", "EXEC_TIME", "POLICY", "PERIOD")
			
	new_df_trace_cpu	<- data.frame(tmp_df_trace_cpu$TIMESTAMP, tmp_df_trace_cpu$CPU_UTIL * tmp_df_alloc_cpu$CPU_ALLOC / 100, tmp_df_trace_cpu$EXEC_TIME, tmp_df_trace_cpu$POLICY, tmp_df_trace_cpu$PERIOD)
	names(new_df_trace_cpu)	<- c("TIMESTAMP", "CPU_UTIL", "EXEC_TIME", "POLICY", "PERIOD")
	
	#MEM
	estimated_trace_mem	<- read.table(paste(input_dir_mem, "server-", trace_number, "-estimated-trace-", pred.abr.labels[[policy]], "-", num_period, "-", pred_type, ".dat", sep = ""), header = T)
	diff_trace_mem		<- read.table(paste(input_dir_mem, "server-", trace_number, "-diff-trace-", pred.abr.labels[[policy]], "-", num_period, "-", pred_type, ".dat", sep = ""), header = T)	

	tmp_df_trace_mem	<- subset(df_trace_mem, TIMESTAMP >= estimated_trace_mem$TIMESTAMP[1])
	tmp_df_trace_mem	<- subset(tmp_df_trace_mem, TIMESTAMP <= estimated_trace_mem$TIMESTAMP[nrow(estimated_trace_mem)])
			
	tmp_df_alloc_mem	<- subset(df_alloc_mem, TIMESTAMP >= estimated_trace_mem$TIMESTAMP[1])
	tmp_df_alloc_mem	<- subset(tmp_df_alloc_mem, TIMESTAMP <= estimated_trace_mem$TIMESTAMP[nrow(estimated_trace_mem)])

	tmp_df_trace_mem[,5] <- num_period
	names(tmp_df_trace_mem)	<- c("TIMESTAMP", "MEM_UTIL", "EXEC_TIME", "POLICY", "PERIOD")
			
	new_df_trace_mem	<- data.frame(tmp_df_trace_mem$TIMESTAMP, tmp_df_trace_mem$MEM_UTIL * tmp_df_alloc_mem$MEM_ALLOC / 100, tmp_df_trace_mem$EXEC_TIME, tmp_df_trace_mem$POLICY, tmp_df_trace_mem$PERIOD)
	names(new_df_trace_mem)	<- c("TIMESTAMP", "MEM_UTIL", "EXEC_TIME", "POLICY", "PERIOD")
				
	#Perfect
	print("")
	print(paste("Server: ", trace_number, "-", pred.abr.labels[[policy]], "-", ad_w_value_cpu, "_", ad_w_value_mem))

	file_name_capacity_cpu	<- paste("../cpu/data/capacity/", pred_type, "/server-", trace_number, "-capacity-planning-0-PERFECT-FALSE-", as.character(capacity_target_cpu), "-linear.dat", sep = "")
	capacity_trace_cpu	     <- read.table(file_name_capacity_cpu, header = T)
	
	file_name_capacity_mem	<- paste("../mem/data/capacity/", pred_type, "/server-", trace_number, "-capacity-planning-0-PERFECT-FALSE-", as.character(capacity_target_mem), "-linear.dat", sep = "")
	capacity_trace_mem		<- read.table(file_name_capacity_mem, header = T)
		    
	file_name_scaling  	     <- paste(outpur_dir, "scaling/", pred_type, "/server-", trace_number, "-scaling-0-PERFECT-60-55-FALSE-", as.character(capacity_target_cpu), "-", as.character(capacity_target_mem), "-", instance_size_ecu, "-", instance_size_ghz, "-", instance_size_mem, "-", ecu_factor, ".dat", sep = "")
	if (!file.exists(file_name_scaling)) {
		scaling_trace	    <- calculate_scaling(capacity_trace_cpu, capacity_trace_mem, time_decision, instance_size_ecu, instance_size_ghz, instance_size_mem, instance_grain, instance_base, trace_number, pred.abr.labels[[policy]], ad_w_value_cpu, ad_w_value_mem, trace_ghz, ecu_factor) 			
		write.table(scaling_trace, file = file_name_scaling, row.names = FALSE)
	}
}

begin	<- Sys.time()

args  <- commandArgs(trailingOnly = TRUE)

time_decision			<- 55
instance_grain			<- 60
instance_base			<- 1
pred_grain			<- 5
cap_grain				<- 5
adjustment_type		<- "acf_neg_i"

trace_number			<- as.integer(args[1])
policy_number			<- as.integer(args[2])
adjustment_window_cpu	<- as.integer(args[3])
adjustment_window_mem	<- as.integer(args[4])
trace_type			<- as.character(args[5])
pred_type				<- as.character(args[6])
adjustment_type		<- as.character(args[7])
slo_limit		          <- as.numeric(args[8])
capacity_target_cpu		<- as.numeric(args[9])
capacity_target_mem		<- as.numeric(args[10])
instance_size_ecu		<- as.numeric(args[11])
instance_size_ghz		<- as.numeric(args[12])
instance_size_mem		<- as.numeric(args[13])

input_dir_cpu			<- paste("../../prediction/cpu/data/pred/", pred_type, "/", sep = "")
input_dir_mem   		<- paste("../../prediction/mem/data/pred/", pred_type, "/", sep = "")
outpur_dir			<- "data/"

error_adjustment		<- T

policies				<- c("last_h", "linear_regression", "auto_correlation", "arima", "auto_regressive", "ensemble")
num_periods			<- c(0, 0, 0, 0, 2016, 0)

policy 				<- policies[policy_number]
num_period			<- num_periods[policy_number]

filename_utilization_cpu	<- paste("../../../data/", trace_type, "/cpu_util-prod-server-", trace_number, "-", trace_type, ".dat", sep = "")
filename_allocation_cpu	<- paste("../../../data/", trace_type, "/cpu_alloc-prod-server-", trace_number, "-", trace_type, ".dat", sep = "")

filename_utilization_mem <- paste("../../../data/", trace_type, "/mem_util-prod-server-", trace_number, "-", trace_type, ".dat", sep = "")
filename_allocation_mem	<- paste("../../../data/", trace_type, "/mem_alloc-prod-server-", trace_number, "-", trace_type, ".dat", sep = "")

ecu_factor               = 0.7602

filename_trace			= "../../../data/normal/machine_data.dat"
trace_info 			= read.table(filename_trace, header = T)
trace_info 			= subset(trace_info, TRACE == trace_number)
trace_ghz 			= trace_info$GHZ[1]

if (is.na(instance_size_ghz)) {
     trace_ghz           = 1
     ecu_factor          = 1
     instance_size_ecu   = 1
}

print(paste("Trace GHz:", trace_ghz))

run_analysis(filename_utilization_cpu, 
             filename_utilization_mem, 
             filename_allocation_cpu, 
             filename_allocation_mem, 
             trace_number, 
             trace_type, 
             pred_type, 
             num_period, 
             policy, 
             input_dir_cpu, 
             input_dir_mem, 
             outpur_dir, 
             instance_size_ecu, 
             instance_size_ghz, 
             instance_size_mem, 
             time_decision, 
             instance_grain, 
             instance_base, 
             pred_grain, 
             cap_grain, 
             error_adjustment, 
             adjustment_type, 
             adjustment_window_cpu, 
             adjustment_window_mem, 
             capacity_target_cpu, 
             capacity_target_mem, 
             trace_ghz, 
             ecu_factor,
             slo_limit)

print(paste("Trace:", trace_number, " - ", policy, " - ", adjustment_window_cpu, " - ", adjustment_window_mem))

diff_mins	<- difftime(Sys.time(), begin, units = 'mins')
print(paste("Minutes:", diff_mins))
print(paste("Hours:", (diff_mins / 60)))
