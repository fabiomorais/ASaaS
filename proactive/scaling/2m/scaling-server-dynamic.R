calculate_scaling = function(capacity_trace_cpu,
						capacity_trace_mem,
						time_decision, 
                             	instance_size_ecu, 
                             	instance_size_ghz, 
                             	instance_size_mem, 
						instance_grain, 
						instance_base, 
						inst_number = NULL, 
						inst_alloc = NULL) {
	
     
	timestamps		<- capacity_trace_cpu$TIMESTAMP
	
	inst_alloc_list	<- c()
	init_it			<- 1
	
	if (is.null(inst_alloc)) {
		
		inst_alloc		<- c()
		inst_number		<- max(instance_base, max(ceiling((capacity_trace_cpu$CAP_EST_TG[1] * trace_ghz) / ecu_factor / instance_size_ecu), ceiling(capacity_trace_mem$CAP_EST_TG[1] / instance_size_mem)))

		for (j in 1:inst_number) {
			inst_alloc[[j]] <- c(j, 0)
		}
		
		init_it			<- 2
		inst_alloc_list	<- c(inst_number)
		
	}
	
	for (i in init_it:nrow(capacity_trace_cpu)) {
		
		cap <- max(ceiling((capacity_trace_cpu$CAP_EST_TG[i] * trace_ghz) / ecu_factor / instance_size_ecu), ceiling(capacity_trace_mem$CAP_EST_TG[i] / instance_size_mem))


		for (j in 1:length(inst_alloc)) {		# update instances
			inst_alloc[[j]][2] <- inst_alloc[[j]][2] + 5
		}

		if ((cap - inst_number) < 0) {		# remove instances
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
			
		}else if ((cap - inst_number) > 0) {	#add instances
			
			inst.add <- abs(cap - inst_number)
			
			for (n in 1:inst.add) {
				inst_alloc[[length(inst_alloc) + 1]] <- c(length(inst_alloc) + 1, 0)
			}
			
			inst_number	<- cap
		} 
		
		inst_alloc_list	<- c(inst_alloc_list, inst_number)
	}
	
	# Uma copia da lista sem os NAs
	new_inst_alloc	<- c()
	
	for (k in 1:length(inst_alloc)) {
		if (!is.na(inst_alloc[[k]][2])) {
			new_inst_alloc[[length(new_inst_alloc) + 1]] <- inst_alloc[[k]]
		}
	}
	
	df_scaling	<- data.frame(timestamps, inst_alloc_list)
	names(df_scaling) <- c("TIMESTAMP", "SCALING")
	
	list(SCALING = df_scaling, HISTORY = new_inst_alloc)
}

calculate_cost_value = function(scaling_trace, instance_grain) {
	
	tmp     <- scaling_trace$SCALING[2:nrow(scaling_trace)] - scaling_trace$SCALING[1:(nrow(scaling_trace) - 1)]
	tmp     <- tmp[tmp < 0]
	
	cost_value <- sum(scaling_trace$SCALING) + sum(abs(tmp), na.rm = T)
	
	cost_value / (instance_grain / 5)	
}

calculate_violation_timestamp = function(df_trace_cpu,
								df_trace_mem,
								scaling_trace, 
								alloc_trace_cpu,
								alloc_trace_mem, 
								target_value, 
								target_residual, 
								residual_value) {
	
	violation_timestamp_cpu <- c()
	violation_timestamp_mem <- c()
	residual_values     <- c()
	
	if (is.null(residual_value)) residual_value <- 0

	for (i in scaling_trace$TIMESTAMP) {
		
		total_cap_cpu	<- subset(alloc_trace_cpu, TIMESTAMP == i)$CPU_ALLOC
		util_value_cpu	<- round(subset(df_trace_cpu, TIMESTAMP == i)$CPU_UTIL, digits = 1)
		
		total_cap_mem	<- subset(alloc_trace_mem, TIMESTAMP == i)$MEM_ALLOC
		util_value_mem	<- round(subset(df_trace_mem, TIMESTAMP == i)$MEM_UTIL, digits = 1)

		scaling_value	<- subset(scaling_trace, TIMESTAMP == i)$SCALING
		tmp_residual 	<- residual_value

		residual_value	<- 0

		util_global_cpu <- if (!is.na(target_residual)) {
							util_value_cpu + tmp_residual
							
						}else{
							
							util_value_cpu
						}

		util_global_mem <- if (!is.na(target_residual)) {
							util_value_mem + tmp_residual
							
						}else{
							
							util_value_mem
						}

		if (util_global_cpu > (scaling_value * target_value)) {
			
			violation_timestamp_cpu <- c(violation_timestamp_cpu, i)
			residual_values 		<- c(residual_values, 0)
			
		}

		if (util_global_mem > (scaling_value * target_value)) {
			
			violation_timestamp_mem <- c(violation_timestamp_mem, i)
			residual_values 		<- c(residual_values, 0)
			
		}
						
	}

	violation_timestamp = unique(sort(c(violation_timestamp_cpu, violation_timestamp_mem)))

	list(TIMESTAMP = violation_timestamp, RESIDUAL = residual_values)
}


run_analysis = function(sla_violation_freq, 
					only_with_correction, 
					behavior_type, 
					original_choice_position, 
					reaction_choice_position, 
					periodicity, 
					filename_utilization_cpu, 
                         filename_utilization_mem, 
                         filename_allocation_cpu, 
                         filename_allocation_mem,  
					trace_number, 
					pred_type, 
					num_periods, 
					policies, 
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
					with_correction, 
					correction_subset, 
					target_residual, 
					safety_margin_cpu,
					safety_margin_mem, 
					violation_target,
					trace_ghz, 
                         ecu_factor,
                         slo_limit) {
	
	pred.labels	<- list(last_h	= "Last window (LW)",
			auto_regressive	= "Auto-regressive (AR)",
			arima			= "Auto-regressive integrated moving average (ARIMA)",
			sarima			= "Sazonal auto-regressive integrated moving average (ARIMA)",
			auto_correlation	= "Auto-correlation (AC)",
			linear_regression	= "Linear regression (LR)",
			ensemble		     = "Ensemble"
	)
	
	pred.abr.labels	<- list(last_h	= "LW",
			auto_regressive		= "AR",
			arima				= "ARIMA",
			sarima				= "SARIMA",
			auto_correlation		= "AC",
			linear_regression		= "LR",
			ensemble			 	= "EN"
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
	
	timestamps_cpu	    <- dff_cpu$TIMESTAMP
	trace_util_cpu	    <- dff_cpu$CPU_UTIL
	trace_alloc_cpu     <- dff_alloc_cpu$CPU_ALLOC
	
	timestamps_mem	    <- dff_mem$TIMESTAMP
	trace_util_mem	    <- dff_mem$MEM_UTIL
	trace_alloc_mem		<- dff_alloc_mem$MEM_ALLOC

	df_trace_cpu		<- data.frame(cbind(timestamps_cpu, trace_util_cpu), 0, "Original")
	names(df_trace_cpu)	<- c("TIMESTAMP", "CPU_UTIL", "EXEC_TIME", "POLICY")
	
	df_alloc_cpu		<- data.frame(cbind(timestamps_cpu, trace_alloc_cpu), 0, "Alloc")
	names(df_alloc_cpu)	<- c("TIMESTAMP", "CPU_ALLOC", "EXEC_TIME", "POLICY")

	df_trace_mem		<- data.frame(cbind(timestamps_mem, trace_util_mem), 0, "Original")
	names(df_trace_mem)	<- c("TIMESTAMP", "MEM_UTIL", "EXEC_TIME", "POLICY")
	
	df_alloc_mem		<- data.frame(cbind(timestamps_mem, trace_alloc_mem), 0, "Alloc")
	names(df_alloc_mem)	<- c("TIMESTAMP", "MEM_ALLOC", "EXEC_TIME", "POLICY")

	file_name_capacity	<- paste("../cpu/data/capacity/", pred_type, "/server-", trace_number, "-capacity-planning-0-PERFECT-FALSE-", as.character(safety_margin), "-linear.dat", sep = "")
	perfect_capacity 	<- read.table(file_name_capacity, header = T)
	
	df_hours		<- data.frame(HOUR = format(as.POSIXct(perfect_capacity$TIMESTAMP - (6 * 12 * 300),  origin = "1970-01-01 00:00:00", tz = "GMT"), "%H:%M:%S"))
	index_hours	<- row.names(subset(df_hours, HOUR == "00:00:00"))
	
	tmp_est_trace	<- perfect_capacity[as.numeric(index_hours),]
	
	df_period		<- data.frame(PERIOD = format(as.POSIXct(tmp_est_trace$TIMESTAMP - (6 * 12 * 300),  origin = "1970-01-01 00:00:00", tz = "GMT"), "%w"))
	index_period   <- row.names(subset(df_period, PERIOD == "0"))
	
	first_timestamp	<- tmp_est_trace[as.numeric(index_period),]$TIMESTAMP[1]
	
	period_number	<- ceiling((perfect_capacity$TIMESTAMP[nrow(perfect_capacity)] - first_timestamp) / (periodicity * 300))
	num_days		<- period_number - 7	

	perfect_cost	<- 0
	
	start_experiment_time	<- first_timestamp + (2016 * 300) - (288 * 300)
	
	perfect_capacity		<- subset(perfect_capacity, TIMESTAMP >= (first_timestamp + (2016 * 300)))
	
	global_scaling			<- c()
	global_analysis		<- c()
	global_violation		<- c()
	global_period_analysis	<- c()	
	df_optmum				<- c()
	
	inst_number_map	<- new.env(hash = T, parent = emptyenv())
	inst_alloc_map	     <- new.env(hash = T, parent = emptyenv())
	residual_map	     <- new.env(hash = T, parent = emptyenv())
	
	choice_position		<- original_choice_position
	operation_mode			<- "Non-conservative"
	
	best_scenario			<- NULL
	base_scenario 			<- NULL
	has_violation			<- FALSE
	comes_from_violation	<- FALSE
	
	reactivities	<- 0
	violation_day	<- 0
	
	ad_w_values = unique(c(ad_w_value_cpu, ad_w_value_mem))

	slo_limit 		<- ceiling(violation_target)

	timestamps		<- (seq(0, (period_number - 1)) * 300 * periodicity) + first_timestamp
	
	file_name_period_scaling <- paste(outpur_dir, "analysis/", pred_type, "/period/dynamic/", adjustment_type, "/server-", trace_number, "-global-dynamic-scaling-", periodicity, "-", original_choice_position, "-", reaction_choice_position, "-", behavior_type, "-", only_with_correction, "-", sla_violation_freq, "-", with_correction, "-", correction_subset, "-", as.character(safety_margin), "-linear-linear-", as.character(target_residual), "-", as.character(violation_target), ".dat", sep = "")
	
	if (!file.exists(file_name_period_scaling)) {
		
		w <- 1
		while (w <= period_number) {
			
			start_period        <- timestamps[w]
			end_period		<- min(perfect_capacity$TIMESTAMP[nrow(perfect_capacity)], (timestamps[w + 1] - 300), na.rm = T)
			real_period		<- ((end_period - start_period + 300) / 300)
			
			print(paste("Period:", w, "/", period_number, "-", real_period, "slots"))
			print("")	
			
			tmp_scaling			<- c()
			df_period_analysis	<- c()
			df_period_violation	<- c()
			
			tmp_inst_number_map	<- new.env(hash = T, parent = emptyenv())
			tmp_inst_alloc_map	<- new.env(hash = T, parent = emptyenv())
			tmp_residual_map	<- new.env(hash = T, parent = emptyenv())
			
			for (i in 1:length(ad_w_values)) {
				
				ad_w		<- ad_w_values[i]
								
				for (p in 1:length(policies)) { 
					
					policy		<- policies[p]
					num_period	<- num_periods[p]

					tmp_df_trace_cpu	<- subset(df_trace_cpu, TIMESTAMP >= start_period)
					tmp_df_trace_cpu	<- subset(tmp_df_trace_cpu, TIMESTAMP <= end_period)
					
					tmp_df_alloc_cpu	<- subset(df_alloc_cpu, TIMESTAMP >= start_period)
					tmp_df_alloc_cpu	<- subset(tmp_df_alloc_cpu, TIMESTAMP <= end_period)
					
					new_df_trace_cpu	<- data.frame(tmp_df_trace_cpu$TIMESTAMP, tmp_df_trace_cpu$CPU_UTIL * tmp_df_alloc_cpu$CPU_ALLOC / 100)
					names(new_df_trace_cpu)	<- c("TIMESTAMP", "CPU_UTIL")
					
					tmp_df_trace_mem	<- subset(df_trace_mem, TIMESTAMP >= start_period)
					tmp_df_trace_mem	<- subset(tmp_df_trace_mem, TIMESTAMP <= end_period)
					
					tmp_df_alloc_mem	<- subset(df_alloc_mem, TIMESTAMP >= start_period)
					tmp_df_alloc_mem	<- subset(tmp_df_alloc_mem, TIMESTAMP <= end_period)
					
					new_df_trace_mem	<- data.frame(tmp_df_trace_mem$TIMESTAMP, tmp_df_trace_mem$MEM_UTIL * tmp_df_alloc_mem$MEM_ALLOC / 100)
					names(new_df_trace_mem)	<- c("TIMESTAMP", "MEM_UTIL")

					key_scenario		<- paste(pred.abr.labels[[policy]], "_", ad_w, sep = "")
					
					file_name_capacity	<- paste("../cpu/data/capacity/", pred_type, "/server-", trace_number, "-capacity-planning-", ad_w, "-", pred.abr.labels[[policy]], "-", error_adjustment, "-", adjustment_type, "-", as.character(slo_limit), "-", as.character(safety_margin), "-linear.dat", sep = "")
					capacity_trace_cpu	<- read.table(file_name_capacity, header = T)
					
					file_name_capacity	<- paste("../mem/data/capacity/", pred_type, "/server-", trace_number, "-capacity-planning-", ad_w, "-", pred.abr.labels[[policy]], "-", error_adjustment, "-", adjustment_type, "-", as.character(slo_limit), "-", as.character(safety_margin), "-linear.dat", sep = "")
					capacity_trace_mem	<- read.table(file_name_capacity, header = T)
					
					capacity_trace_cpu	<- subset(capacity_trace_cpu, TIMESTAMP >= start_period)
					capacity_trace_cpu	<- subset(capacity_trace_cpu, TIMESTAMP <= end_period)
					
					capacity_trace_mem	<- subset(capacity_trace_mem, TIMESTAMP >= start_period)
					capacity_trace_mem	<- subset(capacity_trace_mem, TIMESTAMP <= end_period)

					tmp_base_scenario	<- key_scenario
					
					if (!is.null(base_scenario)) { 
						tmp_base_scenario <- base_scenario
					} 
					
					inst_alloc			<- inst_alloc_map[[tmp_base_scenario]]
					inst_number			<- inst_number_map[[tmp_base_scenario]] 
					residual_value		<- residual_map[[tmp_base_scenario]]
					
					scaling_result	    <- calculate_scaling(	capacity_trace_cpu,
														capacity_trace_mem, 
														time_decision, 
														instance_size_ecu, 
                             									instance_size_ghz, 
                             									instance_size_mem, 
														instance_grain, 
														instance_base, 
														inst_number, 
														inst_alloc)


					scaling_trace		     <- scaling_result$SCALING
					inst_alloc			<- scaling_result$HISTORY
					inst_number			<- scaling_trace$SCALING[nrow(scaling_trace)]

					tmp_df				<- data.frame(	TIMESTAMP = scaling_trace$TIMESTAMP,
														SCALING = scaling_trace$SCALING,
														POLICY = pred.abr.labels[[policy]],
														CORRECTION = ad_w
													 )
					
					tmp_scaling			<- rbind(tmp_scaling, tmp_df)

					violation_analysis	     <- calculate_violation_timestamp(	new_df_trace_cpu,
																	new_df_trace_mem,
																	scaling_trace, 
																	tmp_df_alloc_cpu,
																	tmp_df_alloc_mem, 
																	violation_target, 
																	target_residual, 
																	residual_value)
					
					violation_timestamp <- violation_analysis$TIMESTAMP
					violation_number	<- length(violation_timestamp)
					residual_value 	<- 0

					tmp_inst_alloc_map[[key_scenario]]	<- inst_alloc
					tmp_inst_number_map[[key_scenario]] <- inst_number

					tmp_residual_map[[key_scenario]]	<- 0

					
					cost_value			<- calculate_cost_value(scaling_trace, instance_grain)
					
					tmp_period_analysis		<- data.frame(	POLICY = pred.abr.labels[[policy]],
															CORRECTION = ad_w,
															VIOLATION = violation_number,
															COST = cost_value,
															PERIOD = w,
															TIMESTAMP = start_period
													)
					
					residual_value_null 	<- if (is.null(violation_timestamp)) { NULL } else {0 }

					tmp_violation_analysis	<- data.frame(	POLICY = rep(pred.abr.labels[[policy]], violation_number),
															CORRECTION = rep(ad_w, violation_number), 
															TIMESTAMP = violation_timestamp,
															RESIDUAL = residual_value_null

					)
					
					df_period_analysis	<- rbind(df_period_analysis, tmp_period_analysis)
					df_period_violation	<- rbind(df_period_violation, tmp_violation_analysis)
				}			
			}
			
			day_number	<- if (start_period >= start_experiment_time) {
				
				start_time			<- as.POSIXct(timestamps[1],  origin = "1970-01-01 00:00:00", tz = "GMT")
				end_time			<- as.POSIXct(start_period,  origin = "1970-01-01 00:00:00", tz = "GMT")
			
				floor(as.numeric(difftime(end_time, start_time, units = 'days') + 1)) - 7

			}else{
				0
			}
			
			if (!is.null(best_scenario)) {
				
				splited_scenario 		<- strsplit(best_scenario, split = "_")
				
				tmp_df_violation        <- subset(df_period_violation, POLICY == splited_scenario[[1]][1])
				tmp_df_violation        <- subset(tmp_df_violation, CORRECTION == splited_scenario[[1]][2])
				
				if (!has_violation && behavior_type == "reactive" && nrow(tmp_df_violation) > 0) {
					
					timestamp_violation		<- tmp_df_violation$TIMESTAMP[1]
					
					fut_timestamp		<- min(timestamp_violation + 300, perfect_capacity$TIMESTAMP[nrow(perfect_capacity)])
					
					timestamps			<- unique(as.numeric(na.omit(c(timestamps[1:w], fut_timestamp, timestamps[(w + 1):length(timestamps)]))))
					
					period_number		<- length(timestamps)
					
					has_violation			<- TRUE
					comes_from_violation	<- TRUE
					reactivities	<- reactivities + 1
					
					if (!is.na(sla_violation_freq)) {
 						violation_day	<- day_number
 					}
					
				}else{
					
					tmp_df_scaling          <- subset(tmp_scaling, POLICY == splited_scenario[[1]][1])
					tmp_df_scaling       	<- subset(tmp_df_scaling, CORRECTION == splited_scenario[[1]][2])
					tmp_df_scaling			<- cbind(tmp_df_scaling, data.frame(SCENARIO = rep(best_scenario, nrow(tmp_df_scaling))))
					
					tmp_df_analysis         <- subset(df_period_analysis, POLICY == splited_scenario[[1]][1])
					tmp_df_analysis         <- subset(tmp_df_analysis, CORRECTION == splited_scenario[[1]][2])
					
					
					df_analysis             <- data.frame(	POLICY = splited_scenario[[1]][1],
															CORRECTION = splited_scenario[[1]][2],
															PERIOD = w,
															VIOLATION = tmp_df_analysis$VIOLATION,
															COST = tmp_df_analysis$COST
														)
					
					global_scaling  	<- rbind(global_scaling, tmp_df_scaling)
					global_analysis 	<- rbind(global_analysis, df_analysis)
					global_violation 	<- rbind(global_violation, tmp_df_violation)
					
					has_violation		<- FALSE
					
				}
			}
			
			#print("")
			if (!has_violation) {
				
				global_period_analysis	<- rbind(global_period_analysis, df_period_analysis)
				
				if (start_period >= start_experiment_time) {
					
					#pegar os ultimos dias
					history_length		<- 2016 / periodicity
					timestamp_period	<- df_period_analysis$TIMESTAMP[1]
					
					print(paste("Operation mode:", operation_mode))
					print(paste("Day number:", day_number))
					
					start_time		<- as.POSIXct(timestamps[1],  origin = "1970-01-01 00:00:00", tz = "GMT")
					next_end_time		<- as.POSIXct(min(perfect_capacity$TIMESTAMP[nrow(perfect_capacity)], timestamps[w + 1], na.rm = T),  origin = "1970-01-01 00:00:00", tz = "GMT")
					
					next_day_number	<- floor(as.numeric(difftime(next_end_time, start_time, units = 'days') + 1)) - 7
					
					if (!is.na(sla_violation_freq)) {
						if (violation_day != 0) {
						
							sla_conservative_period	<-	ceiling(violation_day / sla_violation_freq) * sla_violation_freq
						
							if (day_number != next_day_number && next_day_number > sla_conservative_period) {
								choice_position	<- original_choice_position
								violation_day	<- 0
								operation_mode	<- "Non-conservative"
							}else{
								operation_mode	<- "Conservative"				
							}
						}else{
							operation_mode	<- "Non-conservative"
						}
					}
					
					tmp_period_analysis	<- subset(global_period_analysis, TIMESTAMP >= (timestamp_period - (((history_length - 1) * periodicity)  * 300)))
					
					print(paste("Number of considered periods:", length(unique(tmp_period_analysis$PERIOD))))
					
					tmp_period			<- c()
					for (i in 1:length(ad_w_values)) {
						for (p in 1:length(policies)) { 
							
							ad_w		<- ad_w_values[i]
							policy		<- policies[p]
							
							tmp_df		<- subset(tmp_period_analysis, POLICY == pred.abr.labels[[policy]])
							tmp_df		<- subset(tmp_df, CORRECTION == ad_w)
							
							tmp_period	<- rbind(tmp_period, data.frame(POLICY = pred.abr.labels[[policy]], CORRECTION = ad_w, 
							                                           VIOLATION = sum(tmp_df$VIOLATION), COST = sum(tmp_df$COST)))
						}
					}
					
					tmp_period		<- sort(tmp_period, f =~ VIOLATION + COST)
					violation_number	<- tmp_period$VIOLATION[1]
					
					tmp_choice_position		<- 0
					
					if (comes_from_violation) {
						tmp_min_violation	<- subset(tmp_period, VIOLATION == violation_number)
						
						if (is.na(reaction_choice_position)) {
							tmp_choice_position	<- nrow(tmp_min_violation)
						}else{
							tmp_choice_position	<- min(nrow(tmp_min_violation), reaction_choice_position)
						}
 
						if (!is.na(sla_violation_freq)) {
							choice_position		<- NA
						}
					}else{
						tmp_min_violation	<- subset(tmp_period, VIOLATION == violation_number)
						if (is.na(choice_position)) {
							tmp_choice_position	<- nrow(tmp_min_violation)
						}else{
							tmp_choice_position	<- min(nrow(tmp_min_violation), choice_position)
						}
					}
					
					print(paste("Choice position:", tmp_choice_position))
					selected_policy		<- tmp_period$POLICY[tmp_choice_position]
					selected_correction	<- tmp_period$CORRECTION[tmp_choice_position]
					
					base_scenario	<- if (is.null(best_scenario)) paste(selected_policy, "_", selected_correction, sep = "") else best_scenario
					best_scenario	<- paste(selected_policy, "_", selected_correction, sep = "") 
					
					print(paste("Best scenario:", best_scenario))
					print(paste("Base scenario:", base_scenario))
					print("")
					
				}
				
				inst_number_map	<- tmp_inst_number_map
				inst_alloc_map	<- tmp_inst_alloc_map
				residual_map	<- tmp_residual_map
				
				w	<- w + 1
				
				comes_from_violation	<- FALSE
			}
		}


		file_name_period_analysis <- paste(outpur_dir, "analysis/", pred_type, "/period/dynamic/", adjustment_type, "/server-", trace_number, "-global-dynamic-analysis-", periodicity, "-", original_choice_position,  "-", reaction_choice_position, "-", behavior_type, "-", only_with_correction, "-", sla_violation_freq, "-", with_correction, "-", correction_subset, "-", as.character(safety_margin), "-linear-linear-", as.character(target_residual), "-", as.character(violation_target), ".dat", sep = "")
		write.table(global_analysis, file = file_name_period_analysis, row.names = FALSE)	
	
		file_name_period_analysis <- paste(outpur_dir, "analysis/", pred_type, "/period/dynamic/", adjustment_type, "/server-", trace_number, "-global-dynamic-scaling-", periodicity, "-", original_choice_position,  "-", reaction_choice_position, "-", behavior_type, "-", only_with_correction, "-", sla_violation_freq, "-", with_correction, "-", correction_subset, "-", as.character(safety_margin), "-linear-linear-", as.character(target_residual), "-", as.character(violation_target), ".dat", sep = "")
		write.table(global_scaling, file = file_name_period_analysis, row.names = FALSE)	
	
		file_name_period_analysis <- paste(outpur_dir, "analysis/", pred_type, "/period/dynamic/", adjustment_type, "/server-", trace_number, "-global-dynamic-violation-", periodicity, "-", original_choice_position,  "-", reaction_choice_position, "-", behavior_type, "-", only_with_correction, "-", sla_violation_freq, "-", with_correction, "-", correction_subset, "-", as.character(safety_margin), "-linear-linear-", as.character(target_residual), "-", as.character(violation_target), ".dat", sep = "")
		write.table(global_violation, file = file_name_period_analysis, row.names = FALSE)	

		file_name_period_analysis <- paste(outpur_dir, "analysis/", pred_type, "/period/dynamic/", adjustment_type, "/server-", trace_number, "-global-dynamic-period-analysis-", periodicity, "-", original_choice_position, "-", reaction_choice_position, "-", behavior_type, "-", only_with_correction, "-", sla_violation_freq, "-", with_correction, "-", correction_subset, "-", as.character(safety_margin), "-linear-linear-", as.character(target_residual), "-", as.character(violation_target), ".dat", sep = "")
		write.table(global_period_analysis, file = file_name_period_analysis, row.names = FALSE)	

	}else{

		file_name_period_analysis <- paste(outpur_dir, "analysis/", pred_type, "/period/dynamic/", adjustment_type, "/server-", trace_number, "-global-dynamic-analysis-", periodicity, "-", original_choice_position,  "-", reaction_choice_position, "-", behavior_type, "-", only_with_correction, "-", sla_violation_freq, "-", with_correction, "-", correction_subset, "-", as.character(safety_margin), "-linear-linear-", as.character(target_residual), "-", as.character(violation_target), ".dat", sep = "")
		if (file.exists(file_name_period_analysis)) {
			global_analysis <- read.table(file_name_period_analysis, header = T)
		}
	
		file_name_period_analysis <- paste(outpur_dir, "analysis/", pred_type, "/period/dynamic/", adjustment_type, "/server-", trace_number, "-global-dynamic-scaling-", periodicity, "-", original_choice_position,  "-", reaction_choice_position, "-", behavior_type, "-", only_with_correction, "-", sla_violation_freq, "-", with_correction, "-", correction_subset, "-", as.character(safety_margin), "-linear-linear-", as.character(target_residual), "-", as.character(violation_target), ".dat", sep = "")
		if (file.exists(file_name_period_analysis)) {
			global_scaling	<- read.table(file_name_period_analysis, header = T)
		}

		file_name_period_analysis <- paste(outpur_dir, "analysis/", pred_type, "/period/dynamic/", adjustment_type, "/server-", trace_number, "-global-dynamic-violation-", periodicity, "-", original_choice_position, "-", reaction_choice_position, "-", behavior_type, "-", only_with_correction, "-", sla_violation_freq, "-", with_correction, "-", correction_subset, "-", as.character(safety_margin), "-linear-linear-", as.character(target_residual), "-", as.character(violation_target), ".dat", sep = "")
		if (file.exists(file_name_period_analysis)) {
			global_violation <- read.table(file_name_period_analysis, header = T)
		}

		file_name_period_analysis <- paste(outpur_dir, "analysis/", pred_type, "/period/dynamic/", adjustment_type, "/server-", trace_number, "-global-dynamic-period-analysis-", periodicity, "-", original_choice_position, "-", reaction_choice_position, "-", behavior_type, "-", only_with_correction, "-", sla_violation_freq, "-", with_correction, "-", correction_subset, "-", as.character(safety_margin), "-linear-linear-", as.character(target_residual), "-", as.character(violation_target), ".dat", sep = "")
		if (file.exists(file_name_period_analysis)) {	
			global_period_analysis	<- read.table(file_name_period_analysis, header = T)
		}	

	}
	
	file_name <- paste(outpur_dir, "analysis/", pred_type, "/period/dynamic/", adjustment_type, "/general/server-", trace_number, "-general-results-", periodicity, "-", original_choice_position, "-", reaction_choice_position, "-", behavior_type, "-", only_with_correction, "-", sla_violation_freq, "-", with_correction, "-", correction_subset, "-", as.character(safety_margin), "-linear-linear-", as.character(target_residual), "-", as.character(violation_target), ".dat", sep = "")

	if (!file.exists(file_name)) {

		df_result   <- c()
		df_result   <- rbind(df_result, data.frame(PROP = "Server number", VALUE = trace_number))
		df_result   <- rbind(df_result, data.frame(PROP = "Violation number", VALUE = sum(global_analysis$VIOLATION)))
		df_result   <- rbind(df_result, data.frame(PROP = "Cost", VALUE = sum(global_analysis$COST)))
		df_result   <- rbind(df_result, data.frame(PROP = "Efficiency", VALUE = (1 - ((ceiling(sum(global_analysis$COST)) - ceiling(perfect_cost))/(ceiling(over_cost) - ceiling(perfect_cost)))) * 100))
		df_result   <- rbind(df_result, data.frame(PROP = "Saving", VALUE = ((ceiling(over_cost) - ceiling(sum(global_analysis$COST))) / ceiling(over_cost) * 100)))
		df_result   <- rbind(df_result, data.frame(PROP = "Cost relative to perfect", VALUE = ((ceiling(sum(global_analysis$COST)) - ceiling(perfect_cost)) / ceiling(perfect_cost) * 100)))
		df_result   <- rbind(df_result, data.frame(PROP = "Cost relative to overprovisioning", VALUE = ((ceiling(sum(global_analysis$COST)) - ceiling(over_cost)) / ceiling(over_cost) * 100)))
		df_result   <- rbind(df_result, data.frame(PROP = "Perfect cost", VALUE = perfect_cost))
		df_result   <- rbind(df_result, data.frame(PROP = "Overprovisioning cost", VALUE = over_cost))
		df_result   <- rbind(df_result, data.frame(PROP = "Number of reactivities", VALUE = reactivities))
		df_result   <- rbind(df_result, data.frame(PROP = "Policies", VALUE = paste(unique(global_analysis$POLICY), collapse = " ")))

		print("")
		print(df_result)
		print("")

		write.table(df_result, file = file_name, row.names = FALSE)
	
	}else{

		df_result		<- read.table(file_name, header = TRUE)

		print("")
		print(df_result)
		print("")

		write.table(df_result, file = file_name, row.names = FALSE)

	}
}

begin	<- Sys.time()

suppressMessages(library(Hmisc, quietly = TRUE))
suppressMessages(library(taRifx, quietly = TRUE))
suppressMessages(library(gdata, quietly = TRUE))

args  <- commandArgs(trailingOnly = TRUE)

instance_size 			<- 1
time_decision			<- 55
instance_grain			<- 60
instance_base			<- 1
pred_grain			<- 5
cap_grain				<- 5

trace_number			<- as.integer(args[1])
periodicity			<- 288
choice_position		<- as.integer(args[2])
reaction_choice_position <- as.integer(args[3])
only_with_correction	<- as.character(args[4])
sla_violation_freq		<- as.integer(args[5])
with_correction		<- as.character(args[6])
correction_subset		<- as.character(args[7])
safety_margin			<- as.numeric(args[8])
violation_target		<- as.numeric(args[9])
pred_type				<- as.character(args[10])
adjustment_type		<- as.character(args[11])
instance_size_ecu		<- as.numeric(args[12])
instance_size_ghz		<- as.numeric(args[13])
instance_size_mem		<- as.numeric(args[14])

behavior_type			<- "reactive"

input_dir_cpu			<- paste("../../prediction/cpu/data/pred/", pred_type, "/", sep = "")
input_dir_mem   		<- paste("../../prediction/mem/data/pred/", pred_type, "/", sep = "")
outpur_dir			<- "data/"

error_adjustment		<- T

policies				<- c("last_h", "linear_regression", "auto_correlation", "arima", "auto_regressive", "ensemble")
num_periods			<- c(0, 0, 0, 0, 2016, 0)

adjustment_window		<- as.numeric(strsplit(correction_subset, split = "_")[[1]])

if (only_with_correction) {
	adjustment_window	<- adjustment_window[!adjustment_window %in% c(0)] 

}

trace_type = "normal"

target_residual		<- NA

filename_utilization_cpu	<- paste("../../../data/", trace_type, "/cpu_util-prod-server-", trace_number, "-", trace_type, ".dat", sep = "")
filename_allocation_cpu	<- paste("../../../data/", trace_type, "/cpu_alloc-prod-server-", trace_number, "-", trace_type, ".dat", sep = "")

filename_utilization_mem <- paste("../../../data/", trace_type, "/mem_util-prod-server-", trace_number, "-", trace_type, ".dat", sep = "")
filename_allocation_mem	<- paste("../../../data/", trace_type, "/mem_alloc-prod-server-", trace_number, "-", trace_type, ".dat", sep = "")

ecu_factor               = 0.7602

filename_trace	     	= "../../../data/normal/machine_data.dat"
trace_info 			= read.table(filename_trace, header = T)
trace_info 			= subset(trace_info, TRACE == trace_number)
trace_ghz 			= trace_info$GHZ[1]

if (is.na(instance_size_ghz)) {
     trace_ghz           = 1
     ecu_factor          = 1
     instance_size_ecu   = 1
}

adjustment_window_cpu = adjustment_window
adjustment_window_mem = adjustment_window

run_analysis(	sla_violation_freq, 
			only_with_correction, 
			behavior_type, 
			choice_position, 
			reaction_choice_position, 
			periodicity, 
			filename_utilization_cpu, 
               filename_utilization_mem, 
               filename_allocation_cpu, 
               filename_allocation_mem, 
			trace_number, 
			pred_type, 
			num_periods, 
			policies, 
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
			with_correction, 
			correction_subset, 
			target_residual, 
			safety_margin_cpu,
			safety_margin_mem,  
			violation_target,
			trace_ghz, 
               ecu_factor,
               slo_limit)

diff_mins	<- difftime(Sys.time(), begin, units = 'mins')
print(paste("Minutes:", diff_mins))
print(paste("Hours:", (diff_mins / 60)))