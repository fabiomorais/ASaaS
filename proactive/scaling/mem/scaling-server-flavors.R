calculate_scaling = function(capacity_trace, time_decision, instance_size_mem, instance_grain, instance_base, trace_number, policy_abr, adj_w) {
     
     timestamps		<- capacity_trace$TIMESTAMP
     
     inst_number		<- max(instance_base, ceiling(capacity_trace$CAP_EST_TG[1] / instance_size_mem))	
     inst_alloc		<- c()
     
     for (j in 1:inst_number) {
          inst_alloc[[j]] <- c(j, 0)
     }
     
     inst_alloc_list	<- c(inst_number)
     
     for (i in 2:nrow(capacity_trace)) {
          
          try(system(paste("echo ", i, " > output/result-", trace_number, "-", policy_abr, "-", adj_w, ".out", sep = ""), intern = TRUE))
          
          cap <- ceiling(capacity_trace$CAP_EST_TG[i] / instance_size_mem)
          
          for (j in 1:length(inst_alloc)) {
               inst_alloc[[j]][2] <- inst_alloc[[j]][2] + 5
          }	
          
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
               
          } else if ((cap - inst_number) > 0) {
               
               inst.add <- abs(cap - inst_number)
               
               for (n in 1:inst.add) {
                    inst_alloc[[length(inst_alloc) + 1]] <- c(length(inst_alloc) + 1, 0)
               }
               
               inst_number	<- cap
          } 
          
          inst_alloc_list	<- c(inst_alloc_list, inst_number)
     }
     
     df_scaling	<- data.frame(timestamps, inst_alloc_list)
     names(df_scaling) <- c("TIMESTAMP", "SCALING")
     
     df_scaling
}


run_analysis = function(filename_utilization, filename_allocation, trace_number, trace_type, pred_type, num_period, policy, input_dir, outpur_dir, instance_size_mem, time_decision, instance_grain, instance_base, pred_grain, cap_grain, error_adjustment, adjustment_type, ad_w, slo_limit, capacity_target) {
     
     pred.labels	<- list(last_h	= "Last window (LW)",
                         ets 				= "ETS",
                         auto_regressive	= "Auto-regressive (AR)",
                         sar 				= "Sazonal auto-regressive (AR)",
                         arima			= "Auto-regressive integrated moving average (ARIMA)",
                         sarima			= "Sazonal auto-regressive integrated moving average (ARIMA)",
                         auto_correlation	= "Auto-correlation (AC)",
                         linear_regression	= "Linear regression (LR)",
                         ensemble		     = "Ensemble"
     )
     
     pred.abr.labels	<- list(last_h	= "LW",
                             ets 					= "ETS",
                             auto_regressive			= "AR",
                             sar 					= "SAR",
                             arima					= "ARIMA",
                             sarima				= "SARIMA",
                             auto_correlation		= "AC",
                             linear_regression		= "LR",
                             ensemble			     = "EN"
     )
     
     dff		    <- read.table(filename_utilization, header = T)
     dff_alloc	<- if (file.exists(filename_allocation)) {
          read.table(filename_allocation, header = T)
     }else{
          data.frame(MEM_ALLOC = rep(16, nrow(dff)))
     }
     
     timestamps	       <- dff$TIMESTAMP
     trace_util	       <- dff$MEM_UTIL
     trace_alloc	       <- dff_alloc$MEM_ALLOC
     
     df_trace		       <- data.frame(cbind(timestamps, trace_util), 0, "Original")
     names(df_trace)	  <- c("TIMESTAMP", "MEM_UTIL", "EXEC_TIME", "POLICY")
     
     df_alloc		       <- data.frame(cbind(timestamps, trace_alloc), 0, "Alloc")
     names(df_alloc)	  <- c("TIMESTAMP", "MEM_ALLOC", "EXEC_TIME", "POLICY")
     
     estimated_trace	    <- read.table(paste(input_dir, "server-", trace_number, "-estimated-trace-", pred.abr.labels[[policy]], "-", num_period, "-", pred_type, ".dat", sep = ""), header = T)
     diff_trace		    <- read.table(paste(input_dir, "server-", trace_number, "-diff-trace-", pred.abr.labels[[policy]], "-", num_period, "-", pred_type, ".dat", sep = ""), header = T)	
     
     tmp_df_trace		    <- subset(df_trace, TIMESTAMP >= estimated_trace$TIMESTAMP[1])
     tmp_df_trace		    <- subset(tmp_df_trace, TIMESTAMP <= estimated_trace$TIMESTAMP[nrow(estimated_trace)])
     
     tmp_df_alloc		    <- subset(df_alloc, TIMESTAMP >= estimated_trace$TIMESTAMP[1])
     tmp_df_alloc		    <- subset(tmp_df_alloc, TIMESTAMP <= estimated_trace$TIMESTAMP[nrow(estimated_trace)])
     
     tmp_df_trace[,5] 	<- num_period
     names(tmp_df_trace)	<- c("TIMESTAMP", "MEM_UTIL", "EXEC_TIME", "POLICY", "PERIOD")
     
     new_df_trace		<- data.frame(tmp_df_trace$TIMESTAMP, tmp_df_trace$MEM_UTIL * tmp_df_alloc$MEM_ALLOC / 100, tmp_df_trace$EXEC_TIME, tmp_df_trace$POLICY, tmp_df_trace$PERIOD)
     names(new_df_trace)	<- c("TIMESTAMP", "MEM_UTIL", "EXEC_TIME", "POLICY", "PERIOD")
     
     #Normal
     print("")
     print(paste("Server: ", trace_number, "-", pred.abr.labels[[policy]], "-", ad_w))
     
     file_name_capacity	<- paste(outpur_dir, "capacity/", pred_type, "/server-", trace_number, "-capacity-planning-", ad_w, "-", pred.abr.labels[[policy]], "-", error_adjustment, "-", adjustment_type, "-", slo_limit, "-", as.character(capacity_target), "-linear.dat", sep = "")
     capacity_trace		  <- read.table(file_name_capacity, header = T)
     
     file_name_scaling  	<- paste(outpur_dir, "scaling/", pred_type, "/server-", trace_number, "-scaling-", ad_w, "-", pred.abr.labels[[policy]], "-", instance_grain, "-", time_decision, "-", error_adjustment, "-", adjustment_type, "-", slo_limit, "-", as.character(capacity_target), "-linear-", instance_size_mem, ".dat", sep = "")
     
     if (!file.exists(file_name_scaling)) {
          
          scaling_trace	    <- calculate_scaling(capacity_trace, time_decision, instance_size_mem, instance_grain, instance_base, trace_number, pred.abr.labels[[policy]], ad_w) 			
          write.table(scaling_trace, file = file_name_scaling, row.names = FALSE)
          
     }
}

begin	<- Sys.time()

suppressMessages(library(ggplot2, quietly = TRUE))
suppressMessages(library(scales, quietly = TRUE))
suppressMessages(library(Hmisc, quietly = TRUE))

args  <- commandArgs(trailingOnly = TRUE)

time_decision		<- 55
instance_grain		<- 60
instance_base		<- 1

trace_number		<- as.integer(args[1])
policy_number		<- as.integer(args[2])
adjustment_window	<- as.numeric(args[3])
trace_type		<- as.character(args[4])
pred_type			<- as.character(args[5])
adjustment_type	<- as.character(args[6])
slo_limit           <- as.numeric(args[7])
capacity_target	<- as.numeric(args[8])
instance_size_mem	<- as.numeric(args[9])

input_dir			<- paste("../prediction-mem/data/pred/", pred_type, "/", sep = "")
outpur_dir		<- "data/"

error_adjustment	<- T

policies			<- c("last_h", "linear_regression", "auto_correlation", "arima", "auto_regressive", "ensemble")
num_periods		<- c(0, 0, 0, 0, 2016, 0)

policies			<- policies[policy_number]
num_periods		<- num_periods[policy_number]

filename_utilization	<- paste("../data/", trace_type, "/mem_util-prod-server-", trace_number, "-", trace_type, ".dat", sep = "")
filename_allocation		<- paste("../data/", trace_type, "/mem_alloc-prod-server-", trace_number, "-", trace_type, ".dat", sep = "")

run_analysis(filename_utilization, filename_allocation, trace_number, trace_type, pred_type, num_periods, policies, input_dir, outpur_dir, instance_size_mem, time_decision, instance_grain, instance_base, pred_grain, cap_grain, error_adjustment, adjustment_type, adjustment_window, slo_limit, capacity_target)

diff_mins	<- difftime(Sys.time(), begin, units = 'mins')

print("")
print(paste("Trace:", trace_number, " - ", policies[policy_number], " - ", adjustment_window))
print(paste("Minutes:", diff_mins))
print(paste("Hours:", (diff_mins / 60)))
