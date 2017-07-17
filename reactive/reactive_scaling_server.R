calculate_scaling = function(util_trace, time_decision, instance.size, instance.grain, instance.base, trace_number, thup_level, thup_inst, thdw_level, thdw_inst, horizon, metric){
     
     metric_util    <- paste(toupper(metric), "_UTIL", sep = "")
     
     th1_add 	     <- thup_level
     th1_rm		<- thdw_level
     
     th1_add_inst	<- as.numeric(thup_inst)
     th1_rm_inst	<- as.numeric(thdw_inst)
     
     timestamps	<- util_trace$TIMESTAMP[(horizon + 1):nrow(util_trace)]
     
     inst_number	<- max(instance.base, ceiling(util_trace[1, metric_util] / instance.size))	
     inst_alloc	<- c()
     
     #init uptime
     for (j in 1:inst_number) {
          inst_alloc[[j]] <- c(j, 0)
     }
     
     inst_alloc_list	<- c(inst_number)
     
     for (i in 2:(nrow(util_trace) - horizon)) {
          
          try(system(paste("echo ", i, " > output/result-", trace_number, "-", thup_level, "_", thup_inst, "-", thdw_level, "_", thdw_inst, ".out", sep = ""), intern = TRUE))
          
          #update uptime
          for (j in 1:length(inst_alloc)) {
               inst_alloc[[j]][2] <- inst_alloc[[j]][2] + 5
          }	
          
          util 	<- util_trace[i, metric_util]
          f		<- (util / inst_number)
          
          #delete instance
          if (f < th1_rm) {
               
               inst.kill	<- min(th1_rm_inst, (inst_number - instance.base)) 
               
               for (k in 1:length(inst_alloc)) {
                    
                    if (inst.kill != 0 && !is.na(inst_alloc[[k]][2])) {
                         
                         if (((inst_alloc[[k]][2] - (as.integer(inst_alloc[[k]][2] / instance.grain) * instance.grain)) %% time_decision) == 0) {
                              
                              inst_alloc[[k]] <- NA
                              inst.kill <- inst.kill - 1
                              inst_number <- inst_number - 1
                         }
                    }
               }
               
          } #add instance 
          else if (f > th1_add) {
               
               inst.add 	<- th1_add_inst
               
               for (n in 1:inst.add) {
                    
                    inst_alloc[[length(inst_alloc) + 1]] <- c(length(inst_alloc) + 1, 0)
               }
               
               inst_number	<- inst_number + inst.add
          } 
          
          inst_alloc_list	<- c(inst_alloc_list, inst_number)
     }
     
     df_scaling	<- data.frame(timestamps, inst_alloc_list)
     names(df_scaling) <- c("TIMESTAMP", "SCALING")
     
     df_scaling
}


run_analysis = function(filename_utilization, filename_allocation, trace_number, outpur_dir, instance_size, time_decision, instance_grain, instance_base, pred_grain, cap_grain, thup_level, thup_inst, thdw_level, thdw_inst, horizon, metric){
     
     dff			<- read.table(filename_utilization, header = T)
     dff_alloc	     <- read.table(filename_allocation, header = T)
     
     metric_util    <- paste(toupper(metric), "_UTIL", sep = "")
     metric_alloc   <- paste(toupper(metric), "_ALLOC", sep = "")
     
     timestamps	<- dff$TIMESTAMP
     trace_util	<- dff[, metric_util]
     trace_alloc	<- dff_alloc[, metric_alloc]
     
     df_trace		     <- data.frame(cbind(timestamps, trace_util))
     names(df_trace)	<- c("TIMESTAMP", metric_util)
     
     df_alloc		     <- data.frame(cbind(timestamps, trace_alloc))
     names(df_alloc)	<- c("TIMESTAMP", metric_alloc)
     
     trace_base		<- read.table(paste("data/perfect/scaling-perfect-", trace_number, "-1-", metric, ".dat", sep = ""), header = T)
     initial_timestamp	<- trace_base$TIMESTAMP[1] - (300 * horizon)
     final_timestamp	<- trace_base$TIMESTAMP[nrow(trace_base)] #trace_base$TIMESTAMP[1] + (300 * 4)
     
     tmp_df_trace		<- subset(df_trace, TIMESTAMP >= initial_timestamp)
     tmp_df_trace		<- subset(tmp_df_trace, TIMESTAMP <= final_timestamp)
     
     tmp_df_alloc		<- subset(df_alloc, TIMESTAMP >= initial_timestamp)
     tmp_df_alloc		<- subset(tmp_df_alloc, TIMESTAMP <= final_timestamp)
     
     names(tmp_df_trace)	<- c("TIMESTAMP", metric_util)
     
     new_df_trace		<- data.frame(TIMESTAMP = tmp_df_trace$TIMESTAMP)
     new_df_trace[, metric_util] = tmp_df_trace[, metric_util] * tmp_df_alloc[, metric_alloc] / 100
     #Normal
     print(paste("Server: ", trace_number, "-threshold_based-", thup_level, "_", thup_inst, "-", thdw_level, "_",  thdw_inst, "_", metric))
     
     file_name_scaling	<- paste(outpur_dir, "server-", trace_number, "-scaling-threshold-", horizon, "-", thup_level, "_", thup_inst, "-", thdw_level, "_",  thdw_inst, "-", metric, ".dat", sep = "")
     
     if (!file.exists(file_name_scaling)) {
          
          scaling_trace	<- calculate_scaling(new_df_trace, time_decision, instance_size, instance_grain, instance_base, trace_number, thup_level, thup_inst, thdw_level, thdw_inst, horizon, metric) 			
          write.table(scaling_trace, file = file_name_scaling, row.names = FALSE)
          
     }		
}

begin	<- Sys.time()

args  <- commandArgs(trailingOnly = TRUE)

instance_size 		<- 1
time_decision		<- 55
instance_grain		<- 60
instance_base		<- 1
pred_grain		<- 5
cap_grain			<- 5

trace_number		<- as.integer(args[1])
trace_type		<- as.character(args[2])
thup				<- as.character(args[3])
thup_i			<- as.character(args[4])
thdw				<- as.character(args[5])
thdw_i			<- as.character(args[6])
metric			<- as.character(args[7])

horizon			<- 2

thup_level		<- thup
thdw_level		<- thdw

thup_inst			<- thup_i
thdw_inst			<- thdw_i

outpur_dir		<- "data/scaling/"

filename_utilization	<- paste("../data/", trace_type, "/", metric, "_util-prod-server-", trace_number, "-", trace_type, ".dat", sep = "")
filename_allocation		<- paste("../data/", trace_type, "/", metric, "_alloc-prod-server-", trace_number, "-", trace_type, ".dat", sep = "")

run_analysis(filename_utilization, filename_allocation, trace_number, outpur_dir, instance_size, time_decision, instance_grain, instance_base, pred_grain, cap_grain, thup_level, thup_inst, thdw_level, thdw_inst, horizon, metric)

diff_mins	<- difftime(Sys.time(), begin, units = 'mins')
print(paste("Minutes:", diff_mins))
print(paste("Hours:", (diff_mins / 60)))

