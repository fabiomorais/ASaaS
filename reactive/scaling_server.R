calculate_scaling = function(cap_local, timed, billing, minisnt, scaling_status) {
  
  tmp_scaling_status  = scaling_status
  tmp_scaling_result  = c()
  
  act       = ""
  delta     = 0
  
  if (is.null(tmp_scaling_status)) { 
    
    inst_number         = cap_local$CAP
    
    for (index in 1:inst_number) {
     
      tmp_scaling_status[[index]] = list(INDEX = index, UPTIME = 0, FLAVOR = as.character(cap_local$FLAVOR_NAME))
    }
    
  } else {
    
    # update instance lifetime
    for (index in 1:length(tmp_scaling_status)) {
      tmp_scaling_status[[index]]$UPTIME <- tmp_scaling_status[[index]]$UPTIME + 5
    }	
    
    df_tmp 		= data.frame(matrix(unlist(tmp_scaling_status), ncol = 3 , byrow = T)) %>% filter(!is.na(X2))
    inst_number 	= nrow(df_tmp)
    cap             = cap_local$CAP
    
    # remove instances
    if ((cap - inst_number) < 0) {
      
      inst_kill = min(abs(cap - inst_number), (inst_number - minisnt)) 
      
      act    = "RM"
      delta  = inst_kill
      
      for (k in 1:length(tmp_scaling_status)) {
        
        if (inst_kill > 0 && !is.na(tmp_scaling_status[[k]]$UPTIME)) {
          
          if (((tmp_scaling_status[[k]]$UPTIME - (as.integer(tmp_scaling_status[[k]]$UPTIME / billing) * billing)) %% timed) == 0) {
            
            tmp_scaling_status[[k]]$UPTIME     = NA
            tmp_scaling_status[[k]]$FLAVOR     = NA
            inst_kill                          = inst_kill - 1
            
          }
        }
      }
      
    } else if ((cap - inst_number) > 0) { # add instances
      
      inst_add    = abs(cap - inst_number)
      
      act    = "ADD"
      delta  = inst_add
      
      for (n in 1:inst_add) {
        tmp_scaling_status[[length(tmp_scaling_status) + 1]] = list(INDEX = length(tmp_scaling_status) + 1, UPTIME = 0, FLAVOR = as.character(cap_local$FLAVOR_NAME))
      }
    } 
  }
  
  tmp_scaling_result = data.frame(matrix(unlist(tmp_scaling_status), ncol = 3 , byrow = T)) %>% filter(!is.na(X2)) %>% dplyr::mutate(TIMESTAMP = cap_local$TIMESTAMP[1]) %>% select(TIMESTAMP, FLAVOR = X3)
  tmp_scaling_result = tmp_scaling_result %>% group_by(TIMESTAMP, FLAVOR) %>% dplyr::summarise(SCALING = n()) %>% ungroup()
  tmp_scaling_result = tmp_scaling_result %>% mutate(ACT_WANTED = act, ACT_VAL = delta)
  
  list(status = tmp_scaling_status, result = tmp_scaling_result)
}