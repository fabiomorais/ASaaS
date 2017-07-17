library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)

df_trace  = read.table(file = "data/trace_analysis.dat", header = T) %>% filter(METRIC == "cpu")

traces    = df_trace$TRACE
pred_time = c(1)#, 0
num_conf  = c()
metrics   = c("cpu")

for (trace in traces) {
  
  for (metric in metrics) {
    
    scaling_final = read.table(file = paste("data/perfect/scaling-perfect-", trace, "-1-", metric, ".dat", sep = ""), header = T)
    len_trace     = nrow(scaling_final)
    
    for (ptime in pred_time) {
      
      df_threshold   = read.table(paste("data/threshold/threshold-perfect-", trace, "-", ptime, "-", metric, "-5.dat", sep = ""), header = T)
      
      num_tmp        = df_threshold %>% mutate(TRACE = trace, PROV_TIME = ptime, TRACE_LEN = len_trace)
      num_conf       = rbind(num_conf, num_tmp)
      
    }     
  }
}     

df_scaling_freq = num_conf %>% group_by(TRACE) %>% summarise(NSCALING = n(), TRACE_LEN = nth(TRACE_LEN, 1)) %>% mutate(SCALING_FREQ = NSCALING / TRACE_LEN)
df_thr_data     = num_conf %>% group_by(TRACE, ACT_DONE, PROV_TIME, THRESHOLD) %>% mutate(SCALING_UTIL_REF = round(SCALING_UTIL_REF, digits = 2)) %>% summarise(MEAN_T = mean(SCALING_UTIL_REF), MEDIAN_T = median(SCALING_UTIL_REF), MIN_T = min(SCALING_UTIL_REF), MAX_T = max(SCALING_UTIL_REF), SD_T = sd(SCALING_UTIL_REF))
df_freq         = num_conf %>% group_by(TRACE, ACT_DONE, PROV_TIME) %>% 
                  mutate(NSCALING = n()) %>%
                  group_by(TRACE, ACT_DONE, PROV_TIME, THRESHOLD) %>% 
                  summarise(NTHR = n(), NSCALING = nth(NSCALING, 1)) %>% 
                  mutate(FREQ = NTHR / NSCALING) %>% 
                  arrange(desc(FREQ))

# considera todas as configurações mais frequentes
df_top_freq = df_freq %>% group_by(TRACE, ACT_DONE, PROV_TIME) %>% filter(FREQ == max(FREQ)) %>% ungroup() %>% select(TRACE, ACT_DONE, THRESHOLD, THR_FREQ = FREQ)

df_freq_selection = c()
for (trace in traces) {

     df_add = filter(df_top_freq, TRACE == trace, ACT_DONE == "ADD")
     df_rm = filter(df_top_freq, TRACE == trace, ACT_DONE == "RM")
     
     for (index_add in 1:nrow(df_add)) {
          
          for (index_rm in 1:nrow(df_rm)) {
               
               add_thr   = df_add[index_add, ]
               rm_thr    = df_rm[index_rm, ]
               
               df_freq_selection = rbind(df_freq_selection, data.frame(TRACE = trace, ADD = add_thr$THRESHOLD, RM = rm_thr$THRESHOLD, THR_ADD_FREQ = add_thr$THR_FREQ, THR_RM_FREQ = rm_thr$THR_FREQ, DISTANCE = (add_thr$THRESHOLD - rm_thr$THRESHOLD)))
          }
     }
}

write.table(df_freq_selection, file = "data/pre_reactive_scenarios_cpu.dat", row.names = F)

df_freq_selection$TRACE       = factor(df_freq_selection$TRACE, levels = unique(arrange(df_freq_selection, DISTANCE)$TRACE))
df_threshold                  = df_freq_selection %>% filter(DISTANCE > 0, ADD < 1, RM > 0)
df_threshold                  = df_threshold %>% group_by(TRACE) %>% arrange(TRACE, desc(DISTANCE)) %>% top_n(1)
df_threshold                  = df_threshold %>% filter(ADD >= 0.5, ADD <= 0.9) %>% filter(RM <= 0.5)

df_selection                  = df_threshold %>% select(TRACE, ADD, RM) %>% gather(ACT_DONE, THRESHOLD, 2:3)

p = ggplot(df_selection, aes(as.factor(TRACE), THRESHOLD, fill = ACT_DONE)) + geom_bar(stat = "identity", position = "dodge")
p = p + scale_y_continuous(labels = percent, breaks = seq(0, 1, 0.05))
p = p + theme_bw(base_size = 24)
p = p + geom_hline(aes(yintercept = 0.9), color = "blue")
p = p + geom_hline(aes(yintercept = 0.5), color = "blue")
p = p + geom_hline(aes(yintercept = 0.4), color = "blue")
p

df_selection$TRACE = as.numeric(as.character(df_selection$TRACE))

df_final = inner_join(df_selection, num_conf, by = c("TRACE", "ACT_DONE", "THRESHOLD")) %>% group_by(TRACE, ACT_DONE, THRESHOLD, DIFF) %>% 
     summarise(OCC = n()) %>% ungroup() %>% group_by(TRACE, ACT_DONE, THRESHOLD) %>% mutate(TOTAL = sum(OCC), FREQ = OCC / TOTAL)

df_config = c()

for (trace in unique(df_final$TRACE)) {
  
  tmp = df_final %>% filter(TRACE == trace)
  print(tmp)
  
  df_add  =  tmp %>% filter(ACT_DONE == "ADD")
  df_rm   =  tmp %>% filter(ACT_DONE == "RM")
  
  for (index_add in 1:nrow(df_add)) {
    
      for (index_rm in 1:nrow(df_rm)) {
      
        add_thr   = df_add[index_add, ]$THRESHOLD
        rm_thr    = df_rm[index_rm, ]$THRESHOLD
        
        freq_tmp   = df_threshold %>% filter(TRACE == trace, ADD == add_thr, RM == rm_thr)
        
        df_config = rbind(df_config, data.frame(TRACE = trace, 
                                                TH_ADD = df_add[index_add, ]$THRESHOLD, 
                                                VM_ADD = df_add[index_add,]$DIFF, 
                                                FREQ_ADD_CONF = df_add[index_add,]$FREQ,
                                                FREQ_ADD_THR  = freq_tmp$THR_ADD_FREQ,
                                                TH_RM = df_rm[index_rm, ]$THRESHOLD, 
                                                VM_RM = df_rm[index_rm,]$DIFF, 
                                                FREQ_RM_CONF = df_rm[index_rm,]$FREQ,
                                                FREQ_RM_THR  = freq_tmp$THR_RM_FREQ))
      }
  }
}

df_out = inner_join(df_config, df_scaling_freq, by = "TRACE")
write.table(df_out, file = "data/reactive_scenarios_cpu.dat", row.names = F, col.names = F)
