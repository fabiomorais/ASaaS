#fabio

library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(doMC)
library(foreach)

df_trace  = read.table(file = "data/trace_analysis.dat", header = T) %>% filter(METRIC == "cpu")

traces         = df_trace$TRACE
ptime          = 1
num_conf       = c()
num_selection  = c()
metrics        = c("cpu", "mem")

for (metric in metrics) {
     
     for (trace in traces) {
          
          print(trace)
          
          scaling_final = read.table(file = paste("../../reactive/data/perfect/scaling-perfect-", trace, "-1-", metric, ".dat", sep = ""), header = T)
          len_trace     = nrow(scaling_final)
          
          df_threshold   = read.table(paste("../../reactive/data/threshold/threshold-perfect-", trace, "-", ptime, "-", metric, "-", ptime, ".dat", sep = ""), header = T)
          df_threshold   = df_threshold %>% mutate(THRESHOLD = floor(SCALING_UTIL_REF * 100) / 100) 
          
          num_tmp        = df_threshold %>% mutate(TRACE = trace, PROV_TIME = ptime, TRACE_LEN = len_trace, METRIC = metric)
          num_tmp        = num_tmp %>% select(TIMESTAMP, TRACE, METRIC, ACT_DONE, THRESHOLD, DIFF, PROV_TIME)
          
          num_conf       = rbind(num_conf, num_tmp)
     }     
}

write.table(num_conf, file = "data/threshold_configuration_data.dat", row.names = F)

df_freq   = num_conf %>% group_by(TRACE, ACT_DONE, METRIC, PROV_TIME) %>% 
     mutate(NSCALING = n()) %>%
     group_by(TRACE, ACT_DONE, METRIC, PROV_TIME, THRESHOLD) %>% 
     summarise(NTHR = n(), NSCALING = nth(NSCALING, 1)) %>% 
     mutate(FREQ = NTHR / NSCALING) %>% 
     arrange(desc(FREQ))

dplot = df_freq %>% group_by(TRACE, METRIC, ACT_DONE) %>% arrange(THRESHOLD) %>% mutate(FREQ_CUM = ifelse(ACT_DONE == "ADD", rev(cumsum(FREQ)), cumsum(FREQ)))

registerDoMC(cores = 8)
x <- foreach(index = 1:30, .combine = "rbind") %dopar% {
#for (trace in unique(df_freq$TRACE)) {
     
     trace = unique(df_freq$TRACE)[index]
     df_conf  = c()
     
     print(trace)
     
     for (metric in metrics) {
          
          df_add    = dplot %>% filter(TRACE == trace, ACT_DONE == "ADD", METRIC == metric)
          df_rm     = dplot %>% filter(TRACE == trace, ACT_DONE == "RM", METRIC == metric)
          
          for (i in 1:nrow(df_add)) {
               
               for (j in 1:nrow(df_rm)) {
                    
                    c_add     = df_add[i, ]
                    c_rm      = df_rm[j, ]
                    
                    df_conf = rbind(df_conf, data.frame(TRACE = c_add$TRACE, METRIC = c_add$METRIC, ADD = c_add$THRESHOLD , RM = c_rm$THRESHOLD, ADD_FREQ = c_add$FREQ, RM_FREQ = c_rm$FREQ_CUM, ADD_FREQ_CUM = c_add$FREQ_CUM, RM_FREQ_CUM = c_rm$FREQ_CUM))
               }
          }
          
     }
     
     df_conf
}

write.table(x, file = "data/threshold_configuration_metrics.dat", row.names = F)

