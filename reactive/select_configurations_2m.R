library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)

df_trace  = read.table(file = "data/trace_analysis.dat", header = T) %>% filter(METRIC == "cpu")

traces    = df_trace$TRACE
ptime     = 1
num_conf  = c()

for (trace in traces) {
     
     scaling_final = read.table(file = paste("data/perfect/scaling-perfect-", trace, "-1-2m.dat", sep = ""), header = T)
     len_trace     = nrow(scaling_final)
     
     df_threshold   = read.table(paste("data/threshold/threshold-perfect-", trace, "-", ptime, "-2m.dat", sep = ""), header = T)
     
     num_tmp        = df_threshold %>% mutate(TRACE = trace, PROV_TIME = ptime, TRACE_LEN = len_trace)
     num_conf       = rbind(num_conf, num_tmp)
     
}     

head(num_conf)

df_scaling_freq = num_conf %>% group_by(TRACE) %>% summarise(NSCALING = n(), TRACE_LEN = nth(TRACE_LEN, 1)) %>% mutate(SCALING_FREQ = NSCALING / TRACE_LEN)
df_freq         = num_conf %>% group_by(TRACE, ACT_DONE, METRIC, PROV_TIME) %>% 
     mutate(NSCALING = n()) %>%
     group_by(TRACE, ACT_DONE, METRIC, PROV_TIME, THRESHOLD) %>% 
     summarise(NTHR = n(), NSCALING = nth(NSCALING, 1)) %>% 
     mutate(FREQ = NTHR / NSCALING) %>% 
     arrange(desc(FREQ))

df_top_freq = df_freq %>% group_by(TRACE, ACT_DONE, METRIC, PROV_TIME) %>% filter(FREQ == max(FREQ)) %>% ungroup() %>% select(TRACE, ACT_DONE, METRIC, THRESHOLD, THR_FREQ = FREQ)
df_top_freq = df_top_freq %>% filter(THRESHOLD > 0, THRESHOLD < 1)

# Casos com empate no limiar mais frequente
df_empate = df_top_freq %>% group_by(TRACE, ACT_DONE, METRIC) %>% summarise(N = n()) %>% filter(N > 1)

#df_freq %>% filter(TRACE %in% df_empate$TRACE) %>% group_by(TRACE, ACT_DONE, METRIC, PROV_TIME) %>% filter(FREQ == max(FREQ)) %>% data.frame()
#df_freq %>% filter(!TRACE %in% df_empate$TRACE) %>% group_by(TRACE, ACT_DONE, METRIC, PROV_TIME) %>% filter(FREQ == max(FREQ)) %>% data.frame()

df_res = c()
for (trace in traces) {
     
     for (metric in c("cpu", "mem")) {
          
          tmp = df_top_freq %>% filter(TRACE == trace, METRIC == metric)
          
          if (nrow(tmp) > 0) {
               if (length(unique(tmp$ACT_DONE)) > 1) {
                    if (nrow(tmp) == 2) {
                         # print(paste(trace, "e metrica", metric, "possui seleção direta"))  
                         df_res = rbind(df_res, tmp %>% select(TRACE, METRIC, ACT_DONE, THRESHOLD) %>% spread(ACT_DONE, THRESHOLD) %>% mutate(DISTANCE = ADD - RM))
                    } else {
                         #print(paste(trace, "multiplas configurações))
                         df_add         = filter(tmp, ACT_DONE == "ADD")
                         df_rm          = filter(tmp, ACT_DONE == "RM")
                         
                         for (index_add in 1:nrow(df_add)) {
                              
                              for (index_rm in 1:nrow(df_rm)) {
                                   
                                   val_add   = df_add[index_add, ]
                                   val_rm    = df_rm[index_rm, ]
                                   
                                   df_dist = data.frame(TRACE = trace, METRIC = metric, ADD = val_add$THRESHOLD, RM = val_rm$THRESHOLD) %>% mutate(DISTANCE = ADD - RM) 
                                   df_res  = rbind(df_res, df_dist)     
                              }
                         }
                         
                    }
               } else {
                    if (nrow(tmp) > 1) {
                         #print(paste(trace, "seleção de 1 limiar para a métrica")) 
                         if (unique(tmp$ACT_DONE) == "ADD") {
                              df_val = tmp %>% select(-THR_FREQ) %>% filter(THRESHOLD >= 0.5) %>% filter(THRESHOLD == min(THRESHOLD))
                              df_dist = data.frame(TRACE = trace, METRIC = metric, ADD = df_val$THRESHOLD, RM = Inf) %>% mutate(DISTANCE = abs(ADD - RM)) 
                              df_res  = rbind(df_res, df_dist)     
                         } else {
                              df_val = tmp %>% select(-THR_FREQ) %>% filter(THRESHOLD <= 0.5) %>% filter(THRESHOLD == min(THRESHOLD))
                              df_dist = data.frame(TRACE = trace, METRIC = metric, ADD = Inf, RM = df_val$THRESHOLD) %>% mutate(DISTANCE = abs(ADD - RM))
                              df_res  = rbind(df_res, df_dist)     
                         }
                    } else {
                         #print(paste(trace, "metrica", metric, "com apenas uma ação e um limiar"))       
                         df_dist = data.frame(TRACE = tmp$TRACE, METRIC = metric, ADD = ifelse(tmp$ACT_DONE == "ADD", tmp$THRESHOLD, Inf), RM = ifelse(tmp$ACT_DONE == "RM", tmp$THRESHOLD, Inf)) %>% mutate(DISTANCE = abs(ADD - RM)) 
                         df_res  = rbind(df_res, df_dist)
                    }
               }
          } else {
               #print(paste(trace, "metrica", metric, "sem ocorrência"))
          }
     }
}

# Traces com apenas uma configuração de scaling (ADD ou RM), que gera uma distância infinita
df_error  = df_res %>% filter(DISTANCE > 0) %>% group_by(TRACE) %>% arrange(DISTANCE) %>% summarise(N = n(), D = min(DISTANCE)) %>% filter(N == 1, D == Inf)

df_multi  = df_res %>% filter(DISTANCE > 0, !TRACE %in% df_error$TRACE) %>% group_by(TRACE, METRIC) %>% mutate(N = n()) %>% filter(N > 1) %>% data.frame()
df_multi  = df_multi %>% group_by(TRACE) %>% arrange(DISTANCE) %>% filter(ADD == min(ADD)) %>% filter(RM == min(RM))

df_ok     = df_res %>% filter(DISTANCE > 0, !TRACE %in% df_error$TRACE) %>% group_by(TRACE, METRIC) %>% mutate(N = n()) %>% filter(N == 1) %>% data.frame()

df_freq_selection = rbind(df_ok, df_multi %>% data.frame()) %>% arrange(TRACE)
write.table(df_freq_selection, file = "data/pre_reactive_scenarios_2m.dat", row.names = F)

df_threshold                  = df_freq_selection
df_selection                  = df_threshold %>% select(TRACE, METRIC, ADD, RM) %>% gather(ACT_DONE, THRESHOLD, 3:4) %>% arrange(TRACE)

df_selection$THRESHOLD[df_selection$THRESHOLD == Inf] = NA

p = ggplot(df_selection, aes(as.factor(TRACE), THRESHOLD, fill = ACT_DONE)) + geom_bar(stat = "identity", position = "dodge") + facet_grid(METRIC ~ .)
p = p + scale_y_continuous(labels = percent, breaks = seq(0, 1, 0.1))
p = p + theme_bw(base_size = 24)
p = p + geom_hline(aes(yintercept = 1), color = "blue")
p = p + geom_hline(aes(yintercept = 0.9), color = "blue")
p = p + geom_hline(aes(yintercept = 0.5), color = "blue")
p = p + geom_hline(aes(yintercept = 0.4), color = "blue")
p

df_threshold$TRACE = as.numeric(as.character(df_threshold$TRACE))

df_selection_join = df_selection %>% filter(!is.na(THRESHOLD))
head(num_conf)
df_final = inner_join(df_selection_join, num_conf, by = c("TRACE", "METRIC", "ACT_DONE", "THRESHOLD")) %>% group_by(TRACE, METRIC, ACT_DONE, THRESHOLD, DIFF) %>% summarise(OCC = n()) %>% ungroup() %>%
     group_by(TRACE, METRIC, ACT_DONE, THRESHOLD) %>% mutate(TOTAL = sum(OCC), FREQ = OCC / TOTAL)

df_final %>% data.frame()
df_config = c()
for (trace in unique(df_final$TRACE)) {
     
     tmp = df_final %>% filter(TRACE == trace)
     
     df_add_cpu   =  tmp %>% filter(ACT_DONE == "ADD", METRIC == "cpu")
     df_add_mem   =  tmp %>% filter(ACT_DONE == "ADD", METRIC == "mem")
     
     df_rm_cpu    =  tmp %>% filter(ACT_DONE == "RM", METRIC == "cpu")
     df_rm_mem    =  tmp %>% filter(ACT_DONE == "RM", METRIC == "mem")
     
     df_add = c()
     for (i1 in 1:ifelse(nrow(df_add_cpu) == 0, 1, nrow(df_add_cpu))) {
          for (i2 in 1:ifelse(nrow(df_add_mem) == 0, 1, nrow(df_add_mem))) {
               
               dc = df_add_cpu[i1, ]
               dm = df_add_mem[i2, ]
               
               df_add = rbind(df_add, data.frame(TRACE = trace, ADD_THR_CPU = dc$THRESHOLD, ADD_D_CPU = dc$DIFF, ADD_THR_MEM = dm$THRESHOLD, ADD_D_MEM = dm$DIFF))
          }
          
     }
     df_rm = c()
     for (i1 in 1:ifelse(nrow(df_rm_cpu) == 0, 1, nrow(df_rm_cpu))) {
          for (i2 in 1:ifelse(nrow(df_rm_mem) == 0, 1, nrow(df_rm_mem))) {
               
               dc = df_rm_cpu[i1, ]
               dm = df_rm_mem[i2, ]
               
               df_rm = rbind(df_rm, data.frame(TRACE = trace, RM_THR_CPU = dc$THRESHOLD, RM_D_CPU = dc$DIFF, RM_THR_MEM = dm$THRESHOLD, RM_D_MEM = dm$DIFF))
          }
          
     }
     df_rm
     df_add
     
     
     for (index_add in 1:nrow(df_add)) {
          
          for (index_rm in 1:nrow(df_rm)) {
               
               add_i   = df_add[index_add, ]
               rm_i    = df_rm[index_rm, ]
               
               df_config = rbind(df_config, inner_join(add_i, rm_i, by = "TRACE"))
          }
     }
}

write.table(df_config, file = "data/reactive_scenarios_2m.dat", row.names = F, col.names = F)

