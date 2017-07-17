library(ggplot2)
library(scales)
library(dplyr)

range_size = 5
df_result = read.table(file = paste("data/02_data_thresholds-", range_size, ".dat", sep = ""), header = T)
df_result = df_result %>% select(-TOTAL, -OCC)

traces    = unique(df_result$TRACE)
metrics   = unique(df_result$METRIC)
acts      = unique(df_result$ACT_DONE)

df_final  = c()

for (trace in traces) {
     
     print(trace)
     for (metric in metrics) {
          for (act in acts) {

               tmp       = df_result %>% filter(ACT_DONE == act, TRACE == trace, METRIC == metric) %>% select(TIMESTAMP, THRESHOLD, DIFF) 
               df_tmp    = data.frame(TRACE = trace, METRIC = metric, ACT_DONE = act, T1 = tmp$THRESHOLD[1:(nrow(tmp) - 1)], T2 = tmp$THRESHOLD[2:nrow(tmp)])          
               df_final  = rbind(df_final, df_tmp)
          }
     }
}

df_final = df_final %>% mutate(PAIR = paste(T1, T2, sep = "_"))

# numero de diferentes transições no geral
df_final %>% filter(T1 != T2) %>% distinct() %>% group_by(METRIC, ACT_DONE) %>% dplyr::summarise(OCC = n())

# numero de diferentes transições por trace
df_final %>% filter(T1 != T2) %>% distinct() %>% group_by(TRACE, METRIC, ACT_DONE) %>% dplyr::summarise(OCC = n())

# percentual de ocorrencia de transições entre limiares diferentes
df_final %>% group_by(METRIC, ACT_DONE) %>% dplyr::mutate(TOTAL = n()) %>% 
          filter(T1 != T2) %>% dplyr::summarise(OCC = n(), TT = first(TOTAL), FREQ = n()/ first(TOTAL) * 100)

# percentual de ocorrencia de transições entre limiares iguais
df_final %>% group_by(METRIC, ACT_DONE) %>% dplyr::mutate(TOTAL = n())  %>% 
     filter(T1 == T2) %>% dplyr::summarise(OCC = n(), TT = first(TOTAL), FREQ = n()/ first(TOTAL) * 100)

# percentual de ocorrencia de transições entre limiares diferentes por aplicação
dplot     = df_final %>% group_by(TRACE, METRIC, ACT_DONE) %>% dplyr::mutate(TOTAL = n()) %>% 
     filter(T1 != T2) %>% dplyr::summarise(OCC = n(), TT = first(TOTAL), FREQ = n()/ first(TOTAL))
dplot     = mutate(dplot, METRIC_STR = ifelse(METRIC == "cpu", "CPU", "Memória"), ACT_DONE_STR = ifelse(ACT_DONE == "ADD", "Adição", "Remoção"))


p = ggplot(dplot, aes(METRIC_STR, FREQ, fill = ACT_DONE_STR)) + geom_boxplot(width = 0.7)
p = p + xlab("Métrica de provisionamento") + ylab("Frequência de transições distintas") #+ title("aaaa")
p = p + scale_y_continuous(labels = percent, breaks = seq(0, 1, 0.1))
p = p + theme_bw(base_size = 24)
p = p + scale_fill_hue("Ação de provisionamento:")
p = p + theme(legend.position = "top", legend.key.size = unit(1.5, "cm"))
p

png(filename = "img/02_04_threshold_trans_diff_freq_per-app.png", width = 750, height = 400)
print(p)
dev.off()

dplot %>% group_by(METRIC) %>% dplyr::summarise(V = median(FREQ))
#  transições mais frequente entre limiares diferentes por aplicação
dplot     = df_final %>% group_by(TRACE, METRIC, ACT_DONE) %>% dplyr::mutate(TOTAL = n()) %>% 
     filter(T1 != T2) %>% group_by(TRACE, METRIC, ACT_DONE, PAIR) %>% dplyr::summarise(OCC = n() / first(TOTAL)) %>% arrange(desc(OCC)) %>% top_n(1)
dplot     = mutate(dplot, METRIC_STR = ifelse(METRIC == "cpu", "CPU", "Memória"), ACT_DONE_STR = ifelse(ACT_DONE == "ADD", "Adição", "Remoção"))

p = ggplot(dplot, aes(METRIC_STR, OCC, fill = ACT_DONE_STR)) + geom_boxplot(width = 0.7)
p = p + xlab("Métrica de provisionamento") + ylab("Transição mais frequente")
p = p + scale_y_continuous(labels = percent, breaks = c(0.01, seq(0, 1, 0.05)))
p = p + theme_bw(base_size = 24)
p = p + scale_fill_hue("Ação de provisionamento:")
p = p + theme(legend.position = "top", legend.key.size = unit(1.5, "cm"))
p

dplot %>% ungroup() %>%  dplyr::summarise(V = quantile(OCC, probs = 0.9))

png(filename = "img/02_04_threshold_trans_diff_top-freq_per-app.png", width = 750, height = 400)
print(p)
dev.off()

# percentual de ocorrencia de transições diferentes entre as transições diferentes
dplot = df_final %>% filter(T1 != T2) %>% group_by(METRIC, ACT_DONE) %>% mutate(TOTAL = n()) %>% 
          group_by(METRIC, ACT_DONE, PAIR) %>% summarise(OCC = n(), TT = first(TOTAL), FREQ = n()/ first(TOTAL))
dplot     = mutate(dplot, METRIC_STR = ifelse(METRIC == "cpu", "CPU", "Memória"), ACT_DONE_STR = ifelse(ACT_DONE == "ADD", "Adição", "Remoção"))

p = ggplot(dplot, aes(METRIC_STR, FREQ, fill = ACT_DONE_STR)) + geom_boxplot(width = 0.7)
p = p + xlab("Métrica de provisionamento") + ylab("Frequência de transições distintas")
p = p + scale_y_continuous(labels = percent, breaks = c(0.01, seq(0, 0.15, 0.05)))
p = p + theme_bw(base_size = 24)
p = p + scale_fill_hue("Ação de provisionamento:")
p = p + theme(legend.position = "top", legend.key.size = unit(1.5, "cm"))
p

png(filename = "img/02_04_threshold_trans_diff_freq_general.png", width = 750, height = 500)
print(p)
dev.off()
