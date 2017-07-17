library(ggplot2)
library(scales)
library(dplyr)

range_size = 5
df_result = read.table(file = paste("data/02_data_thresholds-", range_size, ".dat", sep = ""), header = T)
df_result = df_result %>% select(-TOTAL, -OCC)

head(df_result)

# frequência de ocorrencia de limiares no geral
dplot     = df_result %>% ungroup() %>% group_by(METRIC, ACT_DONE) %>% mutate(TOTAL = n()) %>% group_by(METRIC, ACT_DONE, THRESHOLD) %>% summarise(FREQ = n() / first(TOTAL))
dplot     = mutate(dplot, METRIC_STR = ifelse(METRIC == "cpu", "CPU", "Memória"), ACT_DONE_STR = ifelse(ACT_DONE == "ADD", "Adição", "Remoção"))

p = ggplot(dplot, aes(METRIC_STR, FREQ, fill = ACT_DONE_STR)) + geom_boxplot(width = 0.5)
p = p + scale_y_continuous(labels = percent, breaks = c(0.01, 0.1, 0.2, 0.3, 0.4))
p = p + scale_fill_hue("Ação de provisionamento:")
p = p + xlab("Métrica de provisionamento") + ylab("Frequência do uso de limiares")
p = p + theme_bw(base_size = 24)
p = p + theme(legend.position = "top", legend.key.size = unit(1.5, "cm"))
p

png(filename = "img/02_02_threshold_freq_general.png", width = 750, height = 400)
print(p)
dev.off()

dplot %>% group_by(METRIC) %>% summarise(MEAN = mean(FREQ) * 100, MEDIAN = median(FREQ) * 100)
dplot %>% group_by(METRIC, ACT_DONE) %>% summarise(MEAN = mean(FREQ) * 100, MEDIAN = median(FREQ) * 100)
dplot %>% ungroup() %>% mutate(TOTAL = n()) %>% summarise(V = sum(FREQ < 0.1) / first(TOTAL)) 


# frequência de ocorrencia dos pares (limiar, #vm) no geral
dplot     = df_result %>% ungroup() %>% group_by(METRIC, ACT_DONE) %>% mutate(TOTAL = n()) %>% group_by(METRIC, ACT_DONE, THRESHOLD, DIFF) %>% summarise(FREQ = n() / first(TOTAL))
dplot     = mutate(dplot, METRIC_STR = ifelse(METRIC == "cpu", "CPU", "Memória"), ACT_DONE_STR = ifelse(ACT_DONE == "ADD", "Adição", "Remoção"))

p = ggplot(dplot, aes(METRIC_STR, FREQ, fill = ACT_DONE_STR)) + geom_boxplot(width = 0.5)
p = p + scale_y_continuous(labels = percent, breaks = c(0.001, 0.01, 0.05, 0.1, 0.2, 0.3, 0.4), trans = "sqrt")
p = p + scale_fill_hue("Ação de provisionamento:")
p = p + xlab("Métrica de provisionamento") + ylab("Frequência do uso de configurações")
p = p + theme_bw(base_size = 24)
p = p + theme(legend.position = "top", legend.key.size = unit(1.5, "cm"))
p

png(filename = "img/02_02_threshold-and-vm_freq_general.png", width = 750, height = 400)
print(p)
dev.off()

# cerca de 90% das configurações é usada menos de 1% das vezes
dplot %>% group_by(METRIC, ACT_DONE) %>% mutate(TOTAL = n()) %>% summarise(V = sum(FREQ < 0.01) / first(TOTAL)) 
dplot %>% ungroup() %>% mutate(TOTAL = n()) %>% summarise(V = sum(FREQ < 0.01) / first(TOTAL)) 
