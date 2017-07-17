library(ggplot2)
library(scales)
library(dplyr)

range_size = 5
df_result = read.table(file = paste("data/02_data_thresholds-", range_size, ".dat", sep = ""), header = T)
df_result = df_result %>% select(-TOTAL, -OCC)

# quantidade de diferentes limiares por aplicação
dplot     = df_result %>% group_by(TRACE, METRIC, ACT_DONE, THRESHOLD, PROV_TIME) %>% distinct() %>% 
     ungroup() %>% group_by(TRACE, METRIC, ACT_DONE, PROV_TIME) %>% dplyr::summarise(FREQ = n())

dplot     = mutate(dplot, METRIC_STR = ifelse(METRIC == "cpu", "CPU", "Memória"), ACT_DONE_STR = ifelse(ACT_DONE == "ADD", "Adição", "Remoção"))

p = ggplot(dplot, aes(METRIC_STR, FREQ, fill = ACT_DONE_STR)) + geom_boxplot(width = 0.7)
p = p + xlab("Métrica de provisionamento") + ylab("Diferentes limiares") #+ title("aaaa")
p = p + scale_y_continuous(breaks = c(1, 5, 10, 15, 20))
p = p + theme_bw(base_size = 24)
p = p + scale_fill_hue("Ação de Provisionamento:")
p = p + theme(legend.position = "top", legend.key.size = unit(1.5, "cm"))
p

png(filename = "img/02_03_threshold_use_number_per-app.png", width = 750, height = 400)
print(p)
dev.off()

dplot %>% group_by(METRIC) %>% dplyr::summarise(V = mean(FREQ))


# quantidade de diferentes pares (limiar, #vm) por aplicação
dplot     = df_result %>% group_by(TRACE, METRIC, ACT_DONE, THRESHOLD, DIFF, PROV_TIME) %>% distinct() %>% 
     ungroup() %>% group_by(TRACE, METRIC, ACT_DONE, PROV_TIME) %>% dplyr::summarise(FREQ = n())
dplot     = mutate(dplot, METRIC_STR = ifelse(METRIC == "cpu", "CPU", "Memória"), ACT_DONE_STR = ifelse(ACT_DONE == "ADD", "Adição", "Remoção"))

p = ggplot(dplot, aes(METRIC_STR, FREQ, fill = ACT_DONE_STR)) + geom_boxplot(width = 0.7)
p = p + xlab("Ação de provisionamento") + ylab("Diferentes configurações") #+ title("aaaa")
p = p + scale_y_continuous(breaks = c(5, 25, 50, 100, 150, 200), trans = "sqrt")
p = p + theme_bw(base_size = 24)
p = p + scale_fill_hue("Métrica:")
p = p + theme(legend.position = "top", legend.key.size = unit(1.5, "cm"))
p

png(filename = "img/02_03_threshold-and-vm_use_number_per-app.png", width = 750, height = 400)
print(p)
dev.off()

dplot %>% group_by(METRIC) %>% dplyr::summarise(V = median(FREQ))

# maior frequência de ocorrencia de limiares por aplicação
dplot     = df_result %>% ungroup() %>% group_by(TRACE, METRIC, ACT_DONE) %>% dplyr::mutate(TOTAL = n()) %>% 
     group_by(TRACE, METRIC, ACT_DONE, THRESHOLD) %>% dplyr::summarise(FREQ = n() / first(TOTAL)) %>% 
     arrange(desc(FREQ)) %>% top_n(1) %>% sample_n(1)
dplot     = mutate(dplot, METRIC_STR = ifelse(METRIC == "cpu", "CPU", "Memória"), ACT_DONE_STR = ifelse(ACT_DONE == "ADD", "Adição", "Remoção"))

p = ggplot(dplot, aes(METRIC_STR, FREQ, fill = ACT_DONE_STR)) + geom_boxplot(width = 0.7)
p = p + xlab("Métrica de provisionamento") + ylab("Limiares mais frequentes") #+ title("aaaa")
p = p + scale_y_continuous(labels = percent, breaks = seq(0, 1, 0.1))
p = p + theme_bw(base_size = 24)
p = p + scale_fill_hue("Ação de provisionamento:")
p = p + theme(legend.position = "top", legend.key.size = unit(1.5, "cm"))
p

png(filename = "img/02_03_threshold-top-freq_per-app.png", width = 750, height = 500)
print(p)
dev.off()

dplot %>% ungroup() %>% filter(METRIC == "mem") %>% group_by(ACT_DONE) %>% dplyr::summarise(V = median(FREQ))
dplot %>% ungroup() %>% filter(METRIC == "cpu") %>% dplyr::summarise(V = median(FREQ))

# maior frequência de ocorrencia do par (limiar,#vm) por aplicação
dplot     = df_result %>% ungroup() %>% group_by(TRACE, METRIC, ACT_DONE) %>% dplyr::mutate(TOTAL = n()) %>% 
     group_by(TRACE, METRIC, ACT_DONE, THRESHOLD, DIFF) %>% dplyr::summarise(FREQ = n() / first(TOTAL)) %>% 
     arrange(desc(FREQ)) %>% top_n(1) %>% sample_n(1)
dplot     = mutate(dplot, METRIC_STR = ifelse(METRIC == "cpu", "CPU", "Memória"), ACT_DONE_STR = ifelse(ACT_DONE == "ADD", "Adição", "Remoção"))

p = ggplot(dplot, aes(METRIC_STR, FREQ, fill = ACT_DONE_STR)) + geom_boxplot(width = 0.7)
p = p + xlab("Métrica de provisionamento") + ylab("Configurações mais frequentes") #+ title("aaaa")
p = p + scale_y_continuous(labels = percent, breaks = seq(0, 1, 0.1))
p = p + theme_bw(base_size = 24)
p = p + scale_fill_hue("Ação de provisionamento:")
p = p + theme(legend.position = "top", legend.key.size = unit(1.5, "cm"))
p

png(filename = "img/02_03_threshold-and-vm-top-freq_per-app.png", width = 750, height = 500)
print(p)
dev.off()

head(dplot)
dplot %>% ungroup() %>% dplyr::mutate(TOTAL = n()) %>% dplyr::summarise(V = sum(FREQ < 0.1) / first(TOTAL))
dplot %>% ungroup() %>% filter(ACT_DONE == "ADD", METRIC == "mem") %>% dplyr::mutate(TOTAL = n()) %>% 
     dplyr::summarise(V = sum(FREQ > 0.1) / first(TOTAL))
