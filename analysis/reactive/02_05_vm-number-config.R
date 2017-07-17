library(ggplot2)
library(scales)
library(dplyr)

range_size = 5
df_result = read.table(file = paste("data/02_data_thresholds-", range_size, ".dat", sep = ""), header = T)
df_result = df_result %>% select(-TOTAL, -OCC)

# sumario do numero de vms usadas no privisionamento no geral
df_result %>% group_by(METRIC, ACT_DONE) %>% summarise(MEAN = mean(DIFF), Q1 = quantile(DIFF, probs = 0.25), MEDIAN = median(DIFF), Q3 = quantile(DIFF, probs = 0.75), MAX = max(DIFF))

# 1Percentual de ações de provisionamento em que foram provisionadas 1 vm 
df_result %>% group_by(METRIC, ACT_DONE) %>% summarise(V = sum(DIFF == 1) / n() * 100)

df_result %>% summarise(V = sum(DIFF == 1) / n() * 100)
df_result %>% summarise(V = sum(DIFF == 2) / n() * 100)
df_result %>% summarise(V = sum(DIFF == 3) / n() * 100)
df_result %>% summarise(V = sum(DIFF == 4) / n() * 100)
df_result %>% summarise(V = sum(DIFF <= 4) / n() * 100)

# sumario do numero de vms usadas no privisionamento no geral, quando #vm diferente de 1
df_result %>% filter(DIFF != 1) %>%  group_by(METRIC, ACT_DONE) %>% 
     summarise(MEAN = mean(DIFF), Q1 = quantile(DIFF, probs = 0.25), MEDIAN = median(DIFF), Q3 = quantile(DIFF, probs = 0.75), MAX = max(DIFF))


dplot = df_result %>% mutate(TOTAL = n()) %>% group_by(DIFF) %>% summarise(V = n() / first(TOTAL)) %>% data.frame()

# boxplot da quantidade de diferentes confs de #vm por limiar no geral
p = ggplot(dplot, aes(DIFF, V)) + geom_line()
p = p + xlab("Quantidade de VMs provisionadas") + ylab("Frequência de uso da configuração") #+ title("aaaa")
p = p + scale_x_continuous(breaks = seq(0, 22, 1))
p = p + scale_y_continuous(labels = percent, breaks = seq(0, 1, 0.1))
p = p + theme_bw(base_size = 24)
p = p + scale_fill_hue("Ação de provisionamento:")
p = p + theme(legend.position = "top")
p

# sumario do numero de vms usadas no privisionamento por aplicação
df_result %>% group_by(TRACE, METRIC, ACT_DONE) %>% 
     summarise(MEAN = mean(DIFF), Q1 = quantile(DIFF, probs = 0.25), MEDIAN = median(DIFF), Q3 = quantile(DIFF, probs = 0.75), Q99 = quantile(DIFF, probs = 0.99), MAX = max(DIFF)) %>%
     data.frame()

# quantidade de diferentes configurações de #vms por limiar agrupado por aplicação (essa análise faz pouco sentido)
df_result %>% group_by(TRACE, METRIC, ACT_DONE, THRESHOLD, DIFF) %>% summarise(N = n()) %>% ungroup() %>% 
     group_by(TRACE, METRIC, ACT_DONE, THRESHOLD) %>% summarise(NUM_CONF = n()) %>% 
     group_by(ACT_DONE, METRIC) %>% summarise(MEAN = mean(NUM_CONF), Q1 = quantile(NUM_CONF, probs = 0.25), MEDIAN = median(NUM_CONF), Q3 = quantile(NUM_CONF, probs = 0.75), MAX = max(NUM_CONF)) %>% ungroup()
     
# sumário da quantidade de diferentes configurações de #vms por limiar no geral (faz mais sentido)
df_result %>% group_by(METRIC, ACT_DONE, THRESHOLD, DIFF) %>% summarise(N = n()) %>% ungroup() %>%  
     group_by(METRIC, ACT_DONE, THRESHOLD) %>% summarise(NUM_CONF = n()) %>% 
     group_by(ACT_DONE, METRIC) %>% summarise(MEAN = mean(NUM_CONF), Q1 = quantile(NUM_CONF, probs = 0.25), MEDIAN = median(NUM_CONF), Q3 = quantile(NUM_CONF, probs = 0.75), MAX = max(NUM_CONF)) %>% ungroup()

df_result %>% group_by(METRIC, THRESHOLD, DIFF) %>% summarise(N = n()) %>% ungroup() %>%  
     group_by(METRIC, THRESHOLD) %>% summarise(NUM_CONF = n()) %>% 
     group_by(METRIC) %>% summarise(MEAN = mean(NUM_CONF), Q1 = quantile(NUM_CONF, probs = 0.25), MEDIAN = median(NUM_CONF), Q3 = quantile(NUM_CONF, probs = 0.75), MAX = max(NUM_CONF)) %>% ungroup()



dplot = df_result %>% group_by(METRIC, ACT_DONE, THRESHOLD, DIFF) %>% summarise(N = n()) %>% ungroup() %>%  
     group_by(METRIC, ACT_DONE, THRESHOLD) %>% summarise(NUM_CONF = n())
dplot     = mutate(dplot, METRIC_STR = ifelse(METRIC == "cpu", "CPU", "Memória"), ACT_DONE_STR = ifelse(ACT_DONE == "ADD", "Adição", "Remoção"))

# boxplot da quantidade de diferentes confs de #vm por limiar no geral
p = ggplot(dplot, aes(METRIC_STR, NUM_CONF, fill = ACT_DONE_STR)) + geom_boxplot(width = 0.7)
p = p + xlab("Métrica de provisionamento") + ylab("Diferentes configurações por limiar") #+ title("aaaa")
p = p + scale_y_continuous(breaks = seq(1, 19, 2))
p = p + theme_bw(base_size = 24)
p = p + scale_fill_hue("Ação de provisionamento:")
p = p + theme(legend.position = "top", legend.key.size = unit(1.5, "cm"))
p

png(filename = "img/02_05_threshold-diff_nvm_general.png", width = 750, height = 500)
print(p)
dev.off()


dplot = df_result %>% group_by(TRACE, METRIC, ACT_DONE, THRESHOLD, DIFF) %>% summarise(N = n()) %>% ungroup() %>% 
     group_by(TRACE, METRIC, ACT_DONE, THRESHOLD) %>% summarise(NUM_CONF = n())
dplot     = mutate(dplot, METRIC_STR = ifelse(METRIC == "cpu", "CPU", "Memória"), ACT_DONE_STR = ifelse(ACT_DONE == "ADD", "Adição", "Remoção"))

# boxplot da quantidade de diferentes confs de #vm por limiar no geral
p = ggplot(dplot, aes(METRIC_STR, NUM_CONF, fill = ACT_DONE_STR)) + geom_boxplot(width = 0.7)
p = p + xlab("Métrica de provisionamento") + ylab("Diferentes configurações por limiar") #+ title("aaaa")
p = p + scale_y_continuous(breaks = seq(1, 16, 2))
p = p + theme_bw(base_size = 24)
p = p + scale_fill_hue("Ação de provisionamento:")
p = p + theme(legend.position = "top", legend.key.size = unit(1.5, "cm"))
p

summary(dplot$NUM_CONF)

quantile(dplot$NUM_CONF, probs = 0.87)

png(filename = "img/02_05_threshold-diff_nvm_per_app.png", width = 750, height = 500)
print(p)
dev.off()