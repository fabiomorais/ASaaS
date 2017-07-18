library(dplyr)
library(ggplot2)
library(scales)

dgg = read.table(file = "data/google/2a_predominance_ratio-google-data.dat", header = T) %>% mutate(label = paste("Google", scP)) %>% select(index, ratio, jobId, label)
dhp = read.table(file = "data/hp/2a_predominance_ratio-hp-data.dat", header = T) %>% mutate(label = "HP") %>% mutate(jobId = paste(jobId, "x"))

head(dgg)
head(dhp)
dfp = rbind(dgg, dhp)
factors = sort(unique(dfp$label), decreasing = T)
dfp$label = factor(dfp$label, levels = factors) 

p = ggplot(dfp, aes(x = ratio, colour = as.factor(jobId))) + stat_ecdf(alpha = 0.5) 
p = p + facet_wrap(~ label, ncol = 3, scales = "free_x")
p = p + scale_x_continuous(labels = comma, trans = "log10", breaks = c(0.001, 1, 1000)) 
p = p + scale_y_continuous(labels = percent)
p = p + theme_bw(base_size = 32)
p = p + theme(legend.position = "none")
p = p + ylab("Proporção de intervalos de tempo") + xlab("Razão (CPU/Memória)")

png(filename = "img/2a_predominance_ratio-all.png", width = 850, height = 500)
print(p)
dev.off()

# analise das ordens de grandeza entre os diferentes ratios das aplicaçoes
x = 0.1 / 2 # 10%

# google
ratio          = dgg %>% filter(!jobId %in% job_error$jobId) %>% select(label, ratio, jobId) %>% group_by(label, jobId) %>% filter(ratio != 0)
ratio          = ratio %>% mutate(thmin = quantile(ratio, probs = x), thmax = quantile(ratio, probs = (1 - x)))
ratio          = ratio %>% filter(ratio > thmin, ratio < thmax)
ratio          = ratio %>% summarise(min_v = min(ratio, na.rm = T), max_v = max(ratio, na.rm = T))
ratio          = ratio %>% mutate(var_times = max_v / min_v, tail = q)

print(summary((ratio %>% filter(label == "Google 1:1"))$var_times))
print(summary((ratio %>% filter(label == "Google 1:4"))$var_times))

# hp
ratio          = dhp %>% select(ratio, jobId) %>% group_by(jobId) %>% filter(ratio != 0)
ratio          = ratio %>% mutate(thmin = quantile(ratio, probs = x), thmax = quantile(ratio, probs = (1 - x)))
ratio          = ratio %>% filter(ratio > thmin, ratio < thmax)
ratio          = ratio %>% summarise(min_v = min(ratio, na.rm = T), max_v = max(ratio, na.rm = T))
ratio          = ratio %>% mutate(var_times = max_v / min_v, tail = q)

print(summary(ratio$var_times))