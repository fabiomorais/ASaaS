library(ggplot2)
library(tidyr)
library(scales)
library(dplyr)

dff = read.table(file = "data/general-data-analysis.dat", header = T)
dff = dff %>% filter(CORR == 0, SAFEM == 0) %>% 
     select(TRACE, METRIC, SCENARIO, SAFEM, FILTER, VIOLP, PERF_REL, OVER_REL) %>% 
     gather("metric", "value", 6:8)

dff$metric[dff$metric == "VIOLP"] = "Percentual\n de violações"
dff$metric[dff$metric == "OVER_REL"] = "Custo realtivo\n ao superprovido"
dff$metric[dff$metric == "PERF_REL"] = "Custo realtivo\n ao perfeito"

dff  = dff %>% mutate(METRIC_STR = ifelse(METRIC == "cpu", "CPU", "Memória"))
dff  = dff %>% mutate(SCENARIO_STR = ifelse(SCENARIO == "Dynamic", "Dinâmico", as.character(SCENARIO)))
dff  = dff %>% mutate(FILTER_STR = ifelse(FILTER == "normal", "Sem filtro", "Com filtro"))

dff$metric = factor(dff$metric, levels = c("Custo realtivo\n ao superprovido", "Custo realtivo\n ao perfeito", "Percentual\n de violações"))
dff$SCENARIO_STR = factor(dff$SCENARIO_STR, levels = c("LW", "AR", "Dinâmico"))
dff$FILTER_STR = factor(dff$FILTER_STR, levels = c("Sem filtro", "Com filtro"))

#dff$METRIC = as.character(dff$METRIC)

p = ggplot(dff, aes(SCENARIO_STR, value, fill = FILTER_STR))
p = p + geom_boxplot(position = position_dodge(), width = 0.7)
p = p + facet_grid(metric ~ METRIC_STR, scales = "free")
p = p + scale_fill_brewer("", palette = "Set1")
p = p + scale_y_continuous(labels = percent)
p = p + theme_bw(base_size = 24)
p = p + theme(legend.position = "top", legend.key.size = unit(1, "cm"))
p = p + xlab("Abordagem de predição") + ylab(NULL)
p = p + guides(fill = guide_legend(nrow = 1, byrow = TRUE))
p

png(filename = paste("img/02_03_filter-perf.png", sep = ""), width = 1000, height = 650)
print(p)
dev.off()

dff %>% filter(metric == "Custo realtivo\n ao superprovido") %>% group_by(METRIC_STR, FILTER) %>% summarise(mean = mean(value) * 100, median = median(value)*100)
dff %>% filter(metric == "Percentual\n de violações") %>% group_by(METRIC_STR, FILTER) %>% summarise(mean = mean(value) * 100, median = median(value)*100)

