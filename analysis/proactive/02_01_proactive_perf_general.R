library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

dff = read.table(file = "data/general-data-analysis.dat", header = T)
dff = dff %>% filter(CORR == 0,  SAFEM == 0, FILTER == "normal") %>% 
     select(TRACE, METRIC, SCENARIO, VIOLP, PERF_REL, OVER_REL) %>% 
     gather("metric", "value", 4:6)

dff$metric[dff$metric == "VIOLP"] = "Percentual\n de violações"
dff$metric[dff$metric == "OVER_REL"] = "Custo realtivo\n ao superprovido"
dff$metric[dff$metric == "PERF_REL"] = "Custo realtivo\n ao perfeito"

dff  = dff %>% mutate(METRIC_STR = ifelse(METRIC == "cpu", "CPU", "Memória"))
dff  = dff %>% mutate(SCENARIO_STR = ifelse(SCENARIO == "Dynamic", "Dinâmico", as.character(SCENARIO)))

dff$metric = factor(dff$metric, levels = c("Custo realtivo\n ao superprovido", "Custo realtivo\n ao perfeito", "Percentual\n de violações"))
dff$SCENARIO_STR = factor(dff$SCENARIO_STR, levels = c("LW", "AR", "Dinâmico"))

p = ggplot(dff, aes(SCENARIO_STR, value))
p = p + geom_boxplot(position = position_dodge(), width = 0.5)
p = p + facet_grid(metric ~ METRIC_STR, scales = "free")
p = p + scale_fill_brewer("Métrica base:", palette = "Set1")
p = p + scale_y_continuous(labels = percent)
p = p + theme_bw(base_size = 24)
p = p + theme(legend.position = "none", legend.key.size = unit(1, "cm"))
p = p + xlab("Abordagem de predição") + ylab(NULL)
p = p + guides(fill = guide_legend(nrow = 1, byrow = TRUE))
p

png(filename = "img/02_01_proactive-pratical-perf.png", width = 1000, height = 650)
print(p)
dev.off()

dff %>% filter(metric == "Custo realtivo\n ao perfeito") %>% group_by(METRIC_STR) %>% summarise(mean = mean(value) * 100)

dff %>% filter(metric == "Percentual\n de violações") %>% group_by(METRIC_STR) %>% summarise(mean = mean(value) * 100)
