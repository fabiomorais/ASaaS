library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)

df_base_cpu = read.table(file = "data/trace_conf_sweep-cpu.dat", header = T)
df_base_cpu = df_base_cpu %>% select(TRACE, PROV = METRIC, GROUP, ADD, RM, DIST, VM_ADD, VM_RM, VIOLP, -COST, PERF_REL, OVER_REL, -VIOL_LEN)
df_base_cpu$PROV = "CPU"

df_base_mem = read.table(file = "data/trace_conf_sweep-mem.dat", header = T)
df_base_mem = df_base_mem %>% select(TRACE, PROV = METRIC, GROUP, ADD, RM, DIST, VM_ADD, VM_RM, VIOLP, -COST, PERF_REL, OVER_REL, -VIOL_LEN)
df_base_mem$PROV = "MEM"

df_result = rbind(df_base_cpu, df_base_mem)

head(df_result)
dpt = df_result %>% gather("METRIC", "VALUE", 9:11)

dpt$VALUE[dpt$METRIC == "VIOL_LEN"] = pmax(dpt$VALUE[dpt$METRIC == "VIOL_LEN"] / 100, 0, na.rm = T)

dpt$METRIC[dpt$METRIC == "VIOLP"] = "Percentual\n de violações"
dpt$METRIC[dpt$METRIC == "OVER_REL"] = "Custo relativo\n ao superprovido"
dpt$METRIC[dpt$METRIC == "PERF_REL"] = "Custo relativo\n ao perfeito"

dpt$METRIC = factor(dpt$METRIC, levels = c("Custo relativo\n ao superprovido", "Custo relativo\n ao perfeito", "Percentual\n de violações"))

dplot     = dpt %>% filter(ADD != 0.1) %>% filter(DIST %in% c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8))
dplot    = mutate(dplot, PROV_STR = ifelse(PROV == "CPU", "CPU", "Memória"))

unique(dplot$ADD)
unique(dplot$RM)
unique(dplot$DIST)
unique(dplot$PROV_STR)

dplot$DIST = paste(dplot$DIST * 100, "%", sep = "")
dplot$DIST = factor(dplot$DIST, levels = c("10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%"))
     
dplot$RMSTR = paste(dplot$RM * 100, "%", sep = "")
dplot$RMSTR = factor(dplot$RMSTR, levels = c("10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%"))

for (metric in c("CPU", "MEM")) {
     
     df_tmp = dplot %>% filter(PROV == metric)
     
     p = ggplot(df_tmp, aes(paste(ADD * 100, "%", sep = ""), VALUE, fill = RMSTR))
     p = p + geom_boxplot(position = position_dodge())
     p = p + facet_grid(METRIC ~ ., scales = "free")
     p = p + scale_fill_brewer("Limiares de remoção:", palette = "Set1")
     p = p + scale_y_continuous(labels = percent)
     p = p + theme_bw(base_size = 24)
     p = p + theme(legend.position = "top", legend.key.size = unit(1, "cm"))
     p = p + xlab("Limiares de adição ") + ylab(NULL)
     p = p + guides(fill = guide_legend(nrow = 1, byrow = TRUE))
     p
     
     png(filename = paste("img/03_02_threshold-pratical-perf-", tolower(metric), ".png", sep = ""), width = 1000, height = 650)
     print(p)
     dev.off()
}

dplot %>% group_by(PROV_STR, METRIC, ADD) %>% filter(ADD == 0.9, RM == 0.8) %>% summarise(V = median(VALUE) * 100)
dplot %>% group_by(PROV_STR, METRIC, ADD) %>% filter(ADD == 0.9) %>% summarise(V = median(VALUE) * 100)
dplot %>% group_by(PROV_STR, METRIC, ADD) %>% filter(ADD == 0.2) %>% summarise(V = sum(VALUE == 0) / 30)

