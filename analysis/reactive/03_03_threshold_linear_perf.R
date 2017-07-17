library(dplyr)
library(ggplot2)
library(scales)


df_base_cpu = read.table(file = "data/trace_conf_sweep-cpu.dat", header = T)
df_base_cpu = df_base_cpu %>% select(TRACE, PROV = METRIC, GROUP, ADD, RM, DIST, VM_ADD, VM_RM, VIOLP, -COST, -PERF_REL, OVER_REL, -VIOL_LEN)
df_base_cpu$PROV = "CPU"

df_base_mem = read.table(file = "data/trace_conf_sweep-mem.dat", header = T)
df_base_mem = df_base_mem %>% select(TRACE, PROV = METRIC, GROUP, ADD, RM, DIST, VM_ADD, VM_RM, VIOLP, -COST, -PERF_REL, OVER_REL, -VIOL_LEN)
df_base_mem$PROV = "MEM"

df_result = rbind(df_base_cpu, df_base_mem)

dpt = df_result %>% gather("METRIC", "VALUE", 9:10)

dpt$VALUE[dpt$METRIC == "VIOL_LEN"] = pmax(dpt$VALUE[dpt$METRIC == "VIOL_LEN"] / 100, 0, na.rm = T)

dpt$METRIC[dpt$METRIC == "VIOLP"] = "Percentual\n de violações"
dpt$METRIC[dpt$METRIC == "OVER_REL"] = "Custo realtivo\n ao superprovido"

dplot = dpt %>% filter(!ADD %in% c(0.1, 0.2)) %>% group_by(TRACE, PROV, METRIC, ADD) %>% 
     do(model_value = summary(lm(.$VALUE ~ .$RM))$r.squared) %>% mutate(R2 = as.numeric(model_value))
dplot    = mutate(dplot, PROV_STR = ifelse(PROV == "CPU", "CPU", "Memória"))

p = ggplot(dplot, aes(paste(ADD * 100, "%", sep = ""), R2))
p = p + geom_boxplot()
p = p + facet_grid(METRIC ~ PROV_STR, scales = "free")
p = p + theme_bw(base_size = 24)
p = p + xlab("Limiares de adição ") + ylab("R2")
p

dplot %>% group_by(PROV, METRIC, ADD) %>% summarise(MEDIAN = median(R2, na.rm = T)) %>% data.frame()

dplot %>% group_by(PROV, METRIC) %>% summarise(MEDIAN = median(R2, na.rm = T)) %>% data.frame()

dplot %>% group_by(PROV) %>% summarise(MEDIAN = median(R2, na.rm = T)) %>% data.frame()
