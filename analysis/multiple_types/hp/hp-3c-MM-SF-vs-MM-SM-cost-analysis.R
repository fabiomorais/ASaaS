library(dplyr)
library(ggplot2)
library(scales)
library(stringr)

flavors      = sort(c("c4.large", "m4.large", "r3.large", "c3.large", "m3.medium"))
f_str        = str_sub(flavors, 1, 2)

df_sm_sf  = read.table(file = "../data/hp/scaling-analysis-SM-SF.dat", header = T)
df_mm_sf  = read.table(file = "../data/hp/scaling-analysis-MM-SF.dat", header = T)
df_mm_mf  = read.table(file = "../data/hp/scaling-analysis-MM-MF.dat", header = T)

head(df_mm_mf)

df_sm_sf  = df_sm_sf %>% select(jobId, flavor, metric_base, len, ecu_viol, mem_viol, tviol, cost_total) %>% 
            group_by(jobId, flavor, metric_base) %>% ungroup() %>% distinct()

df_mm_sf  = df_mm_sf %>% select(jobId, flavor, metric_base, len, ecu_viol, mem_viol, tviol, cost_total) %>% 
            group_by(jobId, flavor, metric_base) %>% ungroup() %>% distinct()

df_mm_mf  = df_mm_mf %>% select(jobId, metric_base, len, ecu_viol, mem_viol, cost_total) %>% 
            group_by(jobId, metric_base) %>% ungroup() %>% distinct()

df1       = df_sm_sf %>% ungroup() %>% group_by(flavor, metric_base) %>% 
            summarise(cost_ref = sum(cost_total))

df2       = df_mm_sf %>% ungroup() %>% group_by(flavor, metric_base) %>% 
            summarise(cost_ref = sum(cost_total))

df3       = df_mm_mf %>% ungroup() %>% summarise(cost = sum(cost_total))

# custo do multiflavor e multimetrics (melhor custo sem violações) em comparação ao single flavor e multi metrics
dplot2 = cbind(data.frame(df3), data.frame(df2)) %>% mutate(saving = (cost_ref - cost) / cost_ref) %>% select(flavor, cost, cost_ref, saving, metric_base)


dp2  = dplot2 
dp2$metric_base = as.character(dp2$metric_base)
dp2$metric_base[dp2$metric_base == "multi"] = "ECU e Memória"

write.table(dp2, file = "../data/hp/3c_cost_reduction_mutiflavors-hp-data.dat", row.names = F)