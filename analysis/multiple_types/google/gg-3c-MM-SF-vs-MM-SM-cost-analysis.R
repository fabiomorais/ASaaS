library(dplyr)
library(ggplot2)
library(scales)
library(stringr)

flavors      = sort(c("c4.large", "m4.large", "r3.large", "c3.large", "m3.medium"))
f_str        = str_sub(flavors, 1, 2)

ref       = data.frame(cpuRef = c(32, 32, 32, 32), memRef = c(32, 64, 128, 256), cpuP = rep(1, 4), memP = c(1, 2, 4, 8)) %>% 
            group_by(memRef) %>% mutate(scenario = paste(cpuRef, memRef, sep = "_"), scP = paste(cpuP, memP, sep = ":"))

df_sm_sf  = read.table(file = "../data/google/scaling-analysis-SM-SF.dat", header = T)
df_mm_sf  = read.table(file = "../data/google/scaling-analysis-MM-SF.dat", header = T)
df_mm_mf  = read.table(file = "../data/google/scaling-analysis-MM-MF.dat", header = T)

df_sm_sf  = df_sm_sf %>% select(scenario, jobId, flavor, metric_base, len, ecu_viol, mem_viol, tviol, cost_total) %>% 
            group_by(scenario, jobId, flavor, metric_base) %>% ungroup() %>% distinct()

df_mm_sf  = df_mm_sf %>% select(scenario, jobId, flavor, metric_base, len, ecu_viol, mem_viol, tviol, cost_total) %>% 
            group_by(scenario, jobId, flavor, metric_base) %>% ungroup() %>% distinct()

df_mm_mf  = df_mm_mf %>% select(scenario, jobId, metric_base, len, ecu_viol, mem_viol, cost_total) %>% 
            group_by(scenario, jobId, metric_base) %>% ungroup() %>% distinct()

df1       = df_sm_sf %>% ungroup() %>% group_by(scenario, flavor, metric_base) %>% 
            summarise(cost_ref = sum(cost_total))

df2       = df_mm_sf %>% ungroup() %>% group_by(scenario, flavor, metric_base) %>% 
            summarise(cost_ref = sum(cost_total))

df3       = df_mm_mf %>% ungroup() %>% group_by(scenario) %>% 
            summarise(cost = sum(cost_total))

# custo do multiflavor e multimetrics (melhor custo sem violações) em comparação ao single flavor e multi metrics
dplot2 = df3 %>% inner_join(df2, by = "scenario") %>% mutate(saving = (cost_ref - cost) / cost_ref) %>% select(scenario, flavor, cost, cost_ref, saving, metric_base)

dplot2$scenario = as.character(dplot2$scenario)
dp2  = dplot2 %>% inner_join(ref, by = "scenario")
dp2  = dp2 %>% filter(scP %in% c('1:1', "1:4")) 
dp2$metric_base = as.character(dp2$metric_base)

dp2$metric_base[dp2$metric_base == "multi"] = "ECU e Memória"

write.table(dp2, file = "../data/google/3c_cost_reduction_mutiflavors-google-data.dat", row.names = F)