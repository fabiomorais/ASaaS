library(dplyr)
library(ggplot2)
library(scales)
library(stringr)

flavors      = sort(c("c4.large", "m4.large", "r3.large", "c3.large", "m3.medium"))
f_str        = str_sub(flavors, 1, 2)

ref          = data.frame(cpuRef = c(32, 32, 32, 32), memRef = c(32, 64, 128, 256), cpuP = rep(1, 4), memP = c(1, 2, 4, 8))%>% 
          group_by(memRef) %>% mutate(scenario = paste(cpuRef, memRef, sep = "_"), scP = paste(cpuP, memP, sep = ":"))

df_sm_sf  = read.table(file = "../data/google/scaling-analysis-SM-SF.dat", header = T)
df_mm_sf  = read.table(file = "../data/google/scaling-analysis-MM-SF.dat", header = T)


df_sm_sf  = df_sm_sf %>% select(scenario, jobId, flavor, metric_base, len, ecu_viol, mem_viol, tviol, cost_total) %>% 
     group_by(scenario, jobId, flavor, metric_base) %>% ungroup() %>% distinct()

df_mm_sf  = df_mm_sf %>% select(scenario, jobId, flavor, metric_base, len, ecu_viol, mem_viol, tviol, cost_total) %>% 
     group_by(scenario, jobId, flavor, metric_base) %>% ungroup() %>% distinct()

df1       = df_sm_sf %>% ungroup() %>% group_by(scenario, flavor, metric_base) %>% summarise(cost = sum(cost_total)) %>% mutate(metric_base = as.character(metric_base))
df2       = df_mm_sf %>% ungroup() %>% group_by(scenario, flavor, metric_base) %>% summarise(cost = sum(cost_total)) %>% mutate(metric_base = as.character(metric_base))

df3       = inner_join(df2, df1, by = c("scenario", "flavor")) %>% rename(metric_base = metric_base.x, metric_ref = metric_base.y, cost_mult = cost.x, cost_single = cost.y)
df3       = df3 %>% mutate(incrase = (cost_mult - cost_single) / cost_single)
df3

df3$metric_ref[df3$metric_ref == "ecu"] = "ECU"
df3$metric_ref[df3$metric_ref == "mem"] = "Memória"

dplot     = df3 %>% inner_join(ref, by = "scenario")
dplot     = dplot %>% filter(scP %in% c("1:1", "1:4"))

write.table(dplot, file = "../data/google/3b_cost_violations_trade-off-google-data.dat", row.names = FALSE)

