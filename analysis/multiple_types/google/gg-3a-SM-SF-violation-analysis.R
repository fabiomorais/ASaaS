library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(tidyr)

flavors      = sort(c("c4.large", "m4.large", "r3.large", "c3.large", "m3.medium"))
f_str        = str_sub(flavors, 1, 2)

ref          = data.frame(cpuRef = c(32, 32, 32, 32), memRef = c(32, 64, 128, 256), cpuP = rep(1, 4), memP = c(1, 2, 4, 8)) %>% 
               group_by(memRef) %>% mutate(scenario = paste(cpuRef, memRef, sep = "_"), scP = paste(cpuP, memP, sep = ":"))

df_analysis = read.table(file = "../data/google/scaling-analysis-SM-SF.dat", header = T)

df1 = df_analysis %>% select(scenario, jobId, flavor, metric_base, len, ecu_viol, mem_viol, tviol, cost_total) %>% distinct() #%>% group_by(scenario, jobId, flavor, metric_base) 
df1 = df1 %>% inner_join(ref, by = "scenario")

df1 = df1 %>% filter(scP %in% c("1:1", "1:4"))
df1$scP = factor(df1$scP, levels = ref$scP)

# colateral violations
dfp = df1 %>% group_by(scP, flavor, metric_base) %>% mutate(n = sum(len)) %>% summarise(ECU = sum(ecu_viol) / nth(n, 1), Memory = sum(mem_viol) / nth(n, 1))
dfp = dfp %>% gather("metric", "value", 4:5)

dfp$metric_base = as.character(dfp$metric_base)
dfp$metric_base[dfp$metric_base == "ecu"] = "ECU"
dfp$metric_base[dfp$metric_base == "mem"] = "Memória"

write.table(dfp, file = "../data/google/3a_colateral_violations-google-data.dat", row.names = F)