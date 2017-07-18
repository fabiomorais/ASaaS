library(dplyr)
library(stringr)
library(tidyr)

flavors      = sort(c("c4.large", "m4.large", "r3.large", "c3.large", "m3.medium"))
f_str        = str_sub(flavors, 1, 2)

df_analysis = read.table(file = "../data/hp/scaling-analysis-SM-SF.dat", header = T)

df1 = df_analysis %>% select(jobId, flavor, metric_base, len, ecu_viol, mem_viol, tviol, cost_total) %>% dplyr::group_by(jobId, flavor, metric_base) %>% ungroup() %>% distinct()

dfp = df1 %>% group_by(flavor, metric_base) %>% mutate(n = sum(len)) %>% summarise(ECU = sum(ecu_viol) / nth(n, 1), Memory = sum(mem_viol) / nth(n, 1))
dfp = dfp %>% gather("metric", "value", 3:4)

dfp$metric_base = as.character(dfp$metric_base)
dfp$metric_base[dfp$metric_base == "ecu"] = "ECU"
dfp$metric_base[dfp$metric_base == "mem"] = "Memória"

write.table(dfp, file = "../data/hp/3a_colateral_violations-hp-data.dat", row.names = F)
