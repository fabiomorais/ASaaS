library(dplyr)
library(ggplot2)

df_analysis = read.table(file = "../../../multiple_types/hp/data/scaling_perf_max.dat", header = T)
print(df_analysis)

# percentual de vezes que cada flavor foi usado
df_total  = df_analysis %>% group_by(jobId) %>% select(jobId, len) %>% ungroup() %>% distinct() %>% summarise(total = sum(len))

dfp       = df_analysis %>% group_by(flavor) %>% summarise(occ = sum(n)) %>% cbind(data.frame(df_total)) %>% mutate(freq = occ / total)

write.table(dfp, file = "../data/hp/4a_flavor_occurence-hp-data.dat", row.names = F)

njobs = length(unique(df_analysis$jobId))
dfp = df_analysis %>% group_by(jobId) %>% summarise(nflavor = n()) %>% ungroup() %>% group_by(nflavor) %>% summarise(occ = n()) %>% mutate(total = njobs, freq = occ / njobs)

write.table(dfp, file = "../data/hp/4a_flavor_number_occurence-hp-data.dat", row.names = F)