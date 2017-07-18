library(dplyr)
library(ggplot2)
library(scales)
library(stringr)

job_error      = read.table(file = "../../../multiple_types/google/data/job_without_util.dat", header = T) 

df_analysis = read.table(file = "../../../multiple_types/google/data/scaling_perf_max.dat", header = T)
df_analysis = df_analysis %>% filter(!jobId %in% job_error$jobId)
print(df_analysis)

ref          = data.frame(cpuRef = c(32, 32, 32, 32), memRef = c(32, 64, 128, 256), cpuP = rep(1, 4), memP = c(1, 2, 4, 8)) %>% 
               group_by(memRef) %>% mutate(scenario = paste(cpuRef, memRef, sep = "_"), scP = paste(cpuP, memP, sep = ":"))

# percentual de vezes que cada flavor foi usado
df_total  = df_analysis %>% group_by(scenario, jobId) %>% select(scenario, jobId, len) %>% ungroup() %>% distinct() %>% group_by(scenario) %>% summarise(total = sum(len))

dfp       = df_analysis %>% group_by(scenario, flavor) %>% summarise(occ = sum(n)) %>% inner_join(df_total, by = "scenario") %>% mutate(freq = occ / total)

dfp = dfp %>% inner_join(ref, by = "scenario")
dfp$scP = factor(dfp$scP, levels = ref$scP[nrow(ref):1])

dfp = dfp %>% filter(scP %in% c("1:1", "1:4"))
dfp$scP = factor(dfp$scP, levels = c("1:1", "1:4"))

write.table(dfp, file = "../data/google/4a_flavor_occurence-google-data.dat", row.names = F)

njobs = length(unique(df_analysis$jobId))
dfp = df_analysis %>% group_by(scenario, jobId) %>% summarise(nflavor = n()) %>% ungroup() %>% group_by(scenario, nflavor) %>% summarise(occ = n()) %>% mutate(total = njobs, freq = occ / njobs)

dfp = dfp %>% inner_join(ref, by = "scenario")
dfp$scP = factor(dfp$scP, levels = ref$scP[nrow(ref):1])

dfp = dfp %>% filter(scP %in% c("1:1", "1:4"))

write.table(dfp, file = "../data/google/4a_flavor_number_occurence-google-data.dat", row.names = F)
