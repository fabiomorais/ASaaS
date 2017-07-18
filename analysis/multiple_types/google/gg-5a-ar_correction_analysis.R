library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(tidyr)

ref             = data.frame(cpuRef = c(32, 32, 32, 32), memRef = c(32, 64, 128, 256), cpuP = rep(1, 4), memP = c(1, 2, 4, 8)) %>% 
     group_by(memRef) %>% mutate(scenario = paste(cpuRef, memRef, sep = "_"), scP = paste(cpuP, memP, sep = ":"))

job_error       = read.table(file = "../../../multiple_types/google/data/job_without_util.dat", header = T) 

df_analysis     = read.table(file = "../../../multiple_types/google/data/scaling-general-data.dat", header = T) %>% filter(!jobid %in% job_error$jobId)
df_analysis     = df_analysis %>% group_by(scenario, jobid, type, tgt) %>% mutate(class = paste(type, tgt, sep = "_"))

df_pred         = df_analysis %>% ungroup() %>% select(-ecu_viol, -mem_viol, -p_ecu_viol, -p_mem_viol, -len, -tgt, -type, -over_cost, -saving) %>% filter(class %in% c("ar_1", "ar_acf_42_1", "ar_acf_84_1", "ar_acf_168_1"))
df_pred         = df_pred %>% rename(jobId = jobid)

df_perf         = read.table(file = "../data/google/scaling-analysis-MM-MF.dat", header = T) %>% select(-ecu_viol, -mem_viol, -p_ecu_viol, -p_mem_viol, -len, -metric_base)
df_perf         = df_perf %>% dplyr::rename(cost_perf = cost_total)

dfp             = inner_join(df_pred, df_perf, by = c("scenario", "jobId")) %>% inner_join(ref, by = "scenario") %>% filter(class %in% c("ar_1", "ar_acf_42_1", "ar_acf_84_1", "ar_acf_168_1"))
dfp             = dfp %>% group_by(scenario, jobId) %>% mutate(rel_cost = (cost_total - cost_perf) / cost_perf, diff_cost = (cost_total - cost_perf))

df_ar = dfp %>% select(scP, jobId, class, pviol, rel_cost) %>% 
     rename(Violation = pviol, Cost = rel_cost) %>% gather("metric", "value", 5:6)

df_ar$class[df_ar$class == "ar_1"] = "AR"
df_ar$class[df_ar$class == "ar_acf_168_1"] = "AR 100"
df_ar$class[df_ar$class == "ar_acf_84_1"] = "AR 50"
df_ar$class[df_ar$class == "ar_acf_42_1"] = "AR 25"

df_ar$class = factor(df_ar$class, levels = c("AR", "AR 25", "AR 50", "AR 100"))

df_ar = df_ar %>% filter(scP %in% c("1:1", "1:4"))

write.table(df_ar, file = "../data/google/5a_ar_correction_analysis-google-data-raw.dat", row.names = F)

library(Rmisc)
y = df_ar %>% dplyr::group_by(scP, class, metric) %>% dplyr::summarise(resp = Rmisc::CI(value)[2], s1 = Rmisc::CI(value)[1], s2 = Rmisc::CI(value)[3])

a = df_ar %>% filter(metric == "Violation", class == "AR")
b = df_ar %>% filter(metric == "Violation", class != "AR")

summary(a$value * 100)
summary(b$value * 100)

(y %>% filter(metric == "Cost"))$resp * 100
(y %>% filter(metric == "Violation"))$resp * 100

limits <- aes(ymax = s1, ymin = s2)
dodge <- position_dodge(width = 0.9)

write.table(y, file = "../data/google/5a_ar_correction_analysis-google-data.dat", row.names = F)