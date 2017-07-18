library(dplyr)
library(ggplot2)
library(scales)
library(stringr)

ref             = data.frame(cpuRef = c(32, 32, 32, 32), memRef = c(32, 64, 128, 256), cpuP = rep(1, 4), memP = c(1, 2, 4, 8)) %>% 
  group_by(memRef) %>% mutate(scenario = paste(cpuRef, memRef, sep = "_"), scP = paste(cpuP, memP, sep = ":"))

fact            = c(-0.4, -0.2, 0, 0.2) #c(-0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2)
job_error       = read.table(file = "../../../multiple_types/google/data/job_without_util.dat", header = T) 

df_analysis     = read.table(file = "../../../multiple_types/google/data/scaling-general-data.dat", header = T) %>% filter(!jobid %in% job_error$jobId)

df_analysis     = df_analysis %>% group_by(scenario, jobid, type, tgt) %>% mutate(class = paste(type, tgt, sep = "_"))
df_pred         = df_analysis %>% ungroup() %>% select(-ecu_viol, -mem_viol, -p_ecu_viol, -p_mem_viol, -len, -tgt, -type, -over_cost, -saving) %>% filter(class %in% c("ar_1", "ar_acf_42_1", "ar_acf_84_1", "ar_acf_168_1"))
df_pred         = df_pred %>% filter(scenario %in% c("32_32", "32_128"))

df_over         = read.table(file = "../data/google/over-provisioning-data.dat", header = T)
unique(df_over$factor)
df_over         = df_over %>% select(scenario, jobId, factor, viol, p_viol, over_cost, over_flavor) %>% filter(factor %in% fact)
df_over         = df_over %>% group_by(scenario, jobId, factor) %>% filter(over_cost == min(over_cost)) %>% sample_n(1) %>% ungroup() 
df_over         = df_over %>% mutate(class = paste("OP(", factor * 100, ")", sep = "")) %>% select(scenario, jobId, tviol_over = viol, pviol_over = p_viol, over_cost, class_over = class, factor)
df_over         = df_over %>% filter(scenario %in% c("32_32", "32_128"))

dfp             = inner_join(df_pred, df_over, by = c("scenario", "jobid" = "jobId"))
#dfp             = dfp %>% group_by(scenario, jobid) %>% mutate(rel_cost = (over_cost - cost_total) / over_cost)
dfp             = dfp %>% group_by(scenario, jobid) %>% mutate(rel_cost = (cost_total - over_cost) / over_cost)
dfp             = dfp %>% inner_join(ref, by = "scenario") 

dfp$class[dfp$class == "ar_1"] = "AR"
dfp$class[dfp$class == "ar_acf_168_1"] = "AR 100"
dfp$class[dfp$class == "ar_acf_84_1"] = "AR 50"
dfp$class[dfp$class == "ar_acf_42_1"] = "AR 25"

dfp$class_over = factor(dfp$class_over, levels = paste("OP(", fact * 100, ")", sep = ""))
dfp$class = factor(dfp$class, levels = c("AR",  "AR 25", "AR 50", "AR 100"))

# paper
# TEXT: On average, 40% cheaper than the perfect over-provisioned
# dfp %>% filter(factor == 0) %>% group_by(scenario, class) %>% dplyr::summarise(median = median(rel_cost))
a = dfp %>% filter(factor == 0)
summary(a$rel_cost)

# TEXT: On average, the solution (with correction) is able to reduce the cost in 35\%, when compared to a perfectly over-provisioned scenario
a = dfp %>% filter(factor == 0, class != "AR")
summary(a$rel_cost)

# TEXT: and at least 40\% cheaper than the over-provisioned scenario when the peak demand is predicted 20\% higher than its actual value
# dfp %>% filter(factor == 0.2) %>% group_by(scenario, class) %>% dplyr::summarise(median = median(rel_cost))
a = dfp %>% filter(factor == 0.2)
summary(a$rel_cost)

# TEXT : the average provisioning cost of the AR-based approach is 27\% lower than the over-provisioned one
a = dfp %>% filter(factor == -0.2)
summary(a$rel_cost)

# TEXT: and in at least 40\%, when compared with an over-provisioned scenario that overestimates the peak demand in 20\%.
a = dfp %>% filter(factor == 0.2, class != "AR")
summary(a$rel_cost)

dfp$label = paste(dfp$factor * 100, "%", sep = "")
dfp$label = factor(dfp$label, levels = c("-40%", "-20%", "0%", "20%"))

write.table(dfp, file = "../data/google/5c-pred_over_costs-google-data.dat", row.names = F)

fact            = c(-0.4, -0.2) #c(-0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2)
df_over         = read.table(file = "../data/google/over-provisioning-data.dat", header = T) %>% filter(factor %in% fact)

df_over         = df_over %>% group_by(scenario, jobId, factor) %>% filter(over_cost == min(over_cost)) %>% sample_n(1) %>% ungroup() 
df_over         = df_over %>% filter(scenario %in% c("32_32", "32_128"))
df1             = df_over %>% mutate(type = "Superprovisionado", class = paste("SP ", factor * 100, sep = "")) %>% select(scenario, jobId, pviol = p_viol , class, type)

df2             = df_pred %>% mutate(type = "Prático") %>% select(scenario, jobId = jobid, pviol, class, type)

dff             = rbind(df1, df2)
dff             = dff %>% inner_join(ref, by = "scenario") 

dff$class[dff$class == "ar_1"] = "AR"
dff$class[dff$class == "ar_acf_168_1"] = "AR 100"
dff$class[dff$class == "ar_acf_84_1"] = "AR 50"
dff$class[dff$class == "ar_acf_42_1"] = "AR 25"

dff$type  = factor(dff$type, levels = c("Prático", "Superprovisionado"))
dff$class = factor(dff$class, levels = c("AR", "AR 25", "AR 50", "AR 100", "SP -40", "SP -20"))

dff %>% filter(class != "AR") %>% group_by(class, scP) %>% summarise(median = median(pviol)) %>% ungroup() %>% arrange(median)
dff %>% group_by(class) %>% summarise(median = median(pviol) * 100) %>% ungroup() %>% arrange(median)

head(dff)
# paper
# TEXT: This comes at the expenses of yielding SLO violations in 1.2\%
a = dff %>% filter(class %in% c("AR 25", "AR 50", "AR 100")) 
summary(a$pviol * 100)

# TEXT: The rate of SLO violations in this scenario is equal to 4.9\% for the dynamic AR-based selection
a = dff %>% filter(class %in% c("AR", "AR 25", "AR 50", "AR 100")) 
summary(a$pviol * 100)

# and 1.7\% for the over-provisioned -20\% scenario
a = dff %>% filter(class == "SP -20") 
head(a)
summary(a$pviol * 100)

dfp %>% select(scenario, jobid)
tail(dff, n = 20)

write.table(dff, file = "../data/google/5c-pred_over_violations-google-data.dat", row.names = F)