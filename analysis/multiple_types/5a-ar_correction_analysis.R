library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(tidyr)

dgg = read.table(file = "data/google/5a_ar_correction_analysis-google-data.dat", header = T) %>% mutate(label = paste("Google", scP)) %>% select(-scP)
dhp = read.table(file = "data/hp/5a_ar_correction_analysis-hp-data.dat", header = T) %>% mutate(label = "HP")

dfp = rbind(dgg, dhp)
factors = c("HP", "Google 1:1", "Google 1:4")
dfp$label = factor(dfp$label, levels = factors)

limits <- aes(ymax = s1, ymin = s2)
dodge <- position_dodge(width = 0.9)

dfp$class = factor(dfp$class, levels = c("AR", "AR 25", "AR 50", "AR 100"))

dfp = dfp %>% mutate(metric_pt = ifelse(metric == "Cost", "Custo", "Violações de SLO"))

p = ggplot(dfp, aes(label, resp, fill = class)) + geom_bar(stat = "identity", position = "dodge", alpha = 0.7) + geom_errorbar(limits, position = dodge, width = 0.25, size = 1)
p = p + scale_y_continuous(labels = percent) + facet_wrap(~ metric_pt, scales = "free")
p = p + scale_fill_brewer("Abordagem de predição:", palette = "Set1")
p = p + theme_bw(base_size = 32) + theme(legend.position = "top")
p = p + xlab("Dado de referência") + ylab(NULL)
p = p + theme(legend.position = "top", legend.key.size = unit(1, "cm"))
p = p + theme(axis.title.y = element_text(vjust = 1.5), axis.title.x = element_text(vjust = 3.1))
p

png(filename = "img/5a_ar_correction_analysis-all.png", width = 1200, height = 500)
print(p)
dev.off()

dfp %>% filter(class == "AR", metric_pt == "Violações de SLO")
dfp %>% filter(class != "AR", metric_pt == "Custo") %>% mutate(V = resp * 100)

# Teste para ver se é menor que o custo do ótimo
dfa = read.table(file = "data/hp/5a_ar_correction_analysis-hp-data-raw.dat", header = T) 
dfb = read.table(file = "data/google/5a_ar_correction_analysis-google-data-raw.dat", header = T) 

da = dfa %>% filter(class == "AR", metric == "Cost")
db = dfb %>% filter(class == "AR", metric == "Cost")

wilcox.test(da$value, alternative = "less")
wilcox.test(db$value, alternative = "less")

# Media de violações com correção
da = dfa %>% filter(class != "AR", metric == "Violation")
db = dfb %>% filter(class != "AR", metric == "Violation")

summary(da)
summary(db)
summary(c(da$value, db$value))
