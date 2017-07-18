library(dplyr)
library(ggplot2)
library(scales)

dgg = read.table(file = "data/google/5c-pred_over_costs-google-data.dat", header = T) %>% mutate(label_data = paste("Google", scP)) %>% select(-scP, -scenario, -cpuRef, -memRef, -cpuP, -memP)
dhp = read.table(file = "data/hp/5c-pred_over_costs-hp-data.dat", header = T) %>% mutate(label_data = "HP")

dfp = rbind(dgg, dhp)
factors = c("HP", "Google 1:1", "Google 1:4")
dfp$label_data = factor(dfp$label_data, levels = factors)
dfp$label = factor(dfp$label, levels = paste(seq(-0.4, 0.2, 0.2) * 100, "%", sep = ""))
dfp$class = factor(dfp$class, levels = c("AR", "AR 25", "AR 50", "AR 100", "SP -40", "SP -20"))

# paper
# TEXT: On average, 40% cheaper than the perfect over-provisioned
# dfp %>% filter(factor == 0) %>% group_by(scenario, class) %>% dplyr::summarise(median = median(rel_cost))
a = dfp %>% filter(factor == 0)
summary(a$rel_cost)

# TEXT: and at least 40\% cheaper than the over-provisioned scenario when the peak demand is predicted 20\% higher than its actual value
# dfp %>% filter(factor == 0.2) %>% group_by(scenario, class) %>% dplyr::summarise(median = median(rel_cost))
a = dfp %>% filter(factor == 0.2)
summary(a$rel_cost)

# TEXT : the average provisioning cost of the AR-based approach is 27\% lower than the over-provisioned one
a = dfp %>% filter(factor == -0.2) #, label_data != "HP")
summary(a$rel_cost)

a = dfp %>% filter(factor == -0.4, label_data != "HP")
summary(a$rel_cost)

a = dfp %>% filter(factor == -0.4, label_data == "HP")
summary(a$rel_cost)

p = ggplot(dfp, aes(label, rel_cost, fill = class)) + geom_boxplot(alpha = 0.7) + facet_grid(label_data ~ .)
p = p + scale_y_continuous(labels = percent)
p = p + scale_fill_brewer("Abordagem de predição:", palette = "Set1")
p = p + theme_bw(base_size = 30) + theme(legend.position = "top")
p = p + xlab("Erro de estimativa") + ylab("Custo")
p = p + theme(legend.position = "top", legend.key.size = unit(1, "cm"))
p = p + theme(axis.title.y = element_text(vjust = 1.5), axis.title.x = element_text(vjust = 3.1))
p

png(filename = "img/5c-pred_over_costs-all.png", width = 1200, height = 600)
print(p)
dev.off()

dgg = read.table(file = "data/google/5c-pred_over_violations-google-data.dat", header = T) %>% mutate(label_data = paste("Google", scP)) %>% select(-scP, -scenario, -cpuRef, -memRef, -cpuP, -memP)
dhp = read.table(file = "data/hp/5c-pred_over_violations-hp-data.dat", header = T) %>% mutate(label_data = "HP")

dfp = rbind(dgg, dhp)
factors = c("HP", "Google 1:1", "Google 1:4")
dfp$label_data = factor(dfp$label_data, levels = factors)
dfp$class = factor(dfp$class, levels = c("AR", "AR 25", "AR 50", "AR 100", "SP -40", "SP -20"))

# TEXT: The rate of SLO violations in this scenario is equal to 4.9\% for the dynamic AR-based selection
a = dfp %>% filter(class %in% c("AR", "AR 25", "AR 50", "AR 100")) #%>% filter(label_data != "HP") 
summary(a$pviol * 100)

# and 1.7\% for the over-provisioned -20\% scenario
a = dfp %>% filter(class == "SP -20") #%>% filter(label_data != "HP") 
summary(a$pviol * 100)

# valores do AR com correção e over -40 dadps da google
a = dfp %>% filter(!class %in% c("SP -20", "SP -40", "AR")) %>% filter(label_data != "HP") 
unique(a$class)
summary(a$pviol * 100)

a = dfp %>% filter(class == "SP -40") %>% filter(label_data != "HP") 
unique(a$class)
unique(a$label_data)
summary(a$pviol * 100)

# valores do AR e over -40 dadps da HP
a = dfp %>% filter(!class %in% c("SP -20", "SP -40")) %>% filter(label_data == "HP") 
unique(a$class)
summary(a$pviol * 100)

a = dfp %>% filter(class == "SP -40") %>% filter(label_data == "HP") 
unique(a$class)
unique(a$label_data)
summary(a$pviol * 100)


dfp = dfp %>% mutate(type_n = ifelse(type == "Prático", "Prático", "Provisionamento estático"))
p = ggplot(dfp, aes(class, pviol, fill = type_n)) + geom_boxplot(width = 0.5, alpha = 0.7) + facet_grid(label_data ~ .)
p = p + scale_y_continuous(labels = percent, trans = "log10")
p = p + scale_fill_brewer("", palette = "Set1")
p = p + theme_bw(base_size = 32)
p = p + xlab("") + ylab("Violação")
p = p + theme(legend.position = "top", legend.key.size = unit(1, "cm"))
p = p + theme(axis.title.y = element_text(vjust = 1.5), axis.title.x = element_text(vjust = 3.1))
p

png(filename = "img/5c-pred_over_violations-all.png", width = 1200, height = 600)
print(p)
dev.off()