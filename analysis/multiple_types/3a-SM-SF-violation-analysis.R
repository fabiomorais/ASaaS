library(dplyr)
library(ggplot2)
library(scales)
library(stringr)

flavors      = sort(c("c4.large", "m4.large", "r3.large", "c3.large", "m3.medium"))
f_str        = str_sub(flavors, 1, 2)

dgg = read.table(file = "data/google/3a_colateral_violations-google-data.dat", header = T) %>% mutate(label = paste("Google", scP)) %>% select(-scP)
dhp = read.table(file = "data/hp/3a_colateral_violations-hp-data.dat", header = T) %>% mutate(label = "HP")

dhp %>% filter(value == 0)
dhp %>% filter(flavor %in% c("r3.large", "m4.large", "m3.medium")) %>% 
     group_by(metric_base, metric) %>% summarise(mean(value))

head(dgg)
head(dhp)

dfp = rbind(dgg, dhp)
factors = c("HP", "Google 1:1", "Google 1:4")
dfp$label = factor(dfp$label, levels = factors)

p = ggplot(dfp, aes(flavor, value, fill = metric_base)) + geom_bar(stat = "identity", position = "dodge", alpha = 0.7) + facet_wrap(~ label, ncol = 3)
p = p + scale_y_continuous(labels = percent, breaks = seq(0, 1, 0.1)) + scale_fill_brewer("Métrica base:", palette = "Set1")
p = p + scale_x_discrete(breaks = flavors, labels = f_str)
p = p + theme_bw(base_size = 32) + theme(legend.position = "top")
p = p + xlab("Tipo de instância") + ylab("Violações colaterais")
p = p + theme(legend.position = "top", legend.key.size = unit(1, "cm"))
p = p + theme(axis.title.y = element_text(vjust = 1.5), axis.title.x = element_text(vjust = 3.1))
# p = p + theme(panel.border = element_blank(), panel.grid = element_blank(), axis.ticks.y = element_blank())
p

png(filename = "img/3a_colateral_violations-all.png", width = 1200, height = 450)
print(p)
dev.off()