library(dplyr)
library(ggplot2)
library(scales)
library(stringr)

flavors      = sort(c("c4.large", "m4.large", "r3.large", "c3.large", "m3.medium"))
f_str        = str_sub(flavors, 1, 2)

dgg = read.table(file = "data/google/3b_cost_violations_trade-off-google-data.dat", header = T) %>% mutate(label = paste("Google", scP)) %>% select(-scP, -scenario, -cpuRef, -memRef, -cpuP, -memP)
dhp = read.table(file = "data/hp/3b_cost_violations_trade-off-hp-data.dat", header = T) %>% mutate(label = "HP")

head(dgg)
head(dhp)

dfp = rbind(dgg, dhp)
factors = c("HP", "Google 1:1", "Google 1:4")
dfp$label = factor(dfp$label, levels = factors)

p = ggplot(dfp, aes(flavor, incrase, fill = metric_ref)) + geom_bar(stat = "identity", position = "dodge", alpha = 0.7) + facet_wrap(~ label, ncol = 3, scales = "fixed")
p = p + scale_y_continuous(labels = percent, trans = "sqrt", breaks = c(0.01, 0.1, 0.5, 1, 2, 4))
p = p + scale_fill_brewer("Métrica relacionada:", palette = "Set1")
p = p + scale_x_discrete(breaks = flavors, labels = f_str)
p = p + theme_bw(base_size = 32)
p = p + xlab("Tipo de instância") + ylab("Incremento de custo")
p = p + theme(legend.position = "top", legend.key.size = unit(1, "cm"))
p = p + theme(axis.title.y = element_text(vjust = 1.5), axis.title.x = element_text(vjust = 3.1))
p

png(filename = "img/3b_cost_violations_trade-off-all.png", width = 1200, height = 500)
print(p)
dev.off()

summary(dplot$incrase)
