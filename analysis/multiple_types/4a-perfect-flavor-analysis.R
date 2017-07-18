library(dplyr)
library(ggplot2)
library(scales)
library(stringr)

flavors      = sort(c("c4.large", "m4.large", "r3.large", "c3.large", "m3.medium"))
f_str        = str_sub(flavors, 1, 2)

dgg = read.table(file = "data/google/4a_flavor_occurence-google-data.dat", header = T) %>% mutate(label = paste("Google", scP)) %>% select(-scP, -scenario, -cpuRef, -memRef, -cpuP, -memP)
dhp = read.table(file = "data/hp/4a_flavor_occurence-hp-data.dat", header = T) %>% mutate(label = "HP")

head(dgg)
head(dhp)

dfp = rbind(dgg, dhp)
factors = c("HP", "Google 1:1", "Google 1:4")
dfp$label = factor(dfp$label, levels = factors)

p = ggplot(dfp, aes(flavor, freq)) + geom_bar(stat = "identity") + facet_grid(~ label)
p = p + scale_y_continuous(labels = percent, limits = c(0, 0.4))
p = p + scale_x_discrete(breaks = flavors, labels = f_str)
p = p + theme_bw(base_size = 32) + theme(legend.position = "top")
p = p + xlab("Tipo de instância") + ylab("Ocorrência")
p = p + theme(legend.position = "top", legend.key.size = unit(1.5, "cm"))
p = p + theme(axis.title.y = element_text(vjust = 1.5), axis.title.x = element_text(vjust = 3.1))
p

png(filename = "img/4a_flavor_occurence-all.png", width = 1050, height = 450)
print(p)
dev.off()

dgg = read.table(file = "data/google/4a_flavor_number_occurence-google-data.dat", header = T) %>% mutate(label = paste("Google", scP)) %>% select(-scP, -scenario, -cpuRef, -memRef, -cpuP, -memP)
dhp = read.table(file = "data/hp/4a_flavor_number_occurence-hp-data.dat", header = T) %>% mutate(label = "HP")

dfp = rbind(dgg, dhp)
factors = c("HP", "Google 1:1", "Google 1:4")
dfp$label = factor(dfp$label, levels = factors)

p = ggplot(dfp, aes(label, freq, fill = as.factor(nflavor))) + geom_bar(stat = "identity", position = position_dodge(), width = 0.7)
p = p + scale_fill_brewer("Número de tipos:", palette = "Set1")
p = p + scale_y_continuous(labels = percent, limit = c(0, 0.5))
p = p + theme_bw(base_size = 32) + theme(legend.position = "top")
p = p + xlab("Dado de referência") + ylab("Ocorrência")
p = p + theme(legend.position = "top", legend.key.size = unit(1.5, "cm"))
p = p + theme(axis.title.y = element_text(vjust = 1.5), axis.title.x = element_text(vjust = 3.1))
p

png(filename = "img/4a_flavor_number_occurence-all.png", width = 1200, height = 450)
print(p)
dev.off()

dfp %>% group_by(label) %>% filter(nflavor == 1)