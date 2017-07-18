library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(stringr)

flavors      = sort(c("c4.large", "m4.large", "r3.large", "c3.large", "m3.medium"))
f_str        = str_sub(flavors, 1, 2)

dgg = read.table(file = "data/google/3c_cost_reduction_mutiflavors-google-data.dat", header = T) %>% mutate(label = paste("Google", scP)) %>% select(-scP, -scenario, -cpuRef, -memRef, -cpuP, -memP)
dhp = read.table(file = "data/hp/3c_cost_reduction_mutiflavors-hp-data.dat", header = T) %>% mutate(label = "HP")

head(dgg)
head(dhp)

dfp = rbind(dgg, dhp)
factors = c("HP", "Google 1:1", "Google 1:4")
dfp$label = factor(dfp$label, levels = factors)

p = ggplot(dfp, aes(flavor, saving)) + geom_bar(stat = "identity", position = "dodge") 
p = p + facet_wrap( ~label, ncol = 3)
p = p + scale_y_continuous(labels = percent) #+ scale_fill_manual("Métrica base:", breaks = c("ECU e Memória"), values = c("#984ea3")) #scale_fill_brewer("", palette = "Set1")
p = p + scale_x_discrete(breaks = flavors, labels = f_str)
p = p + theme_bw(base_size = 32) + theme(legend.position = "top", legend.key.size = unit(1, "cm"))
p = p + theme(axis.title.y = element_text(vjust = 1.5), axis.title.x = element_text(vjust = 3.1))
p = p + xlab("Tipo de instância") + ylab("Economia de custo")
p

png(filename = "img/3c_cost_reduction_mutiflavors-all.png", width = 1200, height = 500)
print(p)
dev.off()

# paper
dp2 %>% arrange(saving)

# TEXT: Our results show that suitable selection of the most cost-effective instance type yields a potential cost saving of as much as 50\% when compared to the case where the auto-scaling mechanism is oblivious to the instance type selection.
summary(dp2$saving)
summary(dfp$saving)
