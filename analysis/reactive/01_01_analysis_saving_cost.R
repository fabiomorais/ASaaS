library(ggplot2)
library(scales)
library(dplyr)

dff = read.table(file = "data/01_data_saving.dat", header = T)
dff = mutate(dff, METRIC_STR = ifelse(METRIC == "cpu", "CPU", "Memória"))

p = ggplot(dff, aes(METRIC_STR, SAVING)) + geom_boxplot(width = 0.5)
p = p + scale_y_continuous(labels = percent, breaks = c(0.1, 0.3, 0.5, 0.7, 0.9))
p = p + xlab("Métrica de provisionamento") + ylab("Economia de custo")
p = p + theme_bw(base_size = 24)
p

png(filename = "img/01_01_saving_cost.png", width = 750, height = 500)
print(p)
dev.off()