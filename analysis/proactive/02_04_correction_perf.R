library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
     require(grid)
     
     # Make a list from the ... arguments and plotlist
     plots <- c(list(...), plotlist)
     
     numPlots = length(plots)
     
     # If layout is NULL, then use 'cols' to determine layout
     if (is.null(layout)) {
          # Make the panel
          # ncol: Number of columns of plots
          # nrow: Number of rows needed, calculated from # of cols
          layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                           ncol = cols, nrow = ceiling(numPlots/cols))
     }
     
     if (numPlots == 1) {
          print(plots[[1]])
          
     } else {
          # Set up the page
          grid.newpage()
          pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
          
          # Make each plot, in the correct location
          for (i in 1:numPlots) {
               # Get the i,j matrix positions of the regions that contain this subplot
               matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
               
               print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                               layout.pos.col = matchidx$col))
          }
     }
}

# Abordagem como filtro apenas
dff = read.table(file = "data/general-data-analysis.dat", header = T)
dff = dff %>% filter(FILTER == "ceiling_max") %>% 
     select(TRACE, METRIC, SCENARIO, SAFEM, CORR, VIOLP, PERF_REL, OVER_REL) %>% 
     gather("metric", "value", 6:8)

dff$metric[dff$metric == "VIOLP"] = "Percentual\n de violações"
dff$metric[dff$metric == "OVER_REL"] = "Custo realtivo\n ao superprovido"
dff$metric[dff$metric == "PERF_REL"] = "Custo realtivo\n ao perfeito"

dff  = dff %>% mutate(METRIC_STR = ifelse(METRIC == "cpu", "CPU", "Memória"))
dff  = dff %>% mutate(SCENARIO_STR = ifelse(SCENARIO == "Dynamic", "Dinâmico", as.character(SCENARIO)))

df_ref = data.frame(CORR = unique(dff$CORR), CORR_LABEL = c("0%", "1%", "25%", "50%", "100%"))

dff  = dff %>% inner_join(df_ref, by = "CORR") 

dff$metric = factor(dff$metric, levels = c("Custo realtivo\n ao superprovido", "Custo realtivo\n ao perfeito", "Percentual\n de violações"))
dff$SCENARIO_STR = factor(dff$SCENARIO_STR, levels = c("LW", "AR", "Dinâmico"))
dff$CORR_LABEL = factor(dff$CORR_LABEL, levels = c("0%", "1%", "25%", "50%", "100%"))

head(dff)
for (m in c("cpu", "mem")) {
     
     dplot = dff %>% filter(METRIC == m)
     
     p = ggplot(dplot, aes(CORR_LABEL, value, fill = paste(SAFEM * 100, "%", sep = "")))
     p = p + geom_boxplot(position = position_dodge(), width = 0.7)
     p = p + facet_grid(metric ~ SCENARIO_STR, scales = "free")
     p = p + scale_fill_brewer("Margem de segurança:", palette = "Set1")
     p = p + scale_y_continuous(labels = percent)
     p = p + theme_bw(base_size = 24)
     p = p + theme(legend.position = "top", legend.key.size = unit(1, "cm"))
     p = p + xlab("Grau de correção") + ylab(NULL)
     p = p + guides(fill = guide_legend(nrow = 1, byrow = TRUE))
     p
     
     png(filename = paste("img/02_04_correction-perf-", m, ".png", sep = ""), width = 1000, height = 650)
     print(p)
     dev.off()
     
     # Multiplot

     dff1 = filter(dplot, metric != "Percentual\n de violações")
     p = ggplot(dff1, aes(CORR_LABEL, value, fill = paste(SAFEM * 100, "%", sep = "")))
     p = p + geom_boxplot(position = position_dodge(), width = 0.7)
     p = p + facet_grid(metric ~ SCENARIO_STR, scales = "free")
     p = p + scale_fill_brewer("Margem de segurança:", palette = "Set1")
     p = p + scale_y_continuous(labels = percent)
     p = p + theme_bw(base_size = 24)
     p = p + theme(legend.position = "top", legend.key.size = unit(1, "cm"))
     p = p + xlab(NULL) + ylab(NULL)
     p = p + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), strip.text.x = element_blank())
     p = p + guides(fill = guide_legend(nrow = 1, byrow = TRUE))
     p = p + theme(axis.title.x = element_text(vjust = -0.2))
     p1 = p
     
     
     dff2 = filter(dplot, metric == "Percentual\n de violações")
     p = ggplot(dff2, aes(CORR_LABEL, value, fill = paste(SAFEM * 100, "%", sep = "")))
     p = p + geom_boxplot(position = position_dodge(), width = 0.7)
     p = p + facet_grid(metric ~ SCENARIO_STR, scales = "free")
     p = p + scale_fill_brewer("Margem de segurança:", palette = "Set1")
     p = p + scale_y_continuous(labels = percent)
     p = p + theme_bw(base_size = 24)
     p = p + coord_cartesian(ylim = c(0, 0.1))
     p = p + theme(legend.position = "none", legend.key.size = unit(1, "cm"))
     p = p + theme(strip.text.x = element_blank())
     p = p + theme(plot.margin = unit(c(0,4,5,2.3),"mm"))
     p = p + xlab("Grau de correção") + ylab(NULL)
     p = p + guides(fill = guide_legend(nrow = 1, byrow = TRUE))
     p2 = p
     
     Layout <- grid.layout(nrow = 90, ncol = 800, widths = unit(c(1, 1), c("null", "null")), heights = unit(c(1, 1), c("null", "null")))
     vplayout <- function(...) {
          grid.newpage()
          pushViewport(viewport(layout = Layout))
     }
     
     subplot <- function(x, y) viewport(layout.pos.row = x,layout.pos.col = y)
     
     png(filename = paste("img/02_04_correction-perf-", m, "-multi.png", sep = ""), width = 1000, height = 650)
     vplayout()
     print(p1, vp = subplot(1:58,1:800))
     print(p2, vp = subplot(58:90,1:800))
     dev.off()
}

summary(dff)
dff %>% filter(metric == "Percentual\n de violações") %>% filter(SAFEM == 0, CORR_LABEL %in% c("1%", "100%")) %>% group_by(CORR_LABEL) %>% 
     summarise(mean = mean(value) * 100, median = median(value)*100) %>% data.frame()

dff %>% filter(metric == "Percentual\n de violações") %>% filter(CORR_LABEL == "0%", SAFEM %in% c(0.1, 0.5)) %>% group_by(SAFEM) %>% 
     summarise(mean = mean(value) * 100, median = median(value)*100) %>% data.frame()

dff %>% filter(metric == "Custo realtivo\n ao superprovido") %>% filter(SAFEM == 0, CORR_LABEL %in% c("1%", "100%")) %>% group_by(CORR_LABEL) %>% 
     summarise(mean = mean(value) * 100, median = median(value)*100) %>% data.frame()

dff %>% filter(metric == "Custo realtivo\n ao superprovido") %>% filter(CORR_LABEL == "0%", SAFEM %in% c(0.1, 0.5)) %>% group_by(SAFEM) %>% 
     summarise(mean = mean(value) * 100, median = median(value)*100) %>% data.frame()


