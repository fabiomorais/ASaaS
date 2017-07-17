suppressMessages(library(ggplot2, quietly = TRUE))
suppressMessages(library(scales, quietly = TRUE))
suppressMessages(library(grid, quietly = TRUE))
suppressMessages(library(plyr, quietly = TRUE))
suppressMessages(library(dplyr, quietly = TRUE))

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

file_name_abs_cpu_ecdf   <- "data/df_trace_diff_cpu_abs_ecdf.dat"
file_name_rel_cpu_ecdf   <- "data/df_trace_diff_cpu_rel_ecdf.dat"
file_name_util_cpu_ecdf  <- "data/df_trace_util_cpu_final_ecdf.dat"
file_name_util_cpu       <- "data/df_trace_util_cpu_final_raw.dat"

file_name_abs_mem_ecdf	<- "data/df_trace_diff_mem_abs_ecdf.dat"
file_name_rel_mem_ecdf	<- "data/df_trace_diff_mem_rel_ecdf.dat"
file_name_util_mem_ecdf	<- "data/df_trace_util_mem_final_ecdf.dat"
file_name_util_mem	     <- "data/df_trace_util_mem_final_raw.dat"

df_diff_rel_cpu_ecdf	<- read.table(file = file_name_rel_cpu_ecdf, header = T) %>% mutate(METRIC = "CPU")
df_diff_abs_cpu_ecdf	<- read.table(file = file_name_abs_cpu_ecdf, header = T) %>% mutate(METRIC = "CPU")
df_util_final_cpu_ecdf	<- read.table(file = file_name_util_cpu_ecdf, header = T) %>% mutate(METRIC = "CPU")
df_util_final_cpu	     <- read.table(file = file_name_util_cpu, header = T) %>% mutate(METRIC = "CPU")

df_diff_abs_mem_ecdf 	<- read.table(file = file_name_abs_mem_ecdf, header = T) %>% mutate(METRIC = "Memória")
df_diff_rel_mem_ecdf 	<- read.table(file = file_name_rel_mem_ecdf, header = T) %>% mutate(METRIC = "Memória")
df_util_final_mem_ecdf	<- read.table(file = file_name_util_mem_ecdf, header = T) %>% mutate(METRIC = "Memória")
df_util_final_mem	     <- read.table(file = file_name_util_mem, header = T) %>% mutate(METRIC = "Memória")

df_util_final_ecdf       <- rbind(df_util_final_cpu_ecf, df_util_final_mem_ecdf)
df_util_final            <- rbind(df_util_final_cpu, df_util_final_mem)
df_diff_abs_ecdf         <- rbind(df_diff_abs_cpu_ecdf, df_diff_abs_mem_ecdf)

plott_1 <- ggplot(df_util_final_ecdf, aes(UTIL, ECDF, colour = factor(TRACE))) + geom_line()
plott_1 <- plott_1 + facet_grid(~METRIC)
plott_1 <- plott_1 + xlab("Utilização de recursos (%)")
plott_1 <- plott_1 + ylab("Porcentagem de observações")
plott_1 <- plott_1 + scale_y_continuous(labels = percent)
plott_1 <- plott_1 + theme_bw(base_size = 24)
plott_1 <- plott_1 + theme(axis.title.x = element_text(vjust = -0.2)) # Xlab
plott_1 <- plott_1 + theme(axis.title.y = element_text(angle = 90, vjust = 0.3))
plott_1 <- plott_1 + theme(legend.position = "none")
plott_1 <- plott_1 + guides(col = guide_legend(override.aes = list(size = 10)))
plott_1

filename	<- paste("img/00_01_ecdf_util_resources.png", sep = "")
png(filename = filename, width = 900, height = 450)
print(plott_1)
dev.off()

dplot = df_util_final %>% group_by(TRACE, METRIC) %>% dplyr::summarise(SDUSAGE = sd(USAGE), SDUTIL = sd(UTIL/100))
 
plott_1 <- ggplot(dplot, aes(METRIC, SDUSAGE)) + geom_boxplot()
plott_1 <- plott_1 + xlab("Métrica de provisionamento")
plott_1 <- plott_1 + ylab("Desvio padrão da demanda")
plott_1 <- plott_1 + theme_bw(base_size = 24)
plott_1 <- plott_1 + theme(axis.title.x = element_text(vjust = -0.2)) # Xlab
plott_1 <- plott_1 + theme(axis.title.y = element_text(angle = 90, vjust = 0.3))
plott_1

plott_1 <- ggplot(dplot, aes(METRIC, SDUTIL)) + geom_boxplot(width = 0.5)
plott_1 <- plott_1 + xlab("Métrica de provisionamento")
plott_1 <- plott_1 + ylab("Desvio padrão da utilização")
plott_1 <- plott_1 + scale_y_continuous(labels = percent, breaks = c(0.01, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4))
plott_1 <- plott_1 + theme_bw(base_size = 24)
plott_1 <- plott_1 + theme(axis.title.x = element_text(vjust = -0.2)) # Xlab
plott_1 <- plott_1 + theme(axis.title.y = element_text(angle = 90, vjust = 0.3))
plott_1

filename	<- paste("img/00_01_boxplot_sd_util_resources.png", sep = "")
png(filename = filename, width = 700, height = 450)
print(plott_1)
dev.off()

dplot = df_diff_abs_ecdf %>% group_by(TRACE, METRIC) %>% dplyr::summarise(SD = sd(DIFF))

plott_1 <- ggplot(dplot, aes(METRIC, SD)) + geom_boxplot()
plott_1 <- plott_1 + xlab("Utilização de recursos (%)")
plott_1 <- plott_1 + ylab("Porcentagem de observações")
plott_1 <- plott_1 + scale_y_continuous(labels = percent)
plott_1 <- plott_1 + theme_bw(base_size = 24)
plott_1 <- plott_1 + theme(axis.title.x = element_text(vjust = -0.2)) # Xlab
plott_1 <- plott_1 + theme(axis.title.y = element_text(angle = 90, vjust = 0.3))
plott_1

for (metric in c("cpu", "mem")) {
     
     df_diff_abs = if (metric == "cpu") {
          df_diff_abs_cpu_ecdf
     } else {
          df_diff_abs_mem_ecdf
     }
     
     plott_2 <- ggplot(df_diff_abs, aes(DIFF, ECDF, colour = factor(TRACE))) + geom_line()
     plott_2 <- plott_2 + xlab("Variação absoluta de utilização")
     plott_2 <- plott_2 + ylab("Porcentagem de observações")
     plott_2 <- plott_2 + scale_y_continuous(labels = percent)
     plott_2 <- plott_2 + theme_bw(base_size = 22)
     plott_2 <- plott_2 + theme(axis.title.x = element_text(vjust = -0.2)) # Xlab
     plott_2 <- plott_2 + theme(axis.title.y = element_text(angle = 90, vjust = 0.3))
     plott_2 <- plott_2 + theme(legend.position = "none")
     plott_2 <- plott_2 + guides(col = guide_legend(override.aes = list(size = 10)))
     
     alpha		     <- 0.4
     df_diff_abs_pos	<- subset(df_diff_abs, ECDF >= (1 - alpha / 2))
     df_diff_abs_neg	<- subset(df_diff_abs, ECDF <= (alpha / 2))
     
     plott_3 <- ggplot(df_diff_abs_pos, aes(DIFF, ECDF, colour = factor(TRACE))) + geom_line()
     plott_3 <- plott_3 + xlab(NULL)
     plott_3 <- plott_3 + ylab(NULL)
     plott_3 <- plott_3 + scale_y_continuous(labels = percent)
     plott_3 <- plott_3 + theme_bw(base_size = 22)
     plott_3 <- plott_3 + theme(axis.title.x = element_text(vjust = -0.2)) # Xlab
     plott_3 <- plott_3 + theme(axis.title.y = element_text(angle = 90, vjust = 0.3))
     plott_3 <- plott_3 + theme(legend.position = "none")
     
     plott_4 <- ggplot(df_diff_abs_neg, aes(DIFF, ECDF, colour = factor(TRACE))) + geom_line()
     plott_4 <- plott_4 + xlab(NULL)
     plott_4 <- plott_4 + ylab(NULL)
     plott_4 <- plott_4 + scale_y_continuous(labels = percent)
     plott_4 <- plott_4 + theme_bw(base_size = 22)
     plott_4 <- plott_4 + theme(axis.title.x = element_text(vjust = -0.2)) # Xlab
     plott_4 <- plott_4 + theme(axis.title.y = element_text(angle = 90, vjust = 0.3))
     plott_4 <- plott_4 + theme(legend.position = "none")
     
     Layout <- grid.layout(nrow = 2, ncol = 2, widths = unit(c(2, 2), c("null", "null")), heights = unit(c(1, 1), c("null", "null")))
     vplayout <- function(...) {
       grid.newpage()
       pushViewport(viewport(layout = Layout))
     }
     
     subplot <- function(x, y) viewport(layout.pos.row = x,layout.pos.col = y)
     
     filename	<- paste("img/00_01_ecdf_diff_util_resources-", metric, ".png", sep = "")
     png(filename = filename, width = 900, height = 350)
     
     vplayout()
     print(plott_2, vp = subplot(1:2, 1))
     print(plott_3, vp = subplot(1, 2))
     print(plott_4, vp = subplot(2, 2))
     dev.off()

}