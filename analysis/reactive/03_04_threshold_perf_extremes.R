library(dplyr)
library(ggplot2)
library(scales)


df_base_cpu = read.table(file = "data/trace_conf_sweep-cpu.dat", header = T)
df_base_cpu = df_base_cpu %>% select(TRACE, PROV = METRIC, GROUP, ADD, RM, DIST, VM_ADD, VM_RM, VIOLP, COST, PERF_REL, OVER_REL, -VIOL_LEN)
df_base_cpu$PROV = "CPU"

head(df_base_cpu)
df_base_mem = read.table(file = "data/trace_conf_sweep-mem.dat", header = T)
df_base_mem = df_base_mem %>% select(TRACE, PROV = METRIC, GROUP, ADD, RM, DIST, VM_ADD, VM_RM, VIOLP, COST, PERF_REL, OVER_REL, -VIOL_LEN)
df_base_mem$PROV = "MEM"

df_result = rbind(df_base_cpu, df_base_mem)


#refs = c(1/100, 0.5/100, 0.1/100, 0.00/100)
refs = c(1/100, 0.1/100, 0.00/100)

dfall_conf = c()
dfall_goal = c()
dfall_sel  = c()
dfall_sel_s  = c()
dfall_conf_cost = c()
dfall_conf_cost_s = c()

#df_base_sel = df_result %>% filter(DIST != 0.05) %>% group_by(TRACE, PROV, ADD)
df_base_sel = df_result %>% group_by(TRACE, PROV, ADD) %>% filter(DIST %in% c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8))

head(df_base_sel)
for (ref in refs) {
     
     #------------------
     df_sel = df_base_sel %>% group_by(PROV, TRACE, ADD) %>% filter(VIOLP <= ref, OVER_REL < 0) %>%
          mutate(REF = paste(ref * 100, "%", sep = ""))
     
     dfall_sel = rbind(dfall_sel, df_sel)
     
     #------------------
     df_sel_s = df_base_sel %>% group_by(PROV, TRACE, ADD) %>% filter(VIOLP <= ref) %>%
          mutate(REF = paste(ref * 100, "%", sep = ""))
     
     dfall_sel_s = rbind(dfall_sel_s, df_sel_s)
     
     #------------------
     dc = df_sel %>% ungroup() %>% group_by(PROV, ADD, RM) %>% summarise(FREQ = n()/30) %>% 
          mutate(ADD_LABEL = paste(ADD * 100, "%", sep = ""))
     
     dc$REF = paste(ref * 100, "%", sep = "")
     
     dfall_goal = rbind(dfall_goal, dc)

     #------------------
     dx = df_sel %>% ungroup() %>% group_by(PROV, TRACE, REF) %>% 
          filter(COST == min(COST))
      
     dx$REF = paste(ref * 100, "%", sep = "")
     
     dfall_conf_cost = rbind(dfall_conf_cost, dx)
     
     #------------------
     dx_s = df_sel_s %>% ungroup() %>% group_by(PROV, TRACE, REF) %>% 
          filter(COST == min(COST))
     
     dx_s$REF = paste(ref * 100, "%", sep = "")
     
     dfall_conf_cost_s = rbind(dfall_conf_cost_s, dx_s)
     
     #------------------
     de = dx %>% group_by(PROV, ADD, RM) %>% summarise(FREQ = n()/30) %>% 
          mutate(ADD_LABEL = paste(ADD * 100, "%", sep = ""))
     
     de$REF = paste(ref * 100, "%", sep = "")
     
     dfall_conf = rbind(dfall_conf, de)
}

#dfall_goal = dfall_goal %>% mutate(DIST = ADD - RM) %>% filter(DIST %in% c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8))
 
dfall_goal = dfall_goal %>% mutate(DIST = ADD - RM)
 
dfall_goal$DIST = paste(dfall_goal$DIST * 100, "%", sep = "")
dfall_goal$DIST = factor(dfall_goal$DIST, levels = c("10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%"))

dfall_goal$RMSTR = paste(dfall_goal$RM * 100, "%", sep = "")
dfall_goal$RMSTR = factor(dfall_goal$RMSTR, levels = c("10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%"))

for (metric in c("CPU", "MEM")) {
     
     df_tmp = dfall_goal %>% filter(PROV == metric)

     p = ggplot(df_tmp , aes(ADD_LABEL, FREQ, fill = RMSTR)) 
     p = p + geom_bar(stat = "identity", position = position_dodge()) + facet_grid(REF ~ .)
     p = p + scale_fill_brewer("Limiares de remoção:", palette = "Set1")
     p = p + scale_y_continuous(labels = percent) #, breaks = seq(0,1, 0.05))
     p = p + theme_bw(base_size = 24)
     p = p + theme(legend.position = "top", legend.key.size = unit(1, "cm"))
     p = p + xlab("Limiares de adição ") + ylab("Percentual de aplicações")
     p = p + guides(fill = guide_legend(nrow = 1, byrow = TRUE))
     p
     
     png(filename = paste("img/03_03_threshold-pratical-goal-perfect-", tolower(metric), ".png", sep = ""), width = 1000, height = 550)
     print(p)
     dev.off()

}

dfall_goal %>% ungroup() %>% group_by(PROV) %>% arrange(desc(FREQ)) %>% data.frame()
dfall_goal %>% filter(REF == "0%") %>% ungroup() %>% group_by(PROV) %>% arrange(desc(FREQ)) %>% mutate(FREQ * 100) %>% data.frame()
dfall_goal %>% filter(REF == "1%") %>% ungroup() %>% group_by(PROV) %>% arrange(desc(FREQ)) %>% mutate(FREQ * 100) %>% data.frame()
dfall_goal %>% filter(REF == "1%") %>% ungroup() %>% group_by(PROV) %>% summarise(mean(FREQ*100)) %>% data.frame()

dfall_goal %>% filter(REF == "1%") %>% ungroup() %>% group_by(PROV) %>% filter(ADD >= 0.8) %>% summarise(mean(FREQ*100)) %>% data.frame()
dfall_goal %>% filter(REF == "1%") %>% ungroup() %>% group_by(PROV) %>% filter(ADD < 0.8) %>% summarise(mean(FREQ*100)) %>% data.frame()

dfall_goal %>% filter(REF == "0.1%") %>% ungroup() %>% group_by(PROV) %>% summarise(mean(FREQ*100)) %>% data.frame()
dfall_goal %>% filter(REF == "0.1%") %>% ungroup() %>% group_by(PROV, ADD) %>% filter(FREQ == max(FREQ)) %>% 
     ungroup() %>% group_by(PROV) %>% summarise(mean(FREQ*100)) %>% data.frame()

dfall_goal %>% filter(REF == "0%") %>% ungroup() %>% group_by(PROV) %>% summarise(mean(FREQ*100)) %>% data.frame()
dfall_goal %>% filter(REF == "0%") %>% ungroup() %>% group_by(PROV) %>% summarise(max(FREQ*100)) %>% data.frame()
dfall_goal %>% filter(REF == "0%") %>% ungroup() %>% group_by(PROV) %>% arrange(desc(FREQ)) %>% mutate(FF = FREQ * 100) %>% 
     summarise(first(FF)) %>% data.frame()

for (metric in c("CPU", "MEM")) {
     
     df_tmp = dfall_conf %>% filter(PROV == metric)
     
     # selecao das configuracoes que atingem os objetivos com o menor custo
     p = ggplot(df_tmp %>% filter(ADD != 0.2), aes(ADD_LABEL, FREQ, fill = as.factor(RM))) 
     p = p + geom_bar(stat = "identity", position = position_dodge()) + facet_grid(REF ~ .)
     p = p + scale_fill_hue("Distância entre limiares:")
     p = p + scale_y_continuous(labels = percent, breaks = seq(0,1, 0.05))
     #p = p + scale_x_discrete(labels = percent, breaks = seq(0.1, 0.9, 0.1))
     p = p + theme_bw(base_size = 24)
     p = p + theme(legend.position = "top", legend.key.size = unit(1.5, "cm"))
     p = p + xlab("Limiares de adição ") + ylab("Percentual de aplicações")
     p = p + guides(fill = guide_legend(nrow = 1, byrow = TRUE))
     p
     
     png(filename = paste("img/03_03_threshold-pratical-goal-best-perfect-", tolower(metric), ".png", sep = ""), width = 1500, height = 750)
     print(p)
     dev.off()

}

head(dfall_conf_cost)

dfx = dfall_conf_cost %>% data.frame() %>% select(PROV, REF, OVER_REL, PERF_REL) %>% 
     mutate(OVER_REL = abs(OVER_REL)) %>% tidyr::gather("METRIC", "VALUE", 3:4) 

dfx$METRIC[dfx$METRIC == "OVER_REL"] = "Economia de custo\nrealtiva ao superprovido"
dfx$METRIC[dfx$METRIC == "PERF_REL"] = "Custo realtivo\n ao perfeito"
     
dfx$METRIC = factor(dfx$METRIC, levels = c("Economia de custo\nrealtiva ao superprovido", "Custo realtivo\n ao perfeito", "Percentual\n de violações"))
 
dfx = mutate(dfx, PROV_STR = ifelse(PROV == "CPU", "CPU", "Memória"))

p = ggplot(dfx, aes(REF, VALUE, fill = PROV_STR)) + geom_boxplot(width = 0.5)
p = p + scale_y_continuous(labels = percent)
p = p + facet_grid(METRIC ~ ., scales = "free")
p = p + scale_fill_hue("Métrica de provisionamento:")
p = p + theme_bw(base_size = 24)
p = p + theme(legend.position = "top", legend.key.size = unit(1.5, "cm"))
p = p + xlab("Limite de violações de SLO") + ylab(NULL)
p

png(filename = "img/03_03_threshold-pratical-goal-best-cost-perfect.png", width = 900, height = 500)
print(p)
dev.off()

# Seleção sem restrição de custo
dfx = dfall_conf_cost_s %>% data.frame() %>% select(PROV, REF, OVER_REL, PERF_REL) %>% 
     tidyr::gather("METRIC", "VALUE", 3:4) 

dfx$METRIC[dfx$METRIC == "OVER_REL"] = "Custo realtivo\n ao superprovido"
dfx$METRIC[dfx$METRIC == "PERF_REL"] = "Custo realtivo\n ao perfeito"

dfx$METRIC = factor(dfx$METRIC, levels = c("Custo realtivo\n ao superprovido", "Custo realtivo\n ao perfeito", "Percentual\n de violações"))

dfx = mutate(dfx, PROV_STR = ifelse(PROV == "CPU", "CPU", "Memória"))

p = ggplot(dfx, aes(REF, VALUE, fill = PROV_STR)) + geom_boxplot(width = 0.5)
p = p + scale_y_continuous(labels = percent)
p = p + facet_grid(METRIC ~ ., scales = "free")
p = p + scale_fill_hue("Métrica de provisionamento:")
p = p + theme_bw(base_size = 24)
p = p + theme(legend.position = "top", legend.key.size = unit(1.5, "cm"))
p = p + xlab("Limite de violações de SLO") + ylab(NULL)
p

png(filename = "img/03_03_threshold-pratical-goal-best-cost-perfect-v2.png", width = 900, height = 500)
print(p)
dev.off()

head(dfx)

#-------------------------------------
dfx %>% group_by(PROV, REF, METRIC) %>% summarise(median(VALUE) * 100, mean(VALUE) * 100)
dfx %>% group_by(REF, METRIC) %>% summarise(median(VALUE) * 100, mean(VALUE) * 100)
dfx %>% group_by(PROV, REF, METRIC) %>% filter(REF ==  "0%") %>% summarise(median(VALUE) * 100, mean(VALUE) * 100)

for (metric in c("CPU", "MEM")) {
          
     df_tmp = dfx %>% filter(PROV == metric)
     
     p = ggplot(df_tmp, aes(REF, VALUE)) + geom_boxplot(width = 0.5)
     p = p + scale_y_continuous(labels = percent)
     p = p + facet_grid(METRIC ~ ., scales = "free")
     p = p + theme_bw(base_size = 24)
     p = p + xlab("Limite de violações de SLO") + ylab(NULL)
     p
     
     png(filename = paste("img/03_03_threshold-pratical-goal-best-cost-perfect-", tolower(metric), ".png", sep = ""), width = 900, height = 500)
     print(p)
     dev.off()
     
}

# Distância entre o custo do perfeito e o custo do superprovido
df_base_cpu = read.table(file = "data/trace_conf_sweep-cpu.dat", header = T)
df_base_cpu = df_base_cpu %>% select(TRACE, PROV = METRIC, PERF_COST, OVER_COST) %>% distinct()
df_base_cpu$PROV = "CPU"

df_base_mem = read.table(file = "data/trace_conf_sweep-mem.dat", header = T)
df_base_mem = df_base_mem %>% select(TRACE, PROV = METRIC, PERF_COST, OVER_COST) %>% distinct()
df_base_mem$PROV = "MEM"

df_base = rbind(df_base_cpu, df_base_mem) %>% mutate(COST_REL = (OVER_COST - PERF_COST) / PERF_COST)

ggplot(df_base, aes(PROV, COST_REL * 100)) + geom_boxplot()


# eficiencia
dfall_goal %>% filter(REF == "0%") %>% ungroup() %>% group_by(PROV) %>% arrange(desc(FREQ)) %>% mutate(FREQ * 100) %>% data.frame()

# todos os cenarios em que foi obtido o objetivo
df_predo = dfall_sel %>% select(REF, TRACE, PROV, ADD, RM) %>% group_by(REF, PROV) %>% mutate(TOTAL_APP = length(unique(TRACE))) %>%
     group_by(REF, PROV, ADD, RM) %>% summarise(CONF_APP = n(), TOTAL_APP = first(TOTAL_APP)) %>% 
     mutate(FREQ = CONF_APP / TOTAL_APP) %>% data.frame()

df_predo %>% group_by(REF, PROV) %>% filter(FREQ == max(FREQ)) %>% summarise(FREQ = first(FREQ))
df_predo %>% group_by(REF, PROV) %>% summarise(NCONF = n(), TOTAL_APP = first(TOTAL_APP)) 

df_predo_c = dfall_conf_cost %>% ungroup() %>% select(REF, TRACE, PROV, ADD, RM) %>% group_by(REF, PROV) %>% mutate(TOTAL_APP = length(unique(TRACE))) %>%
     group_by(REF, PROV, ADD, RM) %>% summarise(CONF_APP = n(), TOTAL_APP = first(TOTAL_APP)) %>% 
     mutate(FREQ = CONF_APP / TOTAL_APP) %>% data.frame()

df_predo_c
# a configuração ótima que mais resolve, resolve para 46% das aplicações em que o objetivo foi atingido
df_predo_c %>% group_by(REF, PROV) %>% filter(FREQ == max(FREQ)) %>% summarise(FREQ = first(FREQ))
df_predo_c %>% group_by(REF, PROV) %>% filter(FREQ == max(FREQ)) %>% summarise(FREQ = first(FREQ)) %>% group_by(REF) %>% summarise(mean(FREQ))


df_predo_c %>% group_by(REF, PROV) %>% summarise(NCONF = n(), TOTAL_APP = first(TOTAL_APP)) %>% ungroup() %>% mutate(FF = (TOTAL_APP - NCONF)/ TOTAL_APP)
df_predo_c %>% group_by(REF, PROV) %>% summarise(NCONF = n(), TOTAL_APP = first(TOTAL_APP)) %>% ungroup() %>% mutate(FF = NCONF / TOTAL_APP)
