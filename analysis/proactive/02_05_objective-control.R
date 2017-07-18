library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

# Abordagem como filtro apenas
dff = read.table(file = "data/general-data-analysis.dat", header = T)
dff = dff %>% filter(FILTER == "ceiling_max") %>% 
     select(TRACE, METRIC, SCENARIO, SAFEM, CORR, COST, VIOLP, PERF_REL, OVER_REL)

head(dff)
dff  = dff %>% mutate(METRIC_STR = ifelse(METRIC == "cpu", "CPU", "Memória"))
dff  = dff %>% mutate(SCENARIO_STR = ifelse(SCENARIO == "Dynamic", "Dinâmico", as.character(SCENARIO)))

df_ref    = data.frame(CORR = unique(dff$CORR), CORR_LABEL = c("0%", "1%", "25%", "50%", "100%"))
dff       = dff %>% inner_join(df_ref, by = "CORR") 

dff$SCENARIO_STR = factor(dff$SCENARIO_STR, levels = c("LW", "AR", "Dinâmico"))
dff$CORR_LABEL = factor(dff$CORR_LABEL, levels = c("0%", "1%", "25%", "50%", "100%"))

refs = c(1/100, 0.1/100, 0.00/100)

dfall_goal = c()
dfall_sel  = c()
dfall_sel_s  = c()
dfall_conf_cost = c()
dfall_conf_cost_s = c()

for (ref in refs) {
     
     #------------------
     df_sel = dff %>% group_by(SCENARIO_STR, METRIC, TRACE, CORR) %>% filter(VIOLP <= ref, OVER_REL < 0) %>%
          mutate(REF = paste(ref * 100, "%", sep = ""))
     
     dfall_sel = rbind(dfall_sel, df_sel)
     
     #------------------
     dc = df_sel %>% ungroup() %>% group_by(SCENARIO_STR, METRIC, CORR, SAFEM) %>% summarise(FREQ = n()/30) %>% 
          mutate(SAFEM_LABEL = paste(SAFEM * 100, "%", sep = ""))
     
     dc$REF = paste(ref * 100, "%", sep = "")
     
     dfall_goal = rbind(dfall_goal, dc)
     
     #------------------
     dx = df_sel %>% ungroup() %>% group_by(SCENARIO_STR, METRIC, TRACE, REF) %>% 
          filter(COST == min(COST))
     
     dx$REF = paste(ref * 100, "%", sep = "")
     
     dfall_conf_cost = rbind(dfall_conf_cost, dx)

     #------------------
     df_sel_s = dff %>% group_by(SCENARIO_STR, METRIC, TRACE, CORR) %>% filter(VIOLP <= ref) %>%
          mutate(REF = paste(ref * 100, "%", sep = ""))
     
     dfall_sel_s = rbind(dfall_sel_s, df_sel_s)
     
     #------------------
     dx_s = df_sel_s %>% ungroup() %>% group_by(SCENARIO_STR, METRIC, TRACE, REF) %>% 
          filter(COST == min(COST))
     
     dx_s$REF = paste(ref * 100, "%", sep = "")
     
     dfall_conf_cost_s = rbind(dfall_conf_cost_s, dx_s)
}

df_ref                   = data.frame(CORR = unique(dff$CORR), CORR_LABEL = c("0%", "1%", "25%", "50%", "100%"))
dfall_goal               = dfall_goal %>% inner_join(df_ref, by = "CORR") 
dfall_goal$CORR_LABEL    = factor(dfall_goal$CORR_LABEL, levels = c("0%", "1%", "25%", "50%", "100%"))

for (m in c("cpu", "mem")) {
     
     df_tmp = dfall_goal %>% filter(METRIC == m)

     p = ggplot(df_tmp , aes(CORR_LABEL, FREQ, fill = SAFEM_LABEL)) 
     p = p + geom_bar(stat = "identity", position = position_dodge()) + facet_grid(REF ~ SCENARIO_STR)
     p = p + scale_fill_brewer("Margem de segurança:", palette = "Set1")
     p = p + scale_y_continuous(labels = percent) #, breaks = seq(0,1, 0.05))
     p = p + theme_bw(base_size = 24)
     p = p + theme(legend.position = "top", legend.key.size = unit(1, "cm"))
     p = p + xlab("Grau de correção") + ylab("Percentual de aplicações")
     p = p + guides(fill = guide_legend(nrow = 1, byrow = TRUE))
     p
     
     png(filename = paste("img/02_05_proactive-pratical-goal-perfect-", m, ".png", sep = ""), width = 1000, height = 550)
     print(p)
     dev.off()
     
}

dfall_goal %>% ungroup() %>% group_by(METRIC) %>% arrange(desc(FREQ)) %>% data.frame()
dfall_goal %>% filter(REF == "1%") %>% ungroup() %>% group_by(METRIC) %>% summarise(mean(FREQ*100)) %>% data.frame()
dfall_goal %>% filter(REF == "0.1%") %>% ungroup() %>% group_by(METRIC) %>% summarise(mean(FREQ*100)) %>% data.frame()

dfall_goal %>% filter(REF == "0%") %>% ungroup() %>% group_by(METRIC) %>% summarise(mean(FREQ*100)) %>% data.frame()
dfall_goal %>% filter(REF != "0%") %>% ungroup() %>% group_by(METRIC) %>% summarise(mean(FREQ*100), min(FREQ*100), max(FREQ*100)) %>% data.frame()

dfall_goal %>% filter(REF == "1%") %>% ungroup() %>% group_by(METRIC) %>% summarise(max(FREQ*100)) %>% data.frame()
dfall_goal %>% filter(REF == "0.1%") %>% ungroup() %>% group_by(METRIC) %>% summarise(max(FREQ*100)) %>% data.frame()
dfall_goal %>% filter(REF == "0%") %>% ungroup() %>% group_by(METRIC) %>% summarise(max(FREQ*100)) %>% data.frame()

dfall_goal %>% filter(REF == "1%") %>% ungroup() %>% group_by(METRIC) %>% summarise(mean(FREQ*100)) %>% data.frame()

# Seleção sem restrição de custo
dfx = dfall_conf_cost_s %>% data.frame() %>% select(SCENARIO, METRIC, REF, OVER_REL, PERF_REL) %>%
     tidyr::gather("metric", "value", 4:5)
 
dfx$metric[dfx$metric == "OVER_REL"] = "Custo realtivo\n ao superprovido"
dfx$metric[dfx$metric == "PERF_REL"] = "Custo realtivo\n ao perfeito"

dfx$metric = factor(dfx$metric, levels = c("Custo realtivo\n ao superprovido", "Custo realtivo\n ao perfeito", "Percentual\n de violações"))

dfx  = dfx %>% mutate(METRIC_STR = ifelse(METRIC == "cpu", "CPU", "Memória"))
dfx  = dfx %>% mutate(SCENARIO_STR = ifelse(SCENARIO == "Dynamic", "Dinâmico", as.character(SCENARIO)))

dfx$metric = factor(dfx$metric, levels = c("Custo realtivo\n ao superprovido", "Custo realtivo\n ao perfeito", "Percentual\n de violações"))
dfx$SCENARIO_STR = factor(dfx$SCENARIO_STR, levels = c("LW", "AR", "Dinâmico"))


dfx %>% filter(REF == "1%", metric == "Custo realtivo\n ao perfeito") %>% ungroup() %>% group_by(METRIC) %>% summarise(mean(value*100)) %>% data.frame()
dfx %>% filter(REF == "0.1%", metric == "Custo realtivo\n ao perfeito") %>% ungroup() %>% group_by(METRIC) %>% summarise(mean(value*100)) %>% data.frame()
dfx %>% filter(REF == "0%", metric == "Custo realtivo\n ao perfeito") %>% ungroup() %>% group_by(METRIC) %>% summarise(mean(value*100)) %>% data.frame()

p = ggplot(dfx, aes(REF, value, fill = METRIC_STR)) + geom_boxplot(width = 0.5)
p = p + scale_y_continuous(labels = percent)
p = p + facet_grid(metric ~ SCENARIO_STR, scales = "free")
p = p + scale_fill_hue("Métrica de provisionamento:")
p = p + theme_bw(base_size = 24)
p = p + theme(legend.position = "top", legend.key.size = unit(1.5, "cm"))
p = p + xlab("Limite de violações de SLO") + ylab(NULL)
p
 
png(filename = "img/02_05_proactive-pratical-goal-best-cost-perfect-v2.png", width = 1000, height = 500)
print(p)
dev.off()

head(dfx)
dfx %>% filter(REF %in% c("0.1%", "1%"))

# eficiencia
dfall_goal %>% filter(REF == "0%") %>% ungroup() %>% group_by(METRIC) %>% arrange(desc(FREQ)) %>% mutate(FREQ * 100) %>% data.frame()

dfall_goal %>% filter(REF == "1%") %>% ungroup() %>% group_by(METRIC) %>% arrange(METRIC, desc(FREQ)) %>% mutate(FREQ * 100) %>% top_n(1) %>% data.frame()
dfall_goal %>% filter(REF == "0.1%") %>% ungroup() %>% group_by(METRIC) %>% arrange(METRIC, desc(FREQ)) %>% mutate(FREQ * 100) %>% top_n(1) %>% data.frame()
dfall_goal %>% filter(REF == "0%") %>% ungroup() %>% group_by(METRIC) %>% arrange(METRIC, desc(FREQ)) %>% mutate(FREQ * 100) %>% top_n(1) %>% data.frame()


# todos os cenarios em que foi obtido o objetivo
head(dfall_sel)
df_predo = dfall_sel %>% ungroup() %>% select(SCENARIO, REF, TRACE, METRIC, CORR_LABEL, SAFEM) %>% group_by(SCENARIO, REF, METRIC) %>% mutate(TOTAL_APP = length(unique(TRACE))) %>%
     group_by(SCENARIO, REF, METRIC, CORR_LABEL, SAFEM) %>% summarise(CONF_APP = n(), TOTAL_APP = first(TOTAL_APP)) %>% 
     mutate(FREQ = CONF_APP / TOTAL_APP) %>% data.frame()

df_predo %>% group_by(REF, METRIC) %>% filter(FREQ == max(FREQ)) %>% summarise(FREQ = first(FREQ))
df_predo %>% group_by(REF, METRIC) %>% summarise(NCONF = n(), TOTAL_APP = first(TOTAL_APP)) 


df_predo_c = dfall_conf_cost %>% ungroup() %>% select(SCENARIO, REF, TRACE, METRIC, CORR_LABEL, SAFEM) %>% group_by(SCENARIO, REF, METRIC) %>% mutate(TOTAL_APP = length(unique(TRACE))) %>%
     group_by(SCENARIO, REF, METRIC, CORR_LABEL, SAFEM) %>% summarise(CONF_APP = n(), TOTAL_APP = first(TOTAL_APP)) %>%
     mutate(FREQ = CONF_APP / TOTAL_APP) %>% data.frame()
 
# # a configuração ótima que mais resolve, resolve para 86\% das aplicações em que o objetivo foi atingido
df_predo_c %>% group_by(REF, METRIC) %>% filter(FREQ == max(FREQ)) %>% summarise(FREQ = first(FREQ))

df_predo_c %>% group_by(REF, METRIC) %>% filter(FREQ == max(FREQ)) %>% summarise(FREQ = first(FREQ)) %>% group_by(REF) %>% summarise(mean(FREQ))
