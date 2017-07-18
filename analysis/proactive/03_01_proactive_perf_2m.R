library(dplyr)
library(ggplot2)
library(scales)

# Abordagem como filtro apenas
dff = read.table(file = "data/general-data-analysis-2m.dat", header = T)
dff = dff %>% filter(FILTER == "ceiling_max", SCENARIO %in% c("LW", "AR")) %>% 
     select(TRACE, SCENARIO, SAFEM, CORR, COST, VIOLP, PERF_REL, OVER_REL)

head(dff)
nrow(dff)

df_ref    = data.frame(CORR = unique(dff$CORR), CORR_LABEL = c("0%", "1%", "25%", "50%", "100%"))
dff       = dff %>% inner_join(df_ref, by = "CORR") 


# dff$SCENARIO_STR = factor(dff$SCENARIO_STR, levels = c("LW", "AR"))
dff$CORR_LABEL = factor(dff$CORR_LABEL, levels = c("0%", "1%", "25%", "50%", "100%"))

refs = c(1/100, 0.1/100, 0.00/100)

dfall_sel  = c()
dfall_goal = c()
dfall_conf_cost = c()

head(dff)

for (ref in refs) {
          
     #------------------
     df_sel = dff %>% group_by(SCENARIO, TRACE) %>% filter(VIOLP <= ref, OVER_REL < 0) %>%
          mutate(REF = paste(ref * 100, "%", sep = ""))
     
     dfall_sel = rbind(dfall_sel, df_sel)
     
     #------------------
     dc = df_sel %>% ungroup() %>% group_by(SCENARIO, CORR_LABEL, SAFEM) %>% summarise(FREQ = n()/30)
     
     dc$REF = paste(ref * 100, "%", sep = "")
     
     dfall_goal = rbind(dfall_goal, dc)
     
     #------------------
     dx = df_sel %>% ungroup() %>% group_by(SCENARIO, TRACE, REF) %>% 
          filter(COST == min(COST))
     
     dx$REF = paste(ref * 100, "%", sep = "")
     
     dfall_conf_cost = rbind(dfall_conf_cost, dx)
}

# quais são as maiores percetuais de aplicações em que os objetivos de QoS e Custo são atingidos?
dfall_goal %>% group_by(REF) %>% arrange(desc(FREQ)) %>% filter(FREQ == max(FREQ)) %>% mutate(FREQ = FREQ * 100) %>% data.frame()

# quantas configurações diferentes conseguem chegar ao percentual maximo de aplicações para as diferentes classes de QoS?
dfall_goal %>% group_by(REF) %>% arrange(desc(FREQ)) %>% filter(FREQ == max(FREQ)) %>% mutate(FREQ = FREQ * 100) %>%
     summarise(NCONF = n()) %>% data.frame()

# numero de apps cujo objetivos forma atingidos considerando as melhores configurações
dfall_sel %>% group_by(REF) %>% summarise(NAPP = length(unique(TRACE)))

# numero de apps cujo objetivos forma atingidos considerando uma única configuração para cada classe e para todas as aplicações
dfall_sel %>% group_by(REF, ADD_CPU, ADD_MEM, RM_CPU, RM_MEM) %>% summarise(NAPP = n()) %>% group_by(REF) %>% filter(NAPP == max(NAPP)) %>%
     select(REF, NAPP) %>% ungroup() %>% distinct()

# O percential de configurações dentre todas que atingem os objetivos para uma aplicação que conseguem faze-lo com o menor custo

head(dfall_sel)
dplot = dfall_sel %>% group_by(TRACE, REF) %>% mutate(NCONF = n()) %>% filter(COST == min(COST)) %>%
     summarise(MINCONF = n(), NCONF = first(NCONF)) %>% mutate(FREQ = MINCONF / NCONF)

dplot %>% group_by(REF) %>% summarise(MEAN = mean(FREQ), MEDIAN = median(FREQ) * 100)


p = ggplot(dplot, aes(REF, FREQ)) + geom_boxplot(width = 0.5) + geom_jitter(alpha = 0.3, width = 0.25, size = 4, color = "blue")
p = p + scale_y_continuous(labels = percent, breaks = seq(0, 1.2, 0.1))
p = p + xlab("Limites de violações de SLO") + ylab("Percentual de configurações\ncom custo mínimo")
p = p + theme_bw(base_size = 24)
p

png(filename = "img/03_01_multiple-metrics-configurations.png", width = 750, height = 400)
print(p)
dev.off()

dfall_sel %>% group_by(REF) %>% summarise(MEAN = mean(PERF_REL) * 100, MEDIAN = median(PERF_REL) * 100, SD = sd(PERF_REL) * 100)
dfall_sel %>% ungroup() %>% filter(REF != "0%") %>% summarise(MEAN = mean(PERF_REL) * 100, MEDIAN = median(PERF_REL) * 100, SD = sd(PERF_REL) * 100, QU = quantile(PERF_REL, 0.95) * 100)
dfall_sel %>% ungroup() %>% summarise(MEAN = mean(PERF_REL) * 100, MEDIAN = median(PERF_REL) * 100, SD = sd(PERF_REL) * 100)

# custo em relação ao perfeito das configurações que levaram aos objtivos de SLO com custo menor que o over
p = ggplot(dfall_sel, aes(REF, PERF_REL)) + geom_boxplot(width = 0.5, size = 1)
p = p + scale_y_continuous(labels = percent)
p = p + xlab("Limites de violações de SLO") + ylab("Custo relativo ao\nprovisionamento perfeito")
p = p + theme_bw(base_size = 24)
p

png(filename = "img/03_01_multiple-metrics-goal-costs.png", width = 750, height = 350)
print(p)
dev.off()
