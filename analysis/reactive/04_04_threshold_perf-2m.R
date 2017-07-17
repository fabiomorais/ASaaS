library(dplyr)
library(ggplot2)
library(scales)


df_result = read.table(file = "data/trace_conf_sweep-2m.dat", header = T)
df_result = df_result %>% select(TRACE, PROV = METRIC, ADD_CPU, ADD_MEM, RM_CPU, RM_MEM, VIOLP, COST, PERF_REL, OVER_REL)

# numero de cenários por trace
df_result %>% filter(TRACE == 1) %>% summarise(n())

refs = c(1/100, 0.1/100, 0.00/100)

dfall_sel  = c()
dfall_goal = c()
dfall_conf_cost = c()

for (ref in refs) {
     
     #------------------
     df_sel = df_result %>% group_by(PROV, TRACE) %>% filter(VIOLP <= ref, OVER_REL < 0) %>%
          mutate(REF = paste(ref * 100, "%", sep = ""))
     
     dfall_sel = rbind(dfall_sel, df_sel)
     
     #------------------
     dc = df_sel %>% ungroup() %>% group_by(PROV, ADD_CPU, ADD_MEM, RM_CPU, RM_MEM) %>% summarise(FREQ = n()/30)
     
     dc$REF = paste(ref * 100, "%", sep = "")
     
     dfall_goal = rbind(dfall_goal, dc)

     #------------------
     dx = df_sel %>% ungroup() %>% group_by(PROV, TRACE, REF) %>% 
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
dplot = dfall_sel %>% group_by(PROV, TRACE, REF) %>% mutate(NCONF = n()) %>% filter(COST == min(COST)) %>% 
     summarise(MINCONF = n(), NCONF = first(NCONF)) %>% mutate(FREQ = MINCONF / NCONF)

dplot %>% group_by(REF) %>% summarise(MEAN = mean(FREQ), MEDIAN = median(FREQ) * 100)

p = ggplot(dplot, aes(REF, FREQ)) + geom_boxplot(width = 0.5) + geom_jitter(alpha = 0.3, width = 0.25, size = 4, color = "blue")
p = p + scale_y_continuous(labels = percent, breaks = seq(0, 0.8, 0.1))
p = p + xlab("Limites de violações de SLO") + ylab("Percentual de configurações\ncom custo mínimo") 
p = p + theme_bw(base_size = 24)
p

png(filename = "img/04_04_multiple-metrics-configurations.png", width = 750, height = 400)
print(p)
dev.off()

# custo em relação ao perfeito das configurações que levaram aos objtivos de SLO com custo menor que o over
p = ggplot(dfall_sel, aes(REF, PERF_REL)) + geom_boxplot(width = 0.5, size = 1)
p = p + scale_y_continuous(labels = percent)
p = p + xlab("Limites de violações de SLO") + ylab("Custo relativo ao\nprovisionamento perfeito") 
p = p + theme_bw(base_size = 24)
p

png(filename = "img/04_04_multiple-metrics-goal-costs.png", width = 750, height = 350)
print(p)
dev.off()

# custo em relação ao perfeito das configurações que levaram aos objtivos de SLO com o menor custo por aplicação
dplot = dfall_sel %>% group_by(PROV, TRACE, REF) %>% filter(COST == min(COST))
p = ggplot(dplot, aes(REF, PERF_REL)) + geom_boxplot()
p = p + scale_y_continuous(labels = percent)
p

dfall_sel %>% group_by(REF) %>% summarise(MEAN = mean(PERF_REL))
dplot %>% group_by(REF) %>% summarise(MEAN = mean(PERF_REL))
summary(dplot$PERF_REL)
summary(dfall_sel$PERF_REL)

#------------------------------------- BACKUP DE OUTRAS ANÁLISES

# a configuração média para atingir nao é intuitiva: memoria add é fixa mas ao se restringir o limite reduz add cpu e rm cpu. No entanto, o limiar 
# de rm de mem não segue um padrão definido
dfall_goal %>% group_by(REF) %>% arrange(desc(FREQ)) %>% filter(FREQ == max(FREQ)) %>% mutate(FREQ = FREQ * 100) %>% 
     mutate(DCPU = ADD_CPU - RM_CPU, DMEM = ADD_MEM - RM_MEM) %>% 
     summarise(A_CPU = mean(ADD_CPU), A_MEM = mean(ADD_MEM), R_CPU = mean(RM_CPU), R_MEM = mean(RM_MEM), D_CPU = mean(DCPU), D_MEM = mean(DMEM),
               SA_CPU = sd(ADD_CPU), SA_MEM = sd(ADD_MEM), SR_CPU = sd(RM_CPU), SR_MEM = sd(RM_MEM)) %>% data.frame()


# Numero de diferentes configurações que foram usadas por limite de SLO
dfall_sel %>% ungroup() %>% select(REF, ADD_CPU, ADD_MEM, RM_CPU, RM_MEM) %>% distinct() %>% group_by(REF) %>% summarise(NCONF = n())

# qual o custo para as configurações com o max percentual de predominancia
dplot = dfall_sel %>% group_by(REF, ADD_CPU, ADD_MEM, RM_CPU, RM_MEM) %>% mutate(NAPP = n()) %>% group_by(REF) %>% filter(NAPP == max(NAPP)) %>%
     ungroup() %>% distinct()
p = ggplot(dplot, aes(REF, PERF_REL)) + geom_boxplot()
p = p + scale_y_continuous(labels = percent)
p

# o erro em termos de custo é grande para atingir os objetivos? qual a variabilidade do custo entre as diferentes configurações que conseguem
# atingir os objetivos de uma aplicação ### ESSE É UM COMPLEMENTO DO BOXPLOT DE CUSTO
dplot = dfall_sel %>% group_by(REF, TRACE) %>% summarise(SD = sd(PERF_REL))
p = ggplot(dplot, aes(REF, SD)) + geom_boxplot()
p = p + scale_y_continuous(labels = percent)
p
