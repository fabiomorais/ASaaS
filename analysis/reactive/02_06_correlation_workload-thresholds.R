library(dplyr)

df_trace  = read.table(file = "data/trace_analysis.dat", header = T) %>% select(TRACE, METRIC, SD, SDC)

head(df_trace)
range_size = 5
df_result = read.table(file = paste("data/02_data_thresholds-", range_size, ".dat", sep = ""), header = T)
df_result = df_result %>% select(-TOTAL, -OCC)
head(df_result)

dconf     =  df_result %>% group_by(PROV_TIME, TRACE, METRIC) %>% summarise(NSCALING = n()) 

#- Correlação fraca/moderada entre a variabilidade de CPU e Memória dos traces
spearman  = cor.test(df_trace[df_trace$METRIC == "cpu", ]$SD, df_trace[df_trace$METRIC == "mem", ]$SD, method = "spearman", exact = FALSE) # moderada 0.55
kendall   = cor.test(df_trace[df_trace$METRIC == "cpu", ]$SD, df_trace[df_trace$METRIC == "mem", ]$SD, method = "kendall", exact = FALSE) # fraca 0.4

#- Correlação fraca/moderada entre a variabilidade a quantidade de ações de provisionamento baseadas em diferentes métricas
spearman  = cor.test(dconf[dconf$METRIC == "cpu", ]$NSCALING, dconf[dconf$METRIC == "mem", ]$NSCALING, method = "spearman", exact = FALSE) # moderada 0.57
kendall   = cor.test(dconf[dconf$METRIC == "cpu", ]$NSCALING, dconf[dconf$METRIC == "mem", ]$NSCALING, method = "kendall", exact = FALSE) # fraca 0.4

# Entra no capítulo
# - Correlação positiva de forte a muito forte entre a variabilidade de carga da aplicação e a quantidade de ações de provisionamento baseada em CPU
spearman  = cor.test(df_trace[df_trace$METRIC == "cpu", ]$SD, dconf[dconf$METRIC == "cpu", ]$NSCALING, method = "spearman", exact = FALSE) # muito forte > 0.9
kendall   = cor.test(df_trace[df_trace$METRIC == "cpu", ]$SD, dconf[dconf$METRIC == "cpu", ]$NSCALING, method = "kendall", exact = FALSE) # forte > 0.7

# Entra no capítulo
#- Correlação positiva de moderada a forte entre a variabilidade de carga da aplicação e a quantidade de ações de provisionamento baseada em MEM
spearman  = cor.test(df_trace[df_trace$METRIC == "mem", ]$SD, dconf[dconf$METRIC == "mem", ]$NSCALING, method = "spearman", exact = FALSE) # forte > 0.7
kendall   = cor.test(df_trace[df_trace$METRIC == "mem", ]$SD, dconf[dconf$METRIC == "mem", ]$NSCALING, method = "kendall", exact = FALSE) # moderada > 0.5



dconf     = df_result %>% group_by(TRACE, METRIC, ACT_DONE, THRESHOLD, DIFF) %>% summarise(N = n()) %>% ungroup() %>% group_by(TRACE, METRIC) %>% summarise(NCONF = n())
spearman  = cor.test(df_trace[df_trace$METRIC == "cpu", ]$SD, dconf[dconf$METRIC == "cpu", ]$NCONF, method = "spearman", exact = FALSE) # forte > 0.7
kendall   = cor.test(df_trace[df_trace$METRIC == "cpu", ]$SD, dconf[dconf$METRIC == "cpu", ]$NCONF, method = "kendall", exact = FALSE) # forte > 0.7
 
spearman$estimate
kendall$estimate

# #- Correlação de moderada a forter entre a variabilidade de carga e a quantidade de diferentes configurações de provisionamento por aplicação baseada em Memória
dconf     = df_result %>% group_by(TRACE, METRIC, ACT_DONE, THRESHOLD, DIFF) %>% summarise(N = n()) %>% ungroup() %>% group_by(TRACE, METRIC) %>% summarise(NCONF = n())
spearman  = cor.test(df_trace[df_trace$METRIC == "mem", ]$SD, dconf[dconf$METRIC == "mem", ]$NCONF, method = "spearman", exact = FALSE) # forte > 0.8
kendall   = cor.test(df_trace[df_trace$METRIC == "mem", ]$SD, dconf[dconf$METRIC == "mem", ]$NCONF, method = "kendall", exact = FALSE) # moderada > 0.5

spearman$estimate
kendall$estimate


# Por limiar de provisionamento
# #- Correlação de moderada a forter entre a variabilidade de carga e a quantidade de diferentes configurações de provisionamento por aplicação baseada em CPU
dconf     = df_result %>% group_by(TRACE, METRIC, ACT_DONE, THRESHOLD) %>% summarise(N = n()) %>% ungroup() %>% group_by(TRACE, METRIC) %>% summarise(NCONF = n())
spearman  = cor.test(df_trace[df_trace$METRIC == "cpu", ]$SD, dconf[dconf$METRIC == "cpu", ]$NCONF, method = "spearman", exact = FALSE) # forte > 0.8
kendall   = cor.test(df_trace[df_trace$METRIC == "cpu", ]$SD, dconf[dconf$METRIC == "cpu", ]$NCONF, method = "kendall", exact = FALSE) # moderada > 0.5

spearman$estimate
kendall$estimate


# #- Correlação de moderada a forter entre a variabilidade de carga e a quantidade de diferentes configurações de provisionamento por aplicação baseada em Memória
dconf     = df_result %>% group_by(TRACE, METRIC, ACT_DONE, THRESHOLD) %>% summarise(N = n()) %>% ungroup() %>% group_by(TRACE, METRIC) %>% summarise(NCONF = n())
spearman  = cor.test(df_trace[df_trace$METRIC == "mem", ]$SD, dconf[dconf$METRIC == "mem", ]$NCONF, method = "spearman", exact = FALSE) # forte > 0.8
kendall   = cor.test(df_trace[df_trace$METRIC == "mem", ]$SD, dconf[dconf$METRIC == "mem", ]$NCONF, method = "kendall", exact = FALSE) # moderada > 0.5

spearman$estimate
kendall$estimate

# Por limiar e VM provisionada
dconf     = df_result %>% group_by(TRACE, METRIC, ACT_DONE, THRESHOLD, DIFF) %>% summarise(N = n()) %>% ungroup() %>% group_by(TRACE, METRIC) %>% summarise(NCONF = n())
spearman  = cor.test(df_trace[df_trace$METRIC == "cpu", ]$SD, dconf[dconf$METRIC == "cpu", ]$NCONF, method = "spearman", exact = FALSE) # forte > 0.8
kendall   = cor.test(df_trace[df_trace$METRIC == "cpu", ]$SD, dconf[dconf$METRIC == "cpu", ]$NCONF, method = "kendall", exact = FALSE) # moderada > 0.5

spearman$estimate
kendall$estimate

dconf     = df_result %>% group_by(TRACE, METRIC, ACT_DONE, THRESHOLD, DIFF) %>% summarise(N = n()) %>% ungroup() %>% group_by(TRACE, METRIC) %>% summarise(NCONF = n())
spearman  = cor.test(df_trace[df_trace$METRIC == "mem", ]$SD, dconf[dconf$METRIC == "mem", ]$NCONF, method = "spearman", exact = FALSE) # forte > 0.8
kendall   = cor.test(df_trace[df_trace$METRIC == "mem", ]$SD, dconf[dconf$METRIC == "mem", ]$NCONF, method = "kendall", exact = FALSE) # moderada > 0.5

spearman$estimate
kendall$estimate

# --------------------------------------------
dconf     = df_result %>% group_by(TRACE, METRIC, ACT_DONE, THRESHOLD, DIFF) %>% summarise(N = n()) %>% ungroup() %>% group_by(TRACE, METRIC) %>% summarise(NCONF = n())
dconf %>% ungroup() %>% group_by(METRIC) %>% arrange(desc(NCONF)) %>% top_n(5) # trace 16 e 10 tem as maiores quantidade de configurações (cpu), e 10 e 14 para mem
df_trace %>% ungroup() %>% group_by(METRIC) %>% arrange(desc(SD)) %>% top_n(5) # traces 16 e 10 tem os maiores desvios padrão (cpu), e 10 e 14 para mem

# não existe muita relação para traces com pequenas quantidades de configurações
dconf %>% ungroup() %>% group_by(METRIC) %>% arrange(NCONF) %>% top_n(5) # trace 16 e 10 tem as maiores quantidade de configurações
df_trace %>% ungroup() %>% group_by(METRIC) %>% arrange(SD) %>% top_n(5) # traces 16 e 10 tem os maiores desvios padrão



