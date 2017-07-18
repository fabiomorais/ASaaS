library(dplyr)

df_jobs        = read.csv(file = "data/job-events-reduced.csv", header = F)

# todos os jobs comecam em zero
sum(df_jobs$V1 > 0)

# todos os jobs terminam no mesmo timestamp
table(df_jobs$V4)

tmp            = '27042016T212529'
filename       = paste("data/job/", tmp, "/selected_jobs.dat", sep = "")
jobs           = read.table(file = filename, header = T) 

# selecao de jobs com mais de 1 task
df_jobs        = df_jobs %>% filter(V2 %in% jobs$jobId)

df_final  = c()

dff_value = 300000000
nslots    = seq(6e+08, 2.5062e+12, 300000000)

for (t in 1:nrow(jobs)) {
     
     id = jobs[t, ]$jobId
     
     print(paste(t, "/", nrow(jobs), id))
     
     filename       = paste("data/job/", tmp, "/", id, "_task_usage.csv", sep = "")
     task_usage     = read.csv(file = filename, header = T) %>% arrange(startTime)
 
     df_out         = task_usage %>% mutate(indexslot = (floor(startTime / dff_value) - 1))
     
     filename       = paste("data/job/", tmp, "/", id, "_task_usage_index.csv", sep = "")
     write.csv(df_out, file = filename, row.names = F)
}