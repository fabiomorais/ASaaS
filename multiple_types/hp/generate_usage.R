library(dplyr)

jobs = c(1, 2, 3, 4, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 30, 31, 32, 33, 35, 38, 40)

df_final  = c()

dff_value = 300000000
nslots    = seq(6e+08, 9e+08, 300000000)

for (id  in jobs) {
     
     print(id)
     
     filename       = paste("../../data/normal/cpu_util-prod-server-", id, "-normal.dat", sep = "")
     df_util_cpu    = read.table(file = filename, header = T) %>% arrange(TIMESTAMP)
     
     filename       = paste("../../data/normal/cpu_alloc-prod-server-", id, "-normal.dat", sep = "")
     df_alloc_cpu   = read.table(file = filename, header = T) %>% arrange(TIMESTAMP)
     
     filename       = paste("../../data/normal/mem_util-prod-server-", id, "-normal.dat", sep = "")
     df_util_mem    = read.table(file = filename, header = T) %>% arrange(TIMESTAMP)
     
     filename       = paste("../../data/normal/mem_alloc-prod-server-", id, "-normal.dat", sep = "")
     df_alloc_mem   = read.table(file = filename, header = T) %>% arrange(TIMESTAMP)
     
     df_util        = inner_join(df_util_cpu, df_alloc_cpu) %>% inner_join(df_util_mem) %>% inner_join(df_alloc_mem)
     df_util        = df_util %>% mutate(CPU_USAGE = CPU_UTIL * CPU_ALLOC / 100, MEM_USAGE = MEM_UTIL * MEM_ALLOC / 100) %>%
          mutate(indexslot = seq(1, n()))
     
     head(df_util)
     filename       = paste("data/usage/usage-prod-server-", id, "-normal-index.dat", sep = "")
     write.table(df_util, file = filename)
}