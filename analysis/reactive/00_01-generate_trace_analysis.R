suppressMessages(library(plyr, quietly = TRUE))

traces		<- c(1, 2, 3, 4, 8, seq(10, 27), 30, 31, 32, 33, 35, 38, 40)

file_name_abs_cpu_ecdf   <- "data/df_trace_diff_cpu_abs_ecdf.dat"
file_name_rel_cpu_ecdf   <- "data/df_trace_diff_cpu_rel_ecdf.dat"
file_name_util_cpu_ecdf  <- "data/df_trace_util_cpu_final_ecdf.dat"
file_name_util_cpu       <- "data/df_trace_util_cpu_final_raw.dat"

file_name_abs_mem_ecdf	<- "data/df_trace_diff_mem_abs_ecdf.dat"
file_name_rel_mem_ecdf	<- "data/df_trace_diff_mem_rel_ecdf.dat"
file_name_util_mem_ecdf	<- "data/df_trace_util_mem_final_ecdf.dat"
file_name_util_mem	     <- "data/df_trace_util_mem_final_raw.dat"

df_diff_rel_cpu	<- c()
df_diff_abs_cpu	<- c()
df_util_final_cpu	<- c()

df_diff_rel_mem	<- c()
df_diff_abs_mem	<- c()
df_util_final_mem	<- c()

for (trace in traces) {
     
     print(trace)
     
     filename_util_cpu   <- paste("../../data/normal/cpu_util-prod-server-", trace, "-normal.dat", sep = "")
     filename_alloc_cpu  <- paste("../../data/normal/cpu_alloc-prod-server-", trace, "-normal.dat", sep = "")
     
     filename_util_mem	<- paste("../../data/normal/mem_util-prod-server-", trace, "-normal.dat", sep = "")
     filename_alloc_mem	<- paste("../../data/normal/mem_alloc-prod-server-", trace, "-normal.dat", sep = "")
     
     df_util_cpu    <- read.table(filename_util_cpu, header = T)
     df_alloc_cpu   <- read.table(filename_alloc_cpu, header = T)
     
     df_util_mem	<- read.table(filename_util_mem, header = T)
     df_alloc_mem   <- read.table(filename_alloc_mem, header = T) 
     
     df_diff_abs_cpu	<- rbind(df_diff_abs_cpu, data.frame(TRACE = trace, DIFF = df_util_cpu$CPU_UTIL[2:nrow(df_util_cpu)] - df_util_cpu$CPU_UTIL[1:(nrow(df_util_cpu) - 1)]))	#absolute
     df_diff_rel_cpu     <- rbind(df_diff_rel_cpu, data.frame(TRACE = trace, DIFF = (df_util_cpu$CPU_UTIL[2:nrow(df_util_cpu)] / df_util_cpu$CPU_UTIL[1:(nrow(df_util_cpu) - 1)]))) #relative
     df_util_final_cpu   <- rbind(df_util_final_cpu, data.frame(TRACE = trace, UTIL = df_util_cpu$CPU_UTIL, ALLOC = df_alloc_cpu$CPU_ALLOC, USAGE = df_util_cpu$CPU_UTIL * df_alloc_cpu$CPU_ALLOC / 100))
          
     df_diff_abs_mem	<- rbind(df_diff_abs_mem, data.frame(TRACE = trace, DIFF = df_util_mem$MEM_UTIL[2:nrow(df_util_mem)] - df_util_mem$MEM_UTIL[1:(nrow(df_util_mem) - 1)]))	#absolute
     df_diff_rel_mem	<- rbind(df_diff_rel_mem, data.frame(TRACE = trace, DIFF = (df_util_mem$MEM_UTIL[2:nrow(df_util_mem)] / df_util_mem$MEM_UTIL[1:(nrow(df_util_mem) - 1)]))) #relative
     df_util_final_mem   <- rbind(df_util_final_mem, data.frame(TRACE = trace, UTIL = df_util_mem$MEM_UTIL, ALLOC = df_alloc_mem$MEM_ALLOC, USAGE = df_util_mem$MEM_UTIL * df_alloc_mem$MEM_ALLOC / 100))
}

#cpu
df_diff_rel_cpu_ecdf	  <- ddply(df_diff_rel_cpu, .(TRACE), transform, ECDF = ecdf(DIFF)(DIFF))
df_util_final_cpu_ecdf	  <- ddply(df_util_final_cpu, .(TRACE), transform, ECDF = ecdf(UTIL)(UTIL))
df_diff_abs_cpu_ecdf	  <- ddply(df_diff_abs_cpu, .(TRACE), transform, ECDF = ecdf(DIFF)(DIFF))

write.table(df_diff_rel_cpu_ecdf, file = file_name_rel_cpu_ecdf, row.names = FALSE)
write.table(df_diff_abs_cpu_ecdf, file = file_name_abs_cpu_ecdf, row.names = FALSE)
write.table(df_util_final_cpu_ecdf, file = file_name_util_cpu_ecdf, row.names = FALSE)
write.table(df_util_final_cpu, file = file_name_util_cpu, row.names = FALSE)

#memory
df_diff_rel_mem_ecdf	  <- ddply(df_diff_rel_mem, .(TRACE), transform, ECDF = ecdf(DIFF)(DIFF))
df_util_final_mem_ecdf	  <- ddply(df_util_final_mem, .(TRACE), transform, ECDF = ecdf(UTIL)(UTIL))
df_diff_abs_mem_ecdf	  <- ddply(df_diff_abs_mem, .(TRACE), transform, ECDF = ecdf(DIFF)(DIFF))

write.table(df_diff_rel_mem_ecdf, file = file_name_rel_mem_ecdf, row.names = FALSE)
write.table(df_diff_abs_mem_ecdf, file = file_name_abs_mem_ecdf, row.names = FALSE)
write.table(df_util_final_mem_ecdf, file = file_name_util_mem_ecdf, row.names = FALSE)
write.table(df_util_final_mem, file = file_name_util_mem, row.names = FALSE)
