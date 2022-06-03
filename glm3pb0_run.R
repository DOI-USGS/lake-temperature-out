library(scipiper)

gcm_start <- Sys.time()
message(sprintf("starting 3_summarize for glm3_pb0gcm at %s", gcm_start))
scmake("3_summarize/out/annual_metrics_glm3_pb0gcm.csv")
gcm_end <- Sys.time()
message(sprintf("finished 3_summarize for glm3_pb0gcm at %s", gcm_end))

nldas_start <- Sys.time()
message(sprintf("starting 3_summarize for glm3_pb0nldas at %s", nldas_start))
scmake("3_summarize/out/annual_metrics_glm3_pb0nldas.csv")
nldas_end <- Sys.time()
message(sprintf("finished 3_summarize for glm3_pb0nldas at %s", nldas_end))

message(sprintf('GCM build from %s to %s (diff = %s)', gcm_start, gcm_end, gcm_end-gcm_start))
message(sprintf('NLDAS build from %s to %s (diff = %s)', nldas_start, nldas_end, nldas_end-nldas_start))
