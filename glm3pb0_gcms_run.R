library(scipiper)

message(sprintf("starting 3_summarize for glm3pb0 at %s", Sys.time()))

scmake("3_summarize/out/annual_metrics_glm3_pb0gcm.csv")

message(sprintf("finished 3_summarize for glm3pb0 at %s", Sys.time()))
