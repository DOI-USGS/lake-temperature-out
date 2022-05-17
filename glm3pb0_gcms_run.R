library(scipiper)

message(sprintf("starting 3_summarize for glm3pb0 at %s", Sys.time()))

scmake("3_summarize/out/annual_metrics_glm3pb0_gcms.csv")

message(sprintf("finished 3_summarize for glm3pb0 at %s", Sys.time()))
