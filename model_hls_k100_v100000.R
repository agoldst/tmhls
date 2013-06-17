
source("~/tmhls/make_model.R")

wd <- "/spare2/ag978/hls_k100_v100000"

trainer <- make_model(instance=file.path(wd,"journals.mallet"),
                      output_dir=wd,
                      num.topics=100,
                      n.iters=500,
                      threads=16L, 
                      beta=0.01,
                      n.max.iters=10,
                      n.hyper.iters=20,
                      n.burn.in=50,
                      smoothed=T,
                      normalized=T)
