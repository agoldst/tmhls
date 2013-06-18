
source("~/tmhls/make_model.R")

trainer <- make_model(instances="/spare2/ag978/hls_k48_v100K/journals.mallet",
                      output_dir="/spare2/ag978/hls_k48_v100K",
                      num.topics=48,
                      n.iters=500,
                      threads=16L, 
                      beta=0.01,
                      n.max.iters=10,
                      n.hyper.iters=20,
                      n.burn.in=50,
                      smoothed=T,
                      normalized=T)
