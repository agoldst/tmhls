

source("~/tmhls/make_model.R")

# NB reuse the same instance as for the 48-topic model
# Smoothing and normalization off.

trainer <- make_model(instances="/spare2/ag978/hls_k48_v100K/journals.mallet",
                      output_dir="/spare2/ag978/hls_k150_v100K",
                      num.topics=150,
                      n.iters=500,
                      threads=16L, 
                      beta=0.01,
                      n.max.iters=10,
                      n.hyper.iters=20,
                      n.burn.in=50,
                      smoothed=F,
                      normalized=F)
