# 2013-06-12 AG

result from yesterday's mallet run saved locally in out130611, archived as model130611.tar

# 2013-06-11 AG

With working directories clean for all repos (exept for some untracked files that were not used)

ran stoplist_final.py
ran make_instance.R
moved result to out130611
uploaded journals.mallet to apps at /spare2/ag978/journals130611.mallet

make_instance logging output:
A frequency threshold of 1e-07 or > 11 tokens
leaves 122146 of 1001031 types (0.122) and 108555890 of 110355020 tokens (0.984)
Read 29147541 rows
Removing word types with corpus frequency < 1e-07

(adding belatedly on 2013-06-12, but I did this yesterday:)

ran make_model.R on apps:
instances="/spare2/ag978/journals130611.mallet",
num.topics=150,
alpha.sum=5,
beta=0.01 ,
n.iters=1000,
n.max.iters=10,
n.hyper.iters=20,
n.burn.in=50,
threads=4L
