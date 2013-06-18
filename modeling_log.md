# 2013-06-17 AG

Ran instance_hls_k100_v100000.R. This created a new instance of the flas from the seven lit journals, with a 100K word vocabulary, British spellings translated to American, and documents under 1000 words discarded. This is the logging output:

Plots saved to models/hls_k100_v100000/freqplots.png
A frequency threshold of 1.48480394593994e-07 or > 19 tokens
leaves 98748 of 1122454 types (0.088) and 125490482 of 127963022 tokens (0.981)
The 6047 unique stopwords from ~/Documents/research/20c/hls/tmhls/stoplist_final.txt
correspond to 70003350 of 127963022 tokens (0.547) in the corpus
Removing infrequent word types...
31385833 rows remain.
Instance saved to models/hls_k100_v100000/journals.mallet

I uploaded the resulting instance to apps.rutgers.edu:/spare2/ag978/hls_k100_v100000/journals.mallet. I then launched a 100-topic run using model_hls_k100_v100000.R, which contains these parameters:

num.topics=100,
n.iters=500,
threads=16L, 
beta=0.01,
n.max.iters=10,
n.hyper.iters=20,
n.burn.in=50,
smoothed=T,
normalized=T





# 2013-06-17 TU: HLSk300v120000

The instance was produced by make_HLS_instance.R, which I have pushed to the repo
as "make_instance_nobrit.R," a more comprehensible title. Setting were

* a 120,000 word vocabulary
* stoplist_final.txt <blink>plus "ofthe"</blink>
* Spelling normalization as per in UK2UStransrules.csv,
* the full date range of all journals, and
* only 'fla' files -- the 'brv' mislabeled as 'fla'
* in MLR 2011-12 were removed.

The instance was modeled by run_HLS_instance.R. Settings were

* instances="/Users/tunderwood/Journals/Uresults/HLS/journals.mallet",
* num.topics=300,
* alpha.sum=5,
* beta=0.01 ,
* n.iters=800,
* n.max.iters=10,
* n.hyper.iters=20,
* n.burn.in=50,
* threads=4L

Output was not smoothed or normalized.

# 2013-06-16 TU: AHRk100v20000

Was no good. Incomplete corpus.

# 2013-06-13 AG

result from yesterday's mallet run saved locally in out130612. In
Rstudio on apps, also saved the trainer's topic-word assignments
(unsmoothed, unnormalized) and vocabulary. Copied all that remained of
the console buffer in Rstudio into make_model.log_partial. All resulting
files archived as model130612.tar

# 2013-06-12 AG

made a new instance at models/out130612/journals.mallet:

A frequency threshold of 6.29785577493439e-06 or > 695 tokens
leaves 9998 of 1001031 types (0.010) and 99219809 of 110355020 tokens (0.899)
The 6047 unique stopwords from stoplist_final.txt
correspond to 60637548 of 110355020 tokens (0.549) in the corpus
21385013 rows remain.

uploaded to /spare2/ag978/130612/journals.mallet.
Started mallet run around 6:30 p.m.: 
instances="/spare2/ag978/130612/journals.mallet",
num.topics=150,
alpha.sum=5,
beta=0.01 ,
n.iters=500,
n.max.iters=10,
n.hyper.iters=20,
n.burn.in=50,
threads=16L


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

# 2013-06-18 TU: AHRk150v105

The instance was produced with

* a 100,000 word vocabulary
* stoplist_final plus 'ofthe' (6048 unique stopwords)
* Spelling normalization as per in UK2UStransrules.csv
* the full date range, and
* only 'fla' files, of which there are 3,180

I used make_AHR_instance.R for that, and used run_AHR_instance.R to run the
model itself. Settings were

150 topics; alpha.sum=10; beta=0.01; n.iters=500; n.max.iters=10; n.hyper.iters=20; n.burn.in=50; threads=4L

Output was not smoothed or normalized. I increased alpha-sum to 10 experimentally, but don't see much difference. Also have an 100-topic model of AHR locally; it lacks a "fact-evidence" topic.

# 2013-06-18 TU: HLSk200v105

The instance was produced with

* a 100,000 word vocabulary
* stoplist_final plus 'ofthe' (6048 unique stopwords)
* Spelling normalization as per in UK2UStransrules.csv
* the full date range, and
* only 'fla' files, of which there are 22-odd-thousand

I used make_HLS_instance.R for that, and used run_HLS_instance.R to run the
model itself. Settings were

200 topics; alpha.sum=10; beta=0.01; n.iters=500; n.max.iters=10; n.hyper.iters=20; n.burn.in=50; threads=4L

Output was not smoothed or normalized. I increased alpha-sum to 10 experimentally, but don't see much difference.