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

The instance was produced with

* a 20,000 word vocabulary
* standard stoplist_final (6047 unique stopwords)
* Spelling normalization as per in UK2UStransrules.csv
* the full date range, and
* both 'fla' and 'brv' files

I used make_AHR_instance.R for that, and used run_AHR_instance.R to run the
model itself. Settings were

* 100 topics
* alpha.sum=5,
* beta=0.01 ,
* n.iters=500,
* n.max.iters=10,
* n.hyper.iters=20,
* n.burn.in=50,
* threads=4L

Output was not smoothed or normalized.

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

