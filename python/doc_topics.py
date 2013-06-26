import sys

from collections import defaultdict

# Take a mallet state.gz file and produce a csv of doc-type-topic
# counts. Output to stdout.
#
# USAGE: python simplify_state.py state.gz > state_tallied.csv
#
# The mallet state is saved in a gz file with lots of redundant
# information. For our purposes, we don't know the order of tokens in
# documents, so we can "reduce" the state information to a list of
# 4-tuples: doc,type,topic,count.

def process_file(ss_file,n_topics):

    # utility function for accumulating our doc-topic tallies as we go
    def output_tally(tally):
        for topic in range(n_topics):
            sys.stdout.write(str(tally[topic]))
            if topic + 1 != n_topics:
                sys.stdout.write(",")
            else:
                print

    with open(ss_file) as f:
        doc_tally = defaultdict(int)
        last_doc = 0    # assume we start at doc 0

        skipped_header = False
        for line in f:
            if not skipped_header:
                skipped_header = True
                continue

            # We can assume that the state is written out doc-by-doc,
            # so we only need to count up topic-doc pairs until we get
            # to the end of the state for a given doc

            line.strip()
            doc,wordtype,topic,count = [int(s) for s in line.split(",")]

            if last_doc != doc:
                output_tally(doc_tally)
                doc_tally = defaultdict(int)

            doc_tally[int(topic)] += int(count)
            last_doc = doc

        # final doc: after end of for loop
        if len(doc_tally) > 0:
            output_tally(doc_tally)


if __name__=="__main__":
    script,filename,n = sys.argv
    process_file(filename,int(n))
