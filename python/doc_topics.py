import sys

from collections import defaultdict

# Take a simplified-mallet-state file (made by simplify_state.py) and
# produce a csv of the doc-topic counts matrix. Output to stdout.
#
# USAGE: python doc_topics.py state_simple.csv > document_topics.csv
#
# The results should be consistent with the matrix saved by
# doc_topics_frame() in R. N.B. that the results of this script have no
# headers and no id column.

def process_file(ss_file,n_topics):

    # utility function for accumulating our doc-topic tallies as we go
    def output_tally(tally):
        line = ""
        for topic in range(n_topics):
            line += str(tally[topic])
            if topic + 1 != n_topics:
                line += ","
        print line

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
