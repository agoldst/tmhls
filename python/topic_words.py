import gzip
import sys

from collections import defaultdict

# Take a mallet state.gz file and produce a csv of of topic-word
# counts (no headers or rownames). Output to stdout.
#
# USAGE: python topic_words.py state.gz > topic_words.csv
#
# The mallet state is saved in a gz file with lots of redundant
# information. 

def process_file(state_file):

    # utility function for printing our type-topic tallies as we go
    def output_tally(tw,n_topics,n_types): 
        for topic in xrange(n_topics):
            print ",".join([str(tw[topic][i]) for i in xrange(n_types)])

    with gzip.open(state_file, 'rb') as f:
        tw = defaultdict(lambda : defaultdict(int))
        max_topic = 0
        max_typeindex = 0

        for line in f:
            if line[0] == "#":
                continue

            # We can assume that the state is written out doc-by-doc,
            # so we only need to count up type-topic pairs until we get
            # to the end of the state for a given doc

            line.strip()
            doc,source,pos,typeindex,word,topic = line.split()

            typeindex = int(typeindex)
            topic = int(topic)
            if topic > max_topic:
                max_topic = topic
            if typeindex > max_typeindex:
                max_typeindex = typeindex

            tw[topic][typeindex] += 1 

        output_tally(tw,max_topic + 1,max_typeindex + 1)


if __name__=="__main__":
    script,filename = sys.argv
    process_file(filename)
