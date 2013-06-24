import gzip
import sys

from collections import defaultdict

def process_file(state_file):

    def output_tally(doc,tally):
        for typeindex in tally:
            for topic in tally[typeindex]:
                print "{},{},{},{}".format(doc,typeindex,topic,
                        tally[typeindex][topic])

    with gzip.open(state_file, 'rb') as f:
        doc_tally = defaultdict(lambda : defaultdict(int))
        last_doc = 0    # assume we start at doc 0

        for line in f:
            if line[0] == "#":
                continue
            # We can assume that the state is written out doc-by-doc,
            # so we only need to count up type-topic pairs until we get
            # to the end of the state for a given doc

            line.strip()
            doc,source,pos,typeindex,word,topic = line.split()

            if last_doc != doc:
                output_tally(last_doc,doc_tally)
                doc_tally = defaultdict(lambda : defaultdict(int))

            doc_tally[typeindex][topic] += 1 
            last_doc = doc

        # final doc: after end of for loop
        if len(doc_tally) > 0:
            output_tally(last_doc,doc_tally)


if __name__=="__main__":
    script,filename = sys.argv
    process_file(filename)
