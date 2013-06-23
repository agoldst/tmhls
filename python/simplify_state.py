import gzip
import sys

def process_file(state_file):
    with gzip.open(state_file, 'rb') as f:
        for line in f:
            if line[0] == "#":
                continue

            line.strip()
            doc,source,pos,typeindex,word,topic = line.split()
            print "{},{},{}".format(doc,typeindex,topic)

if __name__=="__main__":
    script,filename = sys.argv
    process_file(filename)
