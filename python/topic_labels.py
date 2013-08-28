# topic_labels.py
# generate a tex macro for labeling topics
#
# usage:
# python topic_labels.py path/to/labels.txt [n]
#
# labels.txt should have lines of the form
#
# k,alpha,word1 word2 word3 ...
# 
# where k is the number of the topic.
#
# The result is the source for a command \topiclabel{k} which expands to
#
# \topicformat{k}{word1 word2 ... word_n}
#
# \topicformat{}{} itself is defined in essay.tex

def main(script,lfile,n="3"):
    n_words = int(n)
    print r'''
\def\topiclabel#1{%
    \ifcase#1
\textbf{no topic zero}'''
    with open(lfile) as f: 
        for line in f:
            topic,alpha,long_label = line.strip().split(',')

            # an easy way to ignore a header line, if present
            if not topic.isdigit():
                continue

            print r'    \or'
            # skip alpha
            label = " ".join(long_label.split(" ")[0:n_words])
            print r'\topicformat{' + topic + "}{" + label + "}%"
        
    print r'''    \fi%
}'''

if __name__=='__main__':
    import sys
    main(*sys.argv)




