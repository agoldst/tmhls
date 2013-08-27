# topic_labels.py
# generate a tex macro for labeling topics
#
# usage:
# python topic_labels.py path/to/labels.txt [n]
#
# labels.txt should have lines of the form
#
# NN word1 word2 word3 ...
# 
# where NN is the number of the topic

def main(script,lfile,n="3"):
    n_words = int(3)
    print r'''
\def\topiclabel#1{
    \ifcase#1
        no topic zero'''
    with open(lfile) as f: 
        for line in f:
            fields = line.strip().split(' ')
            print r'\or'
            label = " ".join(fields[1:n_words + 1])
            print r'\topicformat{' + fields[0] + "}{" + label + "}"
        
    print r'\fi }'

if __name__=='__main__':
    import sys
    main(*sys.argv)




