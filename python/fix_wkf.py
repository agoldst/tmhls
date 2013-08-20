
def main(script_name,tw_file,bad_wkf_file,vocab_file):

    with open(bad_wkf_file) as f:
        alphas = []
        cur_topic = '0'
        count_top = True
        n_top_words = 0
        header = f.readline()
        for line in f:
            fields = line.split(',')
            if cur_topic != fields[0]:
                cur_topic = fields[0]
                alphas.append(fields[1])
                if count_top and n_top_words != 0:
                    n_top_words += 1
                    count_top = False
            else:
                if count_top:
                    n_top_words += 1

    with open(vocab_file) as f:
        vocab = [line.strip() for line in f.readlines()]

    with open(tw_file) as f:
        n = 0
        topic = 0
        result = []
        for line in f:
            topic += 1
            w = [int(x) for x in line.strip().split(',')]
            if n == 0:
                n = len(w)
            js = sorted(xrange(len(w)),
                    cmp=lambda i,j: cmp(w[i],w[j]),reverse=True)
            tw = [(topic,j,w[j]) for j in js[0:n_top_words]]
            result.extend(tw)

    def result_tuple_str(x,vocab,alpha):
        return "{},{},{},{}".format(x[0],alpha,vocab[x[1]],x[2])


    print "topic,alpha,word,weight"
    for x in result:
        print result_tuple_str(x,vocab,alphas[int(x[0])-1])

if __name__=='__main__':
    import sys
    main(*sys.argv)

