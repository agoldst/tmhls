def main(script,lfile,n="3"):
    n_words = int(3)
    with open(lfile) as f: 
        for line in f:
            fields = line.strip().split(' ')
            label = " ".join(fields[1:n_words + 1])
            macro = r'\topiclabel{' + fields[0] + '}{' + label + '}'
            print r'\newcommand{\topic' + fields[0] + "}{" + macro + "}"

if __name__=='__main__':
    import sys
    main(*sys.argv)




