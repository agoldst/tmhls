"""final_list.py

Read in the annotated dubiousStopwords.txt,
then output stoplist.txt by filtering stoplist.tsv accordingly.

"""

discard = dict()
keepers = 0
with open("dubiousStopwords.txt") as f:
    for line in f:
        fields = line.split()
        try:
            if fields[2] == "y":
                discard[fields[0]] = False
                keepers += 1
            else:
                discard[fields[0]] = True
        except IndexError:
            discard[fields[0]] = True
            
print "Read {} dubious stopword lines; {} keepers".format(len(discard),keepers)


output = open("stoplist.txt",mode="w")
discards = 0
with open("stoplist.tsv") as f:
    for line in f:
        w, category = line.split()
        if category == "inGoldstone" or category == "inUnderwood":
            if discard[w]:
                discards += 1
                print "Skip {}".format(w)
            else:
                output.write(w)
                output.write("\n")
        else:
            output.write(w)
            output.write("\n")

output.close()
print """
Wrote stoplist.txt from stoplist.tsv, discarding {} dubious words
""".format(discards)



