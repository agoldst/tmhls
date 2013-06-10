"""final_list.py

Read in the annotated dubiousStopwords.txt,
then output a final stoplist by filtering stoplist.tsv accordingly.

"""
dubiousfile_name = "dubiousStopwords.txt"
stopcats_name = "stoplist.tsv"
outfile_name = "stoplist_final.txt"

discard = dict()
keepers = 0
with open(dubiousfile_name) as f:
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


output = open(outfile_name,mode="w")
discards = 0
with open(stopcats_name) as f:
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
Wrote {} from {}, discarding {} dubious words
""".format(outfile_name,stopcats_name,discards)



