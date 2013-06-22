with open("/Users/tunderwood/Journals/Uresults/minkeys.txt", encoding = 'utf-8') as file:
    filelines = file.readlines()

tuplelist = list()

for line in filelines:
    line = line.rstrip()
    fields = line.split('\t')
    tuplelist.append((float(fields[1]), fields[0], fields[2]))

tuplelist = sorted(tuplelist, reverse = True)
alpha = 0
with open("/Users/tunderwood/Journals/Uresults/sorted_minimal_keys.tsv", mode='w', encoding = 'utf-8') as file:
    for atuple in tuplelist:
        outline = atuple[1] + '\t' + str(atuple[0]) + '\t' + atuple[2] + '\n'
        alpha += atuple[0]
        file.write(outline)
print(alpha)
