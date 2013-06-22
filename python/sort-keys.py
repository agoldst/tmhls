with open("/Users/tunderwood/Journals/Uresults/HLS/keys.csv", encoding = 'utf-8') as file:
    filelines = file.readlines()

tuplelist = list()

for line in filelines[1:]:
    line = line.rstrip()
    fields = line.split(',')
    tuplelist.append((float(fields[1]), float(fields[3]), fields[0], fields[2]))

tuplelist = sorted(tuplelist, reverse = True)

with open("/Users/tunderwood/Journals/Uresults/HLS/sorted_keys.csv", mode='w', encoding = 'utf-8') as file:
    file.write("alpha,topic,word,weight\n")
    for atuple in tuplelist:
        outline = str(atuple[0]) + ',' + str(atuple[2]) + ',' + atuple[3] + ',' + str(atuple[1]) + '\n'
        file.write(outline)
print('Done.')
