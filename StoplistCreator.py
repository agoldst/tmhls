def add_if_not_in(dictionary, word, label):
    if len(word) < 2:
        return dictionary
    if word in dictionary:
        return dictionary
    else:
        dictionary[word] = label
        return dictionary

magnitude = dict()
with open("/Users/tunderwood/Dictionaries/PrecisionDictionary.txt", encoding = 'utf-8') as file:
    filelines = file.readlines()

for line in filelines:
    line = line.rstrip()
    fields = line.split('\t')
    magnitude[fields[0]] = float(fields[1])

with open("/Users/tunderwood/Journals/stoplists/ReMergedEtymologies.txt", encoding='utf-8') as file:
    filelines = file.readlines()

etymo = set()

for line in filelines:
    line = line.rstrip()
    fields = line.split('\t')
    if fields[1] == "1":
        etymo.add(fields[0])

with open("/Users/tunderwood/Journals/stoplists/toocommon.txt", encoding='utf-8') as file:
    filelines = file.readlines()

too = set()

for line in filelines:
    line = line.rstrip()
    too.add(line)

print("Only in toocommon:")
print(too.difference(etymo))

print("Only in etymologies:")
print(etymo.difference(too))

print("In both:")
print(too.intersection(etymo))

stoplist = dict()

for word in too.union(etymo):
    stoplist[word] = "englishfunction"

with open("/Users/tunderwood/Journals/stoplists/romannumerals.txt", encoding='utf-8') as file:
    filelines = file.readlines()

for line in filelines:
    stoplist[line.rstrip()] = "romannumeral"

for character in "abcdefghijklmnopqrstuvwxyz":
    stoplist[character] = "alphabet"

with open("/Users/tunderwood/Journals/stoplists/PersonalNames/PersonalNames.txt", encoding='utf-8') as file:
    filelines = file.readlines()

for line in filelines:
    line = line.rstrip()
    line = line.lower()
    stoplist[line] = "name"

with open("/Users/tunderwood/Journals/stoplists/spanish.txt", encoding='utf-8') as file:
    filelines = file.readlines()

for line in filelines[0:200]:
    line = line.rstrip()
    fields = line.split('\t')
    add_if_not_in(stoplist, fields[1], "spanish")

with open("/Users/tunderwood/Journals/stoplists/italian.txt", encoding='utf-8') as file:
    filelines = file.readlines()

for line in filelines[0:200]:
    line = line.rstrip()
    fields = line.split('\t')
    add_if_not_in(stoplist, fields[1], "italian")

with open("/Users/tunderwood/Journals/stoplists/latin.txt", encoding='utf-8') as file:
    filelines = file.readlines()

for line in filelines[0:200]:
    line = line.rstrip()
    fields = line.split('\t')
    add_if_not_in(stoplist, fields[0], "latin")

with open("/Users/tunderwood/Journals/stoplists/fr.txt", encoding='utf-8') as file:
    filelines = file.readlines()

for line in filelines:
    line = line.rstrip()
    add_if_not_in(stoplist, line, "french")

with open("/Users/tunderwood/Journals/stoplists/de.txt", encoding='utf-8') as file:
    filelines = file.readlines()

for line in filelines:
    line = line.rstrip()
    add_if_not_in(stoplist, line, "german")

with open("/Users/tunderwood/Journals/stoplists/UnderwoodList.txt", encoding='utf-8') as file:
    filelines = file.readlines()

for line in filelines:
    line = line.rstrip()
    add_if_not_in(stoplist, line, "inUnderwood")

with open("/Users/tunderwood/Journals/stoplists/GoldstoneList.txt", encoding='utf-8') as file:
    filelines = file.readlines()

for line in filelines:
    line = line.rstrip()
    add_if_not_in(stoplist, line, "inGoldstone")

categories = ['alphabet', 'englishfunction', 'inGoldstone', 'inUnderwood', 'french', 'german', 'italian', 'latin', 'spanish', 'romannumeral', 'name']

with open("stoplist.txt", mode="a", encoding = 'utf-8') as file:
    for category in categories:
        thiscat = list()
        for key, value in stoplist.items():
            if value == category:
                if key in magnitude:
                    freq = magnitude[key]
                else:
                    freq = 0
                thiscat.append((freq, key))
        thiscat = sorted(thiscat, reverse = True)
        for entry in thiscat:
            file.write(entry[1] + '\t' + category + '\n')
    

