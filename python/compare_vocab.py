
def get_summed_dictionary(a_directory):

    vocab = list()
    
    with open(a_directory + "/vocab.txt", encoding = 'utf-8') as file:
        filelines = file.readlines()

    for line in filelines:
        vocab.append(line.rstrip())

    summed_dictionary = dict()
    for word in vocab:
        summed_dictionary[word] = 0

    with open(a_directory + "/topic_words.csv", encoding = 'utf-8') as file:
        filelines = file.readlines()

    for line in filelines:
        line = line.rstrip()
        fields = line.split(',')
        assert len(fields) == len(vocab)
        for idx, field in enumerate(fields):
            if field == "0":
                continue
            else:
                word = vocab[idx]
                summed_dictionary[word] += int(field)

    return summed_dictionary

old_dict = get_summed_dictionary("/Users/tunderwood/Journals/Uresults/HLSk300v12000")
new_dict = get_summed_dictionary("/Users/tunderwood/Journals/Uresults/HLS")

tuplelist = list()
for word, count in old_dict.items():
    if word not in new_dict:
        tuplelist.append((-old_dict[word], word))
    elif old_dict[word] == new_dict[word]:
        continue
    else:
        tuplelist.append((new_dict[word] - old_dict[word], word))

for word, count in new_dict.items():
    if word not in old_dict:
        tuplelist.append((new_dict[word], word))

tuplelist = sorted(tuplelist)

with open("/Users/tunderwood/Journals/Uresults/Uk2USdiff.tsv", mode='w', encoding = 'utf-8') as file:
    for atuple in tuplelist:
        count, word = atuple
        file.write(word + '\t' + str(count) + '\n')

with open("/Users/tunderwood/Journals/Uresults/vocabsum.tsv", mode='w', encoding = 'utf-8') as file:
    for word, sumtotal in old_dict.items():
        file.write(word + '\t' + str(sumtotal) + '\n')
