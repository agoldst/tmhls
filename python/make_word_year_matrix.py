# make_word_year_matrix.py
import glob

def get_one_file (filepath):
    filepath = filepath.replace('~', '/Users/tunderwood')
    filepath = filepath + "/citations.CSV"
    
    with open(filepath, encoding = 'utf-8') as file:
        filelines = file.readlines()

    listoftuples = list()
    for line in filelines[1:]:
        fields = line.split('\t,')
        if len(fields) < 2:
            continue
        article_id = fields[0].split(',')[0]
        articletype = fields[9]
        if articletype != 'fla':
            continue
        title = fields[1].replace('\t', ' - ')
        author = fields[2].replace('\t', ' - ')
        date = (fields[6][0:4])
        journaltitle = fields[3]
        testing = int(date)
        # just taking the year
        listoftuples.append((article_id, author, title, date, journaltitle))

    return(listoftuples)

directories = ["~/Journals/ELH-CI", "~/Journals/MLR/mlr1905-1970",
"~/Journals/MLR/mlr1971-2013", "~/Journals/ModPhil", "~/Journals/NLH",
"~/Journals/PMLA", "~/Journals/RES/RESto1980",
"~/Journals/RES/RESfrom1981"]

docdates = dict()

for adirectory in directories:
    records = get_one_file(adirectory)
    for record in records:
        article_id, author, title, date, journaltitle = record
        docdates[article_id] = int(date)

stoplist = set()
with open("/Users/tunderwood/Journals/tmhls/stoplist_final.txt", mode = 'r', encoding = 'utf-8') as file:
    filelines = file.readlines()

for line in filelines:
    line = line.rstrip()
    stoplist.add(line)

transrules = dict()
with open("/Users/tunderwood/Journals/tmhls/UK2UStransrules.csv", mode = 'r', encoding = 'utf-8') as file:
    filelines = file.readlines()

for line in filelines[1:]:
    line = line.rstrip()
    fields = line.split(',')
    transrules[fields[0]] = fields[1]
    
def get_one_folder (dirpath):
    global docdates, stoplist, transrules
    dirpath = dirpath.replace('~', '/Users/tunderwood')
    dirpath = dirpath + "/wordcounts/*.CSV"
    filelist = glob.glob(dirpath)
    print(len(filelist))

    folder_dictionary = dict()

    for filepath in filelist:
        slashparts = filepath.split('/')
        fileid = slashparts[-1][11:-4]
        fileid = fileid.replace('_', '/')
        if fileid in docdates:
            date = docdates[fileid]
        else:
            continue
    
        with open(filepath, encoding = 'utf-8') as file:
            filelines = file.readlines()

        for line in filelines[1:]:
            line = line.rstrip()
            fields = line.split(',')
            word = fields[0]
            if word in stoplist:
                continue
            if word in transrules:
                word = transrules[word]
            
            count = int(fields[1])
            if (word, date) in folder_dictionary:
                folder_dictionary[(word, date)] += count
            else:
                folder_dictionary[(word, date)] = count

    return(folder_dictionary)

directories = ["~/Journals/ELH-CI", "~/Journals/MLR/mlr1905-1970",
"~/Journals/MLR/mlr1971-2013", "~/Journals/ModPhil", "~/Journals/NLH",
"~/Journals/PMLA", "~/Journals/RES/RESto1980",
"~/Journals/RES/RESfrom1981"]

maindictionary = dict()

minyear = 3000
maxyear = 0

totalfreqs = dict()
yearlysums = dict()

for adirectory in directories:
    print(adirectory)
    records = get_one_folder(adirectory)
    for key, value in records.items():
        
        if key in maindictionary:
            maindictionary[key] += value
        else:
            maindictionary[key] = value

        word, year = key
        if year < minyear:
            minyear = year
        if year > maxyear:
            maxyear = year

        if word in totalfreqs:
            totalfreqs[word] += value
        else:
            totalfreqs[word] = value

        if year in yearlysums:
            yearlysums[year] += value
        else:
            yearlysums[year] = value

listoftuples = list()
for word, count in totalfreqs.items():
    listoftuples.append((count,word))

listoftuples = sorted(listoftuples, reverse = True)

vocab = listoftuples[0:100000]

with open("/Users/tunderwood/Journals/tmhls/hls_wordcounts.tsv", mode = 'w', encoding = 'utf-8') as file:
    listofyears = [str(x) for x in range(minyear, (maxyear + 1))]
    yearrange = len(listofyears)
    header = "word\ttotal\t" + "\t".join(listofyears) + "\n"
    file.write(header)
    for atuple in vocab:
        total_count, word = atuple
        listofyears = list()
        for year in range(minyear, (maxyear + 1)):
            if (word, year) in maindictionary:
                count = maindictionary[(word, year)]
            else:
                count = 0
                
            listofyears.append(str(count))

        assert(len(listofyears) == yearrange)

        outline = word + '\t' + str(total_count) + '\t' + '\t'.join(listofyears) + '\n'
        file.write(outline)

yearlyoutput = list()
for year, count in yearlysums.items():
    yearlyoutput.append((year,count))

yearlyoutput = sorted(yearlyoutput)

with open("/Users/tunderwood/Journals/tmhls/hls_yearlycounts.tsv", mode = 'w', encoding = 'utf-8') as file:
    file.write('year\tsumtotal\n')
    for atuple in yearlyoutput:
        year, count = atuple
        outline = str(year) + '\t' + str(count) + '\n'
        file.write(outline)
        

    
