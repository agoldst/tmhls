# make_merged_metadata.py

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
        title = fields[1].replace('\t', ' - ')
        author = fields[2].replace('\t', ' - ')
        date = (fields[6][0:4])
        journaltitle = fields[3]
        testing = int(date)
        # just taking the year
        listoftuples.append((article_id, author, title, date, journaltitle))

    return(listoftuples)

directories = ["~/Journals/AHR/AHR02-07", "~/Journals/AHR/AHR46-66",
"~/JournaLS/AHR/AHR67-76", "~/Journals/AHR/AHR77-84",
"~/Journals/AHR/AHR85-92", "~/Journals/AHR/AHR93-01",
"~/Journals/AHR/to1945"]

mergedlist = list()

for adirectory in directories:
    records = get_one_file(adirectory)
    mergedlist.extend(records)

with open("/Users/tunderwood/Journals/tmhls/AHR_metadata.tsv", mode = 'w', encoding = 'utf-8') as file:
    file.write('id\tauthor\ttitle\tdate\tjournaltitle\n')
    for atuple in mergedlist:
        outline = '\t'.join(atuple) + '\n'
        file.write(outline)
        fields = outline.split('\t')
        if len(fields) != 5:
            print(len(fields))
            print(outline)
    
