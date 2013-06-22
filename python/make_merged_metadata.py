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

directories = ["~/Journals/ELH-CI", "~/Journals/MLR/mlr1905-1970",
"~/Journals/MLR/mlr1971-2013", "~/Journals/ModPhil", "~/Journals/NLH",
"~/Journals/PMLA", "~/Journals/RES/RESto1980",
"~/Journals/RES/RESfrom1981"]

mergedlist = list()

for adirectory in directories:
    records = get_one_file(adirectory)
    mergedlist.extend(records)

with open("/Users/tunderwood/Journals/tmhls/merged_metadata.tsv", mode = 'w', encoding = 'utf-8') as file:
    file.write('id\tauthor\ttitle\tdate\tjournaltitle\n')
    for atuple in mergedlist:
        outline = '\t'.join(atuple) + '\n'
        file.write(outline)
        fields = outline.split('\t')
        if len(fields) != 5:
            print(len(fields))
            print(outline)
    
