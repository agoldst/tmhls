with open("/Users/tunderwood/Journals/MLR/mlr1971-2013/citations.CSV", encoding = 'utf-8') as file:
    filelines = file.readlines()

with open("/Users/tunderwood/Journals/MLR/mlr1971-2013/mistakes.CSV", mode='w', encoding = 'utf-8') as file:
    for line in filelines[1:]:
        if len(line) < 3:
            continue
        fields = line.split('\t,')
        if fields[1] == "Review" and fields[9] == "fla":
            file.write(line)
        
    
