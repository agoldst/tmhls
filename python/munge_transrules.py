with open("/Users/tunderwood/Journals/tmhls/UK2UStransrules.txt", encoding = 'utf-8') as file:
    filelines = file.readlines()

with open("/Users/tunderwood/Journals/tmhls/UK2UStransrules.csv", mode='w', encoding = 'utf-8') as file:
    file.write("BRITISH,AMERICAN\n")
    for line in filelines:
        line = line.replace('\t',',')
        file.write(line)
    
