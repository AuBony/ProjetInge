

for k in range(len(X)):
    with open('Xchat/Xchat_'+str(k+1)+'.csv', 'w', newline='') as csvfile:
        spamwriter = csv.writer(csvfile, delimiter=';')
        for i in range(len(X[k])):
            spamwriter.writerow(X[k,i])


with open('ychat.csv', 'w', newline='') as csvfile:
    spamwriter = csv.writer(csvfile, delimiter=';')
    for i in range(len(y)):
        spamwriter.writerow(y[i])
