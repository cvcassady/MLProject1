import pandas as pd
import os
import urllib.request

os.chdir(r"C:\Users\Colin\Documents\Machine Learning\Project1\MLProject1")

apikey = '11d522090a4daf456917a11fc699790075871524'

shootings = pd.read_csv("fatal-police-shootings-data.csv")
places = pd.read_csv("national_places.csv", sep = ";", encoding='latin-1', dtype={'STATEFP':str, 'PLACEFP':str})

shootings['qfcode'] = shootings.apply(lambda x: getCensusCode(x), axis=1)

shootings.groupby('state').count()

pd.options.display.max_rows

shootings.apply(lambda x: x["city"], axis=1)

shootings['state'][1:5]
places[places.STATE == 'OK']

def getCensusCode(x):
    places_in_state = places[places.STATE == x.state]
    matches = places_in_state[places_in_state['PLACENAME'].str.contains(x.city, case=False)]
    if matches.shape[0] == 0:
        return 0
    matches_city = matches[matches['PLACENAME'].str.contains("city", case=False)]
    if matches_city.shape[0] == 0:
        return matches.iloc[0]['STATEFP'] + matches.iloc[0]['PLACEFP']
    return matches_city.iloc[0]['STATEFP'] + matches_city.iloc[0]['PLACEFP']

qfcodes = shootings['qfcode'].unique()


for i in range(0,233):
    current_codes = ','.join([str(x) for x in qfcodes[(i*5):((i+1)*5)]])
    url = r"https://www.census.gov/quickfacts/download.php?fips=" + current_codes + r"&type=csv"
    raw_text = urllib.request.urlopen(url, timeout = 300).read().decode('utf8')
    raw_text = raw_text[0:(raw_text.find("This g") - 1)]
    f = open('censusdata/' + current_codes[1:10] + '.csv', 'w')
    f.write(raw_text)
    f.close()

dfs = []

for f in files:
    test_csv = pd.read_csv('censusdata/' + f, encoding='latin-1').T
    test_csv.columns = test_csv.iloc[0]
    dfs.append(test_csv)

census = pd.concat(dfs)