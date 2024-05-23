import pandas as pd


df = pd.read_csv(r"C:\Users\16155\Desktop\IEMS 341 - Social Network Analysis\Prestige_Data\orchestra_prestige.csv")

prestigious_schools = list(df.loc[df['Prestige'] == 1, 'School'])

df = pd.read_csv(r"C:\Users\16155\Desktop\IEMS 341 - Social Network Analysis\Prestige_Data\output.csv")

count_prestige = 0
count_non_prestige = 0
total_count = 0
for alma_matters in list(df['Schools']):
    if type(alma_matters) is not str:
        total_count -= 1
    elif not any([x in alma_matters for x in prestigious_schools]):
        count_non_prestige += 1
    elif any([x in alma_matters for x in prestigious_schools]):
        count_prestige += 1
    total_count += 1

print(count_prestige)
print(count_non_prestige)
print(total_count)
