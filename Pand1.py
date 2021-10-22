import pandas as pd
import os

filedir = "C:/Users/Ivan-Korostenskij/Desktop/R/Tahimic/DP/1001 Feature"
os.chdir(filedir)
bp = True
for file in os.listdir(filedir):
    name = os.path.splitext(file)[0]
    if bp:
        df = pd.read_table(file, names = ["Identifier", name])
        bp = False
    else:
        dff = pd.read_table(file, names = ["Identifier", name])
        df = df.join(dff[name])

print(df.head)

df.to_csv("finaldf.csv", index = False)
    





