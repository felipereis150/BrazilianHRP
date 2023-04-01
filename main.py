import pandas as pd
import numpy as np
import rolling_hrp as rol
import HRP

df = pd.read_excel("data/ibrx_mensal.xlsx", na_values="-")

# transforming the 'Data' column to a date format
df['Data'] = df['Data'].str.replace('Jan', '01').str.replace('Fev', '02').str.replace('Mar', '03').str.replace('Abr', '04').str.replace('Mai', '05').str.replace('Jun', '06').str.replace('Jul', '07').str.replace('Ago', '08').str.replace('Set', '09').str.replace('Out', '10').str.replace('Nov', '11').str.replace('Dez', '12')
df['Data'] = pd.to_datetime(df['Data'], format='%m-%Y')

df = df[(df['Data'] >= '2000-01-01') & (df['Data'] <= '2020-02-01')]
# df.set_index(df['Data'], inplace=True)

# replacing NA values with 0
df = df.dropna(axis=1)

df2 = df.iloc[:, 1:]
Rport = rol.rolling_window_HRP(df2, 120)

print(Rport)
