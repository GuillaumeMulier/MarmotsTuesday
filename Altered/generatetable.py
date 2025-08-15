import os
import pandas as pd

df = pd.concat((pd.read_excel("Altered/Uniques/" + f) for f in os.listdir("Altered/Uniques")), ignore_index = True)

df.to_excel("Altered/uniques_lyra.xlsx", index = False)