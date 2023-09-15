import pandas as pd
from pyairtable import Table, Api


API_KEY = "patTOLUCI6iSTDQFf.6eed362d9062b32b94085795879df3b2f03cbca981779d986326311355dfb6d6"
base_id_cnv = "appi9YExYI9zlkdry"
cnv_table = "tbloaDQ3UFWzrnZBh"
base_id_plants = "appT85lgku4GeM5js"
plants_table = "tblklih33YcELazvj"

CNV_tracking_table = Table(API_KEY, base_id_cnv, cnv_table)
cn = CNV_tracking_table.all(
fields=[
"Barcode",
"Copy_Number_Mean_bflo",
"Copy_Number_bFLO",
"Copy_Number_Mean_Selection",
"Copy_Number_Selection",
]
)
df_cnv = pd.DataFrame.from_records(cn)
if not df_cnv.empty:
	df_cnv = pd.concat(
	[df_cnv, pd.DataFrame.from_records(list(df_cnv["fields"]))], axis=1
	)
df_cnv.drop(["fields", "createdTime"], inplace=True, axis=1)

#df_cnv["Copy_Number_Mean_bflo"] = df_cnv["Copy_Number_Mean_bflo"].str[0]
#df_cnv["Copy_Number_bFLO"] = df_cnv["Copy_Number_bFLO"].str[0]
#df_cnv["Copy_Number_Mean_Selection"] = df_cnv["Copy_Number_Mean_Selection"].str[0]
#df_cnv["Copy_Number_Selection"] = df_cnv["Copy_Number_Selection"].str[0]

plants_tracking_table = Table(API_KEY, base_id_plants, plants_table)
pt = plants_tracking_table.all(
fields=[
"Barcode",
"Parent Plant Line",
"Generation",
]
)
df_plant = pd.DataFrame.from_records(pt)
if not df_plant.empty:
	df_plant = pd.concat(
	[df_plant, pd.DataFrame.from_records(list(df_plant["fields"]))], axis=1
	)
df_plant.drop(["fields", "createdTime"], inplace=True, axis=1)
df_plant["Generation"] = df_plant["Generation"].str[0]
df_plant["Parent Plant Line"] = df_plant["Parent Plant Line"].str[0]


    # uncomment this to save the table as a csv file
    #df_plant.to_csv("plantTable.csv", index=False)
