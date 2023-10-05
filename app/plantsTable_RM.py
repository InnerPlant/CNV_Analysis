import pandas as pd
from pyairtable import Table, Api


API_KEY = "patayfOCKmsDBBRDF.e4f73fcd9ae5895c330ec89d132621f7a69a7758d0aca54541bb14747b30e955"
base_id = "appT85lgku4GeM5js"
plants_table = "tblklih33YcELazvj"

plants_tracking_table = Table(API_KEY, base_id, plants_table)
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

