import pandas as pd

def add_place_names(name):
    df = pd.read_csv(f"../data/places/places_{name}.csv")
    df.drop(columns=["latitude", "longitude"], inplace=True)

    map = pd.read_csv(f"../data/places_mapping_{name}.csv")

    df_merged = df.merge(map, on="placeID", how="left")
    df_merged["person"] = name

    return df_merged

def merge_csv_with_place_names():
    df_kl = add_place_names("kl")
    df_mi = df_kl.replace("kl", "mi")
    df_jo = df_kl.replace("kl", "jo")

    # df_jo = add_place_names("jo")
    # df_mi = add_place_names("mi")
    # TODO: Uncomment above lines when the files are ready

    df = pd.concat([df_kl, df_jo, df_mi])

    df.to_csv("../data/merged_data.csv", index=False)

def merge_csv_travel():
    df_kl = pd.read_csv("../data/travel/travel_kl_travel.csv")
    df_kl["person"] = "kl"
    df_mi = pd.read_csv("../data/travel/travel_mi_travel.csv")
    df_mi["person"] = "mi"
    df_jo = pd.read_csv("../data/travel/travel_jo_travel.csv")
    df_jo["person"] = "jo"

    df = pd.concat([df_kl, df_jo, df_mi])

    df.to_csv("../data/merged_travel.csv", index=False)

if __name__ == '__main__':
    merge_csv_with_place_names()
    merge_csv_travel()