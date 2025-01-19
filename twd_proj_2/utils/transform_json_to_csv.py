import pandas as pd
import json
from datetime import datetime, timedelta

def transform_place_location(place_location):
    # if place location is str
    if isinstance(place_location, str):
        place_location = place_location.replace("geo:", "")

    elif isinstance(place_location, dict):
        place_location = place_location["latLng"].replace("\u00b0", "")

    lat, lon = place_location.split(",")
    return lat.strip(), lon.strip()

def json_to_dataframe(data):
    rows = []
    for entry in data:
        try:
            place_location = entry["visit"]["topCandidate"]["placeLocation"]
            lat, lon = transform_place_location(place_location)
            row = {
                "endTime": entry["endTime"],
                "startTime": entry["startTime"],
                "hierarchyLevel": int(entry["visit"]["hierarchyLevel"]),
                "visitProbability": float(entry["visit"]["probability"]),
                "topCandidateProbability": float(entry["visit"]["topCandidate"]["probability"]),
                "semanticType": entry["visit"]["topCandidate"]["semanticType"],
                "placeID": entry["visit"]["topCandidate"].get("placeID", entry["visit"]["topCandidate"].get("placeId")),
                "latitude": lat,
                "longitude": lon,
            }
            rows.append(row)
        except (KeyError, TypeError, ValueError):
            continue

    df = pd.DataFrame(rows)
    start_date = datetime(2024, 12, 9)

    if not df.empty:
        df["startTime"] = pd.to_datetime(df["startTime"]).dt.tz_localize(None)

        df["relativeWeekNum"] = ((pd.to_datetime(df["startTime"]) - start_date).dt.days // 7) + 1

    return df

def transform_to_csv(filename):
    with open(f"../data/{filename}.json", 'r') as file:
        data = json.load(file)

    df = json_to_dataframe(data)
    df.to_csv(f"../data/{filename}.csv", index=False)


if __name__ == '__main__':
    transform_to_csv('Os_czasu_kl')
    transform_to_csv('Os_czasu_jo')
    transform_to_csv('Os_czasu_mi')

    df = pd.read_csv("../data/Os_czasu_kl.csv")
    df = df[["latitude", "longitude", "placeID"]].drop_duplicates()

    df.to_csv("../data/places_mapping_kl.csv", index=False)



