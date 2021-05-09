import pandas as pd
pd.set_option('display.max_rows', 1000)
pd.set_option('display.max_columns', 100)

# define paths:
data_path = '/Users/simonneumeyer/Dropbox/Ethiopia IE - Road Safety/Data/'
crashes_path = 'ETRE - Crashes/'
crash_file = 'FinalData/crashes.csv'
traffic_path = 'ETRE - Traffic/'
traffic_file = 'FinalData/traffic.pq'
precipitation_path = 'Precipitation/'
precipitation_file = 'FinalData/precipitation.csv'

# load data
traffic_final = pd.read_parquet(data_path + traffic_path + traffic_file, engine='pyarrow')
precipitation_final = pd.read_csv(data_path + precipitation_path + precipitation_file)
crashes_final = pd.read_csv(data_path + crashes_path + crash_file)

# rename date & hour:
crashes_final = crashes_final.rename(columns={'accident_date': 'date', 'time_of_accident_hour': 'hour'})

# create rectangularized segment-time dataset:
date_range = pd.date_range(precipitation_final.date.min(), precipitation_final.date.max())
date_range = list(date_range)
hour_range = list(range(0,24))
km_range = [1000*x for x in range(0,79)]
segment_time = pd.DataFrame([[x,y,z] for x in date_range for y in hour_range for z in km_range])
segment_time = segment_time.rename(columns={0:'date', 1:'hour', 2:'km_from_addis'})

# create accident counts and add them to segment-time dataset:
crashes_final['accidents'] = 1
crashes_final['km_from_addis'] = crashes_final.distance_from_addis.round(-3)
accid_count = crashes_final.groupby(['date', 'hour', 'km_from_addis'])['accidents'].count()
accid_count = accid_count.reset_index()
accid_count.date = pd.to_datetime(accid_count.date)
segment_time = segment_time.merge(accid_count, how='left', on=['date', 'hour', 'km_from_addis'])

# add precipitation:
precipitation_final.date = pd.to_datetime(precipitation_final.date)
segment_time = segment_time.merge(precipitation_final[['date', 'precip_mm']], on='date', how='left')

# fill missing data with 0 only for the date range where we have crashes data (2015-2017)
crashes_start_date = pd.to_datetime('2015-01-01')
crashes_end_date = pd.to_datetime(crashes_final.date).max()
condition = (segment_time['date'] >= crashes_start_date) & (segment_time['date'] <= crashes_end_date)
segment_time.loc[condition, 'accidents'] = segment_time.loc[condition, 'accidents'].fillna(0)

# merging accident data into it:
relevant_crash_vars = ['distance_from_addis', 'case_no', 'date', 'day', 'hashed_plate_number',
       'vehicle_type', 'vehicle_brand', 'direction',
       'road_geometry', 'road', 'weather', 'fatality', 'serious_injury',
       'slight_injury', 'cause_of_accident', 'type_of_accident',
       'year_of_production', 'owner', 'driver_age', 'gender', 'address',
       'drivers_license_level', 'license_year', 'license_region', 'experience',
       'relation_with_vehicle', 'etre_asset_damage', 'ownership',
       'extent_of_damage', 'year', 'hour',
       'time_of_accident_minute', 'accident_datetime',
       'accident_location_original', 'accident_location_text', 'latitude',
       'longitude', 'axle_number', 'cause_of_accident_simple',
       'type_of_accident_simple', 'accident_cause_vehicle_human',
       'km_from_addis']
crashes_final.date = pd.to_datetime(crashes_final.date)
segment_time = segment_time.merge(crashes_final[relevant_crash_vars], how='left', on=['date', 'hour', 'km_from_addis'])

# add crashes:





# save dataset:
segment_time.to_parquet(data_path + 'Time Segment Data/segment_time_panel.pq')