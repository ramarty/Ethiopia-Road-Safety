import pandas as pd
pd.set_option('display.max_rows', 1000)
pd.set_option('display.max_columns', 100)
import numpy as np
import pyreadr
import datetime
import statsmodels.api as sm
import matplotlib.pyplot as plt
from linearmodels.panel import PanelOLS

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

# kick out NAs: (where we don't have accident data)
segment_time = segment_time.dropna(subset=['accidents'])
segment_time = segment_time.reset_index()


# add traffic:
traffic = pd.read_parquet(data_path + 'Time Segment Data/traffic_feature.pq', engine='pyarrow')

# create km_from_addis variable:
traffic['km_from_addis'] = traffic.km.apply(
       lambda x: 1000 * (80.0-x))  # 80: assuming km in traffic data are from adama, not addis.
traffic = traffic.rename({'tstamp': 'date', 'count': 'traffic'}, axis=1)

# show why we drop some data:
traffic.plot(x='date', y='traffic')
plt.show()

cond_1 = (traffic.date > pd.to_datetime('2014-11-30'))
cond_2 = (traffic.date < pd.to_datetime('2015-04-30'))
traffic[cond_1 & cond_2].plot(x='date', y='traffic')
plt.show()
# It seems that there is a 2-month gap of traffic data for January & February 2015.
# We shall consider only traffic data after February 2015 (Before 2015 there is no accidents data anyway)

# drop accident data older than March 2015:
condition = (segment_time.date >= pd.to_datetime('2015-03-01'))
segment_time = segment_time[condition]

# merge traffic with accidents:
segment_time = segment_time.merge(traffic, how='left', on=['date', 'hour', 'km_from_addis'])

# Now that we removed missing data we can safely assume that remaining traffic NAs are incidents of
# no traffic in that particular time-km segment:
segment_time['traffic'] = segment_time['traffic'].fillna(0)


# add hourly data:
hourly = pd.read_stata(data_path + 'Daily and Hourly Data/FinalData/hourly.dta')
hourly['hour'] = hourly.date_hour.apply(lambda x: x.hour)
hourly = hourly.drop('precip_mm', axis=1)  # because we have it already in the master dataset

# aggregate both directions (subject to change):
hourly = hourly.groupby('date_hour').agg(
    {
    'speed_mean': 'mean', 'speed_p10': 'mean', 'crash': 'sum', 'speed_p25': 'mean',
        'speed_p50': 'mean', 'speed_p75': 'mean', 'speed_p90': 'mean',
        'N_crashes': 'sum', 'N_vehicles': 'sum', 'holiday': 'mean',
        'holiday_plusminus_1day': 'mean', 'holiday_plusminus_2day': 'mean',
        'hour': 'mean', 'date': 'min'
    }
).reset_index()

# merge with time-segment panel:
segment_time = segment_time.merge(hourly, how='left', on=['date', 'hour'], validate='many_to_one')


# add additional features:
# weekend dummy:
segment_time['weekday'] = segment_time.date.apply(lambda x: x.weekday())
segment_time = pd.get_dummies(segment_time, columns=['weekday'])
segment_time['weekend'] = segment_time.apply(
       lambda x: sum([x.weekday_4, x.weekday_5, x.weekday_6]), axis=1)


# save dataset:
segment_time.to_parquet(data_path + 'Time Segment Data/segment_time_panel.pq')