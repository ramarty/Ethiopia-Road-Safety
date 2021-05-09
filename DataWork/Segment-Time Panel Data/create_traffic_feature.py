import pandas as pd
import datetime
import math
from datetime import datetime, timedelta
pd.set_option('display.max_rows', 1000)
pd.set_option('display.max_columns', 100)

# define paths
data_path = '/Users/simonneumeyer/Dropbox/Ethiopia IE - Road Safety/Data/'
traffic_path = 'ETRE - Traffic/'
traffic_file = 'FinalData/traffic.pq'
segment_time_file = 'Time Segment Data/segment_time_panel.pq'

# functions
def datetime_range(start, end, delta):
    current = start
    while current <= end:
        yield current
        current += delta

def transform_row(row):
    # preprocess:
    ent_date = datetime.strptime(str(row['ent_occur_time']), "%Y-%m-%d %H:%M:%S")
    exit_date = datetime.strptime(str(row['trans_occur_time']), "%Y-%m-%d %H:%M:%S")
    n_km = row['exit_km'] - row['entrance_km']
    speed_sec_km = (exit_date - ent_date).total_seconds() / abs(n_km)
    speed_sec_km = math.floor(speed_sec_km)

    new_index = [pd.to_datetime(dt.strftime("%Y-%m-%d %H:%M:%S")) for dt in
                 datetime_range(ent_date, exit_date, timedelta(seconds=speed_sec_km))]

    import pdb
    pdb.set_trace()


    # define step direction (for when entrance_km > exit_km) for km_count:
    step_direction = int(n_km / abs(n_km))
    km_count = [j for j in range(int(row['entrance_km']), int(row['exit_km'] + step_direction), step_direction)]

    # create new dataframe to conduct aggregation on later:
    dt_one = pd.DataFrame(km_count, index=new_index[:len(km_count)], columns=['km'])
    dt_one['hour'] = dt_one.index.hour
    dt_one['year'] = dt_one.index.year
    dt_one['month'] = dt_one.index.month
    dt_one['day'] = dt_one.index.day
    dt_one['tstamp'] = pd.to_datetime(dt_one[['year', 'month', 'day']])
    dt_one['count'] = oversampling_factor

    # if we also want to know which road this car was on, we can add this extra column:
    # dt_one['road'] = s.direction

    dt_one_tr = dt_one.set_index(['tstamp', 'hour', 'km'])['count']  # could add road

    return dt_one_tr


def main(data, frac):
    # Run over a random subset of the data:
    if frac is not None:
        data = data.sample(frac=frac)

    traffic = pd.concat(
        {
            i: transform_row(data.loc[i, :]) for i in data.index
        }, axis=0, names=['car', 'tstamp', 'hour', 'km'])  # could add road

    results = traffic.groupby(['tstamp', 'hour', 'km']).sum()  # could add road
    return results


# load data
traffic_final = pd.read_parquet(data_path + traffic_path + traffic_file, engine='pyarrow')
time_segment = pd.read_parquet(data_path + segment_time_file, engine='pyarrow')

# preprocess
# formatting:
traffic_final.ent_occur_time = pd.to_datetime(traffic_final.ent_occur_time)
traffic_final.trans_occur_time = pd.to_datetime(traffic_final.trans_occur_time)

# dropping rows where exit_km = entrance_km (0.01% of the data)
traffic_final = traffic_final[traffic_final.entrance_km!=traffic_final.exit_km]

# create km_from_addis variable:
#traffic_final['entr_km_from_addis'] = traffic_final.entrance_km.apply(lambda x: 78.0-x)
#traffic_final['exit_km_from_addis'] = traffic_final.exit_km.apply(lambda x: 78.0-x)

# drop timestamp NAs:
traffic_final = traffic_final.dropna(subset=['ent_occur_time', 'trans_occur_time'])
traffic_final = traffic_final.reset_index(drop=True)

# consider only the relevant variables:
traffic_final = traffic_final[['ent_occur_time', 'trans_occur_time', 'entrance_km', 'exit_km']]

# kick out rows where exit time is before entrance time or less then 1 min after:
index_to_keep = [i for i, x in enumerate(traffic_final.trans_occur_time-traffic_final.ent_occur_time) if x.total_seconds() > 60]
traffic_final = traffic_final.loc[index_to_keep, :]
traffic_final = traffic_final.reset_index(drop=True)

# run the main function:
oversampling_factor = 20
a = main(traffic_final, frac=1/oversampling_factor)

a = a.reset_index()
a.to_parquet(data_path + 'Time Segment Data/traffic_feature.pq')