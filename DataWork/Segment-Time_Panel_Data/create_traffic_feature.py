import pandas as pd
import datetime
import math
from datetime import datetime, timedelta

# define paths
data_path = '/Users/simonneumeyer/Dropbox/Ethiopia IE - Road Safety/Data/'
traffic_path = 'ETRE - Traffic/'
traffic_file = 'FinalData/traffic.pq'
segment_time_file = 'Time Segment Data/segment_time_panel.pq'

# choice of segment granularity BEFORE aggregation (for speeding up purposes) :
granularity_km_ex_ante = None
granularity_hour_ex_ante = None

# choice of segment granularity AT aggregation:
granularity_km_ex_post = 1
granularity_hour_ex_post = 1


# functions

def rounder(x, prec=2, base=.05):
    return round(base * round(float(x) / base), prec)


def datetime_range(start, end, delta):
    current = start
    while current <= end:
        yield current
        current += delta


def interpolate_row(row):
    """
    This function takes as input a traffic observation (a car entering & exiting the highway at a
    specific km and hour), interpolates the time at each kilometer with average speed
    and creates and returns a dataframe with all the inferred km-hour combinations as observations
    which can then easily be aggregated in the main function.
    :param row: a car entering & exiting the highway at a specific km and hour
    :return: dataframe with all the inferred km-hour combinations as observations
    """
    # preprocess:
    ent_date = datetime.strptime(str(row['ent_occur_time']), "%Y-%m-%d %H:%M:%S")
    exit_date = datetime.strptime(str(row['trans_occur_time']), "%Y-%m-%d %H:%M:%S")
    n_km = row['exit_km'] - row['entrance_km']

    # create seconds per kilometer variable to interpolate the time at each kilometer:
    speed_sec_km = (exit_date - ent_date).total_seconds() / abs(n_km)
    speed_sec_km = math.floor(speed_sec_km)

    new_index = [pd.to_datetime(dt.strftime("%Y-%m-%d %H:%M:%S")) for dt in
                 datetime_range(ent_date, exit_date, timedelta(seconds=speed_sec_km))]

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
    dt_one['direction'] = row['direction']
    dt_one['cars'] = oversampling_factor * row['cars']
    dt_one['total_weight'] = oversampling_factor * row['total_weight']
    dt_one['speed_km_hr'] = oversampling_factor * row['speed_km_hr']
    dt_one['speed_sec_km'] = oversampling_factor * speed_sec_km

    for i in range(1, 8):
        dt_one[f'veh_type_{i}'] = oversampling_factor * row[f'veh_type_{i}']

    dt_one_tr = dt_one.set_index(['tstamp', 'hour', 'km', 'direction'])[['cars', 'total_weight', 'speed_km_hr',
                                                                         'veh_type_1', 'veh_type_2', 'veh_type_3',
                                                                         'veh_type_4', 'veh_type_5', 'veh_type_6',
                                                                         'veh_type_7', 'speed_sec_km']]

    return dt_one_tr


def main(data, frac):
    """
    This function does 3 things:
    1. Performs, if needed, undersampling of the traffic file
    2. Performs the interpolate_row function on every row of the traffic dataset
        and concatenates the outputs.
    3. Aggregates by hour & km to return: final traffic feature,
    vehicle type proportions, avg. speed & avg. weight per car
    :param data: traffic data
    :param frac: fraction of random sample of traffic data
    :return: traffic feature dataframe
    """
    # Run over a random subset of the data:
    if frac is not None:
        data = data.sample(frac=frac)

    traffic = pd.concat(
        {
            i: interpolate_row(data.loc[i, :]) for i in data.index
        }, axis=0, names=['car', 'tstamp', 'hour', 'km', 'direction'])

    # ex post aggregation if needed:
    traffic = traffic.reset_index()

    if granularity_km_ex_post is not None:
        traffic['km'] = traffic.km.apply(lambda x: rounder(x, prec=2, base=granularity_km_ex_post))
    if granularity_hour_ex_post is not None:
        traffic['hour'] = traffic.hour.apply(lambda x: rounder(x, prec=2, base=granularity_hour_ex_post))

    traffic = traffic.set_index(['tstamp', 'hour', 'km', 'direction'])[['cars', 'total_weight', 'speed_km_hr',
                                                                        'veh_type_1', 'veh_type_2', 'veh_type_3',
                                                                        'veh_type_4', 'veh_type_5', 'veh_type_6',
                                                                        'veh_type_7', 'speed_sec_km']]

    results = traffic.groupby(['tstamp', 'hour', 'km', 'direction']).sum()

    results = results.reset_index()
    # to get the average weight and speed: divide the totals by the amount of cars:
    cols_to_divide = ['total_weight', 'speed_km_hr', 'speed_sec_km']
    for col in cols_to_divide:
        results[col] = results[col] / results['cars']

    # compute ratio of each vehicle type:
    cols_to_sum = [f'veh_type_{i}' for i in range(1, 8)]
    results['veh_type_total'] = results[cols_to_sum].sum(axis=1)
    for col in cols_to_sum:
        results[col] = results[col] / results['veh_type_total']
    results = results.drop(columns=['veh_type_total'])
    return results


# load data:
traffic_final = pd.read_parquet(data_path + traffic_path + traffic_file, engine='pyarrow')
time_segment = pd.read_parquet(data_path + segment_time_file, engine='pyarrow')

# preprocess
# formatting:
traffic_final.ent_occur_time = pd.to_datetime(traffic_final.ent_occur_time)
traffic_final.trans_occur_time = pd.to_datetime(traffic_final.trans_occur_time)

# dropping rows where exit_km = entrance_km (0.01% of the data)
traffic_final = traffic_final[traffic_final.entrance_km != traffic_final.exit_km]

# drop timestamp NAs:
traffic_final = traffic_final.dropna(subset=['ent_occur_time', 'trans_occur_time'])
traffic_final = traffic_final.reset_index(drop=True)

# consider only the relevant variables:
traffic_final = traffic_final[['ent_occur_time', 'trans_occur_time', 'entrance_km',
                               'exit_km', 'direction', 'total_weight', 'veh_type', 'speed_km_hr']]

# kick out rows where exit time is before entrance time or less then 1 min after:
index_to_keep = [i for i, x in enumerate(traffic_final.trans_occur_time-traffic_final.ent_occur_time) if x.total_seconds() > 60]
traffic_final = traffic_final.loc[index_to_keep, :]
traffic_final = traffic_final.reset_index(drop=True)

# preliminary aggregation for efficiency purposes:
traffic_final['ent_date'] = traffic_final.ent_occur_time.dt.date
traffic_final['ent_hour'] = traffic_final.ent_occur_time.dt.hour
traffic_final['exit_date'] = traffic_final.trans_occur_time.dt.date
traffic_final['exit_hour'] = traffic_final.trans_occur_time.dt.hour
traffic_final['cars'] = 1

# round to choice of segment & time granularity level before aggregation:
if granularity_km_ex_ante is not None:
    traffic_final['entrance_km'] = traffic_final.entrance_km.apply(
        lambda x: rounder(x, prec=2, base=granularity_km_ex_ante))
    traffic_final['exit_km'] = traffic_final.exit_km.apply(lambda x: rounder(x, prec=2, base=granularity_km_ex_ante))

if granularity_hour_ex_ante is not None:
    traffic_final['ent_hour'] = traffic_final.ent_hour.apply(
        lambda x: rounder(x, prec=2, base=granularity_hour_ex_ante))
    traffic_final['exit_hour'] = traffic_final.exit_hour.apply(
        lambda x: rounder(x, prec=2, base=granularity_hour_ex_ante))


# vehicle type aggregation:
veh_type_pct = traffic_final.groupby(['ent_date', 'ent_hour', 'exit_date', 'exit_hour', 'entrance_km',
                                      'exit_km', 'direction'
                      ])['veh_type'].value_counts(dropna=False)
veh_type_pct = veh_type_pct.unstack(fill_value=0)
veh_type_pct = veh_type_pct.rename(columns={i: f'veh_type_{i}' for i in range(1,8)})

# cars, total_weight and speed aggregation:
# First by summing, later (when assigned to kilometer and time) total_weight and speed will be divided
# by total amount of cars.
traffic_final = traffic_final.groupby(['ent_date', 'ent_hour', 'exit_date', 'exit_hour', 'entrance_km',
                                       'exit_km', 'direction'
                      ]).agg({'cars': 'sum', 'total_weight': 'sum', 'speed_km_hr': 'sum'})

# merge the two:
traffic_final = traffic_final.merge(veh_type_pct, how='left', left_index=True, right_index=True, validate='one_to_one')

traffic_final = traffic_final.reset_index()

# combine hours and date:
traffic_final['ent_occur_time'] = pd.to_datetime(traffic_final['ent_date']) + pd.to_timedelta(traffic_final['ent_hour'], unit='hr')
traffic_final['trans_occur_time'] = pd.to_datetime(traffic_final['exit_date']) + pd.to_timedelta(traffic_final['exit_hour'], unit='hr')

# adding 30 mins to all dates to keep aggregation more accurate
# (exception: when both timestamps are equal, ent_time only moved 15 mins):
traffic_final['ent_occur_time'] = traffic_final.apply(lambda x: x['ent_occur_time'] + timedelta(minutes=15) if x['trans_occur_time']==x['ent_occur_time'] else x['ent_occur_time'] + timedelta(minutes=30), axis=1)
traffic_final['trans_occur_time'] = traffic_final.trans_occur_time.apply(lambda x: x + timedelta(minutes=30))

# testing whether it worked:
assert len(traffic_final[traffic_final.trans_occur_time==traffic_final.ent_occur_time])==0

# drop redundant columns:
traffic_final = traffic_final.drop(['ent_date', 'ent_hour', 'exit_date', 'exit_hour'], axis=1)

# run the main function:
oversampling_factor = 2
result = main(traffic_final, frac=1/oversampling_factor)

result.to_parquet(data_path + 'Time Segment Data/traffic_feature.pq')
print(result)

