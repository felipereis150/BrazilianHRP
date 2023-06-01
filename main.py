import pandas as pd
import numpy as np
import rolling_hrp as rol
import HRP
import seaborn as sns
import matplotlib.pyplot as plt
import matplotlib.dates as mdates

df = pd.read_excel("data/ibrx_mensal.xlsx", na_values="-")

# transforming the 'Data' column to a date format
df['Data'] = df['Data'].str.replace('Jan', '01').str.replace('Fev', '02').str.replace('Mar', '03').str.replace('Abr', '04').str.replace('Mai', '05').str.replace('Jun', '06').str.replace('Jul', '07').str.replace('Ago', '08').str.replace('Set', '09').str.replace('Out', '10').str.replace('Nov', '11').str.replace('Dez', '12')
    
def filter_covid(df):
    
    df['Data'] = pd.to_datetime(df['Data'], format='%m-%Y')
    df_covid = df[(df['Data'] >= '2000-01-01') & (df['Data'] <= '2022-06-01')].dropna(axis=1) # dataframe with covid period include
    df_w_covid = df[(df['Data'] >= '2000-01-01') & (df['Data'] <= '2020-02-01')].dropna(axis=1) # dataframe with covid period excluded

    return [df_covid, df_w_covid]


data = filter_covid(df)

def generate_stats(dataframes, suffixes, output_file):
    stats = pd.DataFrame()  # Create an empty dataframe to store the results

    for i, df in enumerate(dataframes):
        # Select only numeric columns
        numeric_columns = df.select_dtypes(include=np.number)
        
        temp_stats = numeric_columns.describe(percentiles=[0.25, 0.5, 0.75])
        temp_stats.loc['std'] = numeric_columns.std()  # Add standard deviation
        temp_stats.loc['skewness'] = numeric_columns.skew()  # Add skewness
        temp_stats.loc['kurtosis'] = numeric_columns.kurtosis()  # Add kurtosis
        
        col_suffix = "" + suffixes[i]  # Add suffix based on the provided list
        temp_stats.columns = [col_suffix + col for col in temp_stats.columns]  # Add suffix to column names
        
        stats = pd.concat([stats, temp_stats], axis=1)  # Concatenate the results for each dataframe horizontally with the previous results

    stats = stats[stats.index.isin(['count', 'mean', 'std', 'min', '25%', '50%', '75%', 'max', 'skewness', 'kurtosis'])].T
    stats.to_csv(output_file)
    return stats

returns_stats = generate_stats(data, ['c', ""], 'data/returns_stats.csv')

def apply_rolling_hrp(df, window_length):
    df_result = df.iloc[:, 1:] # skipping the 'Data' column
    df_result = rol.rolling_window_HRP(df_result, window_length)
    df_result = df_result.reset_index(drop=True)
    df_result['Date'] = df['Data'].reset_index(drop=True)
    return df_result

def apply_hrp(lst):
    df_covid, df_w_covid = lst
    results_60c = apply_rolling_hrp(df_covid, 60)
    results_120c = apply_rolling_hrp(df_covid, 120)
    results_60 = apply_rolling_hrp(df_w_covid, 60)
    results_120 = apply_rolling_hrp(df_w_covid, 120)
    return [results_60c, results_120c, results_60, results_120]

data_hrp = apply_hrp(data)

def save_hrp_returns(df):
    names = ['60c', '120c', '60', '120']

    for i, df in enumerate(df):
        df.to_csv(f'data/rolling_hrp_{names[i]}.csv', index=False)

save_hrp_returns(data_hrp)

for i in data:
    weights = HRP.HRP_Portfolio(i.iloc[:, 1:], graph=True)

""" 
code chunk to plot charts

# Plot the returns for each strategy and save as individual images
for i, j in enumerate(data_hrp):
    fig, ax = plt.subplots(figsize=(10, 8))
    title = f"{j.columns[0]} - window length {j.columns[1]}"
    if i == 0:
        title += " (including COVID - 60 months)"
    elif i == 1:
        title += " (including COVID - 120 months)"
    elif i == 2:
        title += " (excluding COVID - 60 months)"
    else:
        title += " (excluding COVID - 120 months)"
    ax.set_title(title)
    
    # Plot the line chart
    for col_name in j.columns[1:]:
        ax.plot(j['Date'], j[col_name], label=col_name)
        ax.legend()
        ax.xaxis.set_major_locator(mdates.YearLocator())
        ax.xaxis.set_major_formatter(mdates.DateFormatter('%Y'))

    ax.set_xlabel('Date')
    ax.set_ylabel('Return')

    plt.show()

strategies_stats = generate_stats(data_hrp, ["60c", "120c", "60", "120"], 'data/strategies_stats.csv')
strategies_stats

fig, axs = plt.subplots(nrows=2, ncols=2, figsize=(16, 12))

# Plot heatmap
for i, j in enumerate(data_hrp):
    row = i // 2
    col = i % 2
    ax = axs[row, col]
    sns.heatmap(j.corr(), cmap="YlGnBu", ax=ax)
    if i == 0:
        ax.set_title("Correlation Heatmap for 60 Months Window Including COVID Period")
    elif i == 1:
        ax.set_title("Correlation Heatmap for 120 Months Window Including COVID Period")
    elif i == 2:
        ax.set_title("Correlation Heatmap for 60 Months Window Excluding COVID Period")
    else:
        ax.set_title("Correlation Heatmap for 120 Months Window Excluding COVID Period")

fig.suptitle('Correlation Heatmaps for HRP Strategies', fontsize=16)
fig.subplots_adjust(hspace=0.4, wspace=0.3)

plt.show()


fig, axs = plt.subplots(nrows=1, ncols=2, figsize=(16, 6))

for i, j in enumerate(data):
    # if j.equals(data[0]):
    #     axs[i].set_title(f"Correlation Heatmap for Assets (including COVID period)")
    # else:
    #     axs[i].set_title(f"Correlation Heatmap for Assets (excluding COVID period)")
    sns.heatmap(j.corr(), cmap="YlGnBu", ax=axs[i])

plt.show()
"""

for i, j in enumerate(data):
    if j.equals(data[0]):
        file_name = f"data\corr_matrix_including_covid.csv"
    else:
        file_name = f"data\corr_matrix_excluding_covid.csv"
        
    corr_matrix = j.corr()
    corr_matrix.to_csv(file_name)


# removing date
data_hrp_w_date = []

for i in range(len(data_hrp)):
    df_result = data_hrp[i].iloc[:, 1:] # skipping the 'Data' column
    data_hrp_w_date.append(df_result)

# Define helper functions for calculating metrics
def annualize_return(return_series):
    return (1 + return_series.mean()) ** 12 - 1

def annualize_std(return_series):
    return return_series.std() * np.sqrt(12)

def sharpe_ratio(return_series, risk_free_rate):
    excess_return = return_series - risk_free_rate
    return np.sqrt(12) * excess_return.mean() / excess_return.std()

def adjusted_sharpe_ratio(return_series, risk_free_rate, factor_loadings):
    excess_return = return_series - risk_free_rate
    denominator = np.sqrt(12 * (1 + (factor_loadings ** 2).sum()))
    return (np.sqrt(12) * excess_return.mean() / excess_return.std()) / denominator

def sortino_ratio(return_series, risk_free_rate):
    downside_returns = return_series - risk_free_rate
    downside_std = downside_returns[downside_returns < 0].std() * np.sqrt(12)
    return np.sqrt(12) * (return_series.mean() - risk_free_rate) / downside_std

# Define parameters
risk_free_rate = 0.1375 # annualized SELIC for march 2023
factor_loadings = np.array([0.5, 0.3, 0.2])  # Assuming 3 factors

# Define output DataFrame
results = pd.DataFrame()

# Loop over each dataframe in data_hrp_w_date and calculate metrics
for i in range(len(data_hrp_w_date)):
    if i == 0:
        name = '60m_covid'
    elif i == 1:
        name = '120m_covid'
    elif i == 2:
        name = '60m_wo_covid'
    else:
        name = '120m_wo_covid'
    
    # Calculate metrics for each column (assuming returns are already in decimal form)
    df = data_hrp_w_date[i]
    av = df.mean() * 12
    sd = df.std() * np.sqrt(12)
    sr = sharpe_ratio(df, risk_free_rate)
    asr = adjusted_sharpe_ratio(df, risk_free_rate, factor_loadings)
    sor = sortino_ratio(df, risk_free_rate)
    to = (np.abs(df.diff()).sum() / 2) / df.shape[0]
    ssw = (df ** 2).sum() / df.shape[0]
    
    # Combine results into a single row for this dataframe
    row = pd.DataFrame({
        f'{name}_AV': av,
        f'{name}_SD': sd,
        f'{name}_SR': sr,
        f'{name}_ASR': asr,
        f'{name}_SoR': sor,
        f'{name}_TO': to,
        f'{name}_SSW': ssw
    })
    
    # Append row to results dataframe
    results = pd.concat([results, row], axis=1)

results = results.rename_axis('metric').reset_index()
results_transposed = results.T
results_transposed.columns = results_transposed.iloc[0]
results_transposed = results_transposed.iloc[1:]
results_transposed.to_csv("data/metrics.csv")

print("All files saved in the data folder")