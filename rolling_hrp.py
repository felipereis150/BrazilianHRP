import pandas as pd
import numpy as np
import HRP

def min_var_portfolio(cov):
    inv_cov = np.linalg.inv(cov)
    ones = np.ones(len(cov))
    return np.dot(inv_cov, ones) / np.dot(np.dot(ones, inv_cov), ones)


def rolling_window_HRP(data, window_size):
    
    """
    This function applies the Hierarchical Risk Parity algorithm to a rolling window of historical returns.

    Parameters:
    data (DataFrame): Historical returns with dates in a column.
    window_size (int): The size of the rolling window.

    Returns:
    DataFrame: A dataframe with the realized returns, HRP returns and MV returns for each out-of-sample window.
    """
    
    out_of_sample = len(data) - window_size
    weights = np.zeros((out_of_sample, data.shape[1] - 1))
    weights_mv = np.zeros((out_of_sample, data.shape[1] - 1))
    Rport = np.empty((out_of_sample, 4))
    
    for i in range(out_of_sample):
        df_rolling = data.iloc[i:i + window_size, 1:]
        df_cov = df_rolling.cov()
        df_cov = pd.DataFrame(df_cov, columns=df_rolling.columns, index=df_rolling.columns)
        weights[i, :] = HRP.HRP_Portfolio(df_cov, graph=False)
        weights_mv[i, :] = min_var_portfolio(df_cov.values)
        Rport[i, 0] = data.iloc[i + window_size, 0]  
        Rport[i, 1] = np.mean(data.iloc[i + window_size, 1:])
        Rport[i, 2] = np.dot(weights[i, :], data.iloc[i + window_size, 1:])
        Rport[i, 3] = np.dot(weights_mv[i, :], data.iloc[i + window_size, 1:])

    Rport = pd.DataFrame(Rport, columns=["Date", "Equal Weights", "HRP", "MV"])
   
    return Rport
