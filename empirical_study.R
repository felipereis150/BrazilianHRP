###########################################
# Is it worth using HRP portfolios in the #
# Brazilian market? An OoS analysis       #
###########################################

library(tidyr)
library(dplyr)
library(readxl)
library(stringr)
library(lubridate)
library(RiskPortfolios)
library(HierPortfolios)
library(moments) # Skewness and Kurtosis
library(xtable)  # Tables in latex format
library(kableExtra) # Tables in latex format
source("oos_measures.R") # Out-of-sample performance measures: AV, SD, SR, ASR, SO, TO, SSPW

###########################################
# Data cleaning
###########################################
## Filter from 2000-01 to 2022-06
monthly_data <- read_xlsx("Data/ibrx_mensal.xlsx", na = "-") |> 
  mutate(Data = str_replace(Data, "Jan", "01"), 
         Data = str_replace(Data, "Fev", "02"),
         Data = str_replace(Data, "Mar", "03"), 
         Data = str_replace(Data, "Abr", "04"),
         Data = str_replace(Data, "Mai", "05"), 
         Data = str_replace(Data, "Jun", "06"),
         Data = str_replace(Data, "Jul", "07"), 
         Data = str_replace(Data, "Ago", "08"),
         Data = str_replace(Data, "Set", "09"), 
         Data = str_replace(Data, "Out", "10"),
         Data = str_replace(Data, "Nov", "11"), 
         Data = str_replace(Data, "Dez", "12")) |> 
  mutate(Data = lubridate::my(Data)) |> 
  dplyr::filter(Data >= '2000-01-01', Data < '2022-07-01')
### select assets with no missing values
full_monthly_data <- monthly_data |> dplyr::select(names(which(apply(is.na(monthly_data), 2, sum) == 0)))
### extract ibov
ibov <- full_monthly_data |> dplyr::select(IBOV)
### assets without ibov
monthly_data <- full_monthly_data |> dplyr::select(-Data, -IBOV)
### Remove no ordinary assets or high correlated from the same company
ordinary_monthly_data <- monthly_data |> 
  dplyr::select(-BBDC4, -ELET6, -PETR4, -ITSA4, -GGBR4)

cor(monthly_data$ITSA4, monthly_data$ITUB4) # 0.946
cor(monthly_data$GGBR4, monthly_data$GOAU4) # 0.926

###########################################
# Descriptive Statistics
###########################################

monthly_data |> 
  pivot_longer(cols = everything(), values_to = "returns", names_to = "assets") |> 
  group_by(assets) |> 
  summarise(Min = min(returns), 
            Max = max(returns),
            Mean = mean(returns),
            Std = sd(returns),
            Skew = skewness(returns),
            Kurt = kurtosis(returns)) |> 
  xtable(digits = 3)

###########################################
# Out-of-sample comparison
###########################################

## Comparison using all assets and only ordinary* assets
## Portfolio allocation strategies implemented:
## - Minimum Variance (MV)
## - Risk Parity (RP)
## - Inverse Volatility (IV)
## - Hierarchical Risk Parity (HRP)
## - Equally-weighted (EW)
## - Ibov index (Ibov)

ordinary = FALSE  # Input TRUE or FALSE
returns <- monthly_data
if (ordinary == TRUE) returns <- ordinary_monthly_data

for (InS in c(60, 120)) {
  OoS <- nrow(returns) - InS
  # Portfolio weights
  w_ew <- matrix(NA, ncol = ncol(returns), nrow = OoS)
  w_mv <- matrix(NA, ncol = ncol(returns), nrow = OoS)
  w_iv <- matrix(NA, ncol = ncol(returns), nrow = OoS)
  w_rp <- matrix(NA, ncol = ncol(returns), nrow = OoS)
  w_md <- matrix(NA, ncol = ncol(returns), nrow = OoS)
  w_mde <- matrix(NA, ncol = ncol(returns), nrow = OoS)
  w_hrp <- matrix(NA, ncol = ncol(returns), nrow = OoS)
  # Portfolio return
  Rp <- matrix(NA, ncol = 8, nrow = OoS)
  colnames(Rp) <- c("Ibov", "EW", "MV", "IV", "RP", "MD", "MDE", "HRP")
  sspw <- matrix(NA, ncol = 8, nrow = OoS)
  to <- matrix(NA, ncol = 8, nrow = OoS - 1)
  
  for (i in 1:OoS) {
    print(sprintf("Window %i of %i", i, OoS))
    # In-sample returns
    ret <- returns[i:(i + InS - 1), ]
    # realized one-step-ahead returns
    realized_returns <- as.numeric(returns[i + InS, ]) 
    # Covariance matrix (input for portfolio allocation techniques)
    cova <- cov(ret)
    # Compute Portfolio Weights
    w_ew[i, ] <- rep(1/ncol(returns), ncol(returns))
    w_mv[i, ] <- optimalPortfolio(Sigma = cova, control = list(type = 'minvol', constraint = 'lo'))
    w_iv[i, ] <- optimalPortfolio(Sigma = cova, control = list(type = 'invvol', constraint = 'lo'))
    w_rp[i, ] <- optimalPortfolio(Sigma = cova, control = list(type = 'erc', constraint = 'lo'))
    w_md[i, ] <- optimalPortfolio(Sigma = cova, control = list(type = 'maxdiv', constraint = 'lo'))
    w_mde[i, ] <- optimalPortfolio(Sigma = cova, control = list(type = 'maxdec', constraint = 'lo'))
    w_hrp[i, ] <- HRP_Portfolio(cova)$weights
    # Compute Portfolio returns
    Rp[i, 1] <- as.numeric(ibov[i + InS, 1])
    Rp[i, 2] <- as.numeric(realized_returns %*% w_ew[i, ])
    Rp[i, 3] <- as.numeric(realized_returns %*% w_mv[i, ])
    Rp[i, 4] <- as.numeric(realized_returns %*% w_iv[i, ])
    Rp[i, 5] <- as.numeric(realized_returns %*% w_rp[i, ])
    Rp[i, 6] <- as.numeric(realized_returns %*% w_md[i, ])
    Rp[i, 7] <- as.numeric(realized_returns %*% w_mde[i, ])
    Rp[i, 8] <- as.numeric(realized_returns %*% w_hrp[i, ])
    # SSPW
    sspw[i, ] <- c(0, sum(w_ew[i, ]^2), sum(w_mv[i, ]^2), sum(w_iv[i, ]^2), 
                   sum(w_rp[i, ]^2), sum(w_md[i, ]^2), sum(w_mde[i, ]^2), sum(w_hrp[i, ]^2))
    # Turnover
    if (i > 1) {
      to[i - 1, ] <- c(0,
                       calculate_to(w_ew[i - 1, ], w_ew[i, ], tail(ret, 1)),
                       calculate_to(w_mv[i - 1, ], w_mv[i, ], tail(ret, 1)),
                       calculate_to(w_iv[i - 1, ], w_iv[i, ], tail(ret, 1)),
                       calculate_to(w_rp[i - 1, ], w_rp[i, ], tail(ret, 1)),
                       calculate_to(w_md[i - 1, ], w_md[i, ], tail(ret, 1)),
                       calculate_to(w_mde[i - 1, ], w_mde[i, ], tail(ret, 1)),
                       calculate_to(w_hrp[i - 1, ], w_hrp[i, ], tail(ret, 1)))
    }
  }
  
  oos_table <- cbind(t(apply(Rp, 2, medidas)), apply(to, 2, mean), apply(sspw, 2, mean))
  colnames(oos_table) <- c("AV","SD", "SR", "ASR", "SO", "TO", "SSPW")
  write.csv(oos_table, paste0("oos_", InS, ".csv"))
  oos_table |> 
    knitr::kable(digits = 4, format = "latex", align = "lccccccc", 
                 table.envir = "table", label = paste0("oos_", InS)) |> 
    save_kable(keep_tex = T, file = paste0("oos_", InS, ".tex"))
  
  write.csv(Rp, paste0("Rp_",InS, ".csv"))
  write.csv(w_ew, paste0("w_ew_",InS, ".csv"))
  write.csv(w_mv, paste0("w_mv_",InS, ".csv"))
  write.csv(w_iv, paste0("w_iv_",InS, ".csv"))
  write.csv(w_rp, paste0("w_rp_",InS, ".csv"))
  write.csv(w_md, paste0("w_md_",InS, ".csv"))
  write.csv(w_mde, paste0("w_mde_",InS, ".csv"))
  write.csv(w_hrp, paste0("w_hrp_",InS, ".csv"))
  
  if (ordinary == TRUE) {
    write.csv(Rp, paste0("o_Rp_",InS, ".csv"))
    write.csv(w_ew, paste0("o_w_ew_",InS, ".csv"))
    write.csv(w_mv, paste0("o_w_mv_",InS, ".csv"))
    write.csv(w_iv, paste0("o_w_iv_",InS, ".csv"))
    write.csv(w_rp, paste0("o_w_rp_",InS, ".csv"))
    write.csv(w_md, paste0("o_w_md_",InS, ".csv"))
    write.csv(w_mde, paste0("o_w_mde_",InS, ".csv"))
    write.csv(w_hrp, paste0("o_w_hrp_",InS, ".csv"))
  }
}
  
  
