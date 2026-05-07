# Rolling Cointegration & Granger Causality Analysis for High-Frequency Data

> **Dynamic analysis of cointegration and causal relationships between PEP and PG stocks using rolling windows (4-hour intervals) with ADF, PP, KPSS tests, and Granger causality.**

## 📋 Project Overview

This project implements **rolling econometric analyses** on high-frequency financial data to detect time-varying relationships between two major consumer goods stocks:

| Stock | Company | Ticker |
|-------|---------|--------|
| PEP | Pepsi Co. | PEP |
| PG | Procter & Gamble | PG |

### Research Questions

1. **Cointegration** – Are PEP and PG prices cointegrated? Does this relationship change over time?
2. **Granger Causality** – Do returns of one stock Granger-cause returns of the other? Does causality direction vary across subperiods?

### Key Innovation

Unlike traditional full-sample cointegration tests, this analysis uses **rolling windows** to capture:
- Time-varying long-run relationships
- Episodic cointegration in high-frequency data
- Dynamic causal structures

---

## 📊 Data Description

| Feature | Detail |
|---------|--------|
| Frequency | High-frequency (minute-level) |
| Timeframe | Intraday trading session |
| Variables | Open, High, Low, Close prices + Volume |
| Key series | PEP.close, PG.close, PEP.returns, PG.returns |
| Transformations | Log-returns in basis points (bps): `10000 × Δln(P)` |
| Window size | 240 observations (~4 hours) |
| Rolling step | 60 observations (~1 hour) |

### Data Source

High-frequency data from `US.data.RData` (University of Warsaw, Faculty of Economic Sciences)

