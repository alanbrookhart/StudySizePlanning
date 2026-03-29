# Study Size Planning Application 

Interactive Shiny application for calculating precision and statistical power in cohort studies with complications including censoring and IPTW.

## Features

- **Risk Difference and Risk Ratio** calculations
- **Censoring adjustment** using Greenwood's variance formula
- **IPTW variance inflation** for confounding adjustment
- **Interactive simulations** for propensity score exploration
- **Comprehensive validation** through simulation studies
- **Unit-tested** statistical functions

## Project Structure

```
├── app.R                      # Main Shiny application
├── simulation_study.R         # Monte Carlo simulation study
├── results.csv                # Results of Monte Carlo simulation study
├── R/
│   └── statistical_functions.R  # Statistical computations (unit-tested)
├── tests/
│   ├── testthat.R            # Test runner
│   └── testthat/
│       └── test-statistical_functions.R  # Unit tests
├── www/
│   └── simulation_study.html # Validation report (auto-generated)
├── simulation_study.Rmd       # Validation simulation study
├── DESCRIPTION                # Package metadata

```

## Local Development

### Prerequisites

- R >= 4.0.0
- Required packages: `shiny`, `tidyverse`, `ggiraph`, `DT`, `testthat`, `survival`

### Installation

```r
# Install required packages
install.packages(c("shiny", "tidyverse", "ggiraph", "DT", "testthat", "survival", "rmarkdown", "knitr"))
```

### Running the Application

```r
# Run locally
shiny::runApp()
```

### Running Tests

```r
# Run all unit tests
testthat::test_dir("tests/testthat")
```

### Updating Simulation Study

```r
The simulation study can be updated by running the R script simulation_study.R.  The results of the simulation will be saved in results.csv, which will be read by the application.
```

### Quick Start

1. Push this repository to GitHub
2. Configure shinyapps.io credentials as GitHub Secrets
3. Push to `main` or `master` branch to trigger automatic deployment


## Testing

The application includes comprehensive unit tests for all statistical functions:

- `greenwood_var_closed()` - Greenwood's variance under censoring
- `variance_baseline()` - Binomial variance
- `metrics_risk_difference()` - RD confidence intervals and power
- `metrics_risk_ratio()` - RR confidence intervals and power
- `variance_iptw()` - IPTW variance inflation
- `calculate_variance_inflation()` - Kish's design effect

Run tests with:

```r
testthat::test_dir("tests/testthat")
```

## Validation

The finite sample performance of all methods is validated through extensive simulations (`simulation_study.Rmd`). The validation study tests:

- **Sample sizes**: 50 to 5,000
- **Censoring rates**: 0% to 60%
- **Confounding levels**: None, Moderate, Strong

Results include:
- Confidence interval coverage rates
- Statistical power
- Bias and RMSE
- Variance inflation factors

View the validation report at: [simulation_study.html](simulation_study.html)

## Statistical Methods

### 1. Baseline Variance

For a proportion $p$ estimated from $n$ observations:

$$\text{Var}(\hat{p}) = \frac{p(1-p)}{n}$$

### 2. Greenwood's Variance (Right Censoring)

Exact asymptotic variance under exponential failure and censoring:

$$\text{Var}(\hat{p}_t) = \frac{(1-p_t)^2}{n} \left( \frac{-\log(1-p_t)}{-\log(1-p_t) - \log(1-p_c)} \right) \left[ \frac{1}{(1-p_t)(1-p_c)} - 1 \right]$$

### 3. IPTW Variance Inflation

If IPT weights are used to control confounding in a Hajek-type estimator (where the IPT weights are normalized to sum to $N$ in each treatment group), an approximate variance inflation for an estimated mean in group $a$ is given by
\[
\hat{\nu}_a = \frac{N_a \sum_{i=1}^N \hat{W_i}^2I(A_i = a)}{\{\sum_{i=1}^N \hat{W_i}I(A_i = a)\}^2},
\]
where $A_i$ is the treatment group and $W_i$ is the IPT weight for patient $i$. Therefore, an approximate variance of the difference between two means in an IPT weighted analysis is given by $\nu_1 \sigma^2_1 + \nu_2 \sigma^2_2$. 

### 4. Risk Difference and Risk Ratio

- **Risk Difference (RD)**: Additive scale, $\text{RD} = p_2 - p_1$
- **Risk Ratio (RR)**: Multiplicative scale, $\text{RR} = p_2 / p_1$ (inference on log scale)

See the **Derivations & Formulas** tab in the application for complete mathematical details.

## License

MIT License

## Contact

For questions or issues, please contact alan.brookhart@duke.edu.

---

*Last updated: 2025*
