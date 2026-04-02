library(shiny)
library(tidyverse)
library(ggiraph)
library(patchwork)
library(DT)

# Source statistical functions
source("R/statistical_functions.R")

# --- 2. UI Definition ---
ui <- fluidPage(
  
  # Initialize MathJax for LaTeX rendering
  withMathJax(),
  
  tags$head(
    tags$style(HTML("
      /* Font Stack */
      body, h1, h2, h3, h4, h5, h6, p, li, label, .control-label {
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', 'Roboto', 'Helvetica Neue', Arial, sans-serif;
      }

      /* Main Title Bar */
      .navbar-default, h2.page-header {
        background-color: #003366;
        color: white;
        text-align: center;
        font-weight: bold;
        font-size: 1.5em;
        padding: 15px;
        margin: 0;
        border: none;
        border-radius: 0;
      }

      /* Sidebar Styling */
      .well {
        background-color: #001A57 !important;
        border: none !important;
        color: white !important;
        padding: 20px !important;
        box-shadow: 2px 0 5px rgba(0,0,0,0.1);
      }

      .well h4 {
        color: white !important;
        font-weight: 600;
        margin-top: 20px;
        margin-bottom: 15px;
      }

      .well label, .well .control-label {
        color: white !important;
        font-weight: normal;
      }

      .well hr {
        border-top: 1px solid rgba(255,255,255,0.3);
      }

      /* Form Inputs in Sidebar */
      .well input[type='number'],
      .well input[type='text'],
      .well select,
      .well .selectize-input {
        background-color: white;
        color: #333;
        border: 1px solid #ccc;
      }

      /* Main Content Area */
      .col-sm-9 {
        background-color: #f8f9fa;
        min-height: 100vh;
        padding: 30px;
      }

      /* Tab Panels */
      .nav-tabs {
        border-bottom: 2px solid #003366;
      }

      .nav-tabs > li > a {
        color: #003366;
        background-color: #e0e0e0;
        border: none;
        margin-right: 2px;
        font-weight: 500;
      }

      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        color: white;
        background-color: #4682B4;
        border: none;
      }

      .nav-tabs > li > a:hover {
        background-color: #ccc;
        border: none;
      }

      .tab-content {
        background-color: white;
        padding: 25px;
        border-radius: 0 5px 5px 5px;
        box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.1);
      }

      /* Section Cards */
      .math-section {
        background-color: #f8f9fa;
        padding: 20px;
        border-radius: 5px;
        margin-bottom: 20px;
        border-left: 4px solid #4682B4;
        box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.05);
      }

      .math-section h3 {
        color: #003366;
        font-weight: 600;
        margin-top: 0;
      }

      .sim-controls {
        background-color: #f0f7ff;
        padding: 20px;
        border-radius: 5px;
        margin-bottom: 20px;
        border-left: 4px solid #C99700;
        box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.05);
      }

      /* Buttons */
      .btn-primary {
        background-color: #C99700;
        border: none;
        color: white;
        padding: 10px 20px;
        border-radius: 5px;
        font-weight: 500;
        transition: background-color 0.3s;
      }

      .btn-primary:hover {
        background-color: #A67600;
      }

      /* Headings */
      h3, h4 {
        color: #003366;
        font-weight: 600;
      }

      /* Paragraphs in content */
      .tab-content p {
        line-height: 1.6;
        color: #333;
      }

      /* Links */
      a {
        color: #4682B4;
      }

      a:hover {
        color: #003366;
      }

      /* Tables */
      .dataTable {
        border-collapse: collapse;
      }

      .dataTable thead {
        background-color: #003366;
        color: white;
      }

      /* Plot containers */
      .ggiraph-container {
        background-color: white;
        padding: 15px;
        border-radius: 5px;
        box-shadow: 0px 2px 4px rgba(0, 0, 0, 0.05);
      }
    "))
  ),

  tags$div(
    style = "background-color: #003366; color: white; text-align: center; padding: 20px; margin: 0;",
    tags$h2(style = "margin: 0; font-weight: bold;", "Study Design: Precision & Power Calculator")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,

      # Calculation Mode Selector
      radioButtons("calc_mode", strong("Calculation Mode:"),
                   choices = c("Calculate Power" = "power",
                               "Calculate Sample Size" = "sample_size"),
                   selected = "power"),

      hr(),
      h4("Baseline Parameters"),
      radioButtons("effect_measure", "Target Estimand:",
                   choices = c("Risk Difference (RD)" = "RD",
                               "Risk Ratio (RR)" = "RR")),
      selectInput("alpha", "Alpha (\u03B1):", choices = c(0.10, 0.05, 0.01, 0.005), selected = 0.05),
      numericInput("p1", "Risk in Group 1 (p1):", 0.10, min = 0.001, max = 0.999, step = 0.01),

      # Power Calculation Mode Inputs
      conditionalPanel(
        condition = "input.calc_mode == 'power'",
        numericInput("n1", "Patients in Group 1 (n1):", 1000, min = 1),
        numericInput("n2", "Patients in Group 2 (n2):", 1000, min = 1)
      ),

      # Sample Size Calculation Mode Inputs
      conditionalPanel(
        condition = "input.calc_mode == 'sample_size'",
        sliderInput("target_power", "Target Power:",
                    min = 0.50, max = 0.99, value = 0.80, step = 0.01),
        numericInput("allocation_ratio", "Allocation Ratio (n2/n1):",
                     1, min = 0.1, max = 10, step = 0.1)
      ),

      # Effect Size Range (conditional on effect measure)
      conditionalPanel(
        condition = "input.effect_measure == 'RD'",
        conditionalPanel(
          condition = "input.calc_mode == 'power'",
          sliderInput("RDrange", "Risk Difference Range:",
                      min = 0, max = 0.2, value = c(0.00, 0.10), step = 0.005)
        ),
        conditionalPanel(
          condition = "input.calc_mode == 'sample_size'",
          sliderInput("RDrange_ss", "Risk Difference Range:",
                      min = -0.2, max = 0.2, value = c(0.01, 0.10), step = 0.005)
        )
      ),
      conditionalPanel(
        condition = "input.effect_measure == 'RR'",
        conditionalPanel(
          condition = "input.calc_mode == 'power'",
          sliderInput("RRrange", "Risk Ratio Range:",
                      min = 0.5, max = 5, value = c(1.0, 2.0), step = 0.1)
        ),
        conditionalPanel(
          condition = "input.calc_mode == 'sample_size'",
          sliderInput("RRrange_ss", "Risk Ratio Range:",
                      min = 0.5, max = 5, value = c(1.1, 2.0), step = 0.1)
        )
      ),

      numericInput("steps", "Evaluation Steps:", 50, min = 2, max = 200),

      hr(),
      h4("Complications"),
      numericInput("cens_1", "Censoring Risk (Group 1):", 0.20, min = 0, max = 0.99, step = 0.05),
      numericInput("cens_2", "Censoring Risk (Group 2):", 0.20, min = 0, max = 0.99, step = 0.05),

      numericInput("v1", "IPTW Variance Inflation (Group 1):", 1.3, min = 1, step = 0.1),
      numericInput("v2", "IPTW Variance Inflation (Group 2):", 1.3, min = 1, step = 0.1)
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("About this app",
                 br(),
                 div(class = "math-section",
                     h3("About This Application"),
                     hr(),
                     p("In cohort studies comparing different exposures or treatments, interest often focuses on estimating differences or ratios in the risk of the outcome under study. Here \"risk\" means the probability that a patient will experience the study outcome during a specified period of follow-up."),
                     
                     p("In this application, we provide tools for assessing the precision, statistical power, or required sample size for estimating both the risk difference—the absolute difference in risk between two treatment groups—and the relative risk (risk ratio)."),
                     
                     p("Here, we define \"precision\" as the width of a \\(1-\\alpha\\) confidence interval. We define \"power\" as the probability of detecting a statistically significant effect when one truly exists. Power is explicitly evaluated and described for both the additive scale (RD) and the multiplicative scale (RR). Even though the power estimates for these two estimands will typically be numerically similar, their variance estimators differ, necessitating separate calculations."),
                     
                     p("We start with a simple case of a fully observed outcome and then explore how the precision and power are impacted as successive complexities are addressed:"),
                     
                     tags$ul(
                       tags$li(strong("Censoring: "), "We allow the user to introduce different degrees of right censoring in each treatment group."),
                       tags$li(strong("Confounding: "), "We allow the user to introduce variance inflation due to the control of confounding via IPT weights.  The variance inflation is computed using Kish's formula, as discussed in Shook-sa and Hudgens. To help the user set varince inflation parameters thoughtfully,  we provide a simple simulation that allows the reader to explore how different assumptions about the propensity score distribution result in different degrees of variance inflation.")
                     )
                 )
        ),
        # Power Calculation Mode Tabs
        tabPanel("Expected Precision",
                 value = "precision_tab",
                 br(),
                 conditionalPanel(
                   condition = "input.calc_mode == 'power'",
                   p("This plot displays the expected width of the confidence interval across various study complexities. A narrower width indicates higher precision."),
                   girafeOutput("plot_precision", width = "100%", height = "500px")
                 ),
                 conditionalPanel(
                   condition = "input.calc_mode == 'sample_size'",
                   div(class = "math-section",
                       p(strong("This tab is only available in 'Calculate Power' mode.")),
                       p("Switch to 'Calculate Power' mode in the sidebar to view precision estimates.")
                   )
                 )),

        tabPanel("Statistical Power",
                 value = "power_tab",
                 br(),
                 conditionalPanel(
                   condition = "input.calc_mode == 'power'",
                   p("This plot displays the expected statistical power to detect an effect significantly different from the null, given the sample size and variance inflations."),
                   girafeOutput("plot_power", width = "100%", height = "500px")
                 ),
                 conditionalPanel(
                   condition = "input.calc_mode == 'sample_size'",
                   div(class = "math-section",
                       p(strong("This tab is only available in 'Calculate Power' mode.")),
                       p("Switch to 'Calculate Power' mode in the sidebar to view statistical power estimates.")
                   )
                 )),

        # Sample Size Calculation Mode Tab
        tabPanel("Sample Size Results",
                 value = "sample_size_tab",
                 br(),
                 conditionalPanel(
                   condition = "input.calc_mode == 'sample_size'",
                   p("These plots display the required sample size to achieve the target statistical power under different assumptions about the effect size, censoring, and confounding."),
                   h4("Required Sample Size in Group 1 (n1)"),
                   girafeOutput("plot_sample_size_n1", width = "100%", height = "400px"),
                   br(),
                   h4("Total Sample Size (n1 + n2)"),
                   girafeOutput("plot_sample_size_total", width = "100%", height = "400px"),
                   br(),
                   h4("Detailed Results"),
                   DTOutput("table_sample_size")
                 ),
                 conditionalPanel(
                   condition = "input.calc_mode == 'power'",
                   div(class = "math-section",
                       p(strong("This tab is only available in 'Calculate Sample Size' mode.")),
                       p("Switch to 'Calculate Sample Size' mode in the sidebar to calculate required sample sizes.")
                   )
                 )),

        tabPanel("IPTW Simulation",
                 br(),
                 p("Simulate a single covariate to explore how different propensity score distributions and weighting schemes affect variance inflation. This simulation uses n = 50,000 to compute stable expected inflation factors."),
                 fluidRow(
                   column(4,
                          div(class = "sim-controls",
                              sliderInput("beta", "Covariate Association (log odds):", min = -4, max = 4, value = 1, step = 0.1),
                              radioButtons("weight_type", "Weighting Scheme:",
                                           c("Standard IPTW" = "iptw",
                                             "SMR Group 1 (ATT)" = "smrw1",
                                             "SMR Group 2 (ATT)" = "smrw2")),
                              br(),
                              actionButton("autopop", "Autopopulate Sidebar Parameters", class = "btn-primary", icon = icon("arrow-left"))
                          )
                   ),
                   column(8,
                          girafeOutput("plot_ps", width = "100%", height = "350px"),
                          br(),
                          tableOutput("table_iptw")
                   )
                 )),
        
        tabPanel("Tabulated Results", 
                 br(),
                 DTOutput("table_results")),
        
        tabPanel("Derivations & Formulas", 
                 br(),
                 p("This section details the mathematical formulas used to calculate precision, statistical power, and required sample size. Power is explicitly evaluated for both the risk difference (additive scale) and risk ratio (multiplicative scale) to account for their distinct variance estimators."),
                 
                 div(class = "math-section",
                     h3("1. Risk Difference (RD) Precision & Power"),
                     p("The Risk Difference is evaluated on the additive scale. The standard Wald estimator for the standard error of the difference between two independent proportions is:"),
                     p("$$se(\\hat{RD}) = \\sqrt{\\frac{p_1(1-p_1)}{n_1} + \\frac{p_2(1-p_2)}{n_2}}$$"),
                     p("The width, \\(W\\), of a \\((1-\\alpha)\\) confidence interval for \\(\\hat{RD}\\) is given by:"),
                     p("$$W_{RD} = 2 Z_{1-\\alpha/2} se(\\hat{RD})$$"),
                     p("Statistical power is calculated against the null hypothesis \\(H_0: RD = 0\\). It represents the probability of rejecting the null hypothesis when the true risk difference is \\(RD\\):"),
                     p("$$Power_{RD} = \\Phi \\left( \\frac{|RD|}{se(\\hat{RD})} - Z_{1-\\alpha/2} \\right) + \\Phi \\left( \\frac{-|RD|}{se(\\hat{RD})} - Z_{1-\\alpha/2} \\right)$$"),
                     p("where \\(\\Phi\\) is the cumulative distribution function of the standard normal distribution.")
                 ),
                 
                 div(class = "math-section",
                     h3("2. Greenwood's Variance Formula for Right Censored Data"),
                     p("As sample size \\(n \\to \\infty\\), Greenwood's formula for the variance of the Kaplan-Meier estimator converges to a continuous integral. The expected proportion of subjects at risk at time \\(u\\) is the product of the event-free survival function \\(S(u)\\) and the censoring-free survival function \\(G(u)\\):"),
                     p("$$Var(\\hat{S}(t)) \\approx \\frac{S(t)^2}{n} \\int_0^t \\frac{\\lambda_T(u)}{S(u)G(u)} du$$"),
                     
                     p("Assuming independent, exponentially distributed event times \\(T \\sim \\text{Exp}(\\lambda_T)\\) and censoring times \\(C \\sim \\text{Exp}(\\lambda_C)\\), the hazards are constant. Substituting the survival functions \\(S(u) = \\exp(-\\lambda_T u)\\) and \\(G(u) = \\exp(-\\lambda_C u)\\) into the integral gives:"),
                     p("$$Var(\\hat{S}(t)) \\approx \\frac{S(t)^2}{n} \\int_0^t \\lambda_T \\exp((\\lambda_T + \\lambda_C)u) du$$"),
                     
                     p("Evaluating this integral with respect to \\(u\\) yields the variance in terms of time \\(t\\) and the hazard rates:"),
                     p("$$Var(\\hat{p}(t)) \\approx \\frac{S(t)^2}{n} \\left( \\frac{\\lambda_T}{\\lambda_T + \\lambda_C} \\right) \\left[ \\exp((\\lambda_T + \\lambda_C)t) - 1 \\right]$$"),
                     
                     p("To make this useful for sample size planning, we map the hazard rates back to the marginal cumulative risk of the event (\\(p_t\\)) and censoring (\\(p_c\\)) at time \\(t\\). Under the exponential model, \\(p_t = 1 - \\exp(-\\lambda_T t)\\) and \\(p_c = 1 - \\exp(-\\lambda_C t)\\). Taking the natural log isolates the parameters: \\(\\lambda_T t = -\\log(1 - p_t)\\) and \\(\\lambda_C t = -\\log(1 - p_c)\\)."),
                     
                     p("By multiplying the hazard ratio term by \\(t/t\\) and recognizing that \\(S(t)^2 = (1 - p_t)^2\\), we can substitute these parameterizations directly into the evaluated integral. Expanding the exponential term as \\(\\exp(\\lambda_T t) \\exp(\\lambda_C t)\\) allows us to substitute the inverse probabilities, yielding a closed-form representation of the asymptotic variance:"),
                     p("$$Var(\\hat{p}(t)) = \\frac{(1-p_t)^2}{n} \\left( \\frac{-\\log(1-p_t)}{-\\log(1-p_t) - \\log(1-p_c)} \\right) \\left[ \\frac{1}{(1-p_t)(1-p_c)} - 1 \\right]$$"),
                     
                     p(HTML("<b>A Note on Asymptotic Limitations: While this closed-form integral provides the exact <i>asymptotic</i> variance based on the data generating process, it serves as an approximation of the true variance in finite samples, particularly breaking down when risk sets become very small.</b>"))
                 ),
                 
                 div(class = "math-section",
                     h3("3. Variance Inflation for IPTW (Hajek Estimator)"),
                     p("When IPT weights are used to control confounding in a Hajek-type estimator (where weights are normalized to sum to \\(N_a\\) in each treatment group), the variance inflation factor for group \\(a\\) is:"),
                     p("$$\\nu_a = \\frac{N_a \\sum_{i=1}^N \\hat{W_i}^2 I(A_i = a)}{\\left\\{\\sum_{i=1}^N \\hat{W_i} I(A_i = a)\\right\\}^2}$$"),
                     p("where \\(A_i\\) is the treatment group indicator and \\(W_i\\) are the normalized IPT weights."),
                     p("The adjusted variance for group \\(a\\) accounting for IPTW is:"),
                     p("$$Var_{adj}(\\hat{p}_a) = \\nu_a \\times Var(\\hat{p}_a)$$"),
                     p("Therefore, the approximate variance of the difference between two means in an IPTW-weighted analysis is:"),
                     p("$$Var(\\hat{RD}) = \\nu_1 \\sigma^2_1 + \\nu_2 \\sigma^2_2$$")
                 ),
                 
                 div(class = "math-section",
                     h3("4. Risk Ratio (RR) Precision & Power"),
                     p("The Risk Ratio is evaluated on the multiplicative scale. Because the sampling distribution of a ratio is skewed, confidence intervals and power are calculated on the log scale. Using the Delta method, where \\(g(x) = \\log(x)\\) and \\(g'(x) = 1/x\\), the variance of the log proportion is:"),
                     p("$$Var(\\log(\\hat{p}_a)) \\approx \\frac{1}{p_a^2} Var(\\hat{p}_a)$$"),
                     p("The variance of the log Risk Ratio is the sum of the group log variances:"),
                     p("$$Var(\\log(\\hat{RR})) \\approx \\frac{Var(\\hat{p}_1)}{p_1^2} + \\frac{Var(\\hat{p}_2)}{p_2^2}$$"),
                     p("The standard error on the log scale is \\(se_{\\log(\\hat{RR})} = \\sqrt{Var(\\log(\\hat{RR}))}\\). The confidence interval is calculated on the log scale and exponentiated. The expected width of the Risk Ratio CI on the original scale is therefore:"),
                     p("$$W_{RR} = RR \\cdot \\left[ \\exp\\left(Z_{1-\\alpha/2} \\cdot se_{\\log(\\hat{RR})}\\right) - \\exp\\left(-Z_{1-\\alpha/2} \\cdot se_{\\log(\\hat{RR})}\\right) \\right]$$"),
                     p("Power is calculated against the null hypothesis \\(H_0: \\log(RR) = 0\\) (which mathematically corresponds to an RR of 1):"),
                     p("$$Power_{RR} = \\Phi \\left( \\frac{|\\log(RR)|}{se_{\\log(\\hat{RR})}} - Z_{1-\\alpha/2} \\right) + \\Phi \\left( \\frac{-|\\log(RR)|}{se_{\\log(\\hat{RR})}} - Z_{1-\\alpha/2} \\right)$$")
                 ),

                 div(class = "math-section",
                     h3("5. Sample Size Calculation Methodology"),
                     p("The sample size calculator solves the inverse problem: given a target power \\(Power_{target}\\), what sample size \\(n_1\\) is required? This requires numerically inverting the power formulas presented above."),

                     p(strong("The Inverse Problem:")),
                     p("For a specified allocation ratio \\(r = n_2/n_1\\), we seek \\(n_1\\) such that the achieved power equals the target power. The variance formulas depend on sample size:"),
                     p("$$Var(\\hat{p}_a) = f(p_a, p_c, \\nu_a, n_a)$$"),
                     p("where \\(f\\) represents the combined effect of censoring (via Greenwood's formula) and confounding (via IPTW variance inflation \\(\\nu_a\\))."),

                     p(strong("Numerical Root Finding:")),
                     p("Since there is no closed-form solution for \\(n_1\\) when censoring and IPTW are present, we use numerical root finding to solve:"),
                     p("$$g(n_1) = Power(n_1, n_2=r \\cdot n_1) - Power_{target} = 0$$"),
                     p("This application uses the Brent-Dekker algorithm (via R's ", code("uniroot()"), " function) to find the root of \\(g(n_1)\\) over the interval \\([10, 10^6]\\)."),

                     p(strong("For Risk Difference:")),
                     p("The objective function is:"),
                     p("$$g_{RD}(n_1) = \\left[ \\Phi \\left( \\frac{|RD|}{se_{RD}(n_1, r \\cdot n_1)} - Z_{1-\\alpha/2} \\right) + \\Phi \\left( \\frac{-|RD|}{se_{RD}(n_1, r \\cdot n_1)} - Z_{1-\\alpha/2} \\right) \\right] - Power_{target}$$"),
                     p("where \\(se_{RD}(n_1, n_2)\\) incorporates the variance formulas from Sections 2 and 3 for both groups."),

                     p(strong("For Risk Ratio:")),
                     p("Similarly, for the Risk Ratio:"),
                     p("$$g_{RR}(n_1) = \\left[ \\Phi \\left( \\frac{|\\log(RR)|}{se_{\\log(RR)}(n_1, r \\cdot n_1)} - Z_{1-\\alpha/2} \\right) + \\Phi \\left( \\frac{-|\\log(RR)|}{se_{\\log(RR)}(n_1, r \\cdot n_1)} - Z_{1-\\alpha/2} \\right) \\right] - Power_{target}$$"),

                     p(strong("Incorporating Study Complexities:")),
                     p("The sample size calculations account for censoring and confounding by using the appropriate variance formulas:"),
                     tags$ul(
                       tags$li(strong("Baseline:"), " Uses binomial variance \\(Var(\\hat{p}_a) = p_a(1-p_a)/n_a\\)"),
                       tags$li(strong("+ Censoring:"), " Uses Greenwood's closed-form variance (Section 2)"),
                       tags$li(strong("+ Censoring + IPTW:"), " Applies variance inflation \\(Var_{adj} = \\nu_a \\times Var_{Greenwood}\\)")
                     ),
                     p("These complications increase the required sample size because they inflate the variance of the estimator, requiring more observations to achieve the same statistical power.")
                 )
        ),
        tabPanel("Methodological Validation",
    fluidRow(
        column(12,
            h2("Monte Carlo Simulation: Finite Sample Validation"),
            p("To validate the approximations used in this application for sample size planning, we conducted a Monte Carlo simulation. The simulation evaluates how well the theoretical variance formulas (specifically the closed-form Greenwood formula and Kish's survey design effect) approximate the true empirical variance of the estimators in finite samples."),
            
            h3("1. Simulation Design"),
            p("Data were generated across 162 distinct scenarios, varying sample size (N=500 to 10,000), baseline risk (~5% and ~50%), censoring rates, and the strength of confounding (both the strenght of the confounder-treatment relation and confounder-outcome relation. For each scenario:"),
            tags$ul(
                tags$li("A 'Super-Population' of N=500,000 was generated to establish the true, causal marginal risks via exact potential outcomes."),
                tags$li("500 finite samples were drawn. For each sample, the data was weighted via Inverse Probability of Treatment Weighting (IPTW), and survival curves were fit using the weighted Kaplan-Meier estimator."),
                tags$li("The empirical variance of the Risk Difference (RD) and log Risk Ratio (RR) across the 500 simulations was compared directly to the theoretical variance calculated by the application's planning formulas.")
            ),
            
            h3("2. Key Findings & The Limits of Kish's Formula"),
            p("The results support the validity of the formulas for the situation when only modest confounding is present. The ratio of empirical to theoretical variance is close to 1.00."),
            p(HTML("However, the results highlight a known issue <b>Kish's Design Effect</b> for sample size planning in causal inference. Kish's formula assumes that statistical weights are independent of the outcome. In the presence of strong confounding the ratio of the actual empirical variance to the theoretical variance will be greater than 1. Kish's formula can be corrected, if pilot data are available (see Shook-sa and Hudgens.)")),
            p("As seen in the plot below, as the absolute confounding bias in the raw data increases, the theoretical variance increasingly underestimates the true empirical variance (Ratio > 1.0). Therefore, while Kish's formula provides a reliable approach for planning, researchers suspecting severe confounding should be aware that their final study may have slightly lower power than the formula predicts."),
            
            hr(),
            h3("Variance Approximation Error by Confounding Strength"),
            girafeOutput("sim_plot", width = "100%", height = "400px"),
            
            br(),
            h3("Detailed Simulation Results Grid"),
            DTOutput("sim_table")
        )
    )
)

      )
    )
  )
)

# --- 3. Server Logic ---
server <- function(input, output, session) {
  
  # Custom theme for plots
  plot_theme <- theme_bw() + 
    theme(legend.position = "bottom",
          legend.title = element_blank())
  
  # --- A. Precision & Power Reactive Data ---
  sim_data <- reactive({
    req(input$p1, input$n1, input$n2)

    alpha <- as.numeric(input$alpha)

    # Generate effect sequence and corresponding p2 based on effect measure
    if (input$effect_measure == "RD") {
      RD_seq <- seq(input$RDrange[1], input$RDrange[2], length.out = input$steps)
      p2_seq <- pmin(pmax(input$p1 + RD_seq, 0.001), 0.999) # Prevent probabilities outside [0,1]
    } else {
      RR_seq <- seq(input$RRrange[1], input$RRrange[2], length.out = input$steps)
      p2_seq <- pmin(pmax(input$p1 * RR_seq, 0.001), 0.999) # Prevent probabilities outside [0,1]
    }

    # 1. Baseline Variance
    var_base_1 <- variance_baseline(input$p1, input$n1)
    var_base_2 <- variance_baseline(p2_seq, input$n2)

    # 2. Censored Variance (Closed Form)
    var_cens_1 <- greenwood_var_closed(input$p1, input$cens_1, input$n1)
    var_cens_2 <- greenwood_var_closed(p2_seq, input$cens_2, input$n2)

    # 3. IPTW Variance
    var_iptw_1 <- variance_iptw(var_cens_1, input$v1)
    var_iptw_2 <- variance_iptw(var_cens_2, input$v2)

    # Helper function to calculate CI width and Power for both RD and RR
    calc_metrics <- function(v1, v2, desc) {
      # Risk Difference Calculation
      rd_metrics <- metrics_risk_difference(input$p1, p2_seq, v1, v2, alpha)

      # Risk Ratio Calculation
      rr_metrics <- metrics_risk_ratio(input$p1, p2_seq, v1, v2, alpha)

      data.frame(
        RD = rd_metrics$RD,
        RR = rr_metrics$RR,
        P_1 = input$p1,
        P_2 = p2_seq,
        Width_RD = rd_metrics$Width,
        Power_RD = rd_metrics$Power,
        Width_RR = rr_metrics$Width,
        Power_RR = rr_metrics$Power,
        Description = desc
      )
    }

    # Combine all scenarios
    bind_rows(
      calc_metrics(var_base_1, var_base_2, "1. Baseline"),
      calc_metrics(var_cens_1, var_cens_2, "2. +Censoring"),
      calc_metrics(var_iptw_1, var_iptw_2, "3. +Censoring +IPTW")
    )
  })
  
  # Reactive subset for dynamic plotting based on user selection
  plot_data <- reactive({
    d <- sim_data()
    if (input$effect_measure == "RD") {
      d$Effect <- d$RD
      d$Width <- d$Width_RD
      d$Power <- d$Power_RD
      d$X_Label <- "Risk Difference"
      d$Scale_Label <- "Additive Scale"
      d$Null_Hyp <- "H\u2080: RD = 0" 
    } else {
      d$Effect <- d$RR
      d$Width <- d$Width_RR
      d$Power <- d$Power_RR
      d$X_Label <- "Risk Ratio"
      d$Scale_Label <- "Multiplicative Scale"
      d$Null_Hyp <- "H\u2080: log(RR) = 0"
    }
    d
  })
  
  # --- B. IPTW Simulation Reactive Data ---
  iptw_sim_data <- reactive({
    req(input$beta)

    # Calculate prevalence based on mode
    if (input$calc_mode == "power") {
      req(input$n1, input$n2)
      prev <- min(max(input$n2 / (input$n2 + input$n1), 0.001), 0.999)
    } else {
      req(input$allocation_ratio)
      prev <- min(max(input$allocation_ratio / (input$allocation_ratio + 1), 0.001), 0.999)
    }

    n_sim <- 50000
    set.seed(101)
    X <- rnorm(n_sim)
    prob <- 1 / (1 + exp(-log(prev / (1 - prev)) - input$beta * X))
    A <- rbinom(n_sim, size = 1, prob = prob)

    if (input$weight_type == "iptw") {
      iptw <- A / prob + (1 - A) / (1 - prob)
    } else if (input$weight_type == "smrw2") {
      iptw <- A + (1 - A) * prob / (1 - prob)
    } else {
      iptw <- (1 - A) + A * (1 - prob) / prob
    }

    A_grp <- A + 1
    Treatment <- factor(A_grp, labels = c("Group 1 (Control)", "Group 2 (Treated)"))
    data.frame(X, prob, A_grp, Treatment, iptw)
  })
  
  sum_iptw <- reactive({
    dat <- iptw_sim_data()
    w1 <- dat$iptw[dat$A_grp == 1]
    var_inf_1 <- length(w1) * sum(w1^2) / (sum(w1))^2
    w2 <- dat$iptw[dat$A_grp == 2]
    var_inf_2 <- length(w2) * sum(w2^2) / (sum(w2))^2

    data.frame(
      `Variance Inflation Group 1` = round(var_inf_1, 3),
      `Variance Inflation Group 2` = round(var_inf_2, 3),
      check.names = FALSE
    )
  })

  # --- C. Sample Size Calculator Reactive Data ---
  ss_data <- reactive({
    req(input$p1, input$target_power, input$allocation_ratio)

    alpha <- as.numeric(input$alpha)
    target_power <- input$target_power

    # Generate effect sequence based on measure type
    if (input$effect_measure == "RD") {
      effect_seq <- seq(input$RDrange_ss[1], input$RDrange_ss[2], length.out = input$steps)
      # Filter out zero effect (no power calculation needed)
      effect_seq <- effect_seq[effect_seq != 0]
      p2_seq <- pmin(pmax(input$p1 + effect_seq, 0.001), 0.999)

      # Calculate required n1 for each RD
      n1_baseline <- sapply(seq_along(effect_seq), function(i) {
        solve_sample_size_rd(target_power, input$p1, p2_seq[i],
                             input$allocation_ratio, 0, 0, 1, 1, alpha)
      })

      n1_censoring <- sapply(seq_along(effect_seq), function(i) {
        solve_sample_size_rd(target_power, input$p1, p2_seq[i],
                             input$allocation_ratio,
                             input$cens_1, input$cens_2, 1, 1, alpha)
      })

      n1_iptw <- sapply(seq_along(effect_seq), function(i) {
        solve_sample_size_rd(target_power, input$p1, p2_seq[i],
                             input$allocation_ratio,
                             input$cens_1, input$cens_2,
                             input$v1, input$v2, alpha)
      })

    } else {
      # Risk Ratio
      effect_seq <- seq(input$RRrange_ss[1], input$RRrange_ss[2], length.out = input$steps)
      # Filter out RR = 1 (null)
      effect_seq <- effect_seq[effect_seq != 1]
      p2_seq <- pmin(pmax(input$p1 * effect_seq, 0.001), 0.999)

      # Calculate required n1 for each RR
      n1_baseline <- sapply(seq_along(effect_seq), function(i) {
        solve_sample_size_rr(target_power, input$p1, p2_seq[i],
                             input$allocation_ratio, 0, 0, 1, 1, alpha)
      })

      n1_censoring <- sapply(seq_along(effect_seq), function(i) {
        solve_sample_size_rr(target_power, input$p1, p2_seq[i],
                             input$allocation_ratio,
                             input$cens_1, input$cens_2, 1, 1, alpha)
      })

      n1_iptw <- sapply(seq_along(effect_seq), function(i) {
        solve_sample_size_rr(target_power, input$p1, p2_seq[i],
                             input$allocation_ratio,
                             input$cens_1, input$cens_2,
                             input$v1, input$v2, alpha)
      })
    }

    # Create data frame with results
    bind_rows(
      data.frame(
        Effect = effect_seq,
        P_2 = p2_seq,
        N1 = n1_baseline,
        N2 = n1_baseline * input$allocation_ratio,
        N_Total = n1_baseline * (1 + input$allocation_ratio),
        Description = "1. Baseline"
      ),
      data.frame(
        Effect = effect_seq,
        P_2 = p2_seq,
        N1 = n1_censoring,
        N2 = n1_censoring * input$allocation_ratio,
        N_Total = n1_censoring * (1 + input$allocation_ratio),
        Description = "2. +Censoring"
      ),
      data.frame(
        Effect = effect_seq,
        P_2 = p2_seq,
        N1 = n1_iptw,
        N2 = n1_iptw * input$allocation_ratio,
        N_Total = n1_iptw * (1 + input$allocation_ratio),
        Description = "3. +Censoring +IPTW"
      )
    )
  })

  # --- D. Output Renderers ---
  
  # Precision Plot
  output$plot_precision <- renderGirafe({
    d <- plot_data()
    
    # Construct the tooltip column BEFORE plotting
    d$tooltip_text <- sprintf(
      "<b>%s</b><br>Estimand: %s (%s)<br>Effect: %.3f<br>CI Width: %.3f", 
      d$Description, d$X_Label, d$Scale_Label, d$Effect, d$Width
    )
    
    p <- ggplot(d, aes(x = Effect, y = Width, color = Description)) +
      geom_line(alpha = 0.5) +
      geom_point_interactive(
        aes(tooltip = tooltip_text), # Map to the pre-built column cleanly
        size = 2
      ) +
      labs(y = "Confidence Interval Width\n", x = paste0("\n", d$X_Label[1])) +
      plot_theme +
      scale_color_viridis_d(end = 0.8)
    
    girafe(ggobj = p, options = list(opts_hover(css = "fill:black; stroke:black;")))
  })
  
  # Power Plot
  output$plot_power <- renderGirafe({
    d <- plot_data()
    
    # Construct the tooltip column BEFORE plotting
    d$tooltip_text <- sprintf(
      "<b>%s</b><br>Estimand: %s (%s)<br>True Effect: %.3f<br>Power (%s): %.1f%%", 
      d$Description, d$X_Label, d$Scale_Label, d$Effect, d$Null_Hyp, d$Power * 100
    )
    
    p <- ggplot(d, aes(x = Effect, y = Power, color = Description)) +
      geom_hline(yintercept = 0.8, linetype = "dashed", color = "gray50") +
      geom_line(alpha = 0.5) +
      geom_point_interactive(
        aes(tooltip = tooltip_text), # Map to the pre-built column cleanly
        size = 2
      ) +
      labs(y = "Statistical Power\n", x = paste0("\n", d$X_Label[1])) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
      plot_theme +
      scale_color_viridis_d(end = 0.8)
    
    girafe(ggobj = p, options = list(opts_hover(css = "fill:black; stroke:black;")))
  })
  
  # --- E. Sample Size Calculator Output Renderers ---
  
  # Sample Size N1 Plot
  output$plot_sample_size_n1 <- renderGirafe({
    d <- ss_data()
    effect_label <- if (input$effect_measure == "RD") "Risk Difference" else "Risk Ratio"
    
    # Construct the tooltip column BEFORE plotting
    d$tooltip_text <- sprintf(
      "<b>%s</b><br>%s: %.3f<br>Required n1: %d<br>Required n2: %d",
      d$Description, effect_label, d$Effect, d$N1, d$N2
    )
    
    p <- ggplot(d, aes(x = Effect, y = N1, color = Description)) +
      geom_line(alpha = 0.5) +
      geom_point_interactive(
        aes(tooltip = tooltip_text), # Map to the pre-built column cleanly
        size = 2
      ) +
      labs(y = "Required Sample Size in Group 1 (n1)\n",
           x = paste0("\n", effect_label),
           title = paste0("Sample Size for ", input$target_power * 100, "% Power")) +
      plot_theme +
      scale_color_viridis_d(end = 0.8) +
      scale_y_continuous(labels = scales::comma)
    
    girafe(ggobj = p, options = list(opts_hover(css = "fill:black; stroke:black;")))
  })
  
  # Total Sample Size Plot
  output$plot_sample_size_total <- renderGirafe({
    d <- ss_data()
    effect_label <- if (input$effect_measure == "RD") "Risk Difference" else "Risk Ratio"
    
    # Construct the tooltip column BEFORE plotting
    d$tooltip_text <- sprintf(
      "<b>%s</b><br>%s: %.3f<br>Total N: %d<br>(n1=%d, n2=%d)",
      d$Description, effect_label, d$Effect, d$N_Total, d$N1, d$N2
    )
    
    p <- ggplot(d, aes(x = Effect, y = N_Total, color = Description)) +
      geom_line(alpha = 0.5) +
      geom_point_interactive(
        aes(tooltip = tooltip_text), # Map to the pre-built column cleanly
        size = 2
      ) +
      labs(y = "Total Sample Size (n1 + n2)\n",
           x = paste0("\n", effect_label),
           title = "Total Study Sample Size") +
      plot_theme +
      scale_color_viridis_d(end = 0.8) +
      scale_y_continuous(labels = scales::comma)
    
    girafe(ggobj = p, options = list(opts_hover(css = "fill:black; stroke:black;")))
  })
  
  # Simulation Histogram
  output$plot_ps <- renderGirafe({
    binwidth <- 0.02
    p <- ggplot(iptw_sim_data(), aes(x = prob, fill = Treatment)) +
      geom_histogram_interactive(
        aes(y = after_stat(density), tooltip = Treatment),
        alpha = 0.75, binwidth = binwidth, 
        position = position_dodge(width = binwidth/4)
      ) +
      labs(x = "Propensity Score", y = "Density\n") +
      scale_x_continuous(limits = c(-0.01, 1.01)) +
      plot_theme +
      scale_fill_viridis_d(end = 0.8, direction = -1)
    
    girafe(ggobj = p, options = list(opts_hover(css = "fill:black; stroke:black;")))
  })
  
  output$table_iptw <- renderTable({ sum_iptw() }, align = 'c')
  
  observeEvent(input$autopop, {
    vi <- sum_iptw()
    updateNumericInput(session, "v1", value = vi[1, 1])
    updateNumericInput(session, "v2", value = vi[1, 2])
  })
  
  # Main Data Table
  output$table_results <- renderDT({
    sim_data() %>%
      mutate(Power_RD = sprintf("%.1f%%", Power_RD * 100),
             Power_RR = sprintf("%.1f%%", Power_RR * 100),
             across(starts_with("Width"), ~round(., 4)),
             across(c(RD, RR), ~round(., 4))) %>%
      rename(`Prob Grp 1` = P_1, `Prob Grp 2` = P_2) %>%
      datatable(
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          pageLength = 15,
          scrollX = TRUE
        ),
        class = "display compact"
      )
  })

  # --- F. Methodological Validation Output Renderers ---

  # Load simulation results
  validation_data <- reactive({
    tryCatch({
      read.csv("results.csv", stringsAsFactors = FALSE)
    }, error = function(e) {
      warning("Could not load results.csv: ", e$message)
      return(NULL)
    })
  })

  # Validation plot: Variance Ratio by Confounding Strength
  output$sim_plot <- renderGirafe({
    req(validation_data())
    d <- validation_data()

    d_plot <- d %>%
      #filter(Abs_Bias_RD > 0) %>%
      mutate(
        Scenario = paste(BaseRisk, Censoring, sep = ", "),
        Sample_Size = factor(N, levels = c(500, 2000, 10000))
      )

    p1 <- ggplot(d_plot, aes(x = Abs_Bias_RD, y = Ratio_RD, color = Assoc_AX)) +
      geom_hline(yintercept = 1.0, linetype = "dashed", color = "gray50") +
      geom_point_interactive(
        aes(tooltip = sprintf("<b>N=%d</b><br>Baseline Risk: %s<br>A-X Association: %s<br>Y-X Association: %s<br>Abs Bias (RD): %.3f<br>Variance Ratio: %.3f", N, BaseRisk, Assoc_AX,Assoc_YX, Abs_Bias_RD, Ratio_RD)),
        size = 3, alpha = 0.7
      ) + 
      labs(
        x = "\nAbsolute Confounding Bias (Raw Risk Difference)",
        y = "Empirical Variance / \nTheoretical Variance\n",
        color = "Sample Size",
        title = "Variance Approximation Error vs. Confounding Strength"
      ) +
      plot_theme +
      scale_color_viridis_d(end = 0.8)
    
    p2 <- ggplot(d_plot, aes(x = Abs_Bias_RR, y = Ratio_logRR, color = Assoc_AX)) +
      geom_hline(yintercept = 1.0, linetype = "dashed", color = "gray50") +
      geom_point_interactive(
        aes(tooltip = sprintf("<b>N=%d</b><br>Baseline Risk: %s<br>A-X Association: %s<br>Y-X Association: %s<br>Abs Bias (log RR): %.3f<br>Variance Ratio: %.3f", N, BaseRisk, Assoc_AX,Assoc_YX, Abs_Bias_RR, Ratio_logRR)),
        size = 3, alpha = 0.7
      ) + 
      labs(
        x = "\nAbsolute Confounding Bias (Log Risk Ratio)",
        y = "Empirical Variance / \n Theoretical Variance\n",
        color = "Sample Size",
        title = "Variance Approximation Error vs. Confounding Strength"
      ) +
      plot_theme +
      scale_color_viridis_d(end = 0.8)
      
    p = p1 / p2

    girafe(ggobj = p, options = list(opts_hover(css = "fill:black; stroke:black;")))
  })

  # Validation table: Full simulation results
  output$sim_table <- renderDT({
    req(validation_data())
    d <- validation_data()

    d %>%
      select(N, BaseRisk, Censoring, Assoc_YX,
             True_p1, True_p2, Abs_Bias_RD,
             Theo_Var_RD, Emp_Var_RD, Ratio_RD,
             Theo_Var_logRR, Emp_Var_logRR, Ratio_logRR) %>%
      mutate(
        across(c(True_p1, True_p2, Abs_Bias_RD), ~round(., 3)),
        across(starts_with("Theo_"), ~format(., scientific = TRUE, digits = 3)),
        across(starts_with("Emp_"), ~format(., scientific = TRUE, digits = 3)),
        across(starts_with("Ratio_"), ~round(., 3))
      ) %>%
      rename(
        `Sample Size` = N,
        `Base Risk` = BaseRisk,
        `Censoring` = Censoring,
        `Confounding` = Assoc_YX,
        `True p1` = True_p1,
        `True p2` = True_p2,
        `Abs Bias (RD)` = Abs_Bias_RD,
        `Theoretical Var (RD)` = Theo_Var_RD,
        `Empirical Var (RD)` = Emp_Var_RD,
        `Ratio (RD)` = Ratio_RD,
        `Theoretical Var (log RR)` = Theo_Var_logRR,
        `Empirical Var (log RR)` = Emp_Var_logRR,
        `Ratio (log RR)` = Ratio_logRR
      ) %>%
      datatable(
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel'),
          pageLength = 20,
          scrollX = TRUE
        ),
        class = "display compact"
      )
  })
}

shinyApp(ui = ui, server = server)