# rareSurrogate

**Surrogate-Guided Sampling for Rare Outcome Classification**

`rareSurrogate` is an R package for simulating and analyzing surrogate-guided sampling (SGS) designs for rare outcomes under partial validation. The methods are inspired by Tan and Heagerty (2019), which study efficient sampling strategies using surrogate variables in electronic medical record (EMR) data.

---

## Overview

In many applications (e.g., EMR data), the true outcome is expensive or difficult to obtain and is only observed for a subset of subjects. A surrogate variable can be used to guide which observations are selected for validation.

This package provides tools to:

- Simulate high-dimensional binary data with surrogate variables  
- Implement sampling designs:
  - Simple random sampling (SRS)  
  - Surrogate-guided sampling (SGS)  
- Compute optimal SGS allocation based on surrogate predictive values  
- Calibrate model parameters using grid search  
- Estimate predictive performance using inverse probability weighting (IPW)

## ⚙️ Installation

```r
# install.packages("devtools")
devtools::install_github("khuetu/rareSurrogate")
