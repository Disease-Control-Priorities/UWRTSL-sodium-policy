# CLAUDE.md

This file provides structured guidance for Claude when working on the
**Sodium Policy Impact & Economic Evaluation Platform** repository.

This repository implements a national-scale comparative risk assessment
(CRA), life-table modeling system, economic evaluation framework, and
interactive Shiny simulation platform to evaluate dietary sodium
reduction policies.

------------------------------------------------------------------------

# Project Summary

The platform estimates the **health and economic impact of achieving a
30% reduction in population sodium intake** through coordinated policy
interventions.

Core components:

-   Comparative Risk Assessment (CRA)
-   Sodium → SBP → Cardiovascular disease causal pathway modeling
-   Population Impact Fraction (PIF) estimation
-   Cause-deleted or multi-decrement life tables
-   Implementation costing (ingredients-based)
-   Economic valuation (healthcare savings, productivity, mortality
    valuation)
-   Interactive Shiny simulation interface

Primary outcomes:

-   Deaths averted
-   Cardiovascular mortality reductions
-   Implementation costs
-   Healthcare cost offsets
-   Productivity gains
-   Net Present Value (NPV)
-   Benefit--Cost Ratios (BCR)

------------------------------------------------------------------------

# Technical Skills Required

Claude should assume the following technical competencies are required.

## 1. Advanced R Programming

-   Modular function development
-   Reproducible project structuring
-   Efficient memory management
-   Scenario-based simulation logic
-   Clean separation between modeling layers (CRA, life-table, costing,
    economic)

This is a modeling-first project. Avoid unnecessary abstraction or
stylistic refactors unless requested.

------------------------------------------------------------------------

## 2. data.table (High Priority)

The project relies on `data.table` for performance, especially when
handling:

-   Age × sex × year × cause panels
-   Intake distributions
-   Mortality projections
-   Scenario comparisons

Claude must:

-   Use in-place updates (`:=`)
-   Use grouped calculations via `by=`
-   Use keyed joins for merges
-   Avoid converting to data.frame unnecessarily
-   Validate row counts after joins

Expected pattern:

``` r
dt[, pif := (RR_new - 1) / RR_new,
   by = .(age, sex, cause, year)]
```

Multi-key join example:

``` r
setkey(dt_exposure, age, sex)
setkey(dt_mortality, age, sex)
dt_mortality[dt_exposure]
```

Efficiency is important because scenarios may be simulated across
multiple years and sensitivity runs.

------------------------------------------------------------------------

## 3. Comparative Risk Assessment (CRA)

Claude must understand:

-   Exposure distributions (mean, SD, percentiles)
-   Exposure--response functions
-   Relative risk transformations
-   Population Impact Fraction (PIF)
-   Counterfactual exposure shifts (e.g., 30% sodium reduction)
-   Joint mediation pathway (Sodium → SBP → CVD)

Interventions modify exposure distributions, not mortality directly.

------------------------------------------------------------------------

## 4. Life-Table & Mortality Modeling

Required knowledge:

-   Cause-deleted life tables
-   Multi-decrement life tables
-   Age-specific mortality rates (mx)
-   Survival probabilities
-   Years of Life Lost (YLL)
-   Aggregation across age groups

The modeling structure typically aggregates:

age × sex × year × cause

Consistency across dimensions is critical.

------------------------------------------------------------------------

## 5. Economic Evaluation Skills

Claude should understand:

-   Ingredients-based costing
-   Programmatic vs healthcare costs
-   Discounting
-   Net Present Value (NPV)
-   Benefit--Cost Ratio (BCR)
-   Productivity gains estimation
-   Monetized mortality valuation (e.g., VSL or VSLY frameworks)

Economic modules must remain modular and separable from health impact
modules.

------------------------------------------------------------------------

## 6. Sensitivity & Scenario Analysis

Claude should support:

-   Alternative compliance assumptions
-   Alternative scale-up speeds
-   Alternative discount rates
-   Alternative valuation parameters
-   Deterministic and probabilistic sensitivity analysis

Scenario logic must be parameterized, not hard-coded.

------------------------------------------------------------------------

## 7. Shiny Application Awareness

The `/app/` directory contains an interactive Shiny interface.

Claude must:

-   Keep model functions separate from UI logic
-   Avoid embedding heavy computations directly in UI components
-   Ensure reproducibility of results outside the Shiny app
-   Maintain stable input/output object structures

------------------------------------------------------------------------

# Repository Structure Awareness

Core directories:

/data/ Input datasets (intake, mortality, costs) /models/ CRA and
life-table modules /costing/ Implementation costing functions /
economic/ Valuation and BCR analysis / app/ Shiny application / output/
Results and figures / docs/ Technical documentation

Claude should modify only the relevant module when implementing changes.

------------------------------------------------------------------------

# Coding Standards

Claude should:

1.  Use data.table as the default data structure.
2.  Avoid unnecessary copying of large objects.
3.  Preserve dimensional integrity after joins.
4.  Keep exposure modeling separate from economic modules.
5.  Maintain transparent parameter definitions.
6.  Avoid rewriting modeling architecture without explicit request.
7.  Preserve reproducibility and modularity.

------------------------------------------------------------------------

# When Assisting With This Project

Claude should prioritize:

-   Correct epidemiologic reasoning
-   Transparent CRA implementation
-   Economically coherent valuation logic
-   Performance efficiency
-   Clean separation between health and economic layers
-   Reproducibility of scenario outputs

Avoid over-engineering, unnecessary tidyverse refactors, or abstraction
layers that reduce clarity of policy modeling logic.

------------------------------------------------------------------------

# Summary

This repository implements a modular sodium policy simulation platform
integrating CRA, life-table modeling, costing, and economic evaluation,
supported by an interactive Shiny interface.

Any assistance should reinforce:

-   Epidemiological correctness
-   data.table fluency
-   Economic evaluation rigor
-   Policy interpretability
-   Computational efficiency
