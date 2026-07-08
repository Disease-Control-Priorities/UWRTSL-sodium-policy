# Sodium Policy Impact & Economic Evaluation Platform

## Overview

This repository contains the modeling framework, economic evaluation
tools, and interactive simulation platform developed to estimate the
**national-scale health and economic impact of dietary sodium reduction
policies** in countries where implementation efforts are underway.

The project integrates:

-   Comparative Risk Assessment (CRA)
-   Life-table and mortality modeling
-   Policy implementation costing
-   Full economic benefit analysis
-   Interactive scenario simulation via Shiny

The primary objective is to quantify the **deaths averted**,
**implementation costs**, and **economic benefits** of achieving a 30%
reduction in population sodium intake through coordinated policy action.

------------------------------------------------------------------------

## Project Objectives

1.  **Estimate Health Impact**
    -   Quantify sodium-attributable cardiovascular mortality at
        baseline.
    -   Simulate deaths averted under reformulation, labeling,
        procurement, and education policy scenarios.
    -   Evaluate alternative implementation intensities and compliance
        assumptions.
2.  **Conduct Economic Evaluation**
    -   Estimate direct programmatic implementation costs using an
        ingredients-based approach.
    -   Calculate healthcare cost offsets from reduced cardiovascular
        events.
    -   Estimate productivity gains and monetized mortality benefits.
    -   Compute net present value (NPV) and benefit--cost ratios (BCR).
3.  **Develop Interactive Policy Simulation Tool**
    -   Provide scenario-based modeling via a Shiny application.
    -   Allow adjustment of scale-up speed, compliance levels, discount
        rates, and valuation assumptions.
    -   Support transparent and reproducible policy decision-making.

------------------------------------------------------------------------

## Modeling Framework

The analytical structure consists of:

-   Population-level sodium intake distributions
-   Exposure-response functions linking sodium → systolic blood pressure
    → cardiovascular outcomes
-   Population Impact Fraction (PIF) calculations
-   Cause-deleted or multi-decrement life tables
-   Economic valuation modules (healthcare savings + productivity +
    mortality valuation)
-   Sensitivity and uncertainty analysis

The framework is modular, enabling adaptation across countries and
policy environments.

------------------------------------------------------------------------

## Repository Structure

    /data/              # Input datasets (intake distributions, mortality, costs)
    /models/            # CRA and life-table modeling scripts
    /costing/           # Intervention costing modules
    /economic/          # Benefit-cost and valuation analysis
    /app/               # Shiny application
    /output/            # Simulation outputs and figures
    /docs/              # Documentation and technical notes

------------------------------------------------------------------------

## Key Outputs

-   Premature deaths averted (annual and cumulative)
-   Changes in cardiovascular mortality rates
-   Intervention implementation costs
-   Healthcare expenditures averted
-   Productivity gains
-   Net present value (NPV)
-   Benefit--cost ratios (BCR)
-   Scenario comparison dashboards

------------------------------------------------------------------------

## Policy Relevance

This platform provides decision-makers with:

-   Country-specific estimates of sodium reduction impact
-   Transparent economic justification for policy adoption
-   Scenario analysis tools to explore implementation trade-offs
-   Evidence aligned with global sodium reduction targets

------------------------------------------------------------------------

## Reproducibility & Transparency

All modeling assumptions, equations, and parameters are explicitly
documented. The repository is designed to ensure:

-   Version control for policy scenarios
-   Modular updates for country-specific inputs
-   Open and reproducible economic evaluation workflows

------------------------------------------------------------------------

## Contact

For questions, collaboration inquiries, or implementation support,
please open an issue in this repository.
