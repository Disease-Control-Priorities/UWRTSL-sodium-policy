# Claude Code prompt: implement baseline-potassium heterogeneity in the LSS model

You are working in the `UWRTSL-sodium-policy` repository. Implement the changes below in the existing pipeline and report.

## Objective

Revise the primary low-sodium salt substitute (LSS) pathway so that it implements the following approach transparently and consistently:

1. Estimate the blood-pressure effect of reduced sodium separately.
2. Estimate the blood-pressure effect of increased potassium separately.
3. Add the sodium- and potassium-mediated SBP effects.
4. Allow the potassium effect to vary according to baseline population potassium intake or urinary potassium.
5. Give potassium-deficient populations a larger potassium-mediated effect and potassium-replete populations a smaller effect.
6. Use the subgroup estimates from Filippini et al. (2020), with explicit and auditable conversions between dietary potassium intake and 24-hour urinary potassium excretion.

This is an analytical and methods update only. Do **not** undertake additional Bloomberg table/figure reporting work; wait for the Resolve team to specify those outputs.

## Scope and file discipline

- Inspect the relevant files under `code/`, but edit only files that are strictly required.
- The principal code change should be in `code/07_run_interventions.R`, where the current LSS constants, potassium dose-response function, primary `na_k_sbp` pathway, LSS audit, cluster exports, and `run_config.rds` metadata are defined.
- Edit another `code/*` file only if inspection proves it is necessary for the implemented pathway to run correctly. Do not edit `code/00_run_model.R`, `code/01_utils.R`, `code/02_load_inputs.R`, `code/03_clean_inputs.R`, `code/04_define_intervention.R`, or `code/05_build_baseline.R` merely for stylistic consistency.
- Modify `reports/report.RMD` only where the LSS methods, equations, parameter tables, audit tables, Huang comparison, limitations, or LSS interpretation need to reflect the implemented change.
- Do not alter unrelated intervention logic, scenarios, fiscal-policy code, Euromonitor trends, sodium source shares, styling, output structure, or narrative.
- Do not rename existing LSS scenarios or change their reach, coverage, uptake, adherence, timing, or SSaSS benchmark logic.
- Do not create new reports, scripts, data files, or documentation files unless a pre-existing test framework requires a narrowly scoped test file.
- Preserve all unrelated user changes already present in the working tree.
- Before editing, inspect `git status` and the relevant diffs. After editing, use `git diff --check` and review the final diff to ensure that only necessary lines changed.

## Evidence to use

Use the repository copies of:

- Filippini et al. (2020), *Potassium Intake and Blood Pressure: A Dose-Response Meta-Analysis of Randomized Controlled Trials*.
- Huang et al. (2026), the global potassium-enriched salt/LSS modelling analysis.
- Reddin et al. (2023) baseline potassium inputs already consumed by the pipeline.

Do not invent effect estimates. Use the numerical values below, which come from Filippini Figures 2 and S16.

## Required primary mathematical implementation

Use the sign convention that a positive `sbp_reduction` means a reduction in SBP.

### A. Sodium-mediated SBP effect

Retain the current sodium-mediated SBP calculation unchanged:

\[
\Delta SBP_{Na}
=
\{2.8H+1.0(1-H)\}\Delta Na,
\]

where `H` is the raised-BP proportion and `Delta Na` is achieved sodium reduction in g/day. Continue applying the existing WHO 2 g/day sodium floor, reach/coverage, uptake/adherence, and scale-up logic exactly as currently implemented.

Do not let the new potassium code alter the sodium-only policy scenarios.

### B. Convert baseline dietary potassium to urinary potassium

The existing Reddin input is dietary potassium intake in g/day. Convert it to baseline 24-hour urinary potassium excretion as:

\[
uK_0\;(mmol/day)
=
\frac{K_{0,intake}\;(g/day)\times1000}
{39.1\;(mg/mmol)\times1.3\;(intake/excretion)}.
\]

Retain the existing constants unless inspection identifies a genuine inconsistency:

- `K_MG_PER_MMOL = 39.1`.
- `K_INTAKE_TO_EXCRETION = 1.3`.

Document that the factor of 1.3 is applied only because the Reddin input is dietary intake. It must not be applied to a value that is already urinary excretion.

The Filippini baseline-potassium threshold is:

- `uK0 < 75 mmol/day`: lower-baseline-potassium group.
- `uK0 >= 75 mmol/day`: higher-baseline-potassium group.

Document the equivalent quantities:

- 75 mmol/day urinary potassium = approximately 2.93 g/day urinary potassium.
- With the 1.3 intake/excretion conversion, this corresponds to approximately 3.81 g/day dietary potassium.

### C. Convert potassium added by LSS to change in urinary potassium

Retain the current 75% NaCl/25% KCl stoichiometry and the existing computation of potassium added through LSS. Clearly distinguish dietary potassium added from the implied urinary-excretion change:

\[
\Delta uK\;(mmol/day)
=
\frac{\Delta K_{intake}\;(g/day)\times1000}
{39.1\times1.3}.
\]

The current variable `k_added_mmol` represents dietary mmol/day before division by 1.3. Rename it locally where safe, or add unambiguous audit fields such as:

- `potassium_added_intake_mmol`;
- `delta_urinary_potassium_mmol`.

Avoid breaking external consumers unnecessarily. If an existing ambiguous field must be retained for backward compatibility, retain it as an explicitly documented alias and make the new unambiguous fields authoritative.

### D. Potassium-mediated SBP dose response from Filippini Figure 2

Replace the active primary potassium calculation based on the Figure 3 achieved-excretion difference

```r
f(k_base_excr) - f(k_post_excr)
```

with an incremental urinary-potassium dose-response based on Filippini Figure 2.

Define a swappable anchor table equivalent to:

```r
LSS_K_DELTA_SBP_ANCHORS <- data.table(
  delta_uk_mmol   = c(0, 30, 60, 90, 120),
  sbp_change_mmhg = c(0, -3.3, -2.0, 1.1, 4.2)
)
```

Here `sbp_change_mmhg` is the treated-minus-control SBP difference reported by Filippini, so negative values indicate lower SBP. Implement a clearly named interpolation helper that returns a **positive SBP reduction**, for example:

```r
k_delta_to_sbp_reduction <- function(delta_uk_mmol,
                                     anchors = LSS_K_DELTA_SBP_ANCHORS) {
  -stats::approx(
    x = anchors$delta_uk_mmol,
    y = anchors$sbp_change_mmhg,
    xout = delta_uk_mmol,
    method = "linear",
    rule = 2
  )$y
}
```

Use the published anchors rather than inserting an artificial 80 mmol/day anchor. Document that linear interpolation implies an approximate zero crossing at:

\[
60+30\frac{2.0}{2.0+1.1}\approx79.4\;mmol/day.
\]

Thus the curve has increasing benefit up to approximately 30 mmol/day, diminishing benefit thereafter, and a possible adverse SBP response above approximately 79–80 mmol/day. Do not silently truncate negative potassium benefits to zero in the primary analysis. Add an audit flag/count for rows exceeding the approximate zero-crossing because the high-dose estimates are imprecise.

### E. Baseline-potassium subgroup modification from Filippini Figure S16

Filippini reports the following pooled SBP differences:

- baseline `uK < 75 mmol/day`: -4.31 mmHg;
- baseline `uK >= 75 mmol/day`: -3.21 mmHg;
- overall: -3.90 mmHg.

Normalize the subgroup effects to the overall relationship so that the Figure 2 curve remains the population-average curve:

\[
m_K(uK_0)=
\begin{cases}
4.31/3.90=1.1051, & uK_0<75,\\
3.21/3.90=0.8231, & uK_0\ge75.
\end{cases}
\]

Implement these as named constants or a small parameter table; do not hard-code unexplained decimal values inside the calculation.

Then calculate:

\[
\Delta SBP_K
=
[-g(\Delta uK)]\times m_K(uK_0),
\]

where `g()` is the Figure 2 treated-minus-control curve and the leading minus sign converts it to the model's positive-reduction convention.

The central estimate for the higher-baseline-potassium group is 0.8231, not zero. Do not describe a negligible effect as Filippini's central estimate. A zero effect for the potassium-replete group may be documented as a possible future conservative sensitivity, but do not add a new reported scenario unless one already exists and can be updated without changing the scenario registry.

### F. Add sodium and potassium effects

For the primary LSS `na_k_sbp` pathway, calculate:

\[
\Delta SBP_{LSS}
=
\Delta SBP_{Na}
+a\Delta SBP_K,
\]

where `a = LSS_ADDITIVITY_FACTOR`, equal to 1.0 in the primary analysis. Preserve the existing 0.8 Huang-style additivity sensitivity hook.

Feed the combined SBP change through the existing ETTEHAD/GBD SBP-to-cause incidence machinery exactly once. Preserve the current guardrail preventing the mechanistic Na/K-SBP pathway and the SSaSS trial-RR pathway from being applied simultaneously to stroke.

## Retire the previous primary modifiers without deleting history

The new primary potassium pathway is modified by baseline potassium status. Therefore, do not also apply the previous active:

- baseline-sodium multipliers from `LSS_K_NA_MODULATION`;
- non-hypertensive multiplier from `LSS_K_NONHTN_FACTOR`;
- Figure 3 achieved-excretion difference `f(uK0) - f(uK1)`.

Applying these together would introduce additional multiplicative interactions not jointly estimated by Filippini and would risk double-counting heterogeneity.

In the code, add concise comments using a label such as:

```r
# UPDATED vs previous implementation:
```

The comments must state that the previous version used the Figure 3 achieved-excretion curve plus baseline-sodium and non-hypertensive modifiers, while the implemented primary version uses the Figure 2 change-in-urinary-potassium curve with the Figure S16 baseline-potassium subgroup modifier.

Do **not** retain or paste large duplicated blocks of obsolete code as comments. Remove unused executable constants/helpers where safe, or retain them only if an existing sensitivity still uses them. If retained, label them clearly as legacy/sensitivity-only and ensure they cannot affect the primary pathway.

## Audit and run-configuration requirements

Update `lss_audit.rds` generation so that the audit uses the same functions and constants as the actual model. Do not let the audit independently reimplement a different potassium formula.

For each applicable country × sex × LSS scenario, include or preserve clearly named fields for:

- baseline dietary potassium, g/day;
- baseline urinary potassium, mmol/day;
- baseline-potassium group (`<75` or `>=75 mmol/day`);
- baseline-potassium multiplier;
- potassium added as dietary g/day;
- potassium added as dietary mmol/day;
- change in urinary potassium, mmol/day;
- Figure 2 unmodified potassium-mediated SBP reduction;
- modified potassium-mediated SBP reduction;
- sodium-mediated SBP reduction;
- combined SBP reduction;
- additivity factor;
- indicator that `delta_urinary_potassium_mmol` exceeds approximately 79.4;
- statement that hyperkalemia/CKD harms are not modelled.

Update `run_config.rds` metadata so the report can read rather than re-create:

- the primary potassium-effect method name, e.g. `filippini_fig2_delta_uk_baseline_uk_modifier`;
- Figure 2 anchor table;
- baseline-uK threshold;
- low- and high-baseline-uK multipliers;
- conversion constants;
- additivity settings;
- whether previous sodium/BP modifiers are active in the primary pathway (they should be false).

Keep the existing overall LSS method/scenario identifier `na_k_sbp` unless changing it is absolutely necessary; use a more specific metadata field to identify the new potassium submethod. This avoids breaking scenario names, output readers, and report logic.

Ensure any new helper or parameter object required by parallel workers is included in the existing cluster-export mechanism.

## Required `reports/report.RMD` changes

Update only the LSS-related sections. The report must consume `run_config.rds` and `lss_audit.rds` as its source of truth rather than duplicating calculations.

Revise the report to explain clearly:

1. The sodium and potassium SBP effects are estimated separately and then added.
2. The sodium-mediated calculation is unchanged from the preceding implementation.
3. The implemented potassium pathway now uses the Figure 2 **change in urinary potassium** dose-response.
4. Baseline potassium modifies the potassium-mediated effect using the Figure S16 `<75` versus `>=75 mmol/day` subgroup estimates.
5. The exact dietary-intake-to-urinary-excretion conversion, including the 39.1 mg/mmol and 1.3 factors.
6. The 75 mmol/day urinary threshold corresponds to approximately 2.93 g/day urinary potassium and 3.81 g/day dietary intake.
7. The potassium curve has maximum estimated benefit near a 30 mmol/day urinary increase and crosses approximately zero near 79.4 mmol/day; estimates at high doses are imprecise.
8. The higher-baseline-potassium group receives a smaller central multiplier (approximately 0.82), not a zero effect.
9. Sodium and potassium effects are additive in the primary analysis, with the existing 80% additivity sensitivity retained.
10. The model remains benefit-focused with respect to CKD/hyperkalemia because it does not include a CKD/hyperkalemia harm state. However, the Figure 2 SBP curve itself must be allowed to yield an adverse SBP change at sufficiently high `delta uK`.

Add a concise, explicit subsection or comparison table titled along the lines of **“Change from the previous LSS implementation”** with the following comparison:

| Element | Previous implementation | Implemented version |
|---|---|---|
| Potassium exposure-response | Figure 3 achieved-excretion curve evaluated at baseline and post-LSS | Figure 2 change-in-urinary-potassium curve |
| Baseline heterogeneity | Continuous baseline level through achieved-excretion curve, plus sodium and BP modifiers | Figure S16 baseline urinary potassium `<75` vs `>=75 mmol/day` |
| Potassium subgroup multipliers | Baseline-sodium bands and non-hypertensive attenuation | 1.1051 for `<75`; 0.8231 for `>=75` |
| Sodium channel | Existing sodium-reduction-to-SBP calculation | Unchanged |
| Combination | Additive Na + K, 80% sensitivity hook | Unchanged |

Also update, where applicable:

- the LSS parameter table;
- baseline-potassium table notes;
- the LSS audit table and column labels;
- the Huang-versus-current-model comparison;
- mathematical equations describing `Delta SBP_K`;
- the implementation limitations;
- any downstream deck artifact variables created inside `report.RMD` that currently expect the old `run_config` or audit fields.

Remove or rewrite statements claiming that the primary potassium effect uses the Figure 3 achieved-excretion curve, baseline-sodium modulation, or non-hypertensive attenuation. Do not change unrelated report narrative, tables, figures, formatting, or references.

Clarify accurately that Huang et al. already allowed the potassium-SBP effect to vary with baseline sodium; the distinguishing update here is the explicit use of country- and sex-specific baseline potassium status with Filippini's baseline-potassium subgroup estimates.

Retain the report's existing source/citation conventions. Use the existing Filippini, Huang, and Reddin bibliography keys if present; do not edit bibliography files unless compilation fails solely because a required existing citation key is missing.

## Required validation

Before considering the work complete, perform targeted tests or assertions demonstrating that:

1. `k_delta_to_sbp_reduction()` returns approximately `0`, `3.3`, `2.0`, `-1.1`, and `-4.2` mmHg at `delta uK = 0`, `30`, `60`, `90`, and `120 mmol/day`, respectively.
2. The interpolated zero crossing is approximately `79.4 mmol/day`.
3. The lower-baseline-potassium multiplier is `4.31/3.90` and is larger than the higher-baseline multiplier `3.21/3.90`.
4. A baseline dietary intake of approximately 3.81 g/day maps to approximately 75 mmol/day urinary potassium.
5. The potassium effect is zero before intervention scale-up because `delta uK` is zero—not because of an unrelated clamp.
6. Sodium-only scenarios reproduce their preceding results or calculations within numerical tolerance.
7. In LSS rows, `combined SBP reduction = sodium SBP reduction + additivity factor × potassium SBP reduction` within numerical tolerance.
8. The mechanistic pathway and SSaSS trial-RR benchmark remain mutually exclusive for stroke.
9. No LSS audit rows have unresolved baseline potassium, subgroup, multiplier, or conversion values.
10. Audit quantities are generated from the same helper/constants used by the modeled pathway.
11. Existing LSS scenario names, reach definitions, coverage, uptake, adherence, and scale-up years remain unchanged.
12. `reports/report.RMD` parses and renders successfully using regenerated pipeline artifacts. If a full render cannot be completed because an external dependency or input is unavailable, run the strongest available syntax/chunk-level checks and report the exact blocker.

Do not weaken existing assertions or error handling to make tests pass.

## Work sequence

1. Inspect the current implementation and report references.
2. State a short file-level plan identifying exactly which files need edits and why.
3. Implement the smallest coherent change.
4. Regenerate only the artifacts needed to test the LSS pathway and report. Do not include generated outputs in the source diff unless they are intentionally version-controlled and required by the repository workflow.
5. Run the targeted numerical, structural, and render checks.
6. Review the final diff for unnecessary changes.

## Final response

Report:

- files changed;
- the implemented formula and subgroup multipliers;
- how the new version differs from the previous implementation;
- tests and render checks run, with results;
- any remaining limitations, especially study-level subgroup evidence, uncertainty at high potassium changes, absence of joint interaction estimates, and absence of CKD/hyperkalemia harms.

Do not claim completion if the model and audit use different potassium calculations or if the report still describes the previous Figure 3 implementation as current.
