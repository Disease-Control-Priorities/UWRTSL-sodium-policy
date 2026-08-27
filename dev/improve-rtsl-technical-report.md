# Claude Code prompt: improve the Resolve to Save Lives sodium technical report

```text
You are working in the existing sodium-policy modeling repository. Improve the executive and technical quality of the Word technical report while preserving the model, results, and current report architecture.

## Objective

Revise the technical report so it reads and looks like a high-quality Resolve to Save Lives (RTSL) product: concise, results-driven, technically rigorous, useful to executives and technical reviewers, and visually consistent with RTSL's current healthier-food communications.

The final deliverable must be a successfully rendered Word `.docx` document.

## Strict scope

Modify only these two source files:

1. `report/technical_report.RMD`
2. `reports/report.RMD`

Before editing, confirm both are tracked files at those exact paths. If the repository instead contains only a hyphenated or differently located version of the technical-report RMD, do not create a duplicate, rename a file, or edit a third source file. Stop and report the path discrepancy.

Do not modify any R scripts, input data, model outputs, bibliographies, CSL files, Word templates, images, spreadsheets, configuration files, or other repository files. Do not change model logic, intervention definitions, scenario values, effect sizes, or numerical results.

Rendering may create the final `.docx` and temporary/generated report assets, but source-code changes must remain limited to the two RMD files. Do not commit or manually edit generated assets.

## Required producer-consumer architecture

Preserve and strengthen the existing separation of responsibilities:

- `reports/report.RMD` is the authoritative producer. It must prepare every report-facing number, display-ready table, figure, title, note, source statement, caption identifier, and metadata item, then store them in the technical-report bundle.
- `report/technical_report.RMD` is the presentation/consumer layer. It may load the bundle and apply Word rendering helpers, but it must not read model inputs directly, recalculate results, recreate plotted data, or contain a second version of any analytical logic.
- All inline numeric findings in the narrative must come dynamically from the bundle. Do not hard-code result values, dates, figure numbers, table numbers, country counts, scenario names, or projection periods.
- Keep the report robust when optional scenarios are absent. Derive table and figure order from the bundle and the objects actually present.

## 1. Review current RTSL conventions before writing

Review these official sources before revising the narrative and visual system:

- https://resolvetosavelives.org/
- https://resolvetosavelives.org/how-we-save-lives/healthier-food/
- https://resolvetosavelives.org/wp-content/uploads/2024/03/611_CVH_6-Step-Sodium-Reduction-Guide_Fact-Sheet_0224_Rev-A_v6.pdf
- https://resolvetosavelives.org/wp-content/uploads/2025/12/Research-Prioritization-to-Accelerate-the-Global-Scale-Up-of-Low-Sodium-Salt-Substitutes_FINAL.pdf

Use only official RTSL sources to infer the current visual conventions. Derive the exact color values from those sources rather than guessing. Define one named RTSL palette in `reports/report.RMD` and reuse it consistently for all technical-report figures and tables. Record the visual source and selected hex values in code comments.

Follow RTSL's narrative conventions visible in these materials:

- lead with the preventable health problem, the practical policy opportunity, and the magnitude of potential impact;
- use direct, active, plain language and short paragraphs;
- emphasize practical, evidence-based policies, country context, partnership, implementation, and scale;
- move from evidence to action while preserving scientific caveats;
- make the headline message easy to find, then provide the technical detail needed to evaluate it.

Do not imitate promotional language at the expense of precision. Do not say that an intervention is “low cost,” “high return,” “cost-effective,” or offers a return on investment unless the analysis in these two files actually reports the required cost evidence. Use “deaths delayed” consistently where that is the modeled outcome; do not silently change it to “deaths averted” or “lives saved.”

## 2. Improve narrative quality and document structure

Edit the report into a polished executive technical report, not a descriptive inventory of outputs.

- Retain the necessary technical content and appendices, but improve transitions and synthesis.
- Make the executive summary answer, in order: what problem is addressed, what was modeled, what the model indicates, how results differ by country and sodium source, what decision-makers should take from the findings, and what the main uncertainties are.
- In the results, lead each subsection with the finding and its meaning, then support it with a table or figure. Avoid paragraphs that merely restate every cell or bar.
- Explain why results differ across countries using population, cardiovascular burden, and sodium-source composition, without implying causality beyond the model.
- Clearly distinguish the packaged-food/public-procurement package from low-sodium salt substitutes.
- Keep the salt-tax pathway labeled exploratory, public-procurement estimates labeled provisional where applicable, and the LSS analysis labeled benefit-only because potential CKD/hyperkalemia harms are not modeled.
- State clearly that results are point estimates unless uncertainty intervals are actually available.
- Reduce repeated caveats: introduce each limitation at the first relevant result, then synthesize the full set in the limitations section.
- Use consistent terminology, capitalization, spelling, and scenario names throughout. Prefer reader-facing labels over implementation tokens.
- Preserve citations and do not invent evidence, data, references, or claims.

## 3. Implement every Word review comment

The reviewed Word document contains seven comments. Implement them as follows:

### A. Baseline mean SBP

Comment: “For completeness, include SBP from NCD risk and 2026 data and state as baseline.”

- In the country/population table, include a clearly labeled column such as `Baseline mean SBP, 2026 (mmHg)`.
- Use the same authoritative NCD-RisC-derived age-sex blood-pressure data that feed the model. Aggregate correctly to the country level using the appropriate population weighting; do not use an unweighted mean across age, sex, or BP-category rows.
- Make the baseline year and source explicit in the table title/note and in the methods narrative.
- If the required authoritative 2026 object is not already available to `reports/report.RMD`, fail with a clear message rather than inventing values or reading a new external file.

### B. Omit media

Comment: “Omit media.”

- Remove mass-media/behavior-change campaigns from all report-facing narrative, figures, tables, notes, limitation rows, dictionaries, metadata, and scenario/effect-size displays.
- In particular, remove the media node/row from the sodium-source-to-intervention map.
- This is a reporting change only. Do not alter the underlying model registry or pipeline.

### C. Clarify packaged-food composition trend

Comment: “This guarantees total intake does not grow.”

- Rewrite the packaged-food trend explanation to state explicitly that increasing the packaged-food share is a compositional reallocation: the other source shares are proportionally rescaled so all sources continue to sum to 100%.
- State plainly that this guarantees total baseline sodium intake does not grow in this module; only its source composition changes.
- Carry the same clarification into the relevant figure/table note and the limitations section.

### D. Include the state-transition model figure

Comment: “Include Tikz model figure.”

- Add a publication-quality state-transition figure showing the model's Well, Sick, disease-specific Dead, and background-death pathways, including the key transitions (incidence, case fatality, and background mortality) and annual aging/population flow where readable.
- The figure must be generated in `reports/report.RMD`, registered in the technical-report bundle, and consumed by `report/technical_report.RMD` in Section 2.11.
- Use the existing TikZ specification if one is already present within the two permitted RMD files. Because the final output is Word, render the diagram to a Word-compatible high-resolution asset from within `reports/report.RMD`; do not add or edit an external `.tex`, image, or style file. If direct TikZ rendering is not portable in the existing environment, create a faithful TikZ-style diagram with existing R/graphics packages rather than dropping the figure.
- Add an informative title above and a note below explaining the states, transitions, independent cause-specific disease processes, and how intervention-versus-baseline differences produce deaths delayed.

### E. Remove the fiscal pathway figure

Comment: “Remove.” This comment is anchored to the fiscal-policy salt-tax pathway figure.

- Remove that figure from the report body, the technical-report bundle, the figure registry/order, and all narrative cross-references.
- Retain the concise fiscal-method narrative and the detailed fiscal parameter/pathway table in the appendix. Do not remove the fiscal scenarios or results.

### F. Convert the country-impact chart into a Pareto chart

Comment: “Transform in Pareto.”

- Replace the current full-package deaths-delayed-by-country chart with a true Pareto chart.
- Sort countries from largest to smallest absolute deaths delayed.
- Show absolute deaths delayed as bars and the cumulative share of the ten-country total as a clearly labeled line on a secondary percentage axis.
- Include a subtle 80% reference line and, if legible, identify the countries that account for roughly 80% of the total.
- Keep the chart readable in Word, avoid misleading dual-axis scaling, and explain both axes and the 80% threshold in the note below.

### G. Remove the redundant by-cause figure

Comment: “Figure is redundant to table.”

- Remove the full-package deaths-delayed-by-cause figure from the report, bundle, and figure order.
- Retain the by-cause table and revise the surrounding narrative so it references only the table.

After adding/removing figures, recompute figure order dynamically. Do not hard-code numbers or leave broken/duplicate cross-references.

## 4. Apply a coherent RTSL visual system

### Figures

- Apply the single official RTSL-derived palette consistently. Use neutral gray for the baseline/reference and reserve saturated colors for scenarios or findings that need emphasis.
- Use the same semantic color for the same cause, intervention family, or sodium source everywhere.
- Remove embedded plot titles, subtitles, and captions when they would duplicate the report-level title and note.
- Use clean backgrounds, restrained grid lines, readable axis labels, direct labeling where practical, consistent typography, and legends placed to minimize eye travel.
- Use colorblind-safe contrasts and ensure the plots remain interpretable in grayscale.
- Export at 300 dpi or better with dimensions appropriate for a portrait Word page; use landscape only when genuinely necessary.
- Avoid tiny labels, overcrowded legends, unexplained abbreviations, implementation tokens, and decorative effects.

### Tables

- Use a consistent RTSL-aligned header color, white header text, restrained row banding, minimal borders, adequate cell padding, and deliberate column widths.
- Left-align descriptive columns; right-align numeric columns; center only short categorical fields where it improves scanning.
- Repeat header rows across page breaks and prevent rows from splitting when practical.
- Use clear units in column headings or notes, consistent significant digits, thousands separators, and em dashes for unavailable values.
- Keep wide tables readable in Word. Use landscape sections or careful column widths before reducing font size.
- Do not use color as the only carrier of meaning.

## 5. Put every title above and every note below

This applies to every figure and table in the main report and appendices.

- Titles must appear immediately above the figure/table and include the dynamic identifier plus an informative, conclusion-neutral title.
- Notes must appear immediately below and begin with `Notes:`. Add `Source:` in the same note block where applicable.
- Each note should give the context a reader needs to interpret the element: units, population, baseline/comparator, period, scenario definition, rounding, abbreviations, and material caveats, as applicable. Avoid boilerplate that says only “current model outputs.”
- Store all title/note/source metadata in the bundle produced by `reports/report.RMD`; `report/technical_report.RMD` should only render it.
- Add a `fig_note()` consumer helper analogous to `tab_note()`.
- Ensure every table and figure object has a nonempty title and a substantive note/source block.
- For figures, do not rely on the default Word/bookdown placement of `fig.cap` below the image. Render a dynamic figure title above the image and the note below it, while preserving stable identifiers/cross-references. Do not create duplicate captions.
- For tables, keep the caption/title above and the note below.

## 6. Word output requirements

- Keep `bookdown::word_document2` as the production output and render the final technical report to `.docx`.
- The cover/title page, heading hierarchy, body spacing, table layout, and figure sizing should feel coherent and executive-ready.
- Do not require a new `reference_docx`, CSS file, Lua filter, or other external style asset because no third source file may be added or edited.
- Ensure all images are embedded in the Word document and all tables are editable Word tables where the current architecture supports that.
- Preserve citations and bibliography rendering.

## 7. Validation and QA

Work in this order: inspect, plan, edit, render, validate, then report.

At minimum:

1. Render `reports/report.RMD` first so it produces the authoritative technical-report bundle and figure/table assets.
2. Render `report/technical_report.RMD` explicitly as `bookdown::word_document2` to create the final `.docx`.
3. Confirm both renders finish without errors and the DOCX exists, is nonempty, and opens successfully.
4. Convert the DOCX to PDF or page images with LibreOffice if available and visually inspect every page. Check title placement, note placement, table overflow, clipped text, tiny labels, broken cross-references, excessive blank space, and awkward page breaks. Iterate until clean.
5. Add or run report-level assertions that verify:
   - every consumed table/figure key exists in the bundle;
   - every bundle title is nonempty;
   - every element has a substantive note and/or source;
   - figure and table order matches appearance order;
   - no removed figure remains in the consumer or registry;
   - the state-transition figure is present;
   - the country figure is a Pareto chart with cumulative percentages ending at 100%, within rounding tolerance;
   - the by-cause table remains present while its figure is absent;
   - the fiscal pathway table remains present while its figure is absent;
   - no report-facing media-campaign text remains;
   - the country table identifies mean SBP as an NCD-RisC-derived 2026 baseline;
   - totals and headline numbers still reconcile with the existing model outputs.
6. Run `git diff --check`.
7. Run `git status --short` and `git diff --name-only`. Confirm that the only modified source files are the two allowed RMD files. Do not alter unrelated pre-existing user changes.

## Completion response

When finished, report:

- the exact two source paths changed;
- the path to the rendered Word document;
- a concise summary of the narrative and visual improvements;
- how each of the seven Word comments was implemented;
- the render/QA checks performed and their results;
- any remaining limitation that could not be resolved within the two-file restriction.

Do not stop after proposing a plan. Implement, render, inspect, and validate the report.
```
