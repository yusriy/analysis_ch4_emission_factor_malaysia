# Reproducibility record for revised Figures 6 and 7

This workspace copy begins from GitHub commit
`14bdd4338ae8a75245e23409edc89cfcb0e24ca7`, the commit recorded in the
manuscript knowledge base. The original `R/ch4_emission_factor.R` is retained
unchanged.

The revised workflow is `R/prepare_revised_figures.R`. It reads the frozen
corrected TIDBRepo export:

- File: `data/raw/tidbrepo_methane_emission_factor_2026-08-28.json`
- Rows: 131
- SHA-256: `f75a22d2a07e2fae777bd9a639c2b91ab21d6b07e0d1bfd2b46a46a27286c830`
- API retrieval date: 28 August 2026

Run from the repository root:

```sh
Rscript R/prepare_revised_figures.R
```

The script exports the title-free manuscript PNG (400 dpi), TIFF (600 dpi,
LZW), and vector SVG versions to `figs/manuscript/`. The exact plotted records
and calculation rules are written
to `data/derived/figure_6_source_data.csv` and
`data/derived/figure_7_source_data.csv`.

## Deliberate changes from the original script

- No live API dependency during rendering.
- No automatic selection of the most frequent unit.
- No percentile-based row removal.
- No blanket use of the `min` field as the plotted value.
- The manuscript Figure 6 is restricted to the three like-for-like
  electricity-generation conditions with both Tier 1 and Tier 2 records. Its
  linear x-axis runs from 0 to `1.2 kg CH₄ TJ⁻¹`.
- Figure titles and panel descriptions are intentionally excluded from the
  artwork. Figure 7 retains only the panel labels `a)` and `b)`; the panel
  descriptions belong in the Word caption.
- Figure 7 uses the recorded mean, then median, then a scalar value. A midpoint
  is derived only when a record has a range but no centre. Reported min–max
  ranges remain visible. Recorded scaling factors are applied to Tier 1 rice
  defaults.
- AFOLU units are corrected to daily (`kg CH₄ ha⁻¹ day⁻¹`).

Figure 7 is descriptive. Its heterogeneous conditions are not matched Tier
comparisons except where the condition labels coincide; therefore, the former
caption claim that Tier 2 values are “significantly higher” should not be used
without a defined statistical test and matched comparison set.
