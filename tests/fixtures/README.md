# R parity fixtures

Snapshots of the R sibling's outputs on the bundled SUR experience data.
The Python parity suite (`tests/test_r_parity.py`) loads these and compares
column-by-column against equivalent Python calls.

## Refreshing

After a deliberate algorithm change that should remain bit-identical
across languages, re-dump from the R repo:

```bash
cd ~/Dropbox/lossratio
Rscript dev/parity_dump.R
Rscript dev/parity_segment_wise.R
cp dev/parity_fixtures/*.csv ~/Dropbox/lossratio-py/tests/fixtures/
```

Then run `pytest tests/test_r_parity.py tests/test_segment_wise_parity.py`
and resolve any diffs.

## Files

- `experience.csv` — raw bundled experience data (R `data(experience)`).
- `triangle_sur.csv` — `as_triangle(...)` on SUR-only slice.
- `lr_sa_full.csv` / `lr_ed_full.csv` / `lr_cl_full.csv` — `fit_lr(method=...)$full`.
- `lr_sa_selected.csv` — `$selected` (per-link factor table).
- `lr_sa_maturity.csv` — `$maturity` (maturity-link factor diagnostic).
- `lr_sa_summary.csv` — `summary(lr_sa)` (per-cohort summary).
- `cl_full.csv` / `cl_mack_full.csv` — `fit_cl(method = "mack")$full`.
- `cl_mack_summary.csv` — `summary(cl_mack)`.
- `ata_selected.csv` / `intensity_selected.csv` — link-level diagnostics.
- `maturity.csv` — `detect_maturity(...)`.
- `regime_changes.csv` — `detect_regime(...)$changes`.
- `backtest_lr_ae_err.csv` — `backtest(target = "lr")$ae_err`.
- `backtest_lr_col_summary.csv` / `*_diag_summary.csv`.
- `segment_wise_*.csv` — `dev/parity_segment_wise.R` outputs (canonical
  SUR-only fixture, segment_wise regime treatment).
