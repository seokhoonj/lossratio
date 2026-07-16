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
- `ratio_sa_full.csv` / `ratio_ed_full.csv` / `ratio_cl_full.csv` — `fit_ratio(method=...)$full`.
- `ratio_sa_selected.csv` — `$selected` (per-link factor table).
- `ratio_sa_summary.csv` — `summary(ratio_sa)` (per-cohort summary).
- `cl_full.csv` / `cl_mack_full.csv` — `fit_cl(method = "mack")$full`.
- `cl_mack_summary.csv` — `summary(cl_mack)`.
- `ata_selected.csv` / `intensity_selected.csv` — link-level diagnostics.
- `regime_changes.csv` — `detect_regime(...)$changes`.
- `backtest_ratio_{ed,sa}_ae_err.csv` — `backtest(target = "ratio", loss_method = ...)$ae_err`, one set per loss method.
- `backtest_ratio_{ed,sa}_duration_summary.csv` / `*_calendar_diagonal_summary.csv`.
- `sigma_extrapolation.csv` — `.extrapolate_sigma_ata` outputs (8 input cases x 5 sigma methods).
