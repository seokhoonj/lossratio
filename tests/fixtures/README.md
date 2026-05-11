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
cp dev/parity_fixtures/*.parquet ~/Dropbox/lossratio-py/tests/fixtures/
```

Then run `pytest tests/test_r_parity.py` and resolve any diffs.

## Files

- `experience.parquet` — raw bundled experience data (R `data(experience)`).
- `triangle_sur.parquet` — `build_triangle(...)` on SUR-only slice.
- `lr_sa_full.parquet` — `fit_lr(method = "sa")$full`.
- `lr_sa_selected.parquet` — `$selected`.
- `lr_sa_maturity.parquet` — `$maturity`.
- `cl_full.parquet` — `fit_cl(method = "mack")$full`.
- `backtest_lr_ae_err.parquet` — `backtest(metric = "lr")$ae_err`.
- `backtest_lr_col_summary.parquet` / `*_diag_summary.parquet`.
