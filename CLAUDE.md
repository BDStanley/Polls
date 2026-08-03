# Pooling the Poles

A Bayesian poll-of-polls for Polish parliamentary vote intention, plus D'Hondt
seat projection and a Shiny app. R throughout; models are `brms` on `cmdstanr`.

## Running things

**`R/PTP.R` is not safe to run casually.** Its last 20 lines deploy the Shiny
app to shinyapps.io, `git add -A && git commit && git push`, and rsync the whole
project into iCloud. Line 2 is also a `system("git pull")`, which touches the
working tree before anything else runs. To exercise the analysis without any of
that, evaluate the script in ranges (`parse(text = ...)` over selected lines),
stub `saveRDS`/`ggsave`, and skip the deploy/git tail — the full fit takes a
while, so a short-run `m1` (fewer iterations, same spec) is usually enough to
test plumbing.

| File | Role |
|---|---|
| `R/poll_data_scraper.R` | Scrapes the Wikipedia polling tables. Sourced by the two model scripts; not run directly. |
| `R/PTP.R` | The main pipeline: data prep → models → figures → `data/*.rds` → deploy → commit → push. |
| `R/app.R` | The Shiny app. Reads only the pre-computed `data/*.rds`; it never fits a model. |
| `R/deploy.R` | `rsconnect::deployApp` to shinyapps.io. Sourced at the end of `PTP.R`. Publishes live. |
| `R/PiS_splinter.R` | Independent counterfactual scenario (see below). Read-only w.r.t. everything else. |

Inputs live in `data-raw/` (2023 election percentages by constituency, the
constituency shapefile). `data/const_map_cartogram.rds` and `data/sim_weights.rds`
are consumed by `deploy.R`/`app.R` but written by **no** script in the repo —
don't delete them. `data/pred_dta.rds` is a stale leftover nothing reads.

## PiS and R+ are modelled as one bloc

This is the thing most likely to trip you up. **R+ (Rozwój Plus) is not a
Dirichlet category.** The model is two-stage:

1. **Stage 1** (`m1`) fits the Dirichlet over `PARTY_COLS_BLOC`, in which `PiS`
   means the whole **PiS + R+ bloc**. Every poll in the series measures that
   quantity, so the outcome matrix has no R+ column and no `muRplus` dpar.
2. **Stage 2** (`m_split`) fits a Beta regression for R+'s *share of the bloc*,
   on the handful of polls that actually offered R+ as a separate option.
3. `split_rplus()` recombines them draw-by-draw into `PiS = bloc × (1 − share)`
   and `Rplus = bloc × share`. It is called inside `consensus_epred()`, so every
   consumer downstream sees an `Rplus` category and needs no special handling.

Why: only a few houses split R+ out; the rest still read out a single PiS figure
containing R+'s voters. Coding those as `R+ = 0` hands the Dirichlet ~200
observations of a near-zero proportion, whose log-likelihood contribution
`(phi*mu − 1) * log(y)` — `log(0.0005) = −7.6` against `log(0.07) = −2.7` for a
real reading — swamps the genuine readings and pins R+ to the floor no matter
what the smooth does. Before this was fixed, R+'s trend line sat flat at ~0.2%
while the polls that measured it read 4.9–8.2%.

Consequences to keep in mind:

- `rplus_separate` (set in the scraper) is what distinguishes "R+ wasn't offered"
  from "R+ scored zero". Never let those collapse again.
- Before `RPLUS_FIRST_DATE`, the split is not applied: R+ is 0 and PiS is the
  whole bloc. That is correct — the party wasn't polled — and the trend chart
  drops R+ rows from that period rather than drawing a line at zero.
- **House effects are reported on the unsplit bloc**, labelled `PiS + R+`. The
  two `consensus_epred(..., split = FALSE)` calls in that block are deliberate:
  stage 2 has no house term, so splitting there would just rescale the bloc's
  house effect and print it twice.
- The scatter points on the trend chart are what each poll *reported* — the bloc
  where R+ wasn't offered, PiS-only where it was. PiS points therefore sit above
  the PiS line. That gap is real disagreement between houses, not a bug.
- Stage 2 is intercept-only until there are enough splitting polls, so R+'s line
  tracks the bloc rather than having its own trajectory. A linear time term
  switches on automatically at `SPLIT_TREND_MIN_POLLS` / `SPLIT_TREND_MIN_SPAN_YEARS`.

## Modelling conventions

- **`consensus_epred()` is the estimand**, not `re_formula = NA`. It predicts
  each observed house and averages the epred within each draw, which marginalises
  out the house deviations. The population hyper-mean carries a
  between-house/√9 uncertainty (~4–5pp) that never shrinks as polls arrive. Use
  this function, not `add_epred_draws` directly, unless you specifically want
  per-house predictions.
- It returns grouped by `.category` and in a fixed `(.category, .obs, .draw)`
  order. Downstream blocks renumber draws with `row_number()` within category and
  then `pivot_wider`, which only pairs categories correctly if that order holds.
- `TINY_CONSTANT` (0.0005) is added to every cell before normalising so the
  simplex is well-defined. It is not a neutral choice — the smaller it is, the
  more negative `log(y)` and the harsher the penalty on any category that a poll
  reports as zero.
- Time is in **years** (`interval(...) / years(1)`, so 365.25 days). The trend
  chart reconstructs dates as `time * 365`, which is lossy — gate on `time`, not
  the reconstructed `date`, when consistency with the model matters.

## Party names

Two parallel naming schemes, and the conversion is by hand-written `levels`/
`labels` vectors repeated in eight places in `PTP.R` alone:

- **Model/column names**: `PiS`, `Rplus`, `KO`, `Lewica`, `Razem`, `Polska2050`,
  `PSL`, `Konfederacja`, `KKP`, `Other`
- **Display names**: `PiS`, `R+`, `KO`, `Lewica`, `Razem`, `Polska 2050`, `PSL`,
  `Konfederacja`, `KKP`, `Other` (plus `MN`, added only at the seat stage)

Adding or renaming a party means editing every one of those vectors, plus
`PARTY_COLORS`, the `coef_map`/`case_when` that assigns each party a regional
coefficient, `seat_party_cols`, `seats_2023`, the coalition-compatibility list,
and the **positional** vector passed to `giveseats()` — where the `seats` result
is unpacked by index (`.x[1]`, `.x[2]`, …), so order matters and a mismatch is
silent. `app.R` carries its own copies of most of these constants.

`MN` (the German minority list) is exempt from the 5% threshold and only stands
in constituency 21; it is injected at the seat stage with a fixed 7.9% local
share, not modelled.

## PiS_splinter.R

A self-contained counterfactual of the July 2026 split (~40 Morawiecki-aligned
MPs ejected from PiS): what if they form a party competing for centre-right
voters? It folds R+ back into PiS (`PiS = PiS + Rplus`) and defines its own
R+-free `PARTY_COLS`, then transfers a sampled fraction of each donor party's
constituency vote to the new party. It fits and caches its own copy of the poll
model and writes only `data/splinter_*.rds` — it does not deploy or commit.
Changes to `PTP.R`'s two-stage logic do not propagate here; the two scripts share
only the scraper.
