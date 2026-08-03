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

## Splinter parties are modelled as blocs

This is the thing most likely to trip you up. **Neither R+ nor KKP is a
Dirichlet category.** Two splinters have broken away during the series, and both
are handled the same way, driven by `SPLIT_SPECS` in `PTP.R`:

| Splinter | Parent | Launch | Stage-2 spec |
|---|---|---|---|
| R+ (Rozwój Plus) | PiS | 2026-07-24 | intercept-only (~6 splitting polls) |
| KKP (Konf. Korony Polskiej) | Konfederacja | 2025-06-09 | smooth + house effects (~142) |

The model is two-stage:

1. **Stage 1** (`m1`) fits the Dirichlet over `PARTY_COLS_BLOC`, in which `PiS`
   means the whole **PiS + R+ bloc** and `Konfederacja` the whole
   **Konfederacja + KKP bloc**. Every poll in the series measures those
   quantities, so the outcome matrix has no R+ or KKP column and no `muRplus` /
   `muKKP` dpar.
2. **Stage 2** (`split_models`) fits a Beta regression per split for the
   splinter's *share of its bloc*, on the polls that offered it separately.
3. `split_all()` recombines them draw-by-draw into `parent = bloc × (1 − share)`
   and `child = bloc × share`. It is called inside `consensus_epred()`, so every
   consumer downstream sees `Rplus` and `KKP` categories and needs no special
   handling.

Why: houses did not all start splitting each parent at once, so the raw series
mixes two quantities under the parent's name. Coding "not offered" as `0` hands
the Dirichlet a mass of near-zero observations, whose log-likelihood contribution
`(phi*mu − 1) * log(y)` — `log(0.0005) = −7.6` against `log(0.07) = −2.7` for a
real reading — swamps the genuine readings and pins the splinter to the floor no
matter what the smooth does. Before this was fixed, R+'s trend line sat flat at
~0.2% while the polls that measured it read 4.9–8.2%.

Consequences to keep in mind:

- `rplus_separate` / `kkp_separate` (set in the scraper) are what distinguish
  "not offered" from "scored zero". Never let those collapse again. Both test
  that the cell **parses to a number**, which is what rules out all three
  not-offered cases: column absent (`NA`), cell blank (`""`), and Wikipedia's
  colspan merge (cell string identical to the parent's).
- **Launch dates are hardcoded in `SPLIT_SPECS`**, on purpose. They are facts
  about the party system, not something to infer from when pollsters started
  asking — a few houses offered each splinter before it existed, and those
  readings measured *potential* support for a hypothetical party. Before its
  launch a split is not applied at all: the splinter is 0 and the parent holds
  the whole bloc.
- Those pre-launch readings are still used by **stage 2** — they measure how the
  parent's electorate divides, which is what stage 2 wants — but not by the
  chart: each line starts at its launch date, and those polls' scatter points
  show only the bloc as the parent, with no splinter point. The `hypothetical`
  flag on `split_data[[child]]` marks them.
- **House effects are reported on the unsplit blocs**, labelled `PiS + R+` and
  `Konfederacja + KKP`. The two `consensus_epred(..., split = FALSE)` calls in
  that block are deliberate: the bloc is what every house has read out across the
  whole series, so it is the level at which they can be compared.
- The scatter points on the trend chart are what each poll *measured* — the bloc
  where the splinter wasn't offered (or wasn't yet real), parent-only where it
  was. Parent points therefore sit above the parent's line while houses disagree.
  That gap is real disagreement, not a bug.
- A parent's line steps down at its launch date, from the bloc to
  `bloc × (1 − share)` (~7pp for PiS). That is the party splitting, not an
  artefact.
- **Stage-2 spec is chosen automatically** by `SPLIT_RICH_MIN_POLLS` (30) and
  `SPLIT_RICH_MIN_SPAN_YEARS` (0.75). Sparse → intercept-only, so the splinter's
  line tracks its bloc rather than having its own trajectory. Rich → the same
  `s(time, k = 8, bs = "cs")` + `(1 | pollster)` as stage 1. There is no linear
  tier in between because `bs = "cs"` shrinks toward flat by itself where the
  data don't support wiggle. KKP needed the rich spec: its share of the bloc rose
  from 0.25 to 0.42 over its first year, and a flat share is wrong at both ends.
- Rich stage-2 predictions average over the houses that split the bloc, for the
  same reason `consensus_epred` does — see `split_share_draws()`.

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
- Poll rows should sum to ~100 once `DK` and `Other` are included; the scraper
  prints the range as a check. They didn't while the KKP cell was routed into
  `Other` for early polls, which pushed early-2025 totals to 113–115% and then
  diluted every party by ~13% once the rows were normalised.

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
