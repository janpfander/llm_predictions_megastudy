# How our evaluation criteria compare to the synthetic-sample literature

A review of four papers in `papers/` against the metric ladder and diagnostics we
run in the Silicon Sample Tournament (`R/functions/statistics.R`,
`preregistration_benchmark.qmd`, `preregistration.qmd`). Goal: check that our
criteria are sound, see what others do, and flag what we might be missing.

Prepared 2026-07-01.

---

## 1. What each paper evaluates, and how

### Ashokkumar, Hewitt, Ghezae & Willer (2026, *Nature*) — the direct precedent
70 US survey experiments (469 effects) + 15 megastudies (606 effects). Prompts
GPT-4 with demographic profiles, simulates responses, infers treatment effects,
compares to real effects. **This is essentially our task, one team, on past data.**

Their scoring:
- **Raw correlation `r`** between predicted and observed effect sizes (their headline).
- **Disattenuated correlation `r_adj`** — corrects for sampling error in the *observed*
  effects via a two-stage random-effects meta-analysis (`metafor`). Raw `r` is biased
  downward when the human effects are imprecisely estimated (small subgroups, small N);
  `r_adj` removes that bias. This moves numbers a lot: e.g. Black-subgroup `r = .62`
  but `r_adj = .86`, the gap being pure small-sample attenuation.
- **Calibration slope `b`** from regressing observed on predicted effects. `b ≈ 0.5`
  everywhere → LLMs (and human forecasters) systematically *overestimate* effect sizes
  ~2×. They recommend linear re-calibration against benchmark data.
- **RMSE and adjusted RMSE** (subtracting the effect's sampling-error variance).
- **Benchmarks**: lay forecasters (N=2,659) and expert forecasters. GPT-4 matched lay
  forecasters; a human+LLM average beat either alone.
- **Robustness subsets**: field, small effects, unpublished, hypothesized-only,
  author-not-recognized — to rule out training-data leakage.
- **Bias**: predicted vs. observed effects *within* demographic subgroups (gender, party,
  race). Small gaps favouring white / Republican samples, but they stress their archive
  had almost no real subgroup heterogeneity, so this test was underpowered.
- **Decision-relevant use cases**: pilot-testing (RMSE vs. human sample size / cost),
  replication-success discrimination (logistic regression), and **top-intervention
  selection** (does LLM+expert pick interventions with larger true effects than expert
  alone — the megastudy-native metric).
- Flags **variance underestimation** as a known LLM failure that corrupts standardized
  effect sizes (Cohen's d) and power analysis.

### Cummins (2025) — the analytic-flexibility critique
Generates 252 silicon-sample configurations (model × temperature × demographics ×
scale-prompting strategy) for one case study, and 66 configs re-doing Argyle et al. 2023.

Their message and method:
- Evaluates three **data features**: participant **ranking** (correlation), **response
  distributions** (Wasserstein distance), and **between-scale correlation**. Configs that
  win on one feature routinely lose on another — *no single best configuration exists*.
- **Human–human split-half baseline**: split the human data in two 2,000× to get the
  distribution of a metric you'd see between two human samples — the noise floor a
  silicon sample should be judged against, not zero.
- **Specification-curve / multiverse** reporting of all defensible configs, so a
  cherry-picked config can't masquerade as "the" fidelity estimate (Argyle-recovery
  ranged r = .23–.84 across configs).
- **Calibration–confirmation (train/test) split**: pick the config on a held-out human
  calibration set, confirm on a second — analytic-flexibility control borrowed from ML.
- Recommends **pre-registering the target feature + benchmark + config set** before
  looking at the focal outcome.

### Park et al. (2026) — individual-level agents grounded in self-reports
Agents built from each person's interview/survey, predicting *that person's* held-out
responses (GSS, Big Five, economic games) + 5 experiment replications.

Their scoring:
- **Individual-level** accuracy (categorical) / MAE (continuous) / correlation — agent vs.
  its own source person.
- **Normalized** by each participant's **two-week test–retest consistency**: 1.0 = "the
  agent predicts the person as well as the person predicts themselves." A per-person noise
  ceiling, the individual-level analogue of Cummins' split-half.
- **Population-level replication**: treatment-effect correlation (r = .91–.99),
  effect-size (Cohen's d) and **p-value significance recovery**. Effects again larger than
  humans (overestimation).
- **Bias via Demographic Parity Difference (DPD)**: the performance gap between the
  best- and worst-served subgroup — one number per demographic axis (ideology, race,
  gender). Richer self-report input shrinks DPD.

### Arruda et al. (2026) — collective cooperation without individual fidelity
Networked Prisoner's Dilemma, 9 LLMs vs. a human benchmark.

The transferable lesson (their whole point):
- **Macro–micro dissociation**: LLM populations reproduce *aggregate* cooperation
  dynamics while **underestimating individual heterogeneity** and getting the
  *decision rules* wrong. Outcome-level agreement can be shallow.
- Validation must therefore span **three levels**: aggregate dynamics, individual
  heterogeneity (the *distribution*), and context-dependent decision rules — never
  aggregate agreement alone. Underestimated response variance recurs here too.

---

## 2. Where our criteria already stand (the good news)

Our pipeline is, by the standards of this literature, **more complete than any single
paper** on the aggregate-recovery side, and structurally sound:

| Dimension | Literature | Us |
|---|---|---|
| Effect-recovery ladder | Ashokkumar: r, r_adj, slope, RMSE | directional %, Spearman ρ, Pearson r, inferential %, RMSE, TOST — a fuller ladder (`compare_estimates`, `pooled_metrics`) |
| Calibration / overestimation | Ashokkumar: slope `b` | full regression: α (offset), β (slope), joint & slope-only F-tests (`run_calibration`) — stricter than their single slope |
| Response-shape / variance | flagged by all four; Cummins: Wasserstein; Arruda: heterogeneity | OVL, KS D, **variance ratio** (`compare_distributions`) — we already test the failure mode all four warn about |
| Bias / stereotyping | Ashokkumar: subgroup r; Park: DPD | subgroup recovery distributions + demographic-predictability R² per moderator (`compare_demographic_predictability`, `compare_demographic_baselines`) |
| Noise floor | Cummins split-half; Park test–retest | **human–human split-half ceiling**, scored in the same row format as every team |
| Multi-level (not outcome-only) | Arruda's core warning | three-section design: ATE + distributions + subgroups + calibration, all reported together |
| Uncertainty on scores | (none of them, on the score itself) | cluster bootstrap over interventions (`cluster_boot`) — ahead of the field |
| Novelty / no leakage | Ashokkumar works hard to argue it | **guaranteed by design**: genuinely novel interventions, predictions escrowed before any data release |

So the foundations are solid. The gaps below are additions, not corrections.

---

## 3. What we're missing — ranked

### A. Disattenuated correlation + adjusted RMSE — **highest priority**
We compute raw Pearson/Spearman/RMSE on ~17 ATE points. Those human ATEs come from an
~18,000-person sample split *in half* for the ceiling, so each effect carries real
sampling error — exactly the condition under which Ashokkumar shows raw `r` is biased
**downward**. Consequence for the tournament: every team *and the human–human ceiling*
are penalized by an amount that depends on how noisily each effect is estimated, not on
predictive skill. The closest precedent (a *Nature* paper doing our exact task) treats
`r_adj` as the headline for this reason.
- **Add**: `metafor`-based disattenuated correlation and adjusted RMSE alongside the raw
  versions, using the HC2 SEs we already extract. The ceiling gives the empirical upper
  bound; `r_adj` gives the leakage-free comparison across teams.
- Cheap: we already have estimates + SEs per effect in `build_pairs_t*()`.

### B. A decision-relevant "top-intervention selection" metric — **high**
The practical point of a 16-intervention megastudy is *which interventions work*.
Spearman is a proxy; Ashokkumar's Fig 3C and the megastudy community use **top-k selection
accuracy / regret**: if a team picks the top-k interventions by predicted effect, how large
are those interventions' *true* effects vs. the true best-k (or vs. an expert/null pick)?
This is what a practitioner actually cares about and is trivial to compute from the pairs
we already build. Add it as a rung (or a companion panel) to the ladder.

### C. Explicit floor baselines, not just the ceiling — **medium-high**
We anchor the top (human–human ceiling) and draw a null line, but there's no *floor row*
scored in the same format. Add one or two trivial baselines as reference submissions:
"predict zero effect," "predict the grand-mean shift," and (if feasible) a naive
demographic-only baseline. This tells readers where the field sits *between* dumb and
ceiling — the interpretive frame Ashokkumar gets from lay/expert forecasters and Cummins
from the split-half band. Without a floor, a mediocre field score is hard to read.

### D. Cross-metric consistency of approaches — **medium**
Cummins' central empirical finding is that a config good on one feature is often bad on
another. Our per-metric field distributions *contain* this information but we never
summarize it: compute, per approach, its rank across the ladder rungs and report the
rank stability (e.g. does leading on directional agreement predict leading on
distribution fidelity?). One small table turns Cummins' warning into a reported result
and directly serves the amendment's "which approaches lead on which rung" framing.

### E. A single bias-parity number (DPD) + foreground the variance finding — **medium**
- Park's **Demographic Parity Difference** (best-minus-worst subgroup performance) is a
  clean one-number bias summary that complements our subgroup distributions and R² tables.
  Add it per moderator.
- All four papers independently flag **variance/heterogeneity underestimation**. We already
  have `variance_ratio` — promote it to a headline diagnostic rather than one of three
  distribution numbers, and state the expected direction (clones < humans).

### F. Robustness of the score to analytic choices — **medium**
Cummins' critique applies to *our scoring choices* too: the half-split seed, the TOST bound
(0.5×|ATE_human|), the BH grouping. A single preregistered seed is defensible, but a
sensitivity check — how much do the ceiling and the field summary move across, say, 100
resplit seeds — would pre-empt the "you got lucky with the split" objection. Report it once
as a robustness note.

### G. State the individual-fidelity scope boundary — **low effort, worth doing**
Park scores individual-level fidelity; Arruda shows aggregate match can hide individual
divergence. Our design **cannot** assess individual fidelity — teams build their own
profiles with no pairing to specific humans — so we only ever see aggregate + distributional
fidelity. That's a legitimate, deliberate scope limit, but we should *say so* (cite Arruda's
macro–micro dissociation) so reviewers don't read the distribution metrics as an
individual-level claim. Our variance/OVL/KS metrics are the population-distribution answer to
Arruda; they are not an individual-fidelity answer, and the text should draw that line.

---

## 4. Bottom line

Our ladder, calibration regression, distribution metrics, subgroup/stereotyping
diagnostics, human–human ceiling, and bootstrap intervals already cover — and in places
exceed — what these four papers do on aggregate recovery, response shape, and bias. The
design also solves the two problems Cummins and Ashokkumar spend the most effort on:
novelty (no training-data leakage) and pre-registration (escrowed predictions).

The most valuable additions, in order: **(A) disattenuated correlation / adjusted RMSE**
so small-sample attenuation doesn't penalize everyone equally, **(B) a top-intervention
selection metric** so the score reflects the megastudy's actual decision, and **(C) floor
baselines** so field scores are readable between null and ceiling. D–G are lower-cost
polish that turn warnings from this literature into things we explicitly report.
