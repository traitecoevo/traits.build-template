# traits.build-template — agent & contributor guide

`traits.build-template` is a worked example / starter repository demonstrating the
[`traits.build`](https://github.com/traitecoevo/traits.build) workflow — a template trait-data
compilation you can copy to bootstrap your own database.

## Repo-local guidance

- **Pipeline:** `remake.yml` defines the build. It loads the `traits.build` package and assembles
  the example datasets into a single `austraits` object, written to `export/data/curr/austraits.rds`.
  (The header notes `remake.yml` is generated from `remake.yml.whisker` in the `traits.build`
  package — treat it as generated.)
- **Config:** `config/` holds the compilation-wide inputs — `metadata.yml` (resource metadata),
  `traits.yml` (trait definitions), `unit_conversions.csv`, and `taxon_list.csv` (taxonomy).
- **Data:** `data/` holds per-dataset folders, each with a `data.csv` + `metadata.yml`
  (`example_dataset_1`, `example_dataset_2`, and several `tutorial_dataset_*`). The two
  `example_dataset_*` are the ones wired into `remake.yml`.
- **Custom code:** `R/custom_R_code.R` for dataset-specific cleaning functions referenced from
  metadata.
- **Build:** this is a remake project, not an R package — there is no `DESCRIPTION`/`tests/`. Build
  in R with the `traits.build` package installed, e.g. `remake::make()` (or `traits.build`'s build
  helpers), which runs the targets in `remake.yml` to produce the combined `austraits` object.

Default branch is `master`.

> Gotcha: `remake.yml` is auto-generated from `traits.build`'s `remake.yml.whisker` — edit the
> source template/config rather than hand-patching it where possible. The `data/tutorial_dataset_*`
> folders are tutorial material and are not part of the wired build targets.

---

## AusTraits family — cross-package context

`traits.build-template` is part of the **AusTraits family** (a subset of the
[`traitecoevo`](https://github.com/traitecoevo) org) — here, a template example compilation
demonstrating the traits.build workflow. Family-wide concerns are documented centrally in
**[austraits-meta](https://github.com/traitecoevo/austraits-meta)** — don't restate them here, read
them there:

- **Start with [`AGENTS.md`](https://github.com/traitecoevo/austraits-meta/blob/main/AGENTS.md)** —
  pipeline order, who owns what, dependency direction, source-of-truth rules, cross-boundary
  artifacts, gotchas.
- **[`dependencies.yml`](https://github.com/traitecoevo/austraits-meta/blob/main/dependencies.yml)** —
  machine-readable package graph + cross-boundary artifacts.
- **[`governance/`](https://github.com/traitecoevo/austraits-meta/tree/main/governance)** —
  label taxonomy, board #9 conventions, release playbooks, triage.

**Filing issues:** the whole family is tracked on one board,
[AusTraits #9](https://github.com/orgs/traitecoevo/projects/9) (new issues auto-add to it). Follow
the [issue & labelling guide](https://github.com/traitecoevo/austraits-meta/blob/main/governance/issue-guide.md):
pick one work-type label (`bug` / `task` / `epic`); Status and Priority are set on the board, not as
labels.

> austraits-meta is hand-maintained prose — a map, not ground truth. Verify specifics against the
> actual repos.
