# Known limitations

**Read this before quoting a number anywhere permanent.**

*No counts here on purpose — they go stale as the data grows. Where a number
matters, `findings_index.csv` has the current one.*

Limitations are grouped by the stage that creates them. A problem created in the
field cannot be fixed by code later, which is the main thing this page is for.

---

## 1. Collecting — the field and iNaturalist

**Nothing downstream can recover these. They need a protocol change.**

| | |
|---|---|
| **Method and observer are confounded before 2024** | Only interns netted, only beeple photographed. Netting stopped after 2023, so no year has interns doing both. |
| **Effort is uneven by design** | Heavier in warm months, heaviest in 2024. Any count is a count of looking as much as of bees. |
| `flower_flowering` rarely filled | Bloom timing is not measurable. |
| `is_10min` never filled | The 10-minute survey cannot be analysed. Record it and it can. |
| No camera or device recorded | Camera-vs-phone is unanswerable. |
| Intern practice photos not flagged | Cannot be separated from real surveys. |
| Casual iNaturalist coordinate precision | Drifts far enough to move a record off its transect. |
| Photos stall above species | *Lasioglossum*, *Ceratina* — a real detection limit, not a data-entry gap. |
| Plants are **by-catch** | Recorded to say what a bee was on, never as a plant survey. No plant checklist, and availability can only be inferred from use. |
| Specimen ID backlog | Some specimens sit at genus or tribe until a specialist sees them. |
| Photo-only taxa with no voucher | Seen but never collected. |

## 2. Cleaning — turning records into tables

| | |
|---|---|
| **Taxonomy follows iNaturalist** | The lookup rebuilds every run, so a name can change under you. |
| **Some checklist bees have no iNat taxon** | Real bees carrying no id. A join must fall back to the name or they vanish — the rule is in `CLAUDE.md`. |
| Many on-transect records are un-attributed | No `surveyor_type`, so they cannot be joined to a person. |
| On-transect means **within 50 m** | Loose on purpose: casual coordinates drift, and a tighter buffer would drop real survey records. |
| Spatial tiers use **iNat place boundaries** | User-contributed, so the edges move. |
| The specimen spell-check is a **heuristic** | It flags for review; it is not an authority. |

## 3. Analysis — what the numbers can carry

| | |
|---|---|
| **2024 is a step, not a slope** | A jump in effort, not a population trend. Transect-year cells are pseudoreplicated, and the apparent significance does not survive accounting for it. |
| **Read the two method windows together** | `fair_method_2021_2023` and `fair_observer_2024`. Either alone will mislead, because of the confound above. |
| Genus- and subgenus-level IDs are **kept** | "Possible at this rank" beats "species or nothing". |
| The plant–bee network uses **all records** | A survey-only network loses most of its edges, and "who visits what" is not an effort question. |
| **Bee** phenology uses all records; **plant** phenology is survey-only | A bee only looks seasonal if somebody was out looking; plant bloom is recorded on the walk, so restricting it keeps effort even. |
| Forage **availability is use-derived** | The proxy for what was blooming is what bees were recorded on. Circular, and the honest limit of by-catch data. |
| "Museum Collection" means **CABR survey specimens only** | Not all SDNHM holdings. |

Every figure states its own scope in its caption. The scope choices above are
listed because the *reason* for them is not visible from the scope itself.

## 4. Publishing — what reaches the public

| | |
|---|---|
| **Coordinates are published as held** | Nothing obscures or rounds them. Running authenticated returns *true* coordinates for taxa iNaturalist obscures, and the occurrence explorer publishes those. **Accepted deliberately**: the park is staffed and collecting is already well prevented on site. The residual is not poaching but consent — observers granted coordinate trust to the park's account, not to the public. Revisit if an observer objects, or if the project ever covers land without that protection. |
| **Building is not publishing** | Pages are built into `data/analysis/**/website/`, copied into `docs/`, and only go live when `docs/` is committed and pushed. |
| A fork publishes **its own site** | Two live sites drift apart, and a stale page does not say how old it is. |
| Without an IUCN key the status column reads **"Not Evaluated"** | Not "no conservation concern". |
| `docs/` is the only public output | Everything else stays in `data/`, which is gitignored. |
