# Interactive tool for digitizing the FISHMORPH landmarks

Opens a 'shiny' application that places the 21 FISHMORPH landmarks on
specimen photographs, along three working queues that can be switched on
the fly (see `mode`). Every record goes first to an append-only journal
([`fm_journal_open()`](https://funtraits.github.io/Rfishmorph/reference/fm_journal_open.md)),
then to the workbook; after a brutal interruption,
[`fishmorph_consolidate()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_consolidate.md)
recovers all the work.

## Usage

``` r
launch_fishmorph_digitizer(
  xlsx_path,
  photo_dir = file.path(dirname(xlsx_path), "Photos utilisees"),
  out_path = NULL,
  seg_sheet = "Global_segments",
  lm_sheet = "Global_Landmark",
  new_sheet = "new_specimens",
  new_photo_dir = file.path(dirname(xlsx_path), "Photos nouvelles"),
  ruler_mm = 10,
  journal_dir = file.path(dirname(xlsx_path), "landmark_journal"),
  operator = NULL,
  xlsx_flush_every = 10L,
  mode = c("reconstruct", "correct", "new"),
  launch.browser = TRUE
)
```

## Arguments

- xlsx_path:

  Path of the master workbook (2 sheets).

- photo_dir:

  Photograph folder (stays local).

- out_path:

  Path of the output copy. NULL -\> "\_reconstructed.xlsx" in the same
  folder. The copy is created if absent; otherwise work resumes on it
  (species already recorded are excluded from the queue).

- seg_sheet, lm_sheet:

  Sheet names.

- new_sheet:

  Sheet the NEW specimens are appended to ("new" mode). Created (with
  the headers of `lm_sheet`) if it does not exist.

- new_photo_dir:

  Folder of the new specimens' photographs ("new" mode). Every image in
  the folder forms the queue. It may not exist: the "new" mode is then
  simply unavailable.

- ruler_mm:

  Real length (mm) of the scale bar digitized by points 20 and 21 in
  "new" mode. Can be changed in the app, specimen by specimen.

- journal_dir:

  Folder of the append-only JOURNAL (see
  [`fm_journal_open()`](https://funtraits.github.io/Rfishmorph/reference/fm_journal_open.md)).
  Every record is appended to it BEFORE any workbook write: it is the
  source of truth, and it survives a crash.
  [`fishmorph_consolidate()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_consolidate.md)
  rebuilds the database at any time.

- operator:

  Operator identifier, traced in the journal and in the session file
  name. NULL -\> the system user.

- xlsx_flush_every:

  Number of records between two writes of the workbook. The workbook
  weighs several Mb and is REWRITTEN IN FULL every time: taking it out
  of the digitizing loop removes both the risk and the wait. Unwritten
  changes stay in memory (and are therefore readable within the
  session), are written at the end of the session, by the dedicated
  button, and are in the journal in any case. 1 = the historical
  behaviour

- mode:

  Starting queue: "reconstruct" (species WITHOUT landmarks, to be
  digitized from the segments), "correct" (species ALREADY landmarked,
  to be reviewed/corrected: the 21 points are reloaded from the
  workbook) or "new" (new photographs from `new_photo_dir`, appended to
  `new_sheet`). Switchable at any moment through the "Queue" selector in
  the app. If the requested queue is empty, the app starts on another
  one.

- launch.browser:

  Where the application opens. `TRUE` (default) or `"browser"` forces
  the system browser, past the RStudio Viewer pane – which is a few
  hundred pixels wide and the one place an application built for
  clicking nineteen points on a photograph must not open. `"viewer"`
  restores the pane, `FALSE` opens nothing and prints the URL, and a
  function is used as given.

## Value

Invisibly `NULL`; called for its side effect (it launches the app).

## Details

The photographs stay LOCAL: they are never copied into the package nor
into the workbook, only their file name and their size in pixels are
recorded. It is `photo_dir` and `new_photo_dir` that make the link.

## Extreme-point check on save

FISHMORPH defines `Bd` as the MAXIMUM body depth: point 3 must therefore
be the most dorsal and point 4 the most ventral. When the box *"Check
3/4 (extremes)"* is ticked (the default), "Save & next" checks that
convention before anything is written and, if it is breached, offers to
**measure again** (the offending point becomes active and the view
centres on it), to **correct automatically** (3, resp. 4, takes the
height of the point overshooting it, keeping its position along the
axis: `Bd` grows, the 3-4 perpendicularity is preserved) or to **save
without correcting**.

Heights are measured perpendicular to the body axis 1-2 – a tilted
photograph therefore does not distort the test – and the dorsal side is
deduced from the relative position of 3 and 4, which makes the check
valid whatever the orientation (head left or right, photograph flipped,
the "Flip dorsal/ventral" box ticked). EXCLUDED from the comparison are
the caudal peduncle and fin (16-19), which exceed the body by
definition, the appendage tips (12 pectoral, 15 jaw) and the DERIVED
ventral points (8, 9, 11), computed from 4 itself – including them would
flag 20.6 per cent of the T-26 specimens for belly-line noise, against
1.5 per cent of outright errors once they are excluded; the scale bar
(20, 21), the derived point (23) and the hinges (24, 25) are not outline
points. The tolerance is 0.003 times the body length (that is 3 pixels
for a fish of 1,000 pixels), below which the discrepancy is click noise.

Points that were snapped carry the status `"adjusted"` in the journal,
distinct from `"placed"`: the automatic correction stays traceable
specimen by specimen.

## Coincident points, a measurement of zero

A bar under the photograph declares the segments that are ZERO on the
species in view. A zero is a measurement like any other – neither a
missing value nor a placement error – and the FISHMORPH ratios are
defined to take it: `OGp = 0` for a mouth opening on the ventral
profile, `PFv = 0` for a pectoral fin inserted on the belly. Five rules
are offered: `Mo = 0` (9, and 23, take the coordinates of 1), `6 = 8`
(the bottom of the head is the body underside, and 23 follows 9),
`PFi = 0` (10 takes the coordinates of 11), `5 = 13` (an eye reaching
the top of the head) and `4 on 22-24` (the belly point of the body depth
lies on the mid axis).

The last one is of a different KIND: the partner is not a landmark but a
LINE, so 4 is not copied onto anything, it is PROJECTED perpendicularly
onto the segment 22-24 – it keeps the abscissa the operator clicked
along the axis and its height becomes zero. The line is not bounded by
its two hinges: the foot of the perpendicular may fall on their
prolongation, as everywhere else in the constrained editing. Because 4
is the master of the belly line, this rule is applied BEFORE the
conventions rather than after them, so that 11, then 8 and 9, are
re-derived from the projected 4; it is replayed at the end, where it is
idempotent. Declaring it also suspends the VENTRAL half of the
extreme-point check on save: 4 no longer claims to be the most ventral
point, so reporting 6, 10 or 14 below it would flag the rule itself. The
dorsal half, on 3, is untouched.

That 23 follows 9 under `6 = 8` is a consequence, not an extra
convention. Point 23 is the intersection of the line (1, 9) with the
line through 6 parallel to the head axis, and the belly line 9, 8, 11 is
itself parallel to that axis. The moment 6 sits ON the belly line, that
parallel IS the belly line, and its intersection with (1, 9) is 9.

Nothing is deleted. Both points keep a position, both are drawn on the
photograph and both are written to the workbook; one simply takes the
coordinates of the other, so the segment between them measures zero. A
coincidence is a measurement, an absence is `NA`, and the two must not
be confused downstream.

In `"reconstruct"` mode a zero already comes from the workbook, since
the points are laid out from the measured segments. The rules are for
`"new"`, where the points are seeded from medians, and `"correct"`,
where a specimen is being repaired. They are applied at the very end of
the reconstruction, after the constrained editing and after point 23 is
rebuilt, which would otherwise re-derive the ventral points on the belly
line at the next click.

Which point moves is a protocol decision and is not the same for every
rule. For the mouth the fixed point is 1, the snout, an anatomical
landmark that must not move – and 23, built on the line (1, 9), is
undefined once 9 sits on 1, so it follows 1 rather than becoming `NA`.
For the two ventral rules the belly line holds: 8 and 11 are its
intersections with the eye and the pectoral verticals, so the head
bottom and the fin insertion come onto them rather than the reverse. For
the eye at the top of the head, 5 comes onto 13, since moving 13 would
change `Ed`, a measurement in its own right. Points moved by a rule take
the `"adjusted"` status in the journal, and the declarations are reset
for every species.

The same box also checks the ORDER of the eye vertical. The six points
5, 13, 7, 14, 6, 8 are placed on one vertical by the FISHMORPH
conventions, and anatomy fixes their order along it, from the back
downwards: top of the head, top of the eye, centre of the eye, bottom of
the eye, bottom of the head, body underside. Two failures follow from
that and from nothing else – 5 no longer topping the group, which
under-measures `Hd` exactly as a misplaced 3 under-measures `Bd`; and a
local swap, typically 13 and 14 when the eye is clicked bottom-first, or
7 outside the 13-14 pair. Neither is visible in a coordinate table: each
pair stays internally consistent, `Ed` (13-14) keeps its length, while
`Eh` (7-8) silently refers to the wrong edge of the eye. An inversion is
reported but **never corrected automatically**: moving a point to
satisfy the order would invent a measurement rather than repair one.

## See also

[`fishmorph_consolidate()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_consolidate.md)
to read the journal back,
[`fishmorph_build_db()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_build_db.md)
to build the database from it,
[`launch_fishmorph_space()`](https://funtraits.github.io/Rfishmorph/reference/launch_fishmorph_space.md)
to explore the morphological space.

## Examples

``` r
if (FALSE) { # \dontrun{
launch_fishmorph_digitizer(
  xlsx_path     = "FishMORPH/FISHMORPH_PUBLI_9556sp.xlsx",
  photo_dir     = "FishMORPH/Photos utilisees",
  new_photo_dir = "FishMORPH/Photos nouvelles",
  operator      = "AT",
  mode          = "new")
} # }
```
