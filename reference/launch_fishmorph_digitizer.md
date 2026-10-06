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
  the headers of `lm_sheet`) if it does not exist. Its rows ALSO feed
  the "correct" queue, so a species entered here – through the "new"
  queue or through the "Absent from FISHMORPH" panel of FishInTrait –
  can be reopened and corrected instead of staying invisible until
  someone promotes it to `lm_sheet`. Corrections go back to the sheet
  the row came from. NAME IT AS THE WORKBOOK DOES: the match is exact,
  and a workbook carrying `New_specimen` opened with the default
  `"new_specimens"` gets a SECOND, empty sheet rather than the one it
  already has.

- new_photo_dir:

  Folder of the new specimens' photographs ("new" mode). Every image in
  the folder forms the queue. It may not exist: the "new" mode is then
  simply unavailable until the "New species" page puts a photograph in
  it, which creates the folder.

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
  digitized from the segments), "correct" (specimens ALREADY landmarked,
  on `lm_sheet` OR on `new_sheet`, to be reviewed/corrected: the 21
  points are reloaded from the workbook) or "new" (new photographs from
  `new_photo_dir`, appended to `new_sheet`). Switchable at any moment
  through the "Queue" selector in the app. If the requested queue is
  empty, the app starts on another one.

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

A declaration is SAVED with the specimen, in the column `collapse_rules`
of the target sheet and in the journal, as the list of rule identifiers
(`"Mo;Hd6"`). Reopening the specimen puts the boxes back. It has to be
written down as such: a copy rule leaves nothing in the coordinates that
distinguishes it from a chance coincidence, and an unticked box cannot
be told from a box that was never ticked. Before this column existed the
statement lived only in the geometry it produced, so a reopened specimen
came back with its points collapsed but its boxes empty – and the first
click, with the rule no longer applied, quietly undid the zero. For
everything entered then, and for any table coming from elsewhere, the
rules are still read back off the coordinates – a pair of coincident
points, a point on the mid axis – and the two readings are unioned.

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

## Quality and review of an entry

The `"Quality"` tab of the side panel carries two fields the coordinates
cannot express: a SCORE from 1 (unusable, landmarks largely guessed) to
5 (excellent – whole fish, strictly lateral, every landmark
unambiguous), and a TICK declaring the specimen checked. They answer two
different questions, hence two fields: how good the entry is, and
whether anyone has actually looked at it. A specimen can perfectly well
be checked AND poor – that is the state a re-photographing list is built
from.

`"Not scored"` and an empty cell are the same statement, and neither is
a score of zero. Both fields are reloaded with the specimen and
rewritten at every save, so returning to a species and saving it again
preserves the review it already carries; the author and the date are
stamped only when something is actually declared, otherwise "nobody has
looked at it" would be indistinguishable from "somebody looked and said
nothing".

They are written to four columns of the target sheet – `quality_score`,
`reviewed`, `reviewed_by`, `review_date`, created on the fly like the
hinge columns, next to the `collapse_rules` of the declared coincidences
– and, at record level, to the journal, from which
[`fishmorph_consolidate()`](https://funtraits.github.io/Rfishmorph/reference/fishmorph_consolidate.md)
brings them back. The journal already said how each POINT was obtained
(`placed`, `seeded`, `derived`...); this says what the ENTRY as a whole
is worth, which only the operator looking at the photograph can decide.

## Bringing a new photograph in ("New species" page)

Until now a photograph entered a session only by being dropped into
`new_photo_dir` from a file manager, before launch, under whatever name
the camera had given it. That is not a detail of housekeeping: the file
name is the ONLY identity an image has before it is measured – the
species is read off it (`.fm_name_from_file()`) and the workbook rows
are matched on it – so the step belongs to the protocol and it belongs
in the application.

The second tab of the main panel does it in four moves, in the only
order that is safe. **Browse** for the file (JPEG, PNG, GIF, BMP or
TIFF; the real format is read from the magic bytes, not from the
extension, since about 7 per cent of the `.jpg` of this collection are
not JPEG). **Name** the specimen `Genus species`, pre-filled from the
file name and checkable against FishBase. **Frame** it – quarter turns,
mirrors, and a crop drawn with the mouse on the preview. **Commit**,
which writes the picture and opens it straight away in the `"new"`
queue.

The framing comes before the first click and must never come after it.
The digitizer records coordinates in the pixels OF THE FILE, so cropping
or rotating a photograph that already carries landmarks would move every
one of them without touching a single recorded number. This page is the
one place where the geometry of an image may still change, and it is
upstream of the first click by construction. Quarter turns and crops are
exact array operations, lossless by nature; the output format follows
the real bytes of the source, so a PNG stays a PNG rather than acquiring
JPEG artefacts on the very pixels the landmarks are read on.

The copy is named `Genus_species.<ext>`, suffixed `_2`, `_3`... when the
folder already holds that species – a SPECIMEN counter, not a duplicate
marker: the `"new"` queue is one entry per PHOTOGRAPH, several specimens
of one species are legitimate, and both readers of the file name strip a
trailing number, so all of them come back to the same binomial. The file
the operator selected is left untouched where it was, and a copy of it
is kept under `_originaux/` beside the queue: what enters the queue has
been cropped and turned, and those pixels are gone.

NOTHING is written to the workbook here. A row of `new_sheet` is the
record of a MEASUREMENT, and a photograph nobody has digitized has no
measurement to declare; the row is created by "Save & next", keyed on
`photo_file`, exactly as for a photograph dropped in the folder by hand.
An empty row written at intake would be indistinguishable from a
specimen whose landmarks all came out `NA`.

The queue is rebuilt on the spot – the alternative being to close the
application and lose the journal position for the sake of one
photograph.

## Order of the queues

The `"reconstruct"` and `"correct"` queues run in ALPHABETICAL order of
the species, not in the order of the workbook rows – which is an
accident of how the sheet was assembled. The order of the queue is the
order of the work: "Save & next" hands over the next NAME, so a session
walks the classification instead of jumping from one unrelated fish to
another. Congeners then arrive together – the same eye, the same fin,
the same ambiguities – and correcting one *Barbus* puts the whole genus
in the operator's hand while the criteria are still fresh. Sorting is
done in the C locale, so the order is identical on every workstation.
The `"new"` queue keeps the alphabetical order of the photograph FILE
names, the only identity those images have before they are named.

The field to the right of the toolbar reaches any species of the current
queue by name; its list is the queue itself, hence alphabetical, and the
search results keep that order instead of being ranked by match score.

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
