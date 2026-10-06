library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(RColorBrewer)
library(scales)
library(ggrepel)
library(plotly)
library(MASS)
library(magrittr)

# ─── Data ────────────────────────────────────────────────────────────────────
#
# Trois sources, dans cet ordre :
#   1. l'option posee par launch_fishmorph_space(data = ...)  -> jeu a jour
#   2. le fichier embarque dans le package                    -> app autonome
#   3. le fichier du repertoire courant                       -> runApp() direct
# La resolution est explicite plutot qu'un simple read.csv() sur le repertoire
# courant : l'app doit demarrer identiquement qu'elle soit lancee par le package
# ou depuis son dossier.

.fms_data_path <- function() {
  p <- getOption("Rfishmorph.space_data", NULL)
  if (!is.null(p) && nzchar(p) && file.exists(p)) return(p)
  p <- system.file("extdata", "fishmorph_data.csv", package = "Rfishmorph")
  if (nzchar(p) && file.exists(p)) return(p)
  if (file.exists("fishmorph_data.csv")) return("fishmorph_data.csv")
  stop("Jeu de donnees introuvable. Passez-le explicitement : ",
       "launch_fishmorph_space(data = \"chemin/vers/fishmorph_data.csv\")",
       call. = FALSE)
}

FISH <- read.csv(.fms_data_path(), sep = ";", stringsAsFactors = FALSE)

# MBl and MBw are already log10-transformed in this dataset
TRAIT_COLS <- c("BEl","VEp","REs","OGp","RMl","BLs","PFv","PFs","CPt")

TRAIT_NAMES <- c(
  BEl = "Body elongation (BEl)",
  VEp = "Vertical eye position (VEp)",
  REs = "Relative eye size (REs)",
  OGp = "Oral gape position (OGp)",
  RMl = "Relative maxillary length (RMl)",
  BLs = "Body lateral shape (BLs)",
  PFv = "Pectoral fin vertical position (PFv)",
  PFs = "Pectoral fin size (PFs)",
  CPt = "Caudal peduncle throttling (CPt)"
)

TRAIT_DESC <- c(
  BEl = "Body length / body depth — distinguishes anguilliform vs. carangiform locomotion",
  VEp = "Eye height / body depth — vertical habitat use: benthic (low) vs. pelagic (high)",
  REs = "Eye diameter / head depth — visual acuity; light environment adaptation",
  OGp = "Mouth position / body depth — feeding stratum: surface (high) vs. benthic (low)",
  RMl = "Jaw length / head depth — gape width; prey size and predatory capacity",
  BLs = "Head depth / body depth — lateral compression; manoeuvrability in structured habitat",
  PFv = "Pectoral fin insertion / body depth — fin thrust position: benthic vs. pelagic",
  PFs = "Pectoral fin length / body length — swimming mode; station-holding ability",
  CPt = "Caudal fin depth / peduncle depth — burst-swimming capacity; sustained speed"
)

# IUCN — include EW and EX from the real dataset
IUCN_LEVELS <- c("LC","NT","VU","EN","CR","EW","EX","DD")
IUCN_COLS <- c(
  LC = "#60B347", NT = "#CCE226", VU = "#F9A825",
  EN = "#E65100", CR = "#B71C1C", EW = "#7B1FA2",
  EX = "#212121", DD = "#9E9E9E"
)

# Order colour palette — built dynamically from real orders
ORDERS <- sort(unique(FISH$Order[!is.na(FISH$Order)]))
ORDER_COLS <- setNames(
  colorRampPalette(brewer.pal(9, "Set1"))(length(ORDERS)),
  ORDERS
)

FUNSPACE_PAL <- colorRampPalette(c(
  "#FFFFCC","#FFEDA0","#FED976","#FEB24C",
  "#FD8D3C","#FC4E2A","#E31A1C","#B10026"
))

# ─── KDE helpers (mirrors plot_FS_new logic) ─────────────────────────────────

density_threshold <- function(z, prob) {
  zv  <- sort(as.vector(z), decreasing = TRUE)
  czv <- cumsum(zv) / sum(zv)
  zv[which(czv >= prob)[1]]
}

compute_kde <- function(scores, n_grid = 200, pad = 2,
                        bw_mult = 0.45, threshold = 0.99, n_bands = 20) {
  sc <- as.matrix(scores[, 1:2])
  colnames(sc) <- c("PC1","PC2")
  rng_x <- range(sc[,1]) + c(-pad, pad)
  rng_y <- range(sc[,2]) + c(-pad, pad)
  bw  <- c(MASS::bandwidth.nrd(sc[,1]) * bw_mult,
            MASS::bandwidth.nrd(sc[,2]) * bw_mult)
  kde <- MASS::kde2d(sc[,1], sc[,2], h = bw, n = n_grid,
                     lims = c(rng_x, rng_y))
  thr99       <- density_threshold(kde$z, threshold)
  qb          <- sapply(seq(0.1, 1, length.out = n_bands),
                        function(p) density_threshold(kde$z, p))
  quant_breaks <- sort(unique(c(0, qb, max(kde$z))))
  df       <- expand.grid(PC1 = kde$x, PC2 = kde$y)
  df$z     <- as.vector(kde$z)
  df$z_clip <- ifelse(df$z >= thr99, df$z, NA)
  list(df = df, thr99 = thr99, quant_breaks = quant_breaks,
       rng_x = rng_x, rng_y = rng_y)
}

# ─── PCA helpers ─────────────────────────────────────────────────────────────

compute_pca <- function(data, traits, include_mbl = FALSE, include_mbw = FALSE) {
  cols <- traits
  if (include_mbl) cols <- c("MBl", cols)
  if (include_mbw) cols <- c("MBw", cols)
  mat  <- data[, cols, drop = FALSE]
  # MBl/MBw are already log10 in this dataset — no further transform needed
  mat  <- mat[complete.cases(mat), ]
  pr   <- prcomp(mat, center = TRUE, scale. = TRUE)
  list(pca = pr, mat = mat, traits = cols, nrow = nrow(mat))
}

project_onto_pca <- function(pca_obj, newdata) {
  cols      <- pca_obj$traits
  ref_means <- pca_obj$pca$center
  ref_sds   <- pca_obj$pca$scale
  mat_new   <- matrix(0, nrow = nrow(newdata), ncol = length(cols))
  colnames(mat_new) <- cols
  for (tr in cols) {
    if (tr %in% names(newdata))
      mat_new[, tr] <- (newdata[[tr]] - ref_means[tr]) / ref_sds[tr]
    # else: leave at 0 (project at centroid for missing trait)
  }
  as.data.frame(mat_new %*% pca_obj$pca$rotation)
}

# ─── Functional diversity ────────────────────────────────────────────────────

compute_fd <- function(sc2d) {
  pts <- sc2d[complete.cases(sc2d), ]
  n   <- nrow(pts)
  if (n < 3) return(list(FRic = NA, FDis = NA, n = n, hull_pts = NULL))
  tryCatch({
    ch   <- chull(pts[,1], pts[,2])
    hull <- pts[ch, ]
    x    <- hull[,1]; y <- hull[,2]; m <- length(x)
    FRic <- abs(sum(x[1:(m-1)]*y[2:m] - x[2:m]*y[1:(m-1)]) +
                  x[m]*y[1] - x[1]*y[m]) / 2
    FDis <- mean(sqrt(rowSums(sweep(pts, 2, colMeans(pts))^2)))
    list(FRic = round(FRic, 4), FDis = round(FDis, 4),
         n = n, hull_pts = as.data.frame(hull))
  }, error = function(e) list(FRic = NA, FDis = NA, n = n, hull_pts = NULL))
}

# ─── UI ──────────────────────────────────────────────────────────────────────

ui <- page_navbar(
  title  = tags$span("🐟 FishMorphSpace"),
  theme  = bs_theme(
    bootswatch   = "flatly",
    primary      = "#1565C0",
    base_font    = font_google("Source Sans Pro"),
    heading_font = font_google("Merriweather"),
    `navbar-bg`  = "#1565C0"
  ),
  bg = "#1565C0", inverse = TRUE,

  # ── Tab 1: Explore ──────────────────────────────────────────────────────────
  nav_panel("🌍 Explore global space",
    layout_sidebar(
      sidebar = sidebar(width = 275, bg = "#F5F7FA",

        h6("Trait selection", class = "text-primary fw-bold"),
        checkboxGroupInput("explore_traits", NULL,
          choices  = setNames(TRAIT_COLS, TRAIT_NAMES),
          selected = TRAIT_COLS),
        checkboxInput("explore_mbl", "Include body length (log MBl)", FALSE),
        checkboxInput("explore_mbw", "Include body width (log MBw)",  FALSE),

        hr(),
        h6("PCA axes", class = "text-primary fw-bold"),
        fluidRow(
          column(6, numericInput("pc_x", "X", 1, 1, 6, 1)),
          column(6, numericInput("pc_y", "Y", 2, 1, 6, 1))
        ),

        hr(),
        h6("Highlight group", class = "text-primary fw-bold"),
        selectInput("hl_var", "Variable",
          choices  = c("None" = "none", "Order" = "Order",
                       "Family" = "Family", "Genus" = "Genus",
                       "IUCN status" = "IUCN"),
          selected = "none"),
        # Dynamic value selector — populated server-side
        uiOutput("hl_val_ui"),
        helpText("Select one group to highlight; all others shown in grey."),

        hr(),
        h6("Filter", class = "text-primary fw-bold"),
        selectInput("filter_order", "Taxonomic order",
          c("All", ORDERS), "All"),
        selectizeInput("filter_iucn", "IUCN status",
          choices  = c("All", IUCN_LEVELS),
          selected = "All", multiple = FALSE),

        hr(),
        h6("KDE parameters", class = "text-primary fw-bold"),
        sliderInput("bw_mult",   "Bandwidth multiplier", 0.15, 1.5, 0.45, 0.05),
        sliderInput("n_bands",   "Density bands",        5,   40,  20,    1),
        sliderInput("threshold", "Coverage (quantile)",  0.90, 0.999, 0.99, 0.005),

        hr(),
        checkboxInput("show_pts",         "Show species points",        FALSE),
        checkboxInput("show_loadings",    "Show trait arrows",          FALSE),
        checkboxInput("show_ref_contour", "Show global reference contour (when filtered)", FALSE),
        conditionalPanel("input.show_pts",
          sliderInput("pt_size",  "Point size",    0.05, 2,   0.2, 0.05),
          sliderInput("pt_alpha", "Point opacity", 0.02, 1.0, 0.25, 0.01)
        ),

        hr(),
        downloadButton("dl_explore", "Download plot (PDF)",
                       class = "btn-sm btn-outline-primary w-100")
      ),

      card(
        card_header("Global morphological space — funspace style"),
        plotlyOutput("explore_plot", height = "580px"),
        card_footer(uiOutput("variance_explained"))
      ),
      card(
        card_header("Trait loadings (PC1–4)"),
        plotOutput("loadings_plot", height = "210px")
      )
    )
  ),

  # ── Tab 2: Project ───────────────────────────────────────────────────────────
  nav_panel("📤 Project your data",
    layout_sidebar(
      sidebar = sidebar(width = 275, bg = "#F5F7FA",

        h6("Upload dataset", class = "text-primary fw-bold"),
        fileInput("user_file", NULL, accept = c(".csv",".txt"),
                  placeholder = "CSV file"),
        helpText("Required: ≥ 2 of BEl VEp REs OGp RMl BLs PFv PFs CPt. Optional: Species, Group."),

        hr(),
        uiOutput("group_var_ui"),

        hr(),
        h6("Background KDE", class = "text-primary fw-bold"),
        checkboxInput("proj_show_bg", "Show global KDE background", TRUE),
        sliderInput("proj_bw",  "Bandwidth mult.", 0.15, 1.5, 0.45, 0.05),
        sliderInput("proj_thr", "Coverage",        0.90, 0.999, 0.99, 0.005),

        hr(),
        sliderInput("proj_pt_size", "Projected point size", 1, 8, 3, 0.5),
        checkboxInput("proj_labels", "Label species", FALSE),

        hr(),
        downloadButton("dl_proj", "Download plot (PDF)",
                       class = "btn-sm btn-outline-primary w-100")
      ),
      card(
        card_header("Projection onto global morphological space"),
        plotOutput("proj_plot", height = "520px")
      ),
      card(
        card_header("Uploaded data preview"),
        DTOutput("proj_table", height = "220px")
      )
    )
  ),

  # ── Tab 3: Functional diversity ──────────────────────────────────────────────
  nav_panel("📊 Functional diversity",
    layout_sidebar(
      sidebar = sidebar(width = 275, bg = "#F5F7FA",

        h6("Assemblage definition", class = "text-primary fw-bold"),
        radioButtons("fd_group", "Group by",
          c("Taxonomic order" = "Order", "IUCN status" = "IUCN",
            "Family" = "Family"),
          "Order"),
        uiOutput("fd_group_select"),

        hr(),
        h6("PCA settings", class = "text-primary fw-bold"),
        checkboxGroupInput("fd_traits", "Traits",
          choices  = setNames(TRAIT_COLS, TRAIT_NAMES),
          selected = TRAIT_COLS),
        fluidRow(
          column(6, numericInput("fd_pc_x", "X", 1, 1, 6, 1)),
          column(6, numericInput("fd_pc_y", "Y", 2, 1, 6, 1))
        ),

        hr(),
        h6("Global background", class = "text-primary fw-bold"),
        checkboxInput("fd_show_global", "Show global KDE", TRUE),
        sliderInput("fd_bw",  "Bandwidth mult.", 0.15, 1.5, 0.45, 0.05),
        sliderInput("fd_thr", "Coverage",        0.90, 0.999, 0.99, 0.005),

        hr(),
        checkboxInput("fd_show_hull", "Show convex hulls",   TRUE),
        checkboxInput("fd_show_pts",  "Show species points", TRUE),

        hr(),
        downloadButton("dl_fd_plot",  "Download plot (PDF)",
                       class = "btn-sm btn-outline-primary w-100 mb-1"),
        downloadButton("dl_fd_table", "Download FD table (CSV)",
                       class = "btn-sm btn-outline-secondary w-100")
      ),
      card(
        card_header("Morphological space by assemblage"),
        plotOutput("fd_plot", height = "480px")
      ),
      card(
        card_header("Functional diversity indices"),
        DTOutput("fd_table", height = "260px")
      )
    )
  ),

  # ── Tab 4: Trait distributions ───────────────────────────────────────────────
  nav_panel("📈 Trait distributions",
    layout_sidebar(
      sidebar = sidebar(width = 240, bg = "#F5F7FA",
        h6("Trait", class = "text-primary fw-bold"),
        selectInput("dist_trait", NULL,
          choices  = setNames(TRAIT_COLS, TRAIT_NAMES),
          selected = "BEl"),
        hr(),
        radioButtons("dist_group", "Compare by",
          c("Taxonomic order" = "Order", "IUCN status" = "IUCN",
            "Family" = "Family"),
          "Order"),
        checkboxInput("dist_log", "Log-transform axis", FALSE),
        downloadButton("dl_dist", "Download plot",
                       class = "btn-sm btn-outline-primary w-100")
      ),
      card(card_header("Trait distribution"),
           plotlyOutput("dist_plot", height = "460px")),
      card(card_header("Trait definition"),
           uiOutput("trait_def_box"))
    )
  ),

  # ── Tab 5: About ─────────────────────────────────────────────────────────────
  nav_panel("ℹ️ About",
    layout_columns(col_widths = c(7, 5),
      card(card_header("FishMorphSpace"), markdown("
**FishMorphSpace** visualizes the global morphological space of freshwater fishes
using the **FISHMORPH** database (Brosse *et al.*, 2021, *GEB*,
doi:[10.1111/geb.13395](https://doi.org/10.1111/geb.13395)).

The density representation follows the **funspace** style (Carmona *et al.*, 2021):
a kernel density estimate (bandwidth × 0.45 by default) partitioned into quantile
bands (yellow → red), with an outer 0.99-quantile contour.

**Variables in this dataset**

| Column | Description |
|--------|-------------|
| Species | Binomial species name |
| Family / Order / Genus | Taxonomy |
| BEl … CPt | 9 dimensionless morphological ratios |
| MBl | log₁₀ maximum body length (cm) |
| MBw | log₁₀ maximum body width (cm) |
| IUCN | IUCN Red List category (LC, NT, VU, EN, CR, EW, EX, DD) |

**Citation**

> [Authors] (in prep). FishMorphSpace. *Ecology and Evolution*.

> Brosse S *et al.* (2021). FISHMORPH. *Global Ecology and Biogeography*, 30, 2330–2336.
      ")),
      card(card_header("Trait glossary"), DTOutput("trait_table"))
    )
  )
)

# ─── Server ──────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # ── Global PCA ─────────────────────────────────────────────────────────────
  global_pca <- reactive({
    req(input$explore_traits)
    validate(need(length(input$explore_traits) >= 2, "Select at least 2 traits."))
    compute_pca(FISH, input$explore_traits, input$explore_mbl, input$explore_mbw)
  })

  global_scores_df <- reactive({
    pr  <- global_pca()
    sc  <- as.data.frame(pr$pca$x)
    # match rows back to FISH via rownames (complete.cases subset)
    idx <- as.integer(rownames(sc))
    meta <- FISH[idx, c("Species","Order","Family","Genus","IUCN","MBl","MBw")]
    cbind(meta, sc)
  })

  filtered_scores <- reactive({
    sc <- global_scores_df()
    if (!is.null(input$filter_order) && input$filter_order != "All")
      sc <- sc[!is.na(sc$Order) & sc$Order == input$filter_order, ]
    if (!is.null(input$filter_iucn) && input$filter_iucn != "All")
      sc <- sc[!is.na(sc$IUCN) & sc$IUCN == input$filter_iucn, ]
    sc
  })

  is_filtered <- reactive({
    (input$filter_order != "All") || (input$filter_iucn != "All")
  })

  # ── KDE (global reference — recomputed when PCA or axes change) ─────────
  kde_global <- reactive({
    sc  <- global_scores_df()
    pcx <- paste0("PC", input$pc_x)
    pcy <- paste0("PC", input$pc_y)
    validate(need(pcx %in% names(sc), "Axis out of range."))
    compute_kde(sc[, c(pcx, pcy)],
                bw_mult = input$bw_mult, threshold = input$threshold,
                n_bands = input$n_bands)
  })

  kde_filtered <- reactive({
    sc  <- filtered_scores()
    pcx <- paste0("PC", input$pc_x)
    pcy <- paste0("PC", input$pc_y)
    validate(need(nrow(sc) > 20, "Not enough species after filtering (need > 20)."))
    compute_kde(sc[, c(pcx, pcy)],
                bw_mult = input$bw_mult, threshold = input$threshold,
                n_bands = input$n_bands)
  })

  # ── Dynamic highlight value selector ─────────────────────────────────────
  output$hl_val_ui <- renderUI({
    var <- input$hl_var
    if (is.null(var) || var == "none") return(NULL)
    sc <- global_scores_df()
    vals <- sort(unique(sc[[var]][!is.na(sc[[var]])]))
    if (var == "IUCN") vals <- intersect(IUCN_LEVELS, vals)
    selectizeInput("hl_val", "Group to highlight",
      choices  = c("— show all —" = "all", vals),
      selected = "all",
      options  = list(maxOptions = 300))
  })

  # ── Explore plot — native plotly ──────────────────────────────────────────
  explore_plotly <- reactive({
    sc_all <- global_scores_df()
    sc_fil <- filtered_scores()
    pr     <- global_pca()
    pcx_nm <- paste0("PC", input$pc_x)
    pcy_nm <- paste0("PC", input$pc_y)
    validate(need(pcx_nm %in% names(sc_all), "Axis out of range."))

    kde_obj <- if (is_filtered()) kde_filtered() else kde_global()
    var_exp <- summary(pr$pca)$importance[2, ] * 100
    xlab    <- sprintf("PC%d (%.2f%%)", input$pc_x, var_exp[input$pc_x])
    ylab    <- sprintf("PC%d (%.2f%%)", input$pc_y, var_exp[input$pc_y])

    thr99  <- kde_obj$thr99
    qb     <- kde_obj$quant_breaks
    qb_in  <- qb[qb >= thr99]   # only inside the 0.99 shell
    rng_x  <- kde_obj$rng_x
    rng_y  <- kde_obj$rng_y

    pts_df <- sc_fil[, c(pcx_nm, pcy_nm, "Species","Order","Family","Genus","IUCN")]
    names(pts_df)[1:2] <- c("PC1","PC2")

    hl_var      <- input$hl_var
    hl_val      <- if (!is.null(input$hl_val)) input$hl_val else "all"
    use_hl      <- !is.null(hl_var) && hl_var != "none"
    show_all_cols <- use_hl && hl_val == "all"

    hl_col <- if (use_hl && !show_all_cols && hl_var == "IUCN")
                unname(IUCN_COLS[hl_val]) else "#C62828"
    if (is.na(hl_col)) hl_col <- "#C62828"

    # ── Loadings arrows (as plotly annotations) ─────────────────────────────
    rot <- as.data.frame(pr$pca$rotation[, c(input$pc_x, input$pc_y)])
    names(rot) <- c("dPC1","dPC2")
    rot$trait  <- rownames(rot)
    sc_range   <- min(diff(rng_x), diff(rng_y))
    rot_scale  <- sc_range / max(abs(as.matrix(rot[, 1:2]))) * 0.35
    rot$dPC1   <- rot$dPC1 * rot_scale
    rot$dPC2   <- rot$dPC2 * rot_scale

    arrow_annots <- if (input$show_loadings) {
      lapply(seq_len(nrow(rot)), function(i) {
        list(
          x = rot$dPC1[i], y = rot$dPC2[i],
          ax = 0, ay = 0,
          xref = "x", yref = "y", axref = "x", ayref = "y",
          text = paste0("<b>", rot$trait[i], "</b>"),
          showarrow = TRUE,
          arrowhead = 2, arrowsize = 1, arrowwidth = 1.2,
          arrowcolor = "black",
          font = list(size = 11, color = "black"),
          xshift = rot$dPC1[i] * 0.14 * 100,
          yshift = rot$dPC2[i] * 0.14 * 100
        )
      })
    } else list()

    # ── KDE contour traces ──────────────────────────────────────────────────
    # Transpose z: plotly expects z[i,j] = value at x[i], y[j]  → t(kde$z)
    kde_raw <- kde_obj  # contains rng_x, rng_y; rebuild kde grid from df
    # Reconstruct grid dims from df
    x_vals <- sort(unique(kde_raw$df$PC1))
    y_vals <- sort(unique(kde_raw$df$PC2))
    nx     <- length(x_vals); ny <- length(y_vals)
    z_full <- matrix(kde_raw$df$z,      nrow = nx, ncol = ny)
    z_clip <- matrix(kde_raw$df$z_clip, nrow = nx, ncol = ny)

    n_levels <- max(length(qb_in) - 1L, 1L)
    cscale <- lapply(seq(0, 1, length.out = 8), function(v) {
      cols <- c("#FFFFCC","#FFEDA0","#FED976","#FEB24C",
                "#FD8D3C","#FC4E2A","#E31A1C","#B10026")
      list(v, cols[round(v * 7) + 1])
    })

    p <- plot_ly() |>
      # Filled density bands (clipped to 0.99 shell)
      add_contour(
        x = x_vals, y = y_vals, z = t(z_clip),
        colorscale  = cscale,
        contours    = list(
          coloring  = "fill",
          showlines = TRUE,
          start     = thr99,
          end       = max(z_full, na.rm = TRUE) * 1.001,
          size      = (max(z_full, na.rm = TRUE) - thr99) / n_levels
        ),
        line        = list(color = "rgba(40,40,40,0.18)", width = 0.4),
        showscale   = FALSE,
        hoverinfo   = "none",
        name        = "density"
      ) |>
      # Outer 0.99 boundary — thick dark contour
      add_contour(
        x = x_vals, y = y_vals, z = t(z_full),
        contours  = list(
          coloring  = "none",
          showlines = TRUE,
          start = thr99, end = thr99, size = 0.0001
        ),
        line      = list(color = "rgba(50,50,50,0.85)", width = 2),
        showscale = FALSE, showlegend = FALSE,
        hoverinfo = "none",
        name      = "boundary"
      )

    # Optional reference contour (when filtered)
    if (input$show_ref_contour && is_filtered()) {
      ref    <- kde_global()
      xr     <- sort(unique(ref$df$PC1))
      yr     <- sort(unique(ref$df$PC2))
      nxr    <- length(xr); nyr <- length(yr)
      zr_full <- matrix(ref$df$z, nrow = nxr, ncol = nyr)
      p <- p %>% add_contour(
        x = xr, y = yr, z = t(zr_full),
        contours  = list(coloring="none", showlines=TRUE,
                         start=ref$thr99, end=ref$thr99, size=0.0001),
        line      = list(color="rgba(21,101,192,0.7)", width=1.5, dash="dash"),
        showscale = FALSE, showlegend = FALSE,
        hoverinfo = "none", name = "global ref"
      )
    }

    # ── Species point traces ─────────────────────────────────────────────────
    if (input$show_pts) {
      pt_sz <- input$pt_size * 6   # plotly marker size in px
      pt_op <- input$pt_alpha

      if (!use_hl) {
        # Plain grey
        p <- p %>% add_trace(
          type = "scatter", mode = "markers",
          data = pts_df, x = ~PC1, y = ~PC2,
          marker = list(size = pt_sz, color = "rgba(30,30,30,0.4)",
                        line = list(width = 0)),
          text  = ~paste0("<b>", Species, "</b><br>",
                          "Order: ", Order, "<br>Family: ", Family,
                          "<br>IUCN: ", IUCN),
          hoverinfo = "text", showlegend = FALSE, name = "species"
        )

      } else if (show_all_cols) {
        # Colour all groups
        if (hl_var == "IUCN") {
          pts_df$col_val <- ifelse(is.na(pts_df$IUCN), "DD", pts_df$IUCN)
          for (lv in IUCN_LEVELS) {
            sub <- pts_df[pts_df$col_val == lv & !is.na(pts_df$col_val), ]
            if (nrow(sub) == 0) next
            p <- p %>% add_trace(
              type = "scatter", mode = "markers",
              data = sub, x = ~PC1, y = ~PC2,
              marker = list(size = pt_sz, color = IUCN_COLS[lv],
                            opacity = pt_op, line = list(width = 0)),
              text  = ~paste0("<b>", Species, "</b><br>IUCN: ", IUCN,
                              "<br>Order: ", Order),
              hoverinfo = "text", name = lv, legendgroup = lv
            )
          }
        } else {
          grp_vals <- pts_df[[hl_var]]
          grp_lvls <- sort(unique(grp_vals[!is.na(grp_vals)]))
          grp_pal  <- setNames(
            colorRampPalette(brewer.pal(9,"Set1"))(length(grp_lvls)), grp_lvls)
          for (lv in grp_lvls) {
            sub <- pts_df[!is.na(pts_df[[hl_var]]) & pts_df[[hl_var]] == lv, ]
            if (nrow(sub) == 0) next
            p <- p %>% add_trace(
              type = "scatter", mode = "markers",
              data = sub, x = ~PC1, y = ~PC2,
              marker = list(size = pt_sz, color = grp_pal[lv],
                            opacity = pt_op, line = list(width = 0)),
              text  = ~paste0("<b>", Species, "</b><br>",
                              hl_var, ": ", .data[[hl_var]],
                              "<br>IUCN: ", IUCN),
              hoverinfo = "text", name = lv, legendgroup = lv
            )
          }
        }
      } else {
        # Specific group highlighted
        bg <- pts_df[is.na(pts_df[[hl_var]]) | pts_df[[hl_var]] != hl_val, ]
        fg <- pts_df[!is.na(pts_df[[hl_var]]) & pts_df[[hl_var]] == hl_val, ]
        if (nrow(bg) > 0)
          p <- p %>% add_trace(
            type = "scatter", mode = "markers",
            data = bg, x = ~PC1, y = ~PC2,
            marker = list(size = pt_sz * 0.8,
                          color = paste0("rgba(160,160,160,", pt_op * 0.4, ")"),
                          line = list(width = 0)),
            text  = ~paste0("<b>", Species, "</b><br>",
                            hl_var, ": ", .data[[hl_var]],
                            "<br>IUCN: ", IUCN),
            hoverinfo = "text", showlegend = FALSE, name = "other"
          )
        if (nrow(fg) > 0)
          p <- p %>% add_trace(
            type = "scatter", mode = "markers",
            data = fg, x = ~PC1, y = ~PC2,
            marker = list(size = pt_sz * 1.3, color = hl_col,
                          opacity = min(pt_op * 2, 1),
                          line = list(width = 0.3, color = "white")),
            text  = ~paste0("<b>", Species, "</b><br>",
                            hl_var, ": ", .data[[hl_var]],
                            "<br>IUCN: ", IUCN),
            hoverinfo = "text", name = hl_val
          )
      }
    }

    # ── Layout ──────────────────────────────────────────────────────────────
    p %>% layout(
      title  = list(
        text = sprintf("<b>Global morphological space — %d species</b>", nrow(pts_df)),
        font = list(size = 14), x = 0.02, xanchor = "left"
      ),
      xaxis  = list(
        title       = xlab,
        range       = rng_x,
        zeroline    = FALSE,
        showgrid    = FALSE,
        showline    = TRUE,
        linecolor   = "black",
        mirror      = TRUE,
        scaleanchor = "y",
        scaleratio  = 1
      ),
      yaxis  = list(
        title     = ylab,
        range     = rng_y,
        zeroline  = FALSE,
        showgrid  = FALSE,
        showline  = TRUE,
        linecolor = "black",
        mirror    = TRUE
      ),
      plot_bgcolor  = "white",
      paper_bgcolor = "white",
      legend = list(
        orientation = "v",
        x = 1.01, y = 0.99,
        bgcolor     = "rgba(255,255,255,0.85)",
        bordercolor = "grey80",
        borderwidth = 1,
        font        = list(size = 11),
        itemsizing  = "constant",
        tracegroupgap = 3
      ),
      annotations = arrow_annots,
      shapes = list(
        list(type="line", x0=rng_x[1], x1=rng_x[2], y0=0, y1=0,
             line=list(color="rgba(100,100,100,0.5)", width=0.8, dash="dash"),
             xref="x", yref="y"),
        list(type="line", x0=0, x1=0, y0=rng_y[1], y1=rng_y[2],
             line=list(color="rgba(100,100,100,0.5)", width=0.8, dash="dash"),
             xref="x", yref="y")
      ),
      margin  = list(l=60, r=20, t=50, b=60),
      hovermode = "closest"
    ) |>
    config(
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c("select2d","lasso2d","autoScale2d"),
      toImageButtonOptions = list(
        format = "png", filename = "FishMorphSpace", scale = 3,
        width = 900, height = 750
      )
    )
  })

  # Keep a ggplot version only for PDF download
  explore_ggplot_pdf <- reactive({
    sc_all <- global_scores_df()
    sc_fil <- filtered_scores()
    pr     <- global_pca()
    pcx_nm <- paste0("PC", input$pc_x)
    pcy_nm <- paste0("PC", input$pc_y)
    kde_obj <- if (is_filtered()) kde_filtered() else kde_global()
    var_exp <- summary(pr$pca)$importance[2,]*100
    xlab <- sprintf("PC%d (%.2f%%)", input$pc_x, var_exp[input$pc_x])
    ylab <- sprintf("PC%d (%.2f%%)", input$pc_y, var_exp[input$pc_y])
    fill_df <- kde_obj$df; thr99 <- kde_obj$thr99
    qb <- kde_obj$quant_breaks; rng_x <- kde_obj$rng_x; rng_y <- kde_obj$rng_y
    pal <- FUNSPACE_PAL(length(qb)-1)
    pts_df <- sc_fil[,c(pcx_nm,pcy_nm,"Species","Order","Family","IUCN")]
    names(pts_df)[1:2] <- c("PC1","PC2")
    hl_var <- input$hl_var; hl_val <- if (!is.null(input$hl_val)) input$hl_val else "all"
    use_hl <- !is.null(hl_var) && hl_var != "none"
    rot <- as.data.frame(pr$pca$rotation[,c(input$pc_x,input$pc_y)])
    names(rot) <- c("dPC1","dPC2"); rot$trait <- rownames(rot)
    rs <- min(diff(rng_x),diff(rng_y))/max(abs(as.matrix(rot[,1:2])))*0.35
    rot$dPC1 <- rot$dPC1*rs; rot$dPC2 <- rot$dPC2*rs
    g <- ggplot() +
      coord_fixed(xlim=rng_x, ylim=rng_y, expand=FALSE) +
      geom_contour_filled(data=fill_df,aes(x=PC1,y=PC2,z=z_clip),breaks=qb) +
      scale_fill_manual(values=pal,na.value="transparent",guide="none") +
      geom_contour(data=fill_df,aes(x=PC1,y=PC2,z=z),
                   breaks=qb[qb>thr99],colour="grey20",linewidth=0.07) +
      geom_contour(data=fill_df,aes(x=PC1,y=PC2,z=z),
                   breaks=thr99,colour="grey30",linewidth=1.1) +
      { if(input$show_pts && !use_hl)
          geom_point(data=pts_df,aes(x=PC1,y=PC2),
                     colour="grey15",size=input$pt_size*0.5,alpha=input$pt_alpha,stroke=0)
        else if(input$show_pts && use_hl && hl_val!="all") list(
          geom_point(data=pts_df[is.na(pts_df[[hl_var]])|pts_df[[hl_var]]!=hl_val,],
                     aes(x=PC1,y=PC2),colour="grey75",size=input$pt_size*0.4,
                     alpha=input$pt_alpha*0.4,stroke=0),
          geom_point(data=pts_df[!is.na(pts_df[[hl_var]])&pts_df[[hl_var]]==hl_val,],
                     aes(x=PC1,y=PC2),colour="#C62828",size=input$pt_size*0.7,
                     alpha=min(input$pt_alpha*2,1),stroke=0)
        ) else NULL } +
      { if(input$show_loadings) list(
          geom_segment(data=rot,aes(x=0,y=0,xend=dPC1,yend=dPC2),
                       arrow=arrow(length=unit(0.18,"cm")),colour="black",linewidth=0.5),
          geom_text_repel(data=rot,aes(x=dPC1*1.12,y=dPC2*1.12,label=trait),
                          size=3,fontface="bold",colour="black",max.overlaps=20)
        ) else NULL } +
      geom_hline(yintercept=0,linetype="dashed",colour="grey65",linewidth=0.3) +
      geom_vline(xintercept=0,linetype="dashed",colour="grey65",linewidth=0.3) +
      labs(x=xlab,y=ylab,
           title=sprintf("Global morphological space — %d species",nrow(pts_df))) +
      theme_classic(base_size=12) +
      theme(plot.title=element_text(face="bold",size=13),
            panel.border=element_rect(fill=NA,colour="black"),
            axis.line=element_blank(),legend.position="none")
    g
  })

  output$explore_plot <- renderPlotly({ explore_plotly() })

  output$variance_explained <- renderUI({
    pr  <- global_pca()
    imp <- summary(pr$pca)$importance
    n   <- min(6, ncol(imp))
    tagList(
      tags$small("Variance explained: "),
      lapply(1:n, function(i)
        tags$span(class = "badge bg-primary me-1",
                  sprintf("PC%d: %.1f%%", i, imp[2,i] * 100)))
    )
  })

  output$loadings_plot <- renderPlot({
    pr   <- global_pca()
    nPCs <- min(4, ncol(pr$pca$rotation))
    rot  <- as.data.frame(pr$pca$rotation[, 1:nPCs])
    rot$trait <- factor(rownames(rot), levels = rownames(rot))
    rot_long  <- pivot_longer(rot, -trait, names_to = "PC", values_to = "loading")
    ggplot(rot_long, aes(x = trait, y = loading, fill = loading > 0)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~PC, nrow = 1) +
      scale_fill_manual(values = c("#E57373","#42A5F5")) +
      geom_hline(yintercept = 0, linewidth = 0.3) +
      theme_bw(base_size = 11) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8.5),
            panel.grid.minor = element_blank(),
            strip.text = element_text(face = "bold")) +
      labs(x = NULL, y = "Loading")
  }, res = 110)

  output$dl_explore <- downloadHandler(
    filename = "FishMorphSpace_explore.pdf",
    content  = function(f) ggsave(f, explore_ggplot_pdf(), width = 9, height = 8, device = "pdf"))

  # ── Project tab ────────────────────────────────────────────────────────────
  user_data <- reactive({
    req(input$user_file)
    df <- tryCatch(
      read.csv(input$user_file$datapath, stringsAsFactors = FALSE,h=T),
      error = function(e) NULL)
    if (is.null(df)) df <- tryCatch(
      read.csv2(input$user_file$datapath, stringsAsFactors = FALSE,h=T),
      error = function(e) NULL)
    validate(need(!is.null(df), "Could not read file."))
    common <- intersect(TRAIT_COLS, names(df))
    validate(need(length(common) >= 2,
                  paste("Need ≥ 2 FISHMORPH traits. Found:", paste(common, collapse=", "))))
    df
  })

  output$group_var_ui <- renderUI({
    df <- user_data()
    cat_cols <- setdiff(
      names(df)[sapply(df, function(x) is.character(x) | is.factor(x))],
      TRAIT_COLS)
    if (length(cat_cols) == 0) helpText("No categorical columns found.")
    else selectInput("proj_group", "Colour by column",
                     c("None", cat_cols), cat_cols[1])
  })

  proj_ggplot <- reactive({
    pr      <- global_pca()
    bg_sc   <- global_scores_df()
    user    <- user_data()
    user_sc <- project_onto_pca(pr, user)
    pcx_nm  <- paste0("PC", input$pc_x)
    pcy_nm  <- paste0("PC", input$pc_y)
    validate(need(pcx_nm %in% names(bg_sc), "Axis out of range."))

    var_exp <- summary(pr$pca)$importance[2,] * 100
    xlab    <- sprintf("PC%d (%.2f%%)", input$pc_x, var_exp[input$pc_x])
    ylab    <- sprintf("PC%d (%.2f%%)", input$pc_y, var_exp[input$pc_y])

    g <- ggplot() +
      theme_classic(base_size = 12) +
      theme(panel.border = element_rect(fill = NA, colour = "black"),
            axis.line    = element_blank(),
            plot.title   = element_text(face = "bold")) +
      labs(x = xlab, y = ylab, title = "Projection onto global morphological space") +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "grey65", linewidth = 0.3) +
      geom_vline(xintercept = 0, linetype = "dashed", colour = "grey65", linewidth = 0.3)

    if (input$proj_show_bg) {
      kde_bg <- compute_kde(bg_sc[, c(pcx_nm, pcy_nm)],
                            bw_mult = input$proj_bw, threshold = input$proj_thr)
      pal_bg <- FUNSPACE_PAL(length(kde_bg$quant_breaks) - 1)
      g <- g +
        coord_fixed(xlim = kde_bg$rng_x, ylim = kde_bg$rng_y, expand = FALSE) +
        geom_contour_filled(data = kde_bg$df, aes(x = PC1, y = PC2, z = z_clip),
                            breaks = kde_bg$quant_breaks) +
        scale_fill_manual(values = pal_bg, na.value = "transparent", guide = "none") +
        geom_contour(data = kde_bg$df, aes(x = PC1, y = PC2, z = z),
                     breaks = kde_bg$thr99, colour = "grey30", linewidth = 1.0)
    }

    proj_df <- cbind(user, PC1 = user_sc[[pcx_nm]], PC2 = user_sc[[pcy_nm]])
    grp_col <- if (!is.null(input$proj_group) && input$proj_group != "None" &&
                   input$proj_group %in% names(proj_df)) input$proj_group else NULL
    lbl_col <- if ("Species" %in% names(proj_df)) "Species" else NULL

    if (!is.null(grp_col)) {
      g <- g +
        geom_point(data = proj_df, aes(x = PC1, y = PC2, colour = .data[[grp_col]]),
                   size = input$proj_pt_size, alpha = 0.9,
                   shape = 21, fill = "white", stroke = 0.8) +
        scale_colour_brewer(palette = "Set1", name = grp_col)
    } else {
      g <- g +
        geom_point(data = proj_df, aes(x = PC1, y = PC2),
                   colour = "#C62828", size = input$proj_pt_size,
                   alpha = 0.9, shape = 21, fill = "white", stroke = 0.8)
    }
    if (input$proj_labels && !is.null(lbl_col))
      g <- g + geom_text_repel(data = proj_df,
                               aes(x = PC1, y = PC2, label = .data[[lbl_col]]),
                               size = 2.8, max.overlaps = 25)
    g
  })

  output$proj_plot  <- renderPlot({ proj_ggplot() }, res = 130)
  output$proj_table <- renderDT({
    datatable(user_data(), options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })
  output$dl_proj <- downloadHandler(
    filename = "FishMorphSpace_projection.pdf",
    content  = function(f) ggsave(f, proj_ggplot(), width = 9, height = 8, device = "pdf"))

  # ── Functional diversity tab ───────────────────────────────────────────────
  fd_pca <- reactive({
    req(input$fd_traits)
    validate(need(length(input$fd_traits) >= 2, "Select ≥ 2 traits."))
    compute_pca(FISH, input$fd_traits)
  })

  fd_scores_all <- reactive({
    pr  <- fd_pca()
    sc  <- as.data.frame(pr$pca$x)
    idx <- as.integer(rownames(sc))
    cbind(FISH[idx, c("Species","Order","Family","IUCN")], sc)
  })

  output$fd_group_select <- renderUI({
    grp  <- input$fd_group
    grps <- sort(unique(FISH[[grp]][!is.na(FISH[[grp]])]))
    if (grp == "IUCN") grps <- intersect(IUCN_LEVELS, grps)
    # Limit Family list length for usability
    if (grp == "Family" && length(grps) > 50)
      grps <- grps[1:50]
    checkboxGroupInput("fd_groups_sel", "Select groups",
      choices  = grps,
      selected = grps[1:min(4, length(grps))])
  })

  fd_ggplot <- reactive({
    sc  <- fd_scores_all()
    req(input$fd_groups_sel)
    pr  <- fd_pca()
    grp <- input$fd_group
    sel <- input$fd_groups_sel
    pcx_nm <- paste0("PC", input$fd_pc_x)
    pcy_nm <- paste0("PC", input$fd_pc_y)
    validate(need(pcx_nm %in% names(sc), "Axis out of range."))

    var_exp <- summary(pr$pca)$importance[2,] * 100
    xlab    <- sprintf("PC%d (%.2f%%)", input$fd_pc_x, var_exp[input$fd_pc_x])
    ylab    <- sprintf("PC%d (%.2f%%)", input$fd_pc_y, var_exp[input$fd_pc_y])

    pal <- if (grp == "IUCN") IUCN_COLS else
           if (grp == "Order") ORDER_COLS else
           setNames(colorRampPalette(brewer.pal(8,"Dark2"))(length(sel)), sel)

    g <- ggplot() +
      theme_classic(base_size = 12) +
      theme(panel.border = element_rect(fill = NA, colour = "black"),
            axis.line    = element_blank(),
            plot.title   = element_text(face = "bold")) +
      labs(x = xlab, y = ylab,
           title = sprintf("Functional space by %s", grp),
           colour = grp, fill = grp) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "grey65", linewidth = 0.3) +
      geom_vline(xintercept = 0, linetype = "dashed", colour = "grey65", linewidth = 0.3)

    if (input$fd_show_global) {
      kde_bg <- compute_kde(sc[, c(pcx_nm, pcy_nm)],
                            bw_mult = input$fd_bw, threshold = input$fd_thr)
      pal_bg <- FUNSPACE_PAL(length(kde_bg$quant_breaks) - 1)
      g <- g +
        coord_fixed(xlim = kde_bg$rng_x, ylim = kde_bg$rng_y, expand = FALSE) +
        geom_contour_filled(data = kde_bg$df, aes(x = PC1, y = PC2, z = z_clip),
                            breaks = kde_bg$quant_breaks) +
        scale_fill_manual(values = pal_bg, na.value = "transparent", guide = "none") +
        geom_contour(data = kde_bg$df, aes(x = PC1, y = PC2, z = z),
                     breaks = kde_bg$thr99, colour = "grey40", linewidth = 0.8)
    }

    for (g_nm in sel) {
      sub <- sc[!is.na(sc[[grp]]) & sc[[grp]] == g_nm, c(pcx_nm, pcy_nm)]
      res <- compute_fd(sub)
      if (input$fd_show_hull && !is.null(res$hull_pts)) {
        hull_cl <- rbind(res$hull_pts, res$hull_pts[1,])
        names(hull_cl) <- c("x","y")
        hull_cl$group  <- g_nm
        g <- g +
          geom_polygon(data = hull_cl, aes(x = x, y = y, fill = group),
                       alpha = 0.15, show.legend = FALSE) +
          geom_path(data = hull_cl, aes(x = x, y = y, colour = group),
                    linewidth = 0.9)
      }
      if (input$fd_show_pts) {
        sub_df <- sc[!is.na(sc[[grp]]) & sc[[grp]] == g_nm, ]
        g <- g + geom_point(data = sub_df,
                            aes(x = .data[[pcx_nm]], y = .data[[pcy_nm]],
                                colour = .data[[grp]]),
                            alpha = 0.4, size = 0.8)
      }
    }
    g + scale_colour_manual(values = pal) + scale_fill_manual(values = pal)
  })

  fd_results <- reactive({
    sc  <- fd_scores_all()
    req(input$fd_groups_sel)
    grp <- input$fd_group
    sel <- input$fd_groups_sel
    pcx <- paste0("PC", input$fd_pc_x)
    pcy <- paste0("PC", input$fd_pc_y)
    lapply(sel, function(g_nm) {
      sub <- sc[!is.na(sc[[grp]]) & sc[[grp]] == g_nm, c(pcx, pcy)]
      res <- compute_fd(sub); res$group <- g_nm; res
    })
  })

  output$fd_plot  <- renderPlot({ fd_ggplot() }, res = 130)
  output$fd_table <- renderDT({
    res <- fd_results()
    df  <- data.frame(
      Group = sapply(res, `[[`, "group"),
      N     = sapply(res, `[[`, "n"),
      FRic  = sapply(res, `[[`, "FRic"),
      FDis  = sapply(res, `[[`, "FDis"))
    datatable(df, rownames = FALSE, options = list(pageLength = 15, dom = "t"),
      colnames = c("Group","N species","FRic (hull area)","FDis (mean dist. centroid)")) %>%
      formatRound(c("FRic","FDis"), 4)
  })
  output$dl_fd_plot <- downloadHandler(
    filename = "FishMorphSpace_FD.pdf",
    content  = function(f) ggsave(f, fd_ggplot(), width = 9, height = 8, device = "pdf"))
  output$dl_fd_table <- downloadHandler(
    filename = "FishMorphSpace_FD.csv",
    content  = function(f) {
      res <- fd_results()
      write.csv(data.frame(
        Group = sapply(res,`[[`,"group"), N = sapply(res,`[[`,"n"),
        FRic  = sapply(res,`[[`,"FRic"), FDis = sapply(res,`[[`,"FDis")),
        f, row.names = FALSE)
    })

  # ── Trait distributions ────────────────────────────────────────────────────
  dist_ggplot <- reactive({
    tr  <- input$dist_trait
    grp <- input$dist_group
    df  <- FISH[!is.na(FISH[[grp]]), ]
    if (grp == "IUCN") df[[grp]] <- factor(df[[grp]], levels = IUCN_LEVELS)
    else df[[grp]] <- factor(df[[grp]])
    pal <- if (grp == "IUCN")  IUCN_COLS  else
           if (grp == "Order") ORDER_COLS else
           setNames(colorRampPalette(brewer.pal(9,"Set3"))(nlevels(df[[grp]])),
                    levels(df[[grp]]))
    g <- ggplot(df, aes(x = .data[[tr]], fill = .data[[grp]], colour = .data[[grp]])) +
      geom_density(alpha = 0.25, linewidth = 0.8) +
      scale_fill_manual(values = pal) +
      scale_colour_manual(values = pal) +
      theme_bw(base_size = 13) +
      theme(panel.grid.minor = element_blank()) +
      labs(x = TRAIT_NAMES[tr], y = "Density", fill = grp, colour = grp)
    if (input$dist_log) g <- g + scale_x_log10()
    g
  })
  output$dist_plot     <- renderPlotly({ ggplotly(dist_ggplot()) })
  output$trait_def_box <- renderUI({
    div(class = "alert alert-info mt-2",
        tags$strong(TRAIT_NAMES[input$dist_trait]), tags$br(),
        TRAIT_DESC[input$dist_trait])
  })
  output$dl_dist <- downloadHandler(
    filename = paste0("FishMorphSpace_", input$dist_trait, ".pdf"),
    content  = function(f) ggsave(f, dist_ggplot(), width = 9, height = 6, device = "pdf"))

  # ── About: trait table ─────────────────────────────────────────────────────
  output$trait_table <- renderDT({
    df <- data.frame(Code = TRAIT_COLS, Name = unname(TRAIT_NAMES),
                     Meaning = unname(TRAIT_DESC), stringsAsFactors = FALSE)
    datatable(df, rownames = FALSE, options = list(pageLength = 10, dom = "t"),
      colnames = c("Code","Full name","Ecological meaning")) %>%
      formatStyle("Code", fontWeight = "bold", fontFamily = "monospace")
  })
}

shinyApp(ui, server)
