# Eye tracking — statistical tests - output/stats.xlsx
# Run independently from analysis.r
# Input: toi_exports/, design_tables/, output/rdata.rds
# Output: output/stats.xlsx
#   Friedman — Friedman test (3 audio conditions: Blank, CirclesVertical)
#   Wilcoxon — Wilcoxon signed-rank test with Bonferroni correction
#   Fisher — Fisher's test


# Set the folder where R packages are stored
lib <- "C:/Users/signe/R/win-library/4.5"
.libPaths(c(lib, .libPaths()))

# Install any missing packages and load them all
req <- c("tidyverse", "readxl", "openxlsx")
new <- req[!(req %in% rownames(installed.packages()))]
if (length(new)) install.packages(new, lib = lib, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(lapply(req, library, character.only = TRUE))

# 1. Load data
message("Loading TOI data")

# Reads one TOI export file and joins it with the design table to get image/audio labels
# (same function as in analysis.r)
load_toi <- function(toi_file, design_file, fixed_image = NULL) {
  design <- read_excel(design_file) |>
    mutate(row_number = as.character(Item))
  if ("Image" %in% names(design)) {
    design <- design |> select(row_number, image = Image, audio = Audio)
  } else {
    design <- design |> mutate(image = fixed_image) |>
      select(row_number, image, audio = Audio)
  }
  read_tsv(toi_file, col_types = cols(.default = "c"), show_col_types = FALSE) |>
    select(participant          = Participant,
           row_number           = Row_number,
           n_fix                = Number_of_whole_fixations,
           total_fix_dur_ms     = Total_duration_of_whole_fixations,
           dur_mean_ms          = Average_duration_of_whole_fixations,
           first_fix_dur_ms     = Duration_of_first_whole_fixation,
           n_saccades           = Number_of_saccades,
           time_to_first_sac_ms = Time_to_first_saccade,
           pupil_diam_mean      = `Average_whole-fixation_pupil_diameter`) |>
    # Numbers in the TSV use commas as decimal separators — convert to dots
    mutate(across(c(-participant, -row_number),
                  ~ as.numeric(gsub(",", ".", .x)))) |>
    left_join(design, by = "row_number") |>
    select(participant, image, audio,
           n_fix, total_fix_dur_ms, dur_mean_ms, first_fix_dur_ms,
           n_saccades, time_to_first_sac_ms, pupil_diam_mean)
}

# Load TOI data for each task
toi_blank <- load_toi("toi_exports/toi_blank.tsv",
                      "design_tables/Blank.xlsx", "Blank")

toi_cv <- load_toi("toi_exports/toi_CirclesVertical.tsv",
                   "design_tables/CirclesVertical.xlsx")

toi_cp <- bind_rows(
  load_toi("toi_exports/CircleTop.tsv",        "design_tables/CirclePosition.xlsx"),
  load_toi("toi_exports/toi_CircleBottom.tsv", "design_tables/CirclePosition.xlsx")
)

toi_cs <- bind_rows(
  load_toi("toi_exports/toi_CircleBig.tsv",   "design_tables/CircleSize.xlsx"),
  load_toi("toi_exports/toi_CircleSmall.tsv", "design_tables/CircleSize.xlsx")
)

# Load visual search TOI data (all three target positions share one design table)
vs_design <- read_excel("design_tables/VisualSearch.xlsx") |>
  mutate(row_number = as.character(Item)) |>
  select(row_number, image = Image, audio = Audio)

toi_vs <- bind_rows(lapply(
  c("toi_exports/toi_VisualSearchTop.tsv",
    "toi_exports/toi_VisualSearchMid.tsv",
    "toi_exports/toi_VisualSearchBottom.tsv"),
  function(f) {
    read_tsv(f, col_types = cols(.default = "c"), show_col_types = FALSE) |>
      select(participant = Participant,
             row_number  = Row_number,
             n_fix       = Number_of_whole_fixations,
             dur_mean_ms = Average_duration_of_whole_fixations,
             n_saccades  = Number_of_saccades,
             rt_ms       = Duration_of_interval) |>  # interval duration = reaction time
      mutate(across(c(-participant, -row_number),
                    ~ as.numeric(gsub(",", ".", .x))))
  }
)) |>
  left_join(vs_design, by = "row_number") |>
  select(participant, image, audio, n_fix, dur_mean_ms, n_saccades, rt_ms)


# 2. Gaze data — CirclesVertical regions, time_to_obj, time_to_target
message("Loading gaze data")

# Load the pre-processed gaze data saved by prepare_data.r
rdata <- readRDS("output/rdata.rds")

# Groups raw gaze samples into individual fixations
# (same logic as in analysis.r)
get_fix <- function(df) {
  df |>
    filter(eye_move == "Fixation", !is.na(img_fix_x), !is.na(img_fix_y)) |>
    arrange(rec_ts) |>
    mutate(fix_event = cumsum(
      row_number() == 1 |
      round(img_fix_x) != round(lag(img_fix_x)) |
      round(img_fix_y) != round(lag(img_fix_y))
    )) |>
    group_by(fix_event) |>
    summarise(img_fix_x = mean(img_fix_x, na.rm = TRUE),
              img_fix_y = mean(img_fix_y, na.rm = TRUE),
              duration  = (max(rec_ts) - min(rec_ts)) / 1000,  # microseconds to milliseconds
              rec_ts    = first(rec_ts), .groups = "drop") |>
    arrange(rec_ts) |>
    mutate(fix_nr = row_number())
}

# Compute fixations for every participant x image x audio combination
fixations <- rdata |>
  group_by(participant, image, audio) |>
  group_modify(~ get_fix(.x)) |>
  ungroup()

# CirclesVertical — label each fixation with which circle area it fell in
# and calculate how long each participant spent looking at each area
cv_fix <- fixations |>
  filter(image == "CirclesVertical") |>
  mutate(circle = case_when(
    img_fix_y < 375 ~ "top",
    img_fix_y < 705 ~ "middle",
    TRUE            ~ "bottom"
  ))

# Total time spent looking at each circle area per participant and audio condition
cv_time <- cv_fix |>
  group_by(participant, audio, circle) |>
  summarise(total_dur_ms = sum(duration, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = circle, values_from = total_dur_ms,
              values_fill = 0, names_prefix = "time_ms_")

# Whether each participant looked at the top or bottom circle at all
cv_visited <- cv_fix |>
  group_by(participant, audio) |>
  summarise(visited_top    = any(circle == "top"),
            visited_bottom = any(circle == "bottom"),
            .groups = "drop")

# Read the direction of the first saccade for each trial from the raw TSV
# and classify it as upward (angle > 180 degrees) or not
cv_sac_dir <- read_tsv("toi_exports/toi_CirclesVertical.tsv",
                       col_types = cols(.default = "c"), show_col_types = FALSE) |>
  mutate(row_number = Row_number,
         participant = Participant,
         first_sac_dir_deg = as.numeric(gsub(",", ".", Direction_of_first_saccade))) |>
  select(participant, row_number, first_sac_dir_deg) |>
  left_join(
    read_excel("design_tables/CirclesVertical.xlsx") |>
      mutate(row_number = as.character(Item)) |>
      select(row_number, audio = Audio),
    by = "row_number"
  ) |>
  mutate(first_sac_up = first_sac_dir_deg > 180) |>  # TRUE = first saccade went upward
  select(participant, audio, first_sac_up)

# Combine all CirclesVertical metrics into one per-participant table
cv_per_part <- toi_cv |>
  left_join(cv_time,    by = c("participant", "audio")) |>
  left_join(cv_visited, by = c("participant", "audio")) |>
  left_join(cv_sac_dir, by = c("participant", "audio"))

# CirclePosition — find how long it took each participant to first look at the circle

# Pixel coordinates and radius of the circle in each image
OBJ_POS <- tribble(
  ~image,         ~obj_x, ~obj_y, ~obj_r,
  "CircleTop",      360,    215,    140,
  "CircleBottom",   360,    870,    140
)

# Record when each trial started
cp_ranges <- rdata |>
  filter(image %in% c("CircleTop", "CircleBottom")) |>
  group_by(participant, image, audio) |>
  summarise(t_start = min(rec_ts), .groups = "drop")

# Find the first fixation that landed on the circle and calculate how many ms it took
time_to_obj <- fixations |>
  filter(image %in% c("CircleTop", "CircleBottom")) |>
  left_join(OBJ_POS, by = "image") |>
  mutate(on_obj = sqrt((img_fix_x - obj_x)^2 + (img_fix_y - obj_y)^2) <= obj_r) |>
  filter(on_obj) |>
  left_join(cp_ranges, by = c("participant", "image", "audio")) |>
  group_by(participant, image, audio) |>
  slice_min(rec_ts, n = 1, with_ties = FALSE) |>  # keep only the earliest on-circle fixation
  ungroup() |>
  mutate(time_to_obj_ms = (rec_ts - t_start) / 1000) |>
  select(participant, image, audio, time_to_obj_ms)

# Combine circle position TOI metrics with time-to-object
cp_per_part <- toi_cp |>
  left_join(time_to_obj, by = c("participant", "image", "audio"))

# VisualSearch — find how long it took each participant to first fixate the target C

# Pixel coordinates of the target C in each visual search image
VS_TARGETS <- tribble(
  ~image,               ~target_x, ~target_y,
  "VisualSearchTop",          405,        71,
  "VisualSearchMid",          567,       495,
  "VisualSearchBottom",       243,       919
)
# A fixation counts as "on target" if it falls within this many pixels of the target centre
TARGET_RADIUS <- 100

# Record when each visual search trial started
vs_ranges <- rdata |>
  filter(grepl("VisualSearch", image)) |>
  group_by(participant, image, audio) |>
  summarise(t_start = min(rec_ts), .groups = "drop")

# Find the first fixation on the target and calculate the time from trial start
time_to_target <- fixations |>
  filter(grepl("VisualSearch", image)) |>
  left_join(VS_TARGETS, by = "image") |>
  mutate(on_target = sqrt((img_fix_x - target_x)^2 +
                           (img_fix_y - target_y)^2) <= TARGET_RADIUS) |>
  filter(on_target) |>
  left_join(vs_ranges, by = c("participant", "image", "audio")) |>
  group_by(participant, image, audio) |>
  slice_min(rec_ts, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(time_to_target_ms = (rec_ts - t_start) / 1000) |>
  select(participant, image, audio, time_to_target_ms)

# Combine visual search TOI metrics with time-to-target
vs_per_part <- toi_vs |>
  left_join(time_to_target, by = c("participant", "image", "audio"))


# 3. Helper functions for tests

# Runs a Friedman test for one metric across three or more conditions
# The Friedman test is a non-parametric equivalent of a repeated-measures ANOVA —
# it checks whether the same participants scored differently across conditions
# without assuming the data is normally distributed
# Returns chi-square statistic, degrees of freedom, p-value, and Kendall's W (effect size)
run_friedman <- function(data, cond_col, metric) {
  mat <- data |>
    select(participant, all_of(c(cond_col, metric))) |>
    filter(!is.na(.data[[metric]])) |>
    pivot_wider(names_from  = all_of(cond_col),
                values_from = all_of(metric)) |>
    select(-participant) |>
    as.matrix()
  if (!is.matrix(mat) || nrow(mat) == 0 || ncol(mat) == 0) return(NULL)
  # Keep only participants who have data in all conditions
  comp <- mat[complete.cases(mat), , drop = FALSE]
  if (!is.matrix(comp) || nrow(comp) < 3 || ncol(comp) < 3) return(NULL)
  test <- friedman.test(comp)
  n <- nrow(comp); k <- ncol(comp)
  tibble(
    metric    = metric,
    chi2      = round(test$statistic, 3),
    df        = as.integer(test$parameter),
    p         = round(test$p.value, 4),
    kendall_W = round(test$statistic / (n * (k - 1)), 3),  # effect size (0–1)
    n         = n
  )
}

# Runs a paired Wilcoxon signed-rank test comparing two conditions for one metric
# This is the non-parametric equivalent of a paired t-test
# p_bonf_k = number of comparisons for Bonferroni correction (multiply p by this value)
# Returns W statistic, p-value, Bonferroni-adjusted p, effect size r, Cohen's d, and n
run_wilcoxon <- function(data, cond_col, metric, c1, c2, p_bonf_k = 1) {
  wide <- data |>
    filter(.data[[cond_col]] %in% c(c1, c2)) |>
    select(participant, all_of(c(cond_col, metric))) |>
    pivot_wider(names_from  = all_of(cond_col),
                values_from = all_of(metric))
  a <- wide[[c1]]; b <- wide[[c2]]
  # Keep only participants who have data in both conditions
  ok <- !is.na(a) & !is.na(b)
  a <- a[ok]; b <- b[ok]
  if (length(a) < 4) return(NULL)  # need at least 4 pairs to run the test
  test <- suppressWarnings(wilcox.test(a, b, paired = TRUE, exact = FALSE))
  z    <- abs(qnorm(test$p.value / 2))
  diff <- a - b
  d    <- mean(diff) / sd(diff)
  tibble(
    metric     = metric,
    comparison = paste(c1, "vs", c2),
    W          = test$statistic,
    p          = round(test$p.value, 4),
    p_adj      = round(pmin(test$p.value * p_bonf_k, 1), 4),  # Bonferroni-corrected p
    r          = round(z / sqrt(length(a)), 3),                # effect size r
    cohens_d   = round(abs(d), 3),
    n          = length(a)
  )
}

# Runs Fisher's exact test for a binary outcome across groups
# Used when the outcome is a yes/no variable (e.g. did the participant find the target?)
# and the groups are independent (different audio conditions)
run_fisher <- function(data, group_col, binary_col) {
  tbl <- table(data[[group_col]], data[[binary_col]])
  if (nrow(tbl) < 2 || ncol(tbl) < 2)
    return(tibble(test = "Fisher", metric = binary_col, comparison = NA_character_, p = NA_real_))
  res <- fisher.test(tbl, simulate.p.value = TRUE, B = 10000)  # simulation used because of small sample
  tibble(test = "Fisher", metric = binary_col, comparison = NA_character_,
         p = round(res$p.value, 4))
}

# Runs McNemar's test for a binary outcome measured under two paired conditions
# Used when the same participant is measured in both conditions (e.g. did first saccade go up?)
# p_bonf_k = number of comparisons for Bonferroni correction
run_mcnemar <- function(data, cond_col, binary_col, c1, c2, p_bonf_k = 1) {
  wide <- data |>
    filter(.data[[cond_col]] %in% c(c1, c2)) |>
    select(participant, all_of(c(cond_col, binary_col))) |>
    pivot_wider(names_from = all_of(cond_col), values_from = all_of(binary_col))
  tbl <- table(wide[[c1]], wide[[c2]])
  if (nrow(tbl) < 2 || ncol(tbl) < 2)
    return(tibble(test = "McNemar", metric = binary_col,
                  comparison = paste(c1, "vs", c2), p = NA_real_, p_adj = NA_real_))
  res <- mcnemar.test(tbl, correct = FALSE)
  tibble(test = "McNemar", metric = binary_col,
         comparison = paste(c1, "vs", c2),
         chi2 = round(res$statistic, 3),
         p    = round(res$p.value, 4),
         p_adj = round(pmin(res$p.value * p_bonf_k, 1), 4))
}

# Runs Cochran's Q test — the non-parametric equivalent of a repeated-measures test
# for binary outcomes across three or more conditions
# Checks whether the proportion of "yes" answers differs significantly across conditions
run_cochran_q <- function(data, cond_col, binary_col) {
  wide <- data |>
    select(participant, all_of(c(cond_col, binary_col))) |>
    pivot_wider(names_from = all_of(cond_col), values_from = all_of(binary_col)) |>
    select(-participant) |>
    mutate(across(everything(), as.integer))
  comp <- as.matrix(wide[complete.cases(wide), ])
  N <- nrow(comp); k <- ncol(comp)
  if (N < 3 || k < 2) return(NULL)
  # Calculate the Q statistic manually
  T_total <- sum(comp)
  denom <- k * T_total - sum(rowSums(comp)^2)
  if (denom == 0) return(NULL)
  Q <- (k - 1) * (k * sum(colSums(comp)^2) - T_total^2) / denom
  tibble(test = "Cochran_Q", metric = binary_col, comparison = "all_conditions",
         chi2 = round(Q, 3), p = round(pchisq(Q, df = k - 1, lower.tail = FALSE), 4))
}


# 4. Friedman tests
message("Friedman tests")

# Metrics to test for the blank screen (baseline) task
blank_m <- c("n_fix", "total_fix_dur_ms", "dur_mean_ms", "first_fix_dur_ms",
             "n_saccades", "time_to_first_sac_ms", "pupil_diam_mean")

# Metrics to test for the three-circles-vertical task
# (includes time spent in each circle area)
cv_m <- c("n_fix", "total_fix_dur_ms", "dur_mean_ms", "first_fix_dur_ms",
          "n_saccades", "time_to_first_sac_ms", "pupil_diam_mean",
          "time_ms_top", "time_ms_middle", "time_ms_bottom")

# Metrics to test for the visual search task
vs_m <- c("rt_ms", "time_to_target_ms", "n_fix", "dur_mean_ms")

# For visual search, combine image and audio into one condition label
# so all 5 conditions (Top_200hz, Top_4000hz, Mid_1000hz, ...) can be compared at once
vs_per_part_cond <- vs_per_part |>
  mutate(vs_cond = paste(image, audio, sep = "_"))

friedman_vs <- map_dfr(vs_m,
  ~ run_friedman(vs_per_part_cond, "vs_cond", .x)
) |> mutate(condition = "VisualSearch_5cond", .before = 1)

# Run Friedman tests for blank and CirclesVertical tasks across the 3 audio conditions,
# then combine all results into one table
friedman_results <- bind_rows(
  map_dfr(blank_m, ~ run_friedman(toi_blank,    "audio", .x)) |>
    mutate(condition = "Blank",           .before = 1),
  map_dfr(cv_m,    ~ run_friedman(cv_per_part,  "audio", .x)) |>
    mutate(condition = "CirclesVertical", .before = 1),
  friedman_vs
)


# 5. Wilcoxon tests
message("Wilcoxon tests")

# All possible pairs of audio conditions for post-hoc pairwise comparisons
audio_pairs <- list(c("200hz", "1000hz"), c("200hz", "4000hz"), c("1000hz", "4000hz"))

# Blank — pairwise post-hoc comparisons between all 3 audio conditions
# Bonferroni correction for 3 comparisons (multiply p by 3)
wil_blank <- map_dfr(blank_m, function(m)
  map_dfr(audio_pairs, ~ run_wilcoxon(toi_blank, "audio", m, .x[1], .x[2], 3))
) |> mutate(condition = "Blank", .before = 1)

# CirclesVertical — pairwise post-hoc comparisons between all 3 audio conditions
wil_cv <- map_dfr(cv_m, function(m)
  map_dfr(audio_pairs, ~ run_wilcoxon(cv_per_part, "audio", m, .x[1], .x[2], 3))
) |> mutate(condition = "CirclesVertical", .before = 1)

# CirclePosition — compare congruent vs incongruent conditions
# (congruent = high tone with top circle or low tone with bottom circle)
# Average metrics across top and bottom images so each participant has one value per congruence level
cp_cong <- cp_per_part |>
  mutate(congruence = case_when(
    image == "CircleTop"    & audio == "4000hz" ~ "Kongruents",
    image == "CircleBottom" & audio == "200hz"  ~ "Kongruents",
    TRUE                                         ~ "Nekongruents"
  )) |>
  group_by(participant, congruence) |>
  summarise(across(c(n_fix, total_fix_dur_ms, dur_mean_ms,
                     n_saccades, time_to_first_sac_ms, time_to_obj_ms),
                   mean, na.rm = TRUE), .groups = "drop")

wil_cp <- map_dfr(
  c("n_fix", "total_fix_dur_ms", "dur_mean_ms",
    "n_saccades", "time_to_first_sac_ms", "time_to_obj_ms"),
  ~ run_wilcoxon(cp_cong, "congruence", .x, "Kongruents", "Nekongruents")
) |> mutate(condition = "CirclePosition_kongruence", .before = 1)

# CirclePosition — congruent vs incongruent separately for the top and bottom image
cp_m <- c("n_fix", "total_fix_dur_ms", "dur_mean_ms",
           "n_saccades", "time_to_first_sac_ms", "time_to_obj_ms")

wil_cp_top <- map_dfr(cp_m,
  ~ run_wilcoxon(cp_per_part |> filter(image == "CircleTop"),
                 "audio", .x, "4000hz", "200hz")
) |> mutate(condition = "CircleTop_4000vs200", .before = 1)

wil_cp_bottom <- map_dfr(cp_m,
  ~ run_wilcoxon(cp_per_part |> filter(image == "CircleBottom"),
                 "audio", .x, "200hz", "4000hz")
) |> mutate(condition = "CircleBottom_200vs4000", .before = 1)

# CirclePosition — pairwise audio comparisons averaged across both images,
# with Bonferroni correction for 3 comparisons
cp_audio <- cp_per_part |>
  group_by(participant, audio) |>
  summarise(across(c(n_fix, total_fix_dur_ms, dur_mean_ms,
                     n_saccades, time_to_first_sac_ms, time_to_obj_ms),
                   mean, na.rm = TRUE), .groups = "drop")

wil_cp_audio <- map_dfr(
  c("n_fix", "total_fix_dur_ms", "dur_mean_ms",
    "n_saccades", "time_to_first_sac_ms", "time_to_obj_ms"),
  function(m)
    map_dfr(audio_pairs, ~ run_wilcoxon(cp_audio, "audio", m, .x[1], .x[2], 3))
) |> mutate(condition = "CirclePosition_audio", .before = 1)

# CircleSize — compare big circle vs small circle (averaged across audio conditions)
cs_avg <- toi_cs |>
  group_by(participant, image) |>
  summarise(across(c(n_fix, total_fix_dur_ms, dur_mean_ms, n_saccades),
                   mean, na.rm = TRUE), .groups = "drop")

wil_cs <- map_dfr(
  c("n_fix", "total_fix_dur_ms", "dur_mean_ms", "n_saccades"),
  ~ run_wilcoxon(cs_avg, "image", .x, "CircleBig", "CircleSmall")
) |> mutate(condition = "CircleSize_BigVsSmall", .before = 1)

cs_m <- c("n_fix", "total_fix_dur_ms", "dur_mean_ms", "n_saccades")

# CircleSize — congruent vs incongruent per image
# (CircleBig + 4000hz = congruent, CircleSmall + 200hz = congruent)
wil_cs_big <- map_dfr(cs_m,
  ~ run_wilcoxon(toi_cs |> filter(image == "CircleBig"),
                 "audio", .x, "4000hz", "200hz")
) |> mutate(condition = "CircleSize_Big_4000vs200", .before = 1)

wil_cs_small <- map_dfr(cs_m,
  ~ run_wilcoxon(toi_cs |> filter(image == "CircleSmall"),
                 "audio", .x, "200hz", "4000hz")
) |> mutate(condition = "CircleSize_Small_200vs4000", .before = 1)

# CircleSize — pairwise audio comparisons averaged across both images,
# with Bonferroni correction for 3 comparisons
cs_audio <- toi_cs |>
  group_by(participant, audio) |>
  summarise(across(all_of(cs_m), mean, na.rm = TRUE), .groups = "drop")

wil_cs_audio <- map_dfr(cs_m, function(m)
  map_dfr(audio_pairs, ~ run_wilcoxon(cs_audio, "audio", m, .x[1], .x[2], 3))
) |> mutate(condition = "CircleSize_audio", .before = 1)

# VisualSearch — compare congruent vs incongruent conditions
# (VisualSearchTop + 4000hz = congruent, VisualSearchBottom + 200hz = congruent)
# Use only the 200hz and 4000hz conditions for the congruence comparison
vs_cong <- vs_per_part |>
  filter(audio %in% c("200hz", "4000hz"),
         image %in% c("VisualSearchTop", "VisualSearchBottom")) |>
  mutate(congruence = case_when(
    image == "VisualSearchTop"    & audio == "4000hz" ~ "Kongruents",
    image == "VisualSearchBottom" & audio == "200hz"  ~ "Kongruents",
    TRUE                                               ~ "Nekongruents"
  )) |>
  group_by(participant, congruence) |>
  summarise(across(all_of(vs_m), mean, na.rm = TRUE), .groups = "drop")

wil_vs_cong <- map_dfr(vs_m,
  ~ run_wilcoxon(vs_cong, "congruence", .x, "Kongruents", "Nekongruents")
) |> mutate(condition = "VisualSearch_kongruence", .before = 1)

# VisualSearch — pairwise audio post-hoc comparisons for top and bottom images,
# with Bonferroni correction for 3 comparisons
wil_vs <- bind_rows(
  map_dfr(vs_m, function(m)
    map_dfr(audio_pairs, ~ run_wilcoxon(
      vs_per_part |> filter(image == "VisualSearchTop"),
      "audio", m, .x[1], .x[2], 3))) |>
    mutate(condition = "VisualSearchTop", .before = 1),
  map_dfr(vs_m, function(m)
    map_dfr(audio_pairs, ~ run_wilcoxon(
      vs_per_part |> filter(image == "VisualSearchBottom"),
      "audio", m, .x[1], .x[2], 3))) |>
    mutate(condition = "VisualSearchBottom", .before = 1)
)

# Combine all Wilcoxon test results into one table
wilcoxon_results <- bind_rows(wil_blank, wil_cv,
                              wil_cp, wil_cp_top, wil_cp_bottom, wil_cp_audio,
                              wil_cs, wil_cs_big, wil_cs_small, wil_cs_audio,
                              wil_vs_cong, wil_vs)


# 6. Fisher tests
message("Fisher tests")

# Create a binary column: did the participant fixate the target C at all?
vs_found <- vs_per_part |>
  mutate(found_target = !is.na(time_to_target_ms)) |>
  filter(audio %in% c("200hz", "4000hz"))  # only compare the two tonal conditions

fisher_results <- bind_rows(
  # Cochran's Q: did the direction of the first saccade (up vs not up) differ across all 3 audio conditions?
  run_cochran_q(cv_sac_dir, "audio", "first_sac_up") |>
    mutate(condition = "CirclesVertical", .before = 1),
  # McNemar post-hoc: pairwise comparisons of first saccade direction between audio conditions
  run_mcnemar(cv_sac_dir, "audio", "first_sac_up", "200hz", "4000hz",  3) |>
    mutate(condition = "CirclesVertical", .before = 1),
  run_mcnemar(cv_sac_dir, "audio", "first_sac_up", "200hz", "1000hz",  3) |>
    mutate(condition = "CirclesVertical", .before = 1),
  run_mcnemar(cv_sac_dir, "audio", "first_sac_up", "1000hz", "4000hz", 3) |>
    mutate(condition = "CirclesVertical", .before = 1),
  # Fisher: did audio condition affect whether participants looked at the top circle at all?
  run_fisher(cv_per_part, "audio", "visited_top") |>
    mutate(condition = "CirclesVertical", .before = 1),
  # Fisher: did audio condition affect whether participants looked at the bottom circle at all?
  run_fisher(cv_per_part, "audio", "visited_bottom") |>
    mutate(condition = "CirclesVertical", .before = 1),
  # Fisher: did audio condition affect whether participants found the target in visual search?
  run_fisher(vs_found |> filter(image == "VisualSearchTop"),
             "audio", "found_target") |>
    mutate(condition = "VisualSearchTop", .before = 1),
  run_fisher(vs_found |> filter(image == "VisualSearchBottom"),
             "audio", "found_target") |>
    mutate(condition = "VisualSearchBottom", .before = 1)
)


# 7. Export
message("Exporting output/stats.xlsx")

# Create a new empty Excel workbook
wb_s <- createWorkbook()

# Helper function: adds a sheet, writes data, auto-fits columns,
# formats the header row in bold with a blue background,
# and highlights rows where p < 0.05 in yellow
add_s <- function(wb, name, data) {
  addWorksheet(wb, name)
  writeData(wb, name, data)
  setColWidths(wb, name, cols = seq_len(ncol(data)), widths = "auto")
  hs <- createStyle(textDecoration = "bold", fgFill = "#D9E1F2", border = "Bottom")
  addStyle(wb, name, hs, rows = 1, cols = seq_len(ncol(data)), gridExpand = TRUE)
  # Find the p-value column and highlight significant results in yellow
  p_col <- which(names(data) == "p")
  if (length(p_col)) {
    sig_rows <- which(data$p < 0.05) + 1
    if (length(sig_rows))
      addStyle(wb, name, createStyle(fgFill = "#FFF2CC"),
               rows = sig_rows, cols = p_col, gridExpand = TRUE)
  }
}

# Write each set of test results to its own sheet
add_s(wb_s, "Friedman", friedman_results)
add_s(wb_s, "Wilcoxon", wilcoxon_results)
add_s(wb_s, "Fisher",   fisher_results)

# Save the workbook
saveWorkbook(wb_s, "output/stats.xlsx", overwrite = TRUE)
message("Saved: output/stats.xlsx")
