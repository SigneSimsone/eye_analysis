# Eye tracking — numerical metrics - output/results.xlsx

# Set the folder where R packages are stored
lib <- "C:/Users/signe/R/win-library/4.5"
.libPaths(c(lib, .libPaths()))

# List the packages needed for this script
req <- c("tidyverse", "readxl", "openxlsx")
# Install any packages that are not yet installed
new <- req[!(req %in% rownames(installed.packages()))]
if (length(new)) install.packages(new, lib = lib, repos = "https://cloud.r-project.org")
# Load all packages quietly (without printing startup messages)
suppressPackageStartupMessages(lapply(req, library, character.only = TRUE))

# 1. Helper functions

# Reads one TOI export file and matches each row to its image and audio condition
# using the design table (which maps row numbers to image/audio labels)
load_toi <- function(toi_file, design_file, fixed_image = NULL) {
  design <- read_excel(design_file) |>
    mutate(row_number = as.character(Item))
  # If the design table has an Image column, use it; otherwise use the fixed image name passed in
  if ("Image" %in% names(design)) {
    design <- design |> select(row_number, image = Image, audio = Audio)
  } else {
    design <- design |> mutate(image = fixed_image) |>
      select(row_number, image, audio = Audio)
  }

  # Read the TSV file, keep only the columns that are needed, and rename them to shorter names
  read_tsv(toi_file, col_types = cols(.default = "c"), show_col_types = FALSE) |>
    select(participant          = Participant,
           row_number           = Row_number,
           n_fix                = Number_of_whole_fixations,
           total_fix_dur_ms     = Total_duration_of_whole_fixations,
           dur_mean_ms          = Average_duration_of_whole_fixations,
           first_fix_dur_ms     = Duration_of_first_whole_fixation,
           n_saccades           = Number_of_saccades,
           time_to_first_sac_ms = Time_to_first_saccade,
           sac_amp_mean_deg     = `Average_amplitude_of_saccades`,
           first_sac_amp_deg    = Amplitude_of_first_saccade,
           pupil_diam_mean      = `Average_whole-fixation_pupil_diameter`) |>
    # Numbers in the TSV use commas as decimal separators — convert them to dots so R can read them
    mutate(across(c(-participant, -row_number),
                  ~ as.numeric(gsub(",", ".", .x)))) |>
    # Add the image and audio labels from the design table
    left_join(design, by = "row_number") |>
    select(participant, image, audio,
           n_fix, total_fix_dur_ms, dur_mean_ms, first_fix_dur_ms,
           n_saccades, time_to_first_sac_ms,
           sac_amp_mean_deg, first_sac_amp_deg, pupil_diam_mean)
}

# Calculates the average of a set of angles in degrees
# Regular averaging does not work for angles (e.g. the average of 1 degree and 359 degrees should be 0 degrees, not 180 degrees),
# so this function uses trigonometry to get the correct result
circ_mean <- function(a) {
  r <- a[!is.na(a)] * pi / 180
  if (length(r) == 0) return(NA_real_)
  atan2(mean(sin(r)), mean(cos(r))) * 180 / pi
}

# Returns the most common value in a list of text values
mode_val <- function(x) {
  t <- sort(table(x[!is.na(x)]), decreasing = TRUE)
  if (length(t) == 0) NA_character_ else names(t)[1]
}

# 2. Load all TOI data (Row_number - design table for every file)
message("Loading TOI data")

# Load the blank screen TOI data (shown between stimuli as a baseline)
toi_blank <- load_toi("toi_exports/toi_blank.tsv",
                      "design_tables/Blank.xlsx", "Blank")

# Load the circle position task data (circle at top or bottom of screen)
toi_circle_pos <- bind_rows(
  load_toi("toi_exports/CircleTop.tsv",
           "design_tables/CirclePosition.xlsx"),
  load_toi("toi_exports/toi_CircleBottom.tsv",
           "design_tables/CirclePosition.xlsx")
)

# Load the circle size task data (big or small circle in the centre)
toi_circle_size <- bind_rows(
  load_toi("toi_exports/toi_CircleBig.tsv",
           "design_tables/CircleSize.xlsx"),
  load_toi("toi_exports/toi_CircleSmall.tsv",
           "design_tables/CircleSize.xlsx")
)

# Load the three-circles-vertical task data
toi_cv <- load_toi("toi_exports/toi_CirclesVertical.tsv",
                   "design_tables/CirclesVertical.xlsx")

# Read the visual search design table separately because all three visual search
# TOI files share the same design table
vs_design <- read_excel("design_tables/VisualSearch.xlsx") |>
  mutate(row_number = as.character(Item)) |>
  select(row_number, image = Image, audio = Audio)

# Load all three visual search TOI files (target at top, middle, or bottom),
# combine them into one table, and attach the image/audio labels
toi_vs <- bind_rows(lapply(
  c("toi_exports/toi_VisualSearchTop.tsv",
    "toi_exports/toi_VisualSearchMid.tsv",
    "toi_exports/toi_VisualSearchBottom.tsv"),
  function(f) {
    read_tsv(f, col_types = cols(.default = "c"), show_col_types = FALSE) |>
      select(participant       = Participant,
             row_number        = Row_number,
             n_fix             = Number_of_whole_fixations,
             dur_mean_ms       = Average_duration_of_whole_fixations,
             first_fix_dur_ms  = Duration_of_first_whole_fixation,
             sac_amp_mean_deg  = `Average_amplitude_of_saccades`,
             pupil_diam_mean   = `Average_whole-fixation_pupil_diameter`,
             rt_ms             = Duration_of_interval) |>  # interval duration = reaction time
      mutate(across(c(-participant, -row_number),
                    ~ as.numeric(gsub(",", ".", .x))))
  }
)) |>
  left_join(vs_design, by = "row_number") |>
  select(participant, image, audio,
         n_fix, dur_mean_ms, first_fix_dur_ms,
         sac_amp_mean_deg, pupil_diam_mean, rt_ms)


# 3. Load gaze data for position-dependent metrics
message("Loading gaze data")
# Load the pre-processed gaze data that was saved by prepare_data.r
rdata <- readRDS("output/rdata.rds")

# Groups raw gaze samples into individual fixations
# Consecutive samples at the same screen position are treated as one fixation
get_fix <- function(df) {
  df |>
    filter(eye_move == "Fixation", !is.na(img_fix_x), !is.na(img_fix_y)) |>
    arrange(rec_ts) |>
    # Assign a new fixation number each time the position changes
    mutate(fix_event = cumsum(
      row_number() == 1 |
      round(img_fix_x) != round(lag(img_fix_x)) |
      round(img_fix_y) != round(lag(img_fix_y))
    )) |>
    # Summarise each fixation: average position, duration, and start time
    group_by(fix_event) |>
    summarise(img_fix_x = mean(img_fix_x, na.rm = TRUE),
              img_fix_y = mean(img_fix_y, na.rm = TRUE),
              duration  = (max(rec_ts) - min(rec_ts)) / 1000,  # convert microseconds to ms
              rec_ts    = first(rec_ts),
              .groups   = "drop") |>
    arrange(rec_ts) |>
    mutate(fix_nr = row_number())  # number fixations in chronological order
}

message("Computing fixations...")
# Run get_fix for every participant x image x audio combination
fixations <- rdata |>
  group_by(participant, image, audio) |>
  group_modify(~ get_fix(.x)) |>
  ungroup()

# Calculate the average gaze position (x, y) per participant and condition
fix_pos <- fixations |>
  group_by(participant, image, audio) |>
  summarise(x_mean = mean(img_fix_x),
            y_mean = mean(img_fix_y),
            .groups = "drop")


# 4. per_part: TOI metrics + gaze positions, all conditions
# Combine all task TOI data into one table and attach the average gaze positions
per_part <- bind_rows(toi_blank, toi_circle_pos, toi_circle_size, toi_cv, toi_vs) |>
  left_join(fix_pos, by = c("participant", "image", "audio"))


# 5. SHEET 1: General summary — all 19 conditions
message("Sheet 1: General summary...")

# Calculate group-level means and SDs for the main eye-tracking metrics
# grouped by image and audio condition
sheet_general <- per_part |>
  group_by(image, audio) |>
  summarise(
    n_participants           = n(),
    n_fix_mean               = round(mean(n_fix,         na.rm = TRUE), 2),
    n_fix_sd                 = round(sd(n_fix,           na.rm = TRUE), 2),
    fix_duration_mean_ms     = round(mean(dur_mean_ms,   na.rm = TRUE), 1),
    fix_duration_sd_ms       = round(sd(dur_mean_ms,     na.rm = TRUE), 1),
    fix_x_mean_px            = round(mean(x_mean,        na.rm = TRUE), 1),
    fix_x_sd_px              = round(sd(x_mean,          na.rm = TRUE), 1),
    fix_y_mean_px            = round(mean(y_mean,        na.rm = TRUE), 1),
    fix_y_sd_px              = round(sd(y_mean,          na.rm = TRUE), 1),
    saccade_amp_mean_deg     = round(mean(sac_amp_mean_deg,  na.rm = TRUE), 2),
    saccade_amp_sd_deg       = round(sd(sac_amp_mean_deg,    na.rm = TRUE), 2),
    pupil_diam_sd            = round(sd(pupil_diam_mean,    na.rm = TRUE), 2),
    pupil_diam_mean          = round(mean(pupil_diam_mean, na.rm = TRUE), 2),
    .groups = "drop"
  ) |>
  arrange(image, audio)

# 6. SHEET 2: Blank screen
message("Sheet 2: Blank screen")

# Screen centre coordinates — participants fixate a cross here before each stimulus
CX <- 960; CY <- 540

# For each participant and audio condition during the blank screen period:
# calculate where they looked on average, where their first fixation landed,
# and the distance and direction of the first saccade away from the fixation cross
blank_pos <- fixations |>
  filter(image == "Blank") |>
  arrange(participant, audio, rec_ts) |>
  group_by(participant, audio) |>
  summarise(
    mean_x       = mean(img_fix_x),
    mean_y       = mean(img_fix_y),
    first_fix_x  = first(img_fix_x),
    first_fix_y  = first(img_fix_y),
    # Distance in pixels from the fixation cross to the first fixation
    first_sac_len = sqrt((first(img_fix_x) - CX)^2 +
                          (first(img_fix_y) - CY)^2),
    # Angle in degrees of the first saccade (0 degrees = right, 90 degrees = up)
    first_sac_dir = atan2(-(first(img_fix_y) - CY),
                            first(img_fix_x) - CX) * 180 / pi,
    .groups = "drop"
  )

# Combine TOI metrics with the gaze position data for the blank screen
blank_per_part <- toi_blank |>
  left_join(blank_pos, by = c("participant", "audio"))

# Summarise the blank screen metrics across all participants, grouped by audio condition
sheet_blank <- blank_per_part |>
  group_by(audio) |>
  summarise(
    n_participants                = n(),
    n_fix_mean                    = round(mean(n_fix,                na.rm = TRUE), 2),
    n_fix_sd                      = round(sd(n_fix,                  na.rm = TRUE), 2),
    total_fix_dur_mean_ms         = round(mean(total_fix_dur_ms,     na.rm = TRUE), 1),
    total_fix_dur_sd_ms           = round(sd(total_fix_dur_ms,       na.rm = TRUE), 1),
    fix_duration_mean_ms          = round(mean(dur_mean_ms,          na.rm = TRUE), 1),
    fix_duration_sd_ms            = round(sd(dur_mean_ms,            na.rm = TRUE), 1),
    first_fix_duration_mean_ms    = round(mean(first_fix_dur_ms,     na.rm = TRUE), 1),
    first_fix_duration_sd_ms      = round(sd(first_fix_dur_ms,       na.rm = TRUE), 1),
    n_saccades_mean               = round(mean(n_saccades,           na.rm = TRUE), 2),
    n_saccades_sd                 = round(sd(n_saccades,             na.rm = TRUE), 2),
    time_to_first_sac_mean_ms     = round(mean(time_to_first_sac_ms, na.rm = TRUE), 1),
    time_to_first_sac_sd_ms       = round(sd(time_to_first_sac_ms,   na.rm = TRUE), 1),
    mean_x_px                     = round(mean(mean_x),              1),
    mean_y_px                     = round(mean(mean_y),              1),
    first_fix_x_mean              = round(mean(first_fix_x),         1),
    first_fix_y_mean              = round(mean(first_fix_y),         1),
    first_sac_length_mean_px      = round(mean(first_sac_len),       1),
    first_sac_length_sd_px        = round(sd(first_sac_len),         1),
    first_sac_dir_mean_deg        = round(circ_mean(first_sac_dir),  1),
    pupil_diam_sd                 = round(sd(pupil_diam_mean,        na.rm = TRUE), 2),
    pupil_diam_mean               = round(mean(pupil_diam_mean,      na.rm = TRUE), 2),
    .groups = "drop"
  )


# 7. SHEET 3: CirclesVertical
message("Sheet 3: CirclesVertical")

# Label each fixation with which circle area it fell in (top, middle, or bottom)
# based on the y pixel coordinate of the fixation
cv_fix <- fixations |>
  filter(image == "CirclesVertical") |>
  mutate(circle = case_when(
    img_fix_y < 375 ~ "top",
    img_fix_y < 705 ~ "middle",
    TRUE            ~ "bottom"
  ))

# Calculate how long (in ms) each participant spent looking at each circle area
cv_time <- cv_fix |>
  group_by(participant, audio, circle) |>
  summarise(total_dur_ms = sum(duration, na.rm = TRUE), .groups = "drop") |>
  # Reshape so each circle area becomes its own column
  pivot_wider(names_from = circle, values_from = total_dur_ms,
              values_fill = 0, names_prefix = "time_ms_")

# Find which circle area each participant looked at first
cv_first <- cv_fix |>
  arrange(participant, audio, rec_ts) |>
  group_by(participant, audio) |>
  summarise(first_circle = first(circle), .groups = "drop")

# Record whether each participant looked at each circle area at all during the trial
cv_visited <- cv_fix |>
  group_by(participant, audio) |>
  summarise(
    visited_top    = any(circle == "top"),
    visited_middle = any(circle == "middle"),
    visited_bottom = any(circle == "bottom"),
    .groups = "drop"
  )

# Combine TOI metrics with the circle-area gaze data for each participant
cv_per_part <- toi_cv |>
  left_join(cv_time,    by = c("participant", "audio")) |>
  left_join(cv_first,   by = c("participant", "audio")) |>
  left_join(cv_visited, by = c("participant", "audio"))

# Summarise across participants, grouped by audio condition
sheet_cv <- cv_per_part |>
  group_by(audio) |>
  summarise(
    n_participants              = n(),
    n_fix_mean                  = round(mean(n_fix,                na.rm = TRUE), 2),
    n_fix_sd                    = round(sd(n_fix,                  na.rm = TRUE), 2),
    total_fix_dur_mean_ms       = round(mean(total_fix_dur_ms,     na.rm = TRUE), 1),
    total_fix_dur_sd_ms         = round(sd(total_fix_dur_ms,       na.rm = TRUE), 1),
    fix_duration_mean_ms        = round(mean(dur_mean_ms,          na.rm = TRUE), 1),
    fix_duration_sd_ms          = round(sd(dur_mean_ms,            na.rm = TRUE), 1),
    first_fix_dur_mean_ms       = round(mean(first_fix_dur_ms,     na.rm = TRUE), 1),
    first_fix_dur_sd_ms         = round(sd(first_fix_dur_ms,       na.rm = TRUE), 1),
    n_saccades_mean             = round(mean(n_saccades,           na.rm = TRUE), 2),
    n_saccades_sd               = round(sd(n_saccades,             na.rm = TRUE), 2),
    time_to_first_sac_mean_ms   = round(mean(time_to_first_sac_ms, na.rm = TRUE), 1),
    time_to_first_sac_sd_ms     = round(sd(time_to_first_sac_ms,   na.rm = TRUE), 1),
    first_sac_amp_mean_deg      = round(mean(first_sac_amp_deg,    na.rm = TRUE), 2),
    first_sac_amp_sd_deg        = round(sd(first_sac_amp_deg,      na.rm = TRUE), 2),
    # Which circle area was looked at first most often across participants
    first_circle_most_common    = mode_val(first_circle),
    # How many participants looked at each circle area at least once
    n_visited_top               = sum(visited_top,    na.rm = TRUE),
    n_visited_middle            = sum(visited_middle, na.rm = TRUE),
    n_visited_bottom            = sum(visited_bottom, na.rm = TRUE),
    # Average time spent looking at each circle area
    time_top_mean_ms            = round(mean(time_ms_top,    na.rm = TRUE), 1),
    time_middle_mean_ms         = round(mean(time_ms_middle, na.rm = TRUE), 1),
    time_bottom_mean_ms         = round(mean(time_ms_bottom, na.rm = TRUE), 1),
    pupil_diam_sd               = round(sd(pupil_diam_mean,   na.rm = TRUE), 2),
    pupil_diam_mean             = round(mean(pupil_diam_mean, na.rm = TRUE), 2),
    .groups = "drop"
  )


# 8. SHEET 4: CirclePosition — time to first fixation on the object
message("Sheet 4: Circle position congruence")

# Pixel coordinates and radius of the circle in each image
# Used to check whether a fixation landed on the circle
OBJ_POS <- tribble(
  ~image,         ~obj_x, ~obj_y, ~obj_r,
  "CircleTop",      360,    215,    140,
  "CircleBottom",   360,    870,    140
)

# Record when each trial started (earliest timestamp) for each participant and condition
cp_ranges <- rdata |>
  filter(image %in% c("CircleTop", "CircleBottom")) |>
  group_by(participant, image, audio) |>
  summarise(t_start = min(rec_ts), .groups = "drop")

# Mark each fixation as on the circle or not, using the distance from the circle centre
cp_fix <- fixations |>
  filter(image %in% c("CircleTop", "CircleBottom")) |>
  left_join(OBJ_POS, by = "image") |>
  mutate(on_obj = sqrt((img_fix_x - obj_x)^2 + (img_fix_y - obj_y)^2) <= obj_r)

# Find how long it took each participant to first look at the circle in each trial
time_to_obj <- cp_fix |>
  filter(on_obj) |>
  left_join(cp_ranges, by = c("participant", "image", "audio")) |>
  group_by(participant, image, audio) |>
  slice_min(rec_ts, n = 1, with_ties = FALSE) |>  # keep only the earliest on-circle fixation
  ungroup() |>
  mutate(time_to_obj_ms = (rec_ts - t_start) / 1000) |>
  select(participant, image, audio, time_to_obj_ms)

# Define which image + audio combinations are congruent (high tone = top, low tone = bottom)
CONGRUENCE <- tribble(
  ~image,          ~audio,   ~congruence,
  "CircleTop",    "4000hz", "Kongruents",
  "CircleBottom", "200hz",  "Kongruents",
  "CircleTop",    "200hz",  "Nekongruents",
  "CircleBottom", "4000hz", "Nekongruents"
)

# Summarise circle position metrics across participants, grouped by image and audio condition
sheet_cp <- toi_circle_pos |>
  left_join(fix_pos,      by = c("participant", "image", "audio")) |>
  left_join(time_to_obj,  by = c("participant", "image", "audio")) |>
  group_by(image, audio) |>
  summarise(
    n_participants                = n(),
    total_fix_dur_mean_ms         = round(mean(total_fix_dur_ms,     na.rm = TRUE), 1),
    total_fix_dur_sd_ms           = round(sd(total_fix_dur_ms,       na.rm = TRUE), 1),
    n_fix_mean                    = round(mean(n_fix,                na.rm = TRUE), 2),
    n_fix_sd                      = round(sd(n_fix,                  na.rm = TRUE), 2),
    n_saccades_mean               = round(mean(n_saccades,           na.rm = TRUE), 2),
    n_saccades_sd                 = round(sd(n_saccades,             na.rm = TRUE), 2),
    time_to_first_sac_mean_ms     = round(mean(time_to_first_sac_ms, na.rm = TRUE), 1),
    time_to_first_sac_sd_ms       = round(sd(time_to_first_sac_ms,   na.rm = TRUE), 1),
    time_to_first_fix_obj_mean_ms = round(mean(time_to_obj_ms,       na.rm = TRUE), 1),
    time_to_first_fix_obj_sd_ms   = round(sd(time_to_obj_ms,         na.rm = TRUE), 1),
    fix_duration_mean_ms          = round(mean(dur_mean_ms,          na.rm = TRUE), 1),
    fix_duration_sd_ms            = round(sd(dur_mean_ms,            na.rm = TRUE), 1),
    fix_x_mean_px                 = round(mean(x_mean,               na.rm = TRUE), 1),
    fix_y_mean_px                 = round(mean(y_mean,               na.rm = TRUE), 1),
    saccade_amp_mean_deg          = round(mean(sac_amp_mean_deg,     na.rm = TRUE), 2),
    first_sac_amp_mean_deg        = round(mean(first_sac_amp_deg,    na.rm = TRUE), 2),
    first_sac_amp_sd_deg          = round(sd(first_sac_amp_deg,      na.rm = TRUE), 2),
    pupil_diam_sd                 = round(sd(pupil_diam_mean,        na.rm = TRUE), 2),
    pupil_diam_mean               = round(mean(pupil_diam_mean,      na.rm = TRUE), 2),
    .groups = "drop"
  ) |>
  # Add a congruence label (congruent / incongruent) to each row
  left_join(CONGRUENCE, by = c("image", "audio")) |>
  select(image, audio, congruence, everything()) |>
  arrange(image, audio)

# 9. SHEET 5: CircleSize
message("Sheet 5: Circle size congruence")

# Summarise circle size metrics across participants, grouped by image and audio condition
sheet_cs <- toi_circle_size |>
  left_join(fix_pos, by = c("participant", "image", "audio")) |>
  group_by(image, audio) |>
  summarise(
    n_participants       = n(),
    n_fix_mean           = round(mean(n_fix,            na.rm = TRUE), 2),
    n_fix_sd             = round(sd(n_fix,              na.rm = TRUE), 2),
    fix_duration_mean_ms = round(mean(dur_mean_ms,      na.rm = TRUE), 1),
    fix_duration_sd_ms   = round(sd(dur_mean_ms,        na.rm = TRUE), 1),
    saccade_amp_mean_deg = round(mean(sac_amp_mean_deg, na.rm = TRUE), 2),
    saccade_amp_sd_deg   = round(sd(sac_amp_mean_deg,   na.rm = TRUE), 2),
    pupil_diam_sd        = round(sd(pupil_diam_mean,   na.rm = TRUE), 2),
    pupil_diam_mean      = round(mean(pupil_diam_mean, na.rm = TRUE), 2),
    .groups = "drop"
  ) |>
  arrange(image, audio)

# 10. SHEET 6: VisualSearch — RT, first fixation, target fixation
message("Sheet 6: VisualSearch")

# Pixel coordinates of the target C in each visual search image
VS_TARGETS <- tribble(
  ~image,               ~target_x, ~target_y,
  "VisualSearchTop",          405,        71,   # row 1, col 3
  "VisualSearchMid",          567,       495,   # row 4, col 4
  "VisualSearchBottom",       243,       919    # row 7, col 2
)
# A fixation is counted as "on the target" if it falls within this many pixels of the target centre
TARGET_RADIUS <- 100

# Record when each visual search trial started
vs_ranges <- rdata |>
  filter(grepl("VisualSearch", image)) |>
  group_by(participant, image, audio) |>
  summarise(t_start = min(rec_ts), .groups = "drop")

# Mark each fixation as on the target or not
vs_fix <- fixations |>
  filter(grepl("VisualSearch", image)) |>
  left_join(VS_TARGETS, by = "image") |>
  mutate(on_target = sqrt((img_fix_x - target_x)^2 +
                           (img_fix_y - target_y)^2) <= TARGET_RADIUS)

# Find how long it took each participant to first fixate the target C
time_to_target <- vs_fix |>
  filter(on_target) |>
  left_join(vs_ranges |> select(participant, image, audio, t_start),
            by = c("participant", "image", "audio")) |>
  group_by(participant, image, audio) |>
  slice_min(rec_ts, n = 1, with_ties = FALSE) |>  # keep only the earliest on-target fixation
  ungroup() |>
  mutate(time_to_target_ms = (rec_ts - t_start) / 1000) |>
  select(participant, image, audio, time_to_target_ms)

# Record where each participant looked first in each visual search trial
vs_first_fix <- fixations |>
  filter(grepl("VisualSearch", image)) |>
  arrange(participant, image, audio, fix_nr) |>
  group_by(participant, image, audio) |>
  summarise(first_fix_x = first(img_fix_x),
            first_fix_y = first(img_fix_y),
            .groups = "drop")

# Summarise visual search metrics across participants, grouped by image and audio condition
sheet_vs <- toi_vs |>
  left_join(time_to_target, by = c("participant", "image", "audio")) |>
  left_join(vs_first_fix,   by = c("participant", "image", "audio")) |>
  group_by(image, audio) |>
  summarise(
    n_participants           = n(),
    rt_mean_ms               = round(mean(rt_ms,              na.rm = TRUE), 1),
    rt_sd_ms                 = round(sd(rt_ms,                na.rm = TRUE), 1),
    rt_median_ms             = round(median(rt_ms,            na.rm = TRUE), 1),
    # How many participants pressed a key (i.e. had a recorded reaction time)
    n_with_rt                = sum(!is.na(rt_ms)),
    time_to_target_mean_ms   = round(mean(time_to_target_ms,  na.rm = TRUE), 1),
    time_to_target_sd_ms     = round(sd(time_to_target_ms,    na.rm = TRUE), 1),
    time_to_target_median_ms = round(median(time_to_target_ms, na.rm = TRUE), 1),
    # How many participants fixated the target C at least once
    n_found_target           = sum(!is.na(time_to_target_ms)),
    first_fix_x_mean         = round(mean(first_fix_x),       1),
    first_fix_y_mean         = round(mean(first_fix_y),       1),
    n_fix_mean               = round(mean(n_fix,              na.rm = TRUE), 2),
    fix_duration_mean_ms     = round(mean(dur_mean_ms,        na.rm = TRUE), 1),
    saccade_amp_mean_deg     = round(mean(sac_amp_mean_deg,   na.rm = TRUE), 2),
    pupil_diam_sd            = round(sd(pupil_diam_mean,   na.rm = TRUE), 2),
    pupil_diam_mean          = round(mean(pupil_diam_mean, na.rm = TRUE), 2),
    .groups = "drop"
  ) |>
  arrange(image, audio)

# 11. Export to Excel
message("Exporting to Excel")

# Create a new empty Excel workbook
wb <- createWorkbook()

# Helper function: adds a sheet to the workbook, writes the data,
# auto-fits column widths, and formats the header row in bold with a blue background
add_sheet <- function(wb, name, data) {
  addWorksheet(wb, name)
  writeData(wb, name, data)
  setColWidths(wb, name, cols = seq_len(ncol(data)), widths = "auto")
  hs <- createStyle(textDecoration = "bold", fgFill = "#D9E1F2", border = "Bottom")
  addStyle(wb, name, hs, rows = 1, cols = seq_len(ncol(data)), gridExpand = TRUE)
}

# Add each results table as its own sheet
add_sheet(wb, "General",         sheet_general)
add_sheet(wb, "Blank",           sheet_blank)
add_sheet(wb, "CirclesVertical", sheet_cv)
add_sheet(wb, "CirclePosition",  sheet_cp)
add_sheet(wb, "CircleSize",      sheet_cs)
add_sheet(wb, "VisualSearch",    sheet_vs)

# Save the workbook
saveWorkbook(wb, "output/results.xlsx", overwrite = TRUE)
message("Saved: output/results.xlsx")
