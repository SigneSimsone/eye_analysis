# Eye tracking analysis
# Input:  full_exports/ (one XLSX per recording)
#         stimuli/      (PNG images)
# Output: output/scanpaths/          (per participant x condition)
#         output/scanpaths_combined/ (all participants x condition)


# 1. Packages

# Set the folder where R packages are stored
user_lib <- "C:/Users/signe/R/win-library/4.5"
.libPaths(c(user_lib, .libPaths()))

# List the packages needed for this script
required <- c("tidyverse", "viridis", "png", "readxl", "scales")
# Install any packages that are not yet installed
new_pkg  <- required[!(required %in% rownames(installed.packages()))]
if (length(new_pkg)) install.packages(new_pkg, lib = user_lib,
                                      repos = "https://cloud.r-project.org")
lapply(required, library, character.only = TRUE)

# 2. Configuration

# Folder paths for input data and output images
DATA_DIR <- "full_exports"
IMG_DIR  <- "stimuli"
OUT_SCAN_DUR <- "output/scanpaths_duration"
OUT_COMB_DUR <- "output/scanpaths_combined_duration"
# Create the output folders if they do not exist yet
dir.create(OUT_SCAN_DUR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_COMB_DUR, recursive = TRUE, showWarnings = FALSE)

# Full screen size in pixels (Tobii recording resolution)
SCREEN_W <- 1920
SCREEN_H <- 1080

# Each stimulus image was displayed at a specific position on the screen
# offset_x/y = how many pixels from the left/top edge the image started
# disp_w/h   = width and height of the displayed image in pixels
# These values come from the Tobii Event parameters (W, H, X, Y)
IMAGE_LAYOUT <- tribble(
  ~image,             ~offset_x, ~offset_y, ~disp_w, ~disp_h,
  "CircleBig",             600,         0,     720,    1080,
  "CircleSmall",           600,         0,     720,    1080,
  "CircleTop",             600,         0,     720,    1080,
  "CircleBottom",          600,         0,     720,    1080,
  "CirclesVertical",       600,         0,     720,    1080,
  "VisualSearchTop",       555,         0,     810,    1080,
  "VisualSearchMid",       555,         0,     810,    1080,
  "VisualSearchBottom",    555,         0,     810,    1080
)

# Blank screen covers the full display — no offset needed
BLANK_W   <- 1920
BLANK_H   <- 1080
BLANK_IMG <- file.path(IMG_DIR, "blank.png")


# 3. Load all recordings
message("Loading recordings")
# Find all Excel export files in the data folder
files <- list.files(DATA_DIR, pattern = "\\.xlsx$", full.names = TRUE)
message(length(files), " files found")

# Read every file and stack them all into one big table
# All columns are read as text first so nothing gets misinterpreted
raw <- map_dfr(files, function(f) {
  message("  ", basename(f))
  read_excel(f, col_types = "text")
})
message(nrow(raw), " rows total")

# 4. Rename and convert

# Rename the long Tobii column names to shorter ones and convert numeric columns
# from text to actual numbers so calculations can be done with them
df <- raw |>
  rename(
    rec_ts       = `Recording timestamp`,
    participant  = `Participant name`,
    event        = `Event`,
    event_val    = `Event value`,
    event_params = `Event parameters`,
    gaze_x       = `Gaze point X`,
    gaze_y       = `Gaze point Y`,
    fix_x        = `Fixation point X`,
    fix_y        = `Fixation point Y`,
    eye_move     = `Eye movement type`,
    fix_idx      = `Eye movement type index`,
    eye_dur      = `Eye movement event duration`
  ) |>
  mutate(
    rec_ts  = as.numeric(rec_ts),
    gaze_x  = as.numeric(gaze_x),
    gaze_y  = as.numeric(gaze_y),
    fix_x   = as.numeric(fix_x),
    fix_y   = as.numeric(fix_y),
    fix_idx = as.integer(fix_idx),
    eye_dur = as.numeric(eye_dur)
  )

# 5. Parse events

# Keep only rows that contain event markers (e.g. when a stimulus started or stopped)
events <- df |>
  filter(!is.na(event), event != "") |>
  select(participant, rec_ts, event, event_val, event_params) |>
  arrange(participant, rec_ts)

# Extract audio events: AudioStart records which frequency began, AudioEnd clears it
audio_events <- events |>
  filter(event %in% c("AudioStart", "AudioEnd")) |>
  mutate(
    audio_freq   = str_extract(event_val, "^[^,]+") |> str_trim(),
    # When audio starts, store the frequency; when it ends, store NA
    audio_active = if_else(event == "AudioStart", audio_freq, NA_character_)
  ) |>
  select(participant, rec_ts, audio_active)

# Extract image events: ImageStart records which image appeared, ImageEnd clears it
image_events <- events |>
  filter(event %in% c("ImageStart", "ImageEnd")) |>
  mutate(
    image_name   = str_extract(event_val, "^[^,]+") |> str_trim(),
    # When image starts, store the name; when it ends, store NA
    image_active = if_else(event == "ImageStart", image_name, NA_character_)
  ) |>
  select(participant, rec_ts, image_active)

# 6. Forward-fill events onto gaze rows

# For each gaze sample - need to know which image and audio were active at that moment
# This function does that by merging the event timestamps with the gaze timestamps,
# sorting everything by time, and then carrying the last known value forward
# until it changes (forward-fill)
join_ff <- function(gaze_df, evt_df, val_col, new_col) {
  gaze_ts <- gaze_df |> distinct(participant, rec_ts) |>
    mutate(is_gaze = TRUE, .val = NA_character_)
  evt_tidy <- evt_df |>
    select(participant, rec_ts, .val = !!sym(val_col)) |>
    mutate(is_gaze = FALSE)

  bind_rows(gaze_ts, evt_tidy) |>
    arrange(participant, rec_ts, is_gaze) |>
    group_by(participant) |>
    fill(.val, .direction = "down") |>  # carry the last known value forward in time
    ungroup() |>
    filter(is_gaze) |>
    select(participant, rec_ts, !!new_col := .val)
}

message("Assigning audio to gaze rows...")
audio_ff <- join_ff(df, audio_events, "audio_active", "audio")

message("Assigning image to gaze rows...")
image_ff <- join_ff(df, image_events, "image_active", "image")

# 7. Build main gaze dataset

gaze <- df |>
  # Attach the audio and image labels to every gaze row
  left_join(audio_ff, by = c("participant", "rec_ts"),
            relationship = "many-to-one") |>
  left_join(image_ff, by = c("participant", "rec_ts"),
            relationship = "many-to-one") |>
  # Attach the image layout info (offset and display size)
  left_join(IMAGE_LAYOUT, by = "image") |>
  # Convert screen coordinates to image-relative coordinates
  # (subtract the image's top-left offset so (0,0) is the top-left of the image)
  mutate(
    img_gaze_x = gaze_x - offset_x,
    img_gaze_y = gaze_y - offset_y,
    img_fix_x  = fix_x  - offset_x,
    img_fix_y  = fix_y  - offset_y
  ) |>
  # Keep only rows where the image and audio are known, the gaze is valid,
  # and the gaze point is actually inside the displayed image area
  filter(
    !is.na(image), !is.na(audio),
    !is.na(gaze_x), !is.na(gaze_y),
    img_gaze_x >= 0, img_gaze_x <= disp_w,
    img_gaze_y >= 0, img_gaze_y <= disp_h
  )

message("Gaze rows after filtering: ", nrow(gaze))
message("Conditions found:")
gaze |> count(image, audio) |> print(n = Inf)

# 8. Blank screen

# The blank screen is shown between stimuli as a baseline
# Need to find each blank period, figure out which audio tone played during it,
# and extract the gaze data for that window

message("Processing blank screen...")

# Find the start time of each blank screen episode for each participant
blank_starts <- events |>
  filter(event == "StimulusStart", event_val == "Blank") |>
  arrange(participant, rec_ts) |>
  group_by(participant) |>
  mutate(episode = row_number()) |>
  ungroup() |>
  select(participant, episode, start = rec_ts)

# Find the start time of every stimulus to use it as the end of the blank window
next_stim_starts <- events |>
  filter(event == "StimulusStart") |>
  select(participant, next_ts = rec_ts)

# The blank window ends when the next stimulus starts
blank_windows <- blank_starts |>
  left_join(next_stim_starts, by = "participant", relationship = "many-to-many") |>
  filter(next_ts > start) |>
  group_by(participant, episode) |>
  slice_min(next_ts, n = 1, with_ties = FALSE) |>  # keep only the very next stimulus start
  ungroup() |>
  rename(end = next_ts)

# Find which audio tone was playing during each blank window
# (look for an AudioStart event within 500 ms before the blank starts, up to the blank end)
blank_episode_audio <- events |>
  filter(event == "AudioStart") |>
  mutate(audio = str_extract(event_val, "^[^,]+") |> str_trim()) |>
  select(participant, audio_ts = rec_ts, audio) |>
  inner_join(blank_windows, by = "participant",
             relationship = "many-to-many") |>
  filter(audio_ts >= start - 500000, audio_ts <= end) |>
  group_by(participant, episode) |>
  slice_min(audio_ts, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(participant, episode, audio)

# Attach the audio label to each blank window
blank_windows <- blank_windows |>
  left_join(blank_episode_audio, by = c("participant", "episode"))

# Extract all gaze rows that fall within a blank window,
# and keep only those with valid gaze coordinates on screen
blank_gaze <- df |>
  inner_join(blank_windows, by = "participant",
             relationship = "many-to-many") |>
  filter(rec_ts >= start, rec_ts <= end) |>
  mutate(
    img_gaze_x = gaze_x,
    img_gaze_y = gaze_y,
    img_fix_x  = fix_x,
    img_fix_y  = fix_y
  ) |>
  filter(
    !is.na(audio),
    !is.na(gaze_x), !is.na(gaze_y),
    gaze_x >= 0, gaze_x <= BLANK_W,
    gaze_y >= 0, gaze_y <= BLANK_H
  )

message("Blank rows: ", nrow(blank_gaze))

# 9. Fixation extractor

# Groups raw gaze samples into individual fixations
# Consecutive samples at the same screen position are treated as one fixation
# Returns one row per fixation with its average position, duration, and order number
get_fixations <- function(df, x_col = "img_fix_x", y_col = "img_fix_y") {
  df |>
    filter(eye_move == "Fixation",
           !is.na(.data[[x_col]]), !is.na(.data[[y_col]])) |>
    arrange(rec_ts) |>
    mutate(
      # Assign a new fixation ID each time the position changes
      fix_event = cumsum(
        row_number() == 1 |
        round(.data[[x_col]]) != round(lag(.data[[x_col]])) |
        round(.data[[y_col]]) != round(lag(.data[[y_col]]))
      )
    ) |>
    group_by(fix_event) |>
    summarise(
      img_fix_x = mean(.data[[x_col]], na.rm = TRUE),
      img_fix_y = mean(.data[[y_col]], na.rm = TRUE),
      duration  = (max(rec_ts) - min(rec_ts)) / 1000,  # convert microseconds to milliseconds
      rec_ts    = first(rec_ts),
      .groups   = "drop"
    ) |>
    arrange(rec_ts) |>
    mutate(fix_nr = row_number())  # number fixations in chronological order
}

# 10. Scanpath functions (duration-weighted)

# Build a sorted list of all participants and assign each a unique colour
# so the same participant always gets the same colour in every plot
all_parts <- unique(c(gaze$participant, blank_gaze$participant))
all_parts <- all_parts[order(as.integer(stringr::str_extract(all_parts, "\\d+")))]
PART_COLORS <- setNames(scales::hue_pal()(length(all_parts)), all_parts)

# Draws a scanpath plot for a single participant in one condition
# Circles represent fixations — bigger circle = longer fixation duration
# Numbers inside the circles show the fixation order
# Lines connect fixations in the order they happened (the scanpath)
plot_scanpath_dur <- function(fix_data, participant_name, img_name, audio_name,
                              disp_w, disp_h, bg_path = NULL) {
  if (nrow(fix_data) < 1) return(NULL)

  # Load the stimulus image as a background layer if the file exists
  bg_layer <- NULL
  if (!is.null(bg_path) && file.exists(bg_path)) {
    img_obj  <- png::readPNG(bg_path)
    bg_layer <- annotation_raster(img_obj, xmin = 0, xmax = disp_w,
                                           ymin = 0, ymax = disp_h)
  }

  n_fix     <- nrow(fix_data)
  fix_label <- if (n_fix == 1) "  [1 fixation]" else ""

  p <- ggplot(fix_data, aes(x = img_fix_x, y = img_fix_y)) + bg_layer
  # Draw the path line only if there is more than one fixation
  if (n_fix > 1) p <- p + geom_path(aes(group = 1), color = "steelblue",
                                     linewidth = 0.7, alpha = 0.7)
  p +
    geom_point(aes(size = duration, color = fix_nr), alpha = 0.75) +
    geom_text(aes(label = fix_nr), color = "white", size = 3,
              fontface = "bold") +
    scale_color_viridis(option = "plasma", name = "Order") +
    scale_size_continuous(name = "Duration (ms)", range = c(4, 22)) +
    scale_x_continuous(limits = c(0, disp_w), expand = c(0, 0)) +
    scale_y_reverse(limits = c(disp_h, 0), expand = c(0, 0)) +  # y-axis is flipped because screen coordinates start at the top
    coord_fixed() +
    labs(title    = paste0("Scanpath - ", participant_name,
                           "\n", img_name, " | ", audio_name, fix_label),
         x = "X (px)", y = "Y (px)") +
    theme_minimal(base_size = 11) +
    theme(panel.grid = element_blank())
}

# Draws a combined scanpath plot with all participants overlaid in one image
# Each participant gets a different colour so their paths can be told apart
plot_combined_dur <- function(all_fix, img_name, audio_name,
                              disp_w, disp_h, bg_path = NULL) {
  if (nrow(all_fix) == 0) return(NULL)

  bg_layer <- NULL
  if (!is.null(bg_path) && file.exists(bg_path)) {
    img_obj  <- png::readPNG(bg_path)
    bg_layer <- annotation_raster(img_obj, xmin = 0, xmax = disp_w,
                                           ymin = 0, ymax = disp_h)
  }

  n_parts <- n_distinct(all_fix$participant)
  # Ensure participant colours are always consistent with the global colour map
  all_fix <- all_fix |>
    mutate(participant = factor(participant, levels = names(PART_COLORS)))

  ggplot(all_fix, aes(x = img_fix_x, y = img_fix_y,
                      color = participant, group = participant)) +
    bg_layer +
    geom_path(linewidth = 0.6, alpha = 0.55) +
    geom_point(aes(size = duration), alpha = 0.75) +
    geom_text(aes(label = fix_nr), color = "white", size = 2,
              fontface = "bold") +
    scale_color_manual(values = PART_COLORS, name = "Participant") +
    scale_size_continuous(name = "Duration (ms)", range = c(2, 18)) +
    scale_x_continuous(limits = c(0, disp_w), expand = c(0, 0)) +
    scale_y_reverse(limits = c(disp_h, 0), expand = c(0, 0)) +
    coord_fixed() +
    labs(title = paste0(img_name, " | ", audio_name,
                        "  (", n_parts, " participants)"),
         x = "X (px)", y = "Y (px)") +
    theme_minimal(base_size = 11) +
    theme(panel.grid = element_blank(), legend.position = "right")
}

# 11. Build conditions list

# Get the list of all unique image x audio combinations found in the data
# and attach the display size info needed for plotting
conditions <- gaze |>
  distinct(image, audio) |>
  left_join(IMAGE_LAYOUT, by = "image") |>
  arrange(image, audio)

# Sort participants by number so plots are generated in order (Participant1, 2, 3 ...)
participants <- unique(gaze$participant)
participants <- participants[order(as.integer(stringr::str_extract(participants, "\\d+")))]

# 12. Generate: per-participant scanpaths

message("\nGenerating per-participant scanpaths...")

# Loop through every participant and every condition, extract fixations, and save a plot
for (part in participants) {
  safe_part <- gsub("[^A-Za-z0-9_-]", "_", part)  # make name safe for file system

  for (i in seq_len(nrow(conditions))) {
    img_name  <- conditions$image[i]
    aud_name  <- conditions$audio[i]
    dw        <- conditions$disp_w[i]
    dh        <- conditions$disp_h[i]
    bg_file   <- file.path(IMG_DIR, paste0(img_name, ".png"))
    safe_name <- paste0(img_name, "_", gsub("hz", "Hz", aud_name))

    # Get this participant's gaze data for this condition
    sub <- gaze |> filter(participant == part, image == img_name,
                           audio == aud_name)
    if (nrow(sub) == 0) next  # skip if no data for this combination

    fix_data <- get_fixations(sub)
    if (nrow(fix_data) < 1) next

    p <- plot_scanpath_dur(fix_data, part, img_name, aud_name, dw, dh, bg_file)
    if (is.null(p)) next

    ggsave(file.path(OUT_SCAN_DUR,
                     paste0("scanpath_", safe_part, "_", safe_name, ".png")),
           p, width = 9, height = 7, dpi = 150)
  }

  # Also generate blank screen scanpaths for each audio condition
  for (aud in unique(blank_gaze$audio)) {
    sub <- blank_gaze |>
      filter(participant == part, audio == aud,
             eye_move == "Fixation",
             !is.na(img_fix_x), !is.na(img_fix_y))
    if (nrow(sub) == 0) next

    fix_data <- get_fixations(sub)
    if (nrow(fix_data) < 1) next

    safe_aud  <- gsub("hz", "Hz", aud)
    n_fix     <- nrow(fix_data)
    fix_label <- if (n_fix == 1) "  [1 fixation]" else ""

    p <- plot_scanpath_dur(fix_data, part, "Blank", aud,
                           BLANK_W, BLANK_H, BLANK_IMG)
    if (is.null(p)) next

    ggsave(file.path(OUT_SCAN_DUR, paste0("scanpath_", safe_part,
                                          "_Blank_", safe_aud, ".png")),
           p, width = 12, height = 7, dpi = 150)
  }

  message("  ", part)
}

# Generate combined scanpath plots — all participants shown in one image per condition
message("\nGenerating combined scanpaths...")

for (i in seq_len(nrow(conditions))) {
  img_name  <- conditions$image[i]
  aud_name  <- conditions$audio[i]
  dw        <- conditions$disp_w[i]
  dh        <- conditions$disp_h[i]
  bg_file   <- file.path(IMG_DIR, paste0(img_name, ".png"))
  safe_name <- paste0(img_name, "_", gsub("hz", "Hz", aud_name))

  # Get fixations for all participants in this condition
  all_fix <- gaze |>
    filter(image == img_name, audio == aud_name) |>
    group_by(participant) |>
    group_modify(~ get_fixations(.x)) |>
    ungroup()

  p <- plot_combined_dur(all_fix, img_name, aud_name, dw, dh, bg_file)
  if (is.null(p)) next

  ggsave(file.path(OUT_COMB_DUR, paste0("combined_", safe_name, ".png")),
         p, width = 10, height = 8, dpi = 150)
  message("  ", safe_name)
}

# Also generate combined blank screen plots for each audio condition
for (aud in sort(unique(blank_gaze$audio))) {
  safe_aud <- gsub("hz", "Hz", aud)

  all_fix <- blank_gaze |>
    filter(audio == aud) |>
    group_by(participant) |>
    group_modify(~ get_fixations(.x)) |>
    ungroup()

  p <- plot_combined_dur(all_fix, "Blank", aud, BLANK_W, BLANK_H, BLANK_IMG)
  if (is.null(p)) next

  ggsave(file.path(OUT_COMB_DUR, paste0("combined_Blank_", safe_aud, ".png")),
         p, width = 12, height = 7, dpi = 150)
  message("  Blank_", safe_aud)
}

# 16. Save combined dataset

# Combine the stimulus gaze data and the blank screen gaze data into one table
# and save it as an RDS file so analysis.r and stats.r can load it quickly
message("\nSaving data...")
dir.create("output", showWarnings = FALSE)

# Prepare blank gaze rows with the same column structure as the stimulus gaze rows
blank_export <- blank_gaze |>
  mutate(image = "Blank", disp_w = BLANK_W, disp_h = BLANK_H,
         offset_x = 0L, offset_y = 0L) |>
  select(participant, rec_ts, image, audio,
         gaze_x, gaze_y, fix_x, fix_y,
         img_gaze_x, img_gaze_y, img_fix_x, img_fix_y,
         eye_move, disp_w, disp_h, offset_x, offset_y)

rdata <- bind_rows(
  gaze |> select(participant, rec_ts, image, audio,
                 gaze_x, gaze_y, fix_x, fix_y,
                 img_gaze_x, img_gaze_y, img_fix_x, img_fix_y,
                 eye_move, disp_w, disp_h, offset_x, offset_y),
  blank_export
)

saveRDS(rdata, "output/rdata.rds")
message("Saved: output/rdata.rds")
message("\nDone!")
message("  Per-participant scanpaths: ", OUT_SCAN_DUR)
message("  Combined scanpaths:        ", OUT_COMB_DUR)
message("\nFilter example:")
message("  rdata |> filter(image == 'CircleBig', audio == '200hz')")
