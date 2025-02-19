library(here)
library(fst)
library(data.table)

# Load response data ------------------------------------------------------

responses <- read_fst(here("data", "responses_long.fst"))
setDT(responses)
responses[, time_down := as.numeric(time_down)]
responses[, time_up := as.numeric(time_up)]


# Filter unusable responses -----------------------------------------------

# time_down has to be less than time_up on individual keypress
responses_to_keep <- responses[, .(keep = all(time_down < time_up)), by = response_id][keep == TRUE, .(response_id)]
responses <- merge(responses, responses_to_keep, by = "response_id")

# Within a response, time_down has to be increasing from one key-press to the next
responses_to_keep <- responses[, .(keep = all(diff(time_down) > 0)), by = response_id][keep == TRUE, .(response_id)]
responses <- merge(responses, responses_to_keep, by = "response_id")


# Keystroke dynamics features ---------------------------------------------

# First order features (see Nova et al)
responses[, duration := time_up - time_down, by = response_id] # Duration of this keypress (hold time, dwell time)
responses[, pp_latency := time_down - shift(time_down), by = response_id] # Latency between previous and this keypress (digraph time, inter-key delay)
responses[, rr_latency := time_up - shift(time_up), by = response_id] # Latency between previous and this release
responses[, rp_latency := time_down - shift(time_up), by = response_id] # Latency between previous release and this press (flight time)
responses[, pr_latency := time_up - shift(time_down), by = response_id] # Latency between previous press and this release

# Second order features

# Add previous key (or "START" if this is the first keypress of the response)
responses[, prev_key := shift(key_code, fill = NA_integer_), by = response_id]
responses[, bigram := paste("bigram", prev_key, key_code, sep = "_")]
responses[is.na(prev_key), bigram := NA_character_]




# Subset the responses ----------------------------------------------------

responses_full <- copy(responses)

# Only include sessions with at least 10 distinct bigrams
min_bigrams_per_session <- 10
sessions_to_keep <- responses[, .(n_unique_bigrams = uniqueN(bigram)), by = session_id][n_unique_bigrams >= min_bigrams_per_session, .(session_id)]
responses <- merge(responses, sessions_to_keep, by = "session_id")

# Only include responses from users with at least 3 sessions
min_sessions <- 5
users_to_keep <- responses[, .(n_sessions = uniqueN(session_id)), by = user_id][n_sessions >= min_sessions, .(user_id)]
responses <- merge(responses, users_to_keep, by = "user_id")




# Keystroke features per response -----------------------------------------

keystroke_features <- responses[, .(
  duration_med = median(duration, na.rm = TRUE),
  rp_med = median(rp_latency, na.rm = TRUE),
  rr_med = median(rr_latency, na.rm = TRUE),
  pr_med = median(pr_latency, na.rm = TRUE),
  pp_med = median(pp_latency, na.rm = TRUE), # Median IKD
  pp_perc95 = quantile(pp_latency, 0.95, na.rm = TRUE), # 95th percentile IKD
  pp_mad = mad(pp_latency, na.rm = TRUE), # Median absolute deviation IKD
  backspace_rate = sum(character == "Backspace") / .N
), by = response_id]



# Bigram-specific features per response -----------------------------------

bigram_features <- responses[, .(
  bigram_pp_med = median(pp_latency, na.rm = TRUE)
), by = .(response_id, bigram)]


# How common is each bigram?
bigram_freq <- bigram_features[, .(freq = .N/nrow(bigram_features)), by = bigram][order(-freq)]

# Keep the most common bigrams
n_bigrams <- 40
bigrams_to_keep <- bigram_freq[!is.na(bigram)][1:n_bigrams, .(bigram)]
bigram_features <- merge(bigram_features, bigrams_to_keep, by = "bigram")

# Create a wide format for bigram features
bigram_features_wide <- dcast(bigram_features, response_id ~ bigram, value.var = "bigram_pp_med")

# Learning performance features per response ------------------------------

learning_features <- responses[, .SD[1], by = response_id][, .(response_id, user_id, session_id, correct, reaction_time, alpha)]


# Merge features ----------------------------------------------------------

response_features <- merge(learning_features, keystroke_features, by = "response_id") |>
  merge(bigram_features_wide, by = "response_id")

# Save features -----------------------------------------------------------

write_fst(response_features, here("data", "response_features.fst"), compress = 100)