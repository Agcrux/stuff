# =============================================================================
# 02_build_subset.R — outcome-seeded connected 1,000-user subset (igraph)
# Team 7 SNAP (Track 1)
#
# Downsampling method (TA feedback item 1), in order:
#   1. ACTIVE users: initiated >= 3 public events (7.18M users -> 556,533).
#   2. Pay graph among active users; take its GIANT COMPONENT (51,438 users).
#   3. Label by collection-window activity:  W1 Jul-Aug'18 / W2 Oct'18 /
#      W3 Jan-Feb'19.  Pattern "02" = active W1, observably silent through the
#      fully-collected W2, active again W3  => "reappeared" (n=511 in GC).
#   4. Multi-source BFS/snowball: all 511 reappeared users as seeds, expand to
#      their direct transaction partners until 1,000 users (ego-net design:
#      every candidate keeps its real network context; partners provide the
#      steady/comparison population).
#
# "Long period of no public activity" (TA feedback item 2): a fixed 60-day gap
# is IMPOSSIBLE within any collection window (max window 47 days; 100% of
# >=60-day inter-event gaps span scraper downtime, not user behavior). We
# therefore define the silence as: zero initiated public events during the
# fully-observed 14-day W2 plus the surrounding holes — a minimum implied
# silence of 129 days (2018-08-29 .. 2019-01-05) with direct observational
# support in W2.
#
# Usage: Rscript 02_build_subset.R [data_dir]
# Inputs : transactions_raw_edges.csv
# Outputs: subset_users.csv, subset_edges.csv
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
base <- if (length(args) >= 1) args[1] else "C:/Users/agcru/Downloads/Compressed/venmo-data-master"
lib <- file.path(path.expand("~"), "R", "win-library", "4.6")
if (dir.exists(lib)) .libPaths(c(lib, .libPaths()))
suppressPackageStartupMessages(library(igraph))

TARGET <- 1000; ACTIVE_SENT <- 3
raw <- read.csv(file.path(base, "transactions_raw_edges.csv"),
                colClasses = c(transaction_id = "character",
                               sender_id = "character", receiver_id = "character"))
raw$ts <- as.POSIXct(raw$date_created, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
raw <- raw[!is.na(raw$ts) & !is.na(raw$sender_id) & !is.na(raw$receiver_id), ]

# window assignment
w_of <- function(ts) {
  d <- as.Date(ts)
  ifelse(d >= as.Date("2018-07-26") & d <= as.Date("2018-08-29"), 0L,
  ifelse(d >= as.Date("2018-10-02") & d <= as.Date("2018-10-16"), 1L,
  ifelse(d >= as.Date("2019-01-05") & d <= as.Date("2019-02-21"), 2L, -1L)))
}
raw$win <- w_of(raw$ts)

# 1) active users
sent_n <- table(raw$sender_id)
active <- names(sent_n)[sent_n >= ACTIVE_SENT]

# 2) giant component of active pay graph
pay <- raw[raw$action == "pay" &
           raw$sender_id %in% active & raw$receiver_id %in% active, ]
g <- simplify(graph_from_data_frame(pay[, c("sender_id", "receiver_id")],
                                    directed = FALSE))
comp <- components(g)
gc_ids <- V(g)$name[comp$membership == which.max(comp$csize)]
cat(sprintf("active=%d  GC=%d\n", length(active), length(gc_ids)))

# 3) window patterns -> reappeared seeds
inw <- raw[raw$win >= 0, ]
pat <- tapply(inw$win, inw$sender_id,
              function(w) paste(sort(unique(w)), collapse = ""))
seeds <- intersect(names(pat)[pat == "02"], gc_ids)
cat(sprintf("reappeared (pattern 02) seeds in GC: %d\n", length(seeds)))

# 4) multi-source BFS from all seeds, expand to TARGET
gg <- induced_subgraph(g, gc_ids)
dist <- distances(gg, v = V(gg)[name %in% seeds], to = V(gg), mode = "all")
mind <- apply(dist, 2, min)
ord  <- order(mind, seq_along(mind))          # seeds first, then 1-hop, 2-hop...
subset_ids <- V(gg)$name[ord[seq_len(min(TARGET, length(ord)))]]

# outputs
su <- data.frame(user_id = subset_ids,
                 seed_type = ifelse(subset_ids %in% seeds, "pattern02", "neighbor"),
                 sent_events = as.integer(sent_n[subset_ids]))
write.csv(su, file.path(base, "subset_users.csv"), row.names = FALSE)

ind <- pay[pay$sender_id %in% subset_ids & pay$receiver_id %in% subset_ids, ]
agg <- aggregate(list(weight = rep(1, nrow(ind))),
                 by = list(sender_id = ind$sender_id, receiver_id = ind$receiver_id),
                 FUN = sum)
write.csv(agg, file.path(base, "subset_edges.csv"), row.names = FALSE)
cat(sprintf("wrote subset_users.csv (%d users), subset_edges.csv (%d edges)\n",
            nrow(su), nrow(agg)))
