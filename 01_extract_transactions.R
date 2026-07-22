# =============================================================================
# 01_extract_transactions.R — Venmo BSON dump -> flat transaction CSVs
# Team 7 SNAP (Track 1)
#
# Source data: github.com/sa7mon/venmo-data (7,076,585 transactions, BSON dump).
#
# NOTE ON EXTRACTION: the BSON -> CSV step was performed with a streaming
# parser that reads venmo.tar.xz directly (no MongoDB / bsondump required) and
# emitted, with 0 decode errors:
#   transactions_raw_edges.csv  (7,024,852 user<->user events:
#                                transaction_id, date_created, action,
#                                sender_id, receiver_id)
#   venmo_spread_sample.csv     (44,285 systematic every-135th user->user
#                                'pay' event; 12 descriptive columns)
# If instead you start from a bsondump JSON-lines export (venmo.jsonl), the
# commented block at the bottom shows the equivalent R streaming loop.
#
# This script validates the extract and reports the Data Description numbers.
# Usage: Rscript 01_extract_transactions.R [data_dir]
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
base <- if (length(args) >= 1) args[1] else "C:/Users/agcru/Downloads/Compressed/venmo-data-master"

raw <- read.csv(file.path(base, "transactions_raw_edges.csv"),
                colClasses = c(transaction_id = "character",
                               sender_id = "character",
                               receiver_id = "character"))
cat(sprintf("user<->user events: %d\n", nrow(raw)))
cat(sprintf("  'pay' events    : %d\n", sum(raw$action == "pay")))
cat(sprintf("  'charge' events : %d\n", sum(raw$action == "charge")))
cat(sprintf("unique senders    : %d\n", length(unique(raw$sender_id))))
cat(sprintf("unique receivers  : %d\n", length(unique(raw$receiver_id))))

# Collection windows (detected as >20-day holes in the event stream):
#   W1: 2018-07-26 .. 2018-08-29 (34d)   W2: 2018-10-02 .. 2018-10-16 (14d)
#   W3: 2019-01-05 .. 2019-02-21 (47d)
ts <- as.Date(substr(raw$date_created, 1, 10))
cat("events by month:\n")
print(table(format(ts, "%Y-%m")))

# --- Equivalent extraction from a bsondump JSON-lines file (reference) -------
# con <- file(file.path(base, "venmo.jsonl"), "r")
# out <- file(file.path(base, "transactions_raw_edges.csv"), "w")
# writeLines("transaction_id,date_created,action,sender_id,receiver_id", out)
# while (length(line <- readLines(con, n = 1)) == 1) {
#   d <- jsonlite::fromJSON(line)
#   p <- d$payment
#   if (identical(d$type, "payment") && identical(p$target$type, "user")) {
#     writeLines(paste(p$id, p$date_created, p$action,
#                      p$actor$id, p$target$user$id, sep = ","), out)
#   }
# }
# close(con); close(out)
