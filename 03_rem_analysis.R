# =============================================================================
# 03_rem_analysis.R — Relational Event Model for Venmo public re-visibility
# Team 7 SNAP (Track 1)
#
# Research question: which users become publicly visible again after a period
# of no public activity, and does their social network (tie strength,
# centrality, peer activity) influence that switch?
#
# Sample: 1,000 connected users from the giant component of the active pay
# network — all 511 "reappeared" users (active Jul-Aug 2018 window, observably
# silent during the fully-collected Oct 2018 window, active again Jan-Feb 2019)
# plus 489 of their direct transaction partners (ego-network expansion).
#
# Likelihood: ORDINAL timing (rem.dyad default). Justification: the scrape has
# collection holes (no Sept, no Nov-Dec 2018), so inter-event durations that
# span holes are artifacts; the ordinal likelihood uses only the ORDER of
# events, which the holes do not distort.
#
# Effect-to-RQ mapping (TA feedback item 3):
#   RQ component            rem.dyad effect      interpretation
#   tie strength            FrPSndSnd            share of i's past sends going
#                                                to j raises rate of next i->j
#   tie recency             RSndSnd              recently-used ties are reused
#   centrality (activity)   NIDSnd, NODSnd       normalized in/out-degree of
#                                                sender raises sending rate
#   peer activity (7d)      CovSnd: peer7        partners' events in trailing
#                                                7 days raise ego's rate
#   reappearance status     CovSnd: reappeared   candidates' baseline shift
#   network x reappearance  CovSnd: reap_x_peer  DOES peer activity pull
#                                                silent users back? (key test)
#   control                 CovSnd: acct_age_z   account tenure (z-scored)
# =============================================================================

lib <- file.path(path.expand("~"), "R", "win-library", "4.6")
if (dir.exists(lib)) .libPaths(c(lib, .libPaths()))
suppressPackageStartupMessages(library(relevent))

base <- "C:/Users/agcru/Downloads/Compressed/venmo-data-master"
ev    <- read.csv(file.path(base, "rem_events.csv"))
users <- read.csv(file.path(base, "subset_users_labeled.csv"),
                  colClasses = c(user_id = "character"))
amap  <- read.csv(file.path(base, "rem_actor_map.csv"),
                  colClasses = c(user_id = "character"))
peer7 <- as.matrix(read.csv(file.path(base, "peer_activity_7d.csv"), header = FALSE))

n <- nrow(amap)
M <- nrow(ev)
stopifnot(nrow(peer7) == M, ncol(peer7) == n)
cat(sprintf("events M=%d  actors n=%d\n", M, n))

# --- edgelist for rem.dyad: (time, sender, receiver), strictly increasing ---
el <- as.matrix(ev[, c("time_days", "sender", "receiver")])

# --- static covariates aligned to actor_index order ---
users <- users[match(amap$user_id, users$user_id), ]
reappeared <- as.numeric(users$label == "reappeared_skipW2")
age        <- users$account_age_days_at_start
acct_age_z <- as.numeric(scale(age))
acct_age_z[is.na(acct_age_z)] <- 0
cat(sprintf("reappeared=1 actors: %d of %d\n", sum(reappeared), n))

# --- time-varying CovSnd array: M x n x p ---
peer7_l <- log1p(peer7)                      # heavy-tailed counts -> log1p
p <- 4
covsnd <- array(0, dim = c(M, n, p))
covsnd[, , 1] <- matrix(reappeared, nrow = M, ncol = n, byrow = TRUE)
covsnd[, , 2] <- peer7_l
covsnd[, , 3] <- covsnd[, , 1] * peer7_l     # interaction: the key test
covsnd[, , 4] <- matrix(acct_age_z, nrow = M, ncol = n, byrow = TRUE)

effects <- c("CovSnd", "FrPSndSnd", "RSndSnd", "NIDSnd", "NODSnd")

cat("fitting rem.dyad (ordinal likelihood)...\n")
t0 <- Sys.time()
fit <- rem.dyad(el, n = n,
                effects = effects,
                covar = list(CovSnd = covsnd),
                ordinal = TRUE, hessian = TRUE)
cat(sprintf("fit time: %.1f min\n",
    as.numeric(difftime(Sys.time(), t0, units = "mins"))))

print(summary(fit))

co <- fit$coef
se <- sqrt(diag(fit$cov))
z  <- co / se
res <- data.frame(effect = names(co), coef = co, se = se, z = z,
                  p = 2 * pnorm(-abs(z)), row.names = NULL)
write.csv(res, file.path(base, "rem_results.csv"), row.names = FALSE)

rem_fit <- fit
save(rem_fit, el, users, amap, file = file.path(base, "Team7_SNAP_Track1_rem.Rdata"))
cat("wrote rem_results.csv and Team7_SNAP_Track1_rem.Rdata\n")
cat(sprintf("null deviance %.1f  residual deviance %.1f  AIC %.1f\n",
            fit$null.deviance, fit$residual.deviance, fit$AIC))
