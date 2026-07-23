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
#   reappearance status     CovSnd: reappeared   candidates' baseline shift
#   control                 CovSnd: acct_age_z   account tenure (z-scored)
#   peer activity           -> tested in 04_reappearance_logit.R (see NOTE
#                              below on the relevent time-varying-covar bug)
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

# --- RISK SET restriction: rem.dyad's likelihood spans all n*(n-1) dyads per
# event; n=1000 (1M dyads x 994 events) exceeds memory. Standard practice:
# restrict the risk set to actors observed participating in the sampled event
# stream (isolates never send/receive and only inflate the normalization).
keep <- sort(unique(c(el[, 2], el[, 3])))
remap <- integer(n); remap[keep] <- seq_along(keep)
el[, 2] <- remap[el[, 2]]
el[, 3] <- remap[el[, 3]]
peer7 <- peer7[, keep, drop = FALSE]
amap  <- amap[keep, ]
n <- length(keep)
M <- nrow(el)
cat(sprintf("risk set restricted to event participants: n=%d\n", n))

# NOTE: earlier drafts capped n to ~350 because enumerating the full n*(n-1)
# risk set at every event made the fit intractable (it would hang for hours in
# optim / the GOF pass). That cap is no longer needed — sample.size (case-
# control dyad sampling, set on the rem.dyad call below) makes the full
# participant set tractable, so we keep all n=732 event participants.

# --- static covariates aligned to actor_index order ---
users <- users[match(amap$user_id, users$user_id), ]
reappeared <- as.numeric(users$label == "reappeared_skipW2")
age        <- users$account_age_days_at_start
acct_age_z <- as.numeric(scale(age))
acct_age_z[is.na(acct_age_z)] <- 0
cat(sprintf("reappeared=1 actors: %d of %d\n", sum(reappeared), n))

# --- STATIC CovSnd matrix (n x p) ---
# NOTE: relevent 1.2-1 (R 4.6.1/Windows) crashes with an access violation on
# time-varying covariate arrays (M x n x p), verified by isolation tests.
# Peer-activity effects are therefore tested in the population-scale logistic
# regression (04_reappearance_logit.R); the REM uses static actor covariates
# plus relevent's built-in dynamic effects, which are computed internally in C
# and are unaffected by the bug.
covsnd <- cbind(reappeared = reappeared, acct_age_z = acct_age_z)

effects <- c("CovSnd", "FrPSndSnd", "RSndSnd", "NIDSnd", "NODSnd")

# sample.size = case-control dyad sampling (Butts 2008): rather than enumerate
# the full n*(n-1) risk set at every event, the observed dyad is compared
# against SS random control dyads. This is what makes the full-n fit tractable.
# (Both failure modes we hit trace here: the full risk set HANGS optim/GOF, and
# a time-varying covariate array SEGFAULTS relevent 1.2-1 — static covariates +
# sampling avoid both.) set.seed makes the sampled fit reproducible.
SS <- 100
cat(sprintf("fitting rem.dyad (ordinal, sample.size=%d)...\n", SS))
set.seed(42)
t0 <- Sys.time()
fit <- rem.dyad(el, n = n,
                effects = effects,
                covar = list(CovSnd = covsnd),
                ordinal = TRUE, hessian = TRUE,
                sample.size = SS)
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
gv <- function(x) if (is.null(x)) NA_real_ else x
cat(sprintf("null deviance %.1f  residual deviance %.1f  AIC %.1f\n",
            gv(fit$null.deviance), gv(fit$residual.deviance), gv(fit$AIC)))
