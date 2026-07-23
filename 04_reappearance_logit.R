# =============================================================================
# 04_reappearance_logit.R — Model 2: who becomes publicly visible again?
# Team 7 SNAP (Track 1)
#
# Population (N = 7,012): active (>=3 initiated events) users in the giant
# component of the pay network who initiated events in W1 (Jul 26 - Aug 29
# 2018) and were observably silent as senders throughout the fully-collected
# W2 (Oct 2-16 2018).  Outcome: reappeared = 1 if they initiated events again
# in W3 (Jan 5 - Feb 21 2019); 511 of 7,012 (7.3%) did.
#
# All predictors are measured in W1 only (before the silence):
#   w1_partners    distinct pay partners            [degree centrality]
#   w1_max_tie     max events with a single partner [tie strength]
#   w1_recip_frac  share of reciprocated ties       [tie strength/embeddedness]
#   w1_events      initiated events                 [own activity control]
#   tenure_days    days since first observed event  [tenure proxy control;
#                  lower bound only - true date_joined not available at
#                  population scale]
#   peer_w1_events partners' initiated W1 events    [peer activity]
#   peer_w3_frac   share of W1 partners who are     [peer reappearance -
#                  themselves active in W3           contemporaneous, so
#                                                    interpret as association]
#
# This model complements 03_rem_analysis.R: the REM captures dyadic event
# dynamics within the sampled network; this logit answers the selection
# question "which silent users switch back to public visibility" at
# population scale.  (Peer effects are tested here rather than in the REM
# because relevent 1.2-1 crashes on time-varying covariate arrays.)
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
base <- if (length(args) >= 1) args[1] else "C:/Users/agcru/Downloads/Compressed/venmo-data-master"

d <- read.csv(file.path(base, "logit_data.csv"), colClasses = c(user_id = "character"))
cat(sprintf("N=%d  reappeared=%d (%.1f%%)\n", nrow(d), sum(d$reappeared),
            100 * mean(d$reappeared)))

d$w1_events_l    <- log1p(d$w1_events)
d$peer_w1_l      <- log1p(d$peer_w1_events)
d$tenure_z       <- as.numeric(scale(d$tenure_days))

fit <- glm(reappeared ~ w1_partners + w1_max_tie + w1_recip_frac +
             w1_events_l + tenure_z + peer_w1_l + peer_w3_frac,
           family = binomial, data = d)
print(summary(fit))

or <- exp(cbind(OR = coef(fit), confint.default(fit)))
cat("\nodds ratios (95% CI):\n")
print(round(or, 3))

res <- data.frame(term = names(coef(fit)), coef = coef(fit),
                  se = summary(fit)$coefficients[, 2],
                  z = summary(fit)$coefficients[, 3],
                  p = summary(fit)$coefficients[, 4],
                  OR = exp(coef(fit)), row.names = NULL)
write.csv(res, file.path(base, "logit_results.csv"), row.names = FALSE)

logit_fit <- fit; logit_data <- d
save(logit_fit, logit_data, file = file.path(base, "Team7_SNAP_Track1_logit.Rdata"))
cat(sprintf("\nAIC %.1f  null dev %.1f  resid dev %.1f\nwrote logit_results.csv + .Rdata\n",
            AIC(fit), fit$null.deviance, fit$deviance))
