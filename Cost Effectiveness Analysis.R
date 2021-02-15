set.seed(131)
n_samples <- 1000

# Cost
c <- vector(mode = "list",length = 6)
names(c) <- c("Strategy 1, Grp 1", "Strategy 1, Grp 2", "Strategy 2, Grp 1",
              "Strategy 2, Grp 2", "Strategy 3, Grp 1", "Strategy 3, Grp 2")
c[[1]] <- rlnorm(n_samples, 2, .1)
c[[2]] <- rlnorm(n_samples, 2, .1)
c[[3]] <- rlnorm(n_samples, 11, .15)
c[[4]] <- rlnorm(n_samples, 11, .15)
c[[5]] <- rlnorm(n_samples, 11, .15)
c[[6]] <- rlnorm(n_samples, 11, .15)

# effectiveness
e <- c
e[[1]] <- rnorm(n_samples, 8, .2)
e[[2]] <- rnorm(n_samples, 8, .2)
e[[3]] <- rnorm(n_samples, 10, .8)
e[[4]] <- rnorm(n_samples, 10.5, .8)
e[[5]] <- rnorm(n_samples, 8.5, .6)
e[[6]] <- rnorm(n_samples, 11, .6)

# cost and effectiveness by strategy and simulation
library("data.table")
ce <- data.table(sample = rep(seq(n_samples), length(e)),
                 strategy = rep(paste0("Strategy ", seq(1, 3)), 
                                each = n_samples * 2),
                 grp = rep(rep(c("Group 1", "Group 2"),
                               each = n_samples), 3),
                 cost = do.call("c", c), qalys = do.call("c", e))
head(ce)
# Decision Analysis
ce <- ce[, nmb := 150000 * qalys - cost]
enmb <- ce[, .(enmb = mean(nmb)), by = c("strategy", "grp")]
enmb <- dcast(enmb, strategy ~ grp, value.var = "enmb")
print(enmb)
library("hesim")
ktop <- 200000
cea <-  cea(ce, k = seq(0, ktop, 500), sample = "sample", strategy = "strategy",
            grp = "grp", e = "qalys", c = "cost")
cea_pw <-  cea_pw(ce,  k = seq(0, ktop, 500), comparator = "Strategy 1",
                  sample = "sample", strategy = "strategy", grp = "grp",
                  e = "qalys", c = "cost")
print(cea$summary)
ce[, .(median_cost = median(cost), median_qalys = median(qalys)),
   by = c("strategy", "grp")]
print(cea_pw$summary)

# Cost Effectiveness Plane
head(cea_pw$delta)
library("ggplot2")
library("scales")
theme_set(theme_minimal())

ylim <- max(cea_pw$delta[, ic]) * 1.1
xlim <- ceiling(max(cea_pw$delta[, ie]) * 1.1)
ggplot(cea_pw$delta, aes(x = ie, y = ic, col = factor(strategy))) + 
  geom_jitter(size = .5) + 
  facet_wrap(~grp) + 
  xlab("Incremental QALYs") + 
  ylab("Incremental cost") +
  scale_y_continuous(label = scales::dollar, 
                     limits = c(-ylim, ylim)) +
  scale_x_continuous(limits = c(-xlim, xlim), 
                     breaks = seq(-6, 6, 2)) +
  theme(legend.position = "bottom") + 
  scale_colour_discrete(name = "Strategy") +
  geom_abline(slope = 150000, linetype = "dashed") +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0)

# Cost Effectiveness Acceptability Curves
random_rows <- sample(1:n_samples, 10)
nmb_dt <- dcast(ce[sample %in% random_rows & grp == "Group 2"], 
                sample ~ strategy, value.var = "nmb")
setnames(nmb_dt, colnames(nmb_dt), c("sample", "nmb1", "nmb2", "nmb3"))
nmb_dt <- nmb_dt[, maxj := apply(nmb_dt[, .(nmb1, nmb2, nmb3)], 1, which.max)]
nmb_dt <- nmb_dt[, maxj := factor(maxj, levels = c(1, 2, 3))]
mce <- prop.table(table(nmb_dt$maxj))
print(mce)
ggplot(cea$mce, aes(x = k, y = prob, col = factor(strategy))) +
  geom_line() + 
  facet_wrap(~grp) + 
  xlab("Willingness to pay") +
  ylab("Probability most cost-effective") +
  scale_x_continuous(breaks = seq(0, ktop, 100000), 
                     label = scales::dollar) +
  theme(legend.position = "bottom") + 
  scale_colour_discrete(name = "Strategy")

# Pairwise Comparison
ggplot(cea_pw$ceac, aes(x = k, y = prob, col = factor(strategy))) +
  geom_line() + 
  facet_wrap(~grp) + 
  xlab("Willingness to pay") +
  ylab("Probability most cost-effective") +
  scale_x_continuous(breaks = seq(0, ktop, 100000), 
                     label = scales::dollar) +
  theme(legend.position = "bottom") + 
  scale_colour_discrete(name = "Strategy")

# Cost-effectiveness acceptability frontier (CEAF)
ggplot(cea$mce[best == 1], aes(x = k, y = prob, col = strategy)) +
  geom_line() + 
  facet_wrap(~grp) + 
  xlab("Willingness to pay") +
  ylab("Probability most cost-effective") +
  scale_x_continuous(breaks = seq(0, ktop, 100000), 
                     label = scales::dollar) +
  theme(legend.position = "bottom") +
  scale_colour_discrete(name = "Strategy")

# Value of perfect information
strategymax_g2 <- which.max(enmb[[3]])
nmb_dt <- nmb_dt[, nmbpi := apply(nmb_dt[, .(nmb1, nmb2, nmb3)], 1, max)]
nmb_dt <- nmb_dt[, nmbci := nmb_dt[[strategymax_g2 + 1]]]
table(nmb_dt, digits = 0, format = "html")
enmbpi <- mean(nmb_dt$nmbpi)
enmbci <- mean(nmb_dt$nmbci)
print(enmbpi)
ggplot(cea$evpi, aes(x = k, y = evpi)) +
  geom_line() + facet_wrap(~grp) + xlab("Willingness to pay") +
  ylab("Expected value of perfect information") +
  scale_x_continuous(breaks = seq(0, ktop, 100000), label = scales::dollar) +
  scale_y_continuous(label = scales::dollar) +
  theme(legend.position = "bottom") 
w_dt <- data.table(grp = paste0("Group ", seq(1, 2)), w = c(0.25, .75))
evpi <- cea$evpi
evpi <- merge(evpi, w_dt, by = "grp")
totevpi <- evpi[,lapply(.SD, weighted.mean, w = w),
                by = "k", .SDcols = c("evpi")]
ggplot(totevpi, aes(x = k, y = evpi)) +
  geom_line() + xlab("Willingness to pay") +
  ylab("Total EVPI") +
  scale_x_continuous(breaks = seq(0, ktop, 100000), 
                     label = scales::dollar) +
  scale_y_continuous(label = scales::dollar) +
  theme(legend.position = "bottom") 
# Value of individualized care
# Compute total expected NMB with one-size fits all treatment
ce <- merge(ce, w_dt, by = "grp")
totenmb <- ce[, .(totenmb = weighted.mean(nmb, w = w)), by = c("strategy")]
totenmb_max <- max(totenmb$totenmb)
# Compute total expected NMB with individualized treatment
itotenmb_grp_max <- apply(as.matrix(enmb[, -1]), 2, max)
itotenmb_max <- sum(itotenmb_grp_max * w_dt$w)
# Compute EVIC
totnmb_scenarios <- c(itotenmb_max, totenmb_max)
names(totnmb_scenarios) <- c("Individualized total expected NMB",
                             "One-size fits all total expected NMB")
evic <- totnmb_scenarios[1] - totnmb_scenarios[2]
names(evic) <- "EVIC"
print(evic)
