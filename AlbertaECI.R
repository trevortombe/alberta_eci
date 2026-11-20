########################################################################
# Alberta Economic Conditions Index (ECI) and experimental monthly real GDP
#
# Step through section by section. Each figure is built, shown and saved
# where it is made.
########################################################################

rm(list = ls(all = TRUE))

#########
# Setup #
#########
packages <- c("dplyr", "tidyr", "ggplot2", "scales", "zoo", "lubridate", "jsonlite", "seasonal", "tempdisagg")
missing <- packages[!packages %in% rownames(installed.packages())]
if (length(missing)) install.packages(missing)
invisible(suppressPackageStartupMessages(lapply(packages, library, character.only = TRUE)))

mytheme <- theme_minimal() + theme(
  axis.title.y = element_text(size = 9),
  axis.title.x = element_text(size = 9),
  panel.background = element_rect(fill = "white", color = NA),
  plot.background = element_rect(fill = "white", color = NA),
  legend.position = "top",
  legend.text = element_text(size = 10),
  legend.margin = margin(c(0, 0, -0.25, 0), unit = "cm"),
  plot.caption = element_text(size = 6, color = "gray40", hjust = 1),
  plot.title = element_text(face = "bold"),
  plot.subtitle = element_text(size = 8, color = "gray40"),
  panel.grid.minor = element_blank(),
  panel.grid.major.x = element_blank()
)
col <- c("#CC2529", "#396ab1", "#3E9651", "#DA7C30", "#535154", "#6B4C9A", "#922428", "#948B3D")
colbar <- c("#D35E60", "#7293CB", "#84BA5B", "#E1974C", "#808585", "#9067A7", "#AB6857", "#CCC210")
group_fill <- c("Energy Sector" = "#F4C27A", "Business Activity" = "#9CC9EC",   # light, colourblind-safe tints
                "Labour Markets" = "#8FD1B6", "Households Income/Spending" = "#E6A9CB")

#################
# Update data   #
#################
source("AlbertaData.R")

####################
# Compile the data #
####################
# Indicator groups (used for the decomposition chart)
egy_vars <- c("oil")
biz_vars <- c("mfg", "trade", "exports_nonenergy", "shiftshare")
lab_vars <- c("employment", "seph")
con_vars <- c("payroll", "retail", "food", "housing", "vehicles")
variables <- c(egy_vars, biz_vars, lab_vars, con_vars)
groups <- c(rep("Energy Sector", length(egy_vars)), rep("Business Activity", length(biz_vars)),
            rep("Labour Markets", length(lab_vars)), rep("Households Income/Spending", length(con_vars)))

# Year-over-year log changes, Jan 2002 onward
levels_m <- monthly %>% filter(When >= as.Date("2001-01-01")) %>% select(When, all_of(variables))
change <- levels_m %>%
  mutate(across(all_of(variables), ~ log(.x) - log(lag(.x, 12)))) %>%
  filter(When >= as.Date("2002-01-01"))
X <- as.matrix(change[, variables])
complete <- complete.cases(X)
# drop trailing months where nothing new has been released yet
last_any <- max(which(rowSums(!is.na(X)) > 0))
change <- change[1:last_any, ]; X <- X[1:last_any, ]; complete <- complete[1:last_any]

###################################################
# Estimate the index: first principal component   #
###################################################
# Weights come from months where every indicator is observed
pca <- prcomp(X[complete, ], center = TRUE, scale. = TRUE)
print(summary(pca)$importance[, 1:3])
weights <- pca$rotation[, 1] / sum(pca$rotation[, 1])
print(round(weights, 3))
mu <- colMeans(X[complete, ]); sigma <- apply(X[complete, ], 2, sd)
Z <- sweep(sweep(X, 2, mu), 2, sigma, "/")
index_sd <- sd(as.numeric(Z[complete, ] %*% weights))

# Ragged edge: for recent months where some series aren't out yet, average the released
# series using their weights (re-scaled to sum to one). Publish a month once at least half the
# total weight is in; flag those months as preliminary.
weight_in <- as.numeric((!is.na(Z)) %*% weights)
Zw <- Z; Zw[is.na(Zw)] <- 0
contrib <- sweep(Zw, 2, weights, "*") / weight_in / index_sd     # each series' contribution
eci <- tibble(Ref_Date = as.yearmon(change$When), ABindex = rowSums(contrib),
              weight_in = weight_in, preliminary = !complete) %>%
  bind_cols(as_tibble(contrib))
eci <- eci %>% filter(weight_in >= 0.5)
print(tail(eci %>% select(Ref_Date, ABindex, weight_in, preliminary), 6))

#####################################
# Plot the index and its components #
#####################################
plotdata <- eci %>%
  select(Ref_Date, ABindex, preliminary, all_of(variables)) %>%
  pivot_longer(all_of(variables), names_to = "series", values_to = "index") %>%
  mutate(group = groups[match(series, variables)]) %>%
  group_by(Ref_Date, ABindex, preliminary, group) %>% summarise(index = sum(index), .groups = "drop") %>%
  mutate(group = factor(group, levels = names(group_fill)))
latest <- eci %>% filter(Ref_Date == max(Ref_Date))
first_prelim <- if (any(eci$preliminary)) min(eci$Ref_Date[eci$preliminary]) else NA
p <- ggplot(plotdata, aes(Ref_Date, index, fill = group)) +
  geom_col(position = "stack", color = "white", linewidth = 0.1) +
  geom_line(data = eci, aes(Ref_Date, ABindex), inherit.aes = FALSE, linewidth = 1.25) +
  geom_hline(yintercept = 0, linewidth = 0.75, color = "gray50") +
  scale_fill_manual(name = "", values = group_fill) +
  mytheme +
  scale_x_continuous(expand = c(0, 0), breaks = seq(2005, max(eci$Ref_Date), 5), limit = c(NA, max(eci$Ref_Date) + 6)) +
  annotate("text", x = max(eci$Ref_Date) + 3.5, hjust = 0, y = 1, label = "Above\nTrend", size = 2.5) +
  annotate("text", x = max(eci$Ref_Date) + 3.5, hjust = 0, y = -1, label = "Below\nTrend", size = 2.5) +
  annotate("segment", x = max(eci$Ref_Date) + 3, xend = max(eci$Ref_Date) + 3, y = 0.1, yend = 1.1,
           arrow = arrow(length = unit(1, "mm")), linewidth = 0.25) +
  annotate("segment", x = max(eci$Ref_Date) + 3, xend = max(eci$Ref_Date) + 3, y = -0.1, yend = -1.1,
           arrow = arrow(length = unit(1, "mm")), linewidth = 0.25) +
  geom_label(data = latest, aes(Ref_Date, ABindex, label = paste("Latest:\n", round(ABindex, 2))),
             inherit.aes = FALSE, nudge_x = 0.5, hjust = 0, vjust = 1, size = 3, fill = "white", label.size = 0) +
  geom_point(data = latest, aes(Ref_Date, ABindex), inherit.aes = FALSE, size = 2, shape = 21, stroke = 2, fill = "white") +
  labs(y = "Index of Economic Activity", x = "",
       title = paste0("Economic Conditions Index for Alberta (", min(eci$Ref_Date), " to ", max(eci$Ref_Date), ")"),
       subtitle = paste0("Monthly data of economic activity (GDP) is available only for Canada, not provinces. Instead, this index 'averages' ",
                         length(variables), " monthly data series.\nThe index has mean zero and unit variance. A value of +1 means YoY growth is 1 standard deviation above trend.",
                         if (!is.na(first_prelim)) paste0("\nMonths from ", first_prelim, " on are preliminary: not all series have been released yet.") else ""),
       caption = "Sources: Own calculations from various Statistics Canada data tables. Graph by @trevortombe.\nIt is the 1st principal component from several monthly data series. Based on the Chicago Fed National Activity Index.")
p
ggsave("Figures/plot.png", p, width = 8, height = 4.5)

#######################################
# Compare with annual real GDP growth #
#######################################
gdp_growth <- gdp_annual %>% mutate(GDPGrowth = gdp / lag(gdp) - 1)
plotdata2 <- eci %>%
  select(Ref_Date, ABindex) %>%
  mutate(year = year(as.Date(Ref_Date))) %>%
  left_join(gdp_growth, by = "year") %>%
  left_join(pop_growth %>% mutate(Ref_Date = as.yearmon(When)) %>% select(-When), by = "Ref_Date") %>%
  mutate(GDPGrowth_fit = ifelse(month(as.Date(Ref_Date)) %in% c(1, 12), NA, GDPGrowth))
# The index measures growth relative to trend; population growth shifts Alberta's trend rate,
# so it enters the mapping from the index to GDP growth
regresults <- lm(GDPGrowth_fit ~ ABindex + pop_growth, data = plotdata2)
print(summary(regresults))
plotdata3 <- plotdata2 %>% mutate(index = predict(regresults, newdata = plotdata2))

# For the monthly GDP levels below, use the index alone. Chow-Lin benchmarking to annual GDP already
# absorbs shifts in trend growth; adding population there double-counts it (tested 2010-2025: errors
# rose from 1.2 to 1.6 points outside 2020-21).
regresults_levels <- lm(GDPGrowth_fit ~ ABindex, data = plotdata2)
plotdata3 <- plotdata3 %>% mutate(index_levels = predict(regresults_levels, newdata = plotdata2))

# Implied growth for years StatCan hasn't published yet (average of available months)
nowcast <- plotdata3 %>% filter(year > max(gdp_annual$year)) %>%
  group_by(year) %>% summarise(months = n(), predicted_growth = mean(index))
print(nowcast)

p <- ggplot(plotdata3, aes(Ref_Date)) +
  geom_col(data=filter(plotdata3,!month(as.Date(Ref_Date)) %in% c(1,12)),
           aes(y = GDPGrowth), fill = colbar[2], color = colbar[2]) +
  # geom_point(data=filter(plotdata3,month(as.Date(Ref_Date))==6),aes(y=GDPGrowth),
  #            size=2.5,stroke=2.5,shape=21,fill='white',color=col[2])+
  geom_hline(yintercept = 0, colour = "black", linewidth = 1) +
  geom_line(aes(y = index), linewidth = 1.5, color = col[1]) +
  annotate("text", x = 2013.25, y = -0.0375, label = "Actual GDP\nGrowth", fontface = "bold", color = colbar[2]) +
  annotate("text", x = 2019, y = 0.125, label = "\"Alberta Economic\nConditions Index\"", fontface = "bold", color = col[1], hjust = 1) +
  mytheme +
  scale_y_continuous(breaks = pretty_breaks(8), label = percent) +
  scale_x_continuous(breaks = seq(2002.5, max(plotdata3$year) + 0.5, 2), labels = seq(2002, max(plotdata3$year), 2)) +
  labs(x = "", y = "Year-over-Year Change (%)",
       title = paste("Index of Alberta's Monthly Economic Conditions, Jan 2002 to", max(plotdata3$Ref_Date)),
       subtitle = "The index has mean zero and unit variance. Combined with population growth, it is rescaled to best fit actual real GDP growth.",
       caption = "Sources: Own calculations from multiple monthly data series and Statistics Canada tables 36-10-0711 and 17-10-0009. Graph by @trevortombe.")
p
ggsave("Figures/GDPplot.png", p, width = 8, height = 4, dpi = 200)

#########################
# Save the index as CSV #
#########################
group_totals <- plotdata %>% select(Ref_Date, group, index) %>%
  pivot_wider(names_from = group, values_from = index)
table <- eci %>% select(Date = Ref_Date, `Alberta Economic Conditions Index` = ABindex, Preliminary = preliminary) %>%
  left_join(group_totals, by = c("Date" = "Ref_Date")) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))
write.csv(table, file = "Data/ECI_Index_Data.csv", row.names = FALSE)

#################################################################
# Experimental monthly real GDP                                 #
# 1. Chain the GDP-scaled YoY index into a level (2001 = 1)      #
# 2. Remove residual seasonality with X-13                      #
# 3. Chow-Lin: distribute annual GDP across months, extrapolate #
#################################################################
index_levels <- tibble(Ref_Date = as.yearmon(seq(as.Date("2001-01-01"), by = "month", length.out = 12)), index = 1) %>%
  bind_rows(plotdata3 %>% select(Ref_Date, index = index_levels))
for (i in 13:nrow(index_levels)) index_levels$index[i] <- index_levels$index[i - 12] * (1 + index_levels$index[i])
index_levels$index <- as.numeric(final(seas(ts(index_levels$index, frequency = 12, start = c(2001, 1)))))

GDP_series <- ts(gdp_annual %>% filter(year >= 2001) %>% pull(gdp), start = 2001, frequency = 1)
Index_series <- ts(index_levels$index, start = c(2001, 1), frequency = 12)
model <- td(GDP_series ~ Index_series, conversion = "sum", to = "monthly")
summary(model)

monthly_gdp <- index_levels %>%
  mutate(monthly_gdp = as.numeric(predict(model)),
         annualized = monthly_gdp * 12,
         year = year(as.Date(Ref_Date)),
         benchmarked = year <= max(gdp_annual$year),
         preliminary = Ref_Date %in% eci$Ref_Date[eci$preliminary])
write.csv(monthly_gdp %>% transmute(Date = Ref_Date, `Real GDP, annualized (millions of 2017 dollars)` = round(annualized),
                                    `Matches published annual GDP` = benchmarked, Preliminary = preliminary),
          "Data/Monthly_GDP_Estimates.csv", row.names = FALSE)

p <- ggplot(monthly_gdp %>% filter(Ref_Date >= "Jan 2005"), aes(Ref_Date, annualized / 1000)) +
  annotate("rect", xmin = as.numeric(as.yearmon("Oct 2008")), xmax = as.numeric(as.yearmon("May 2009")),
           ymin = -Inf, ymax = +Inf, alpha = 0.2, fill = "dodgerblue") +
  annotate("rect", xmin = as.numeric(as.yearmon("Oct 2014")), xmax = as.numeric(as.yearmon("Jul 2016")),
           ymin = -Inf, ymax = +Inf, alpha = 0.2, fill = "dodgerblue") +
  annotate("rect", xmin = as.numeric(as.yearmon("Feb 2020")), xmax = as.numeric(as.yearmon("May 2020")),
           ymin = -Inf, ymax = +Inf, alpha = 0.2, fill = "dodgerblue") +
  annotate("text", x = 2014.6, hjust = 1, y = Inf, vjust = 1, alpha = 0.5, label = "Recessions", color = "dodgerblue", size = 2.5) +
  geom_line(linewidth = 1.5, color = col[1]) +
  geom_point(data = filter(monthly_gdp, Ref_Date == max(Ref_Date)), stroke = 2, size = 2, shape = 21, fill = "white", color = col[1]) +
  mytheme +
  scale_y_continuous(label = dollar) +
  scale_x_yearmon(format = "%Y", breaks = pretty_breaks(7))
p + labs(x = "", title = "Experimental Estimates of Alberta's Monthly Real GDP", caption = "Graph by @trevortombe",
         subtitle = paste0("Source: own calculations from a composite of several monthly indicators, constructed to exactly match annual real GDP\nthrough ",
                           max(gdp_annual$year), " (Statistics Canada table 36-10-0711). Later months are extrapolated from the indicators."),
         y = "Billions of (2017) Dollars, Annualized")
ggsave("Figures/MonthlyGDP_Experimental.png", width = 8, height = 4)
p + labs(x = "", y = "Billions of (2017) Dollars, Annualized")
ggsave("Figures/MonthlyGDP_Experimental_notitle.png", width = 7, height = 3.5)

##############################
# Update the README on GitHub #
##############################
readme <- readLines("README_template.md")
readme <- gsub("{{LATEST_MONTH}}", as.character(latest$Ref_Date), readme, fixed = TRUE)
readme <- gsub("{{LATEST_VALUE}}", format(round(latest$ABindex, 2), nsmall = 2), readme, fixed = TRUE)
readme <- gsub("{{PRELIM_NOTE}}", if (latest$preliminary) " (preliminary: not all series have been released for this month)" else "", readme, fixed = TRUE)
readme <- gsub("{{UPDATED}}", format(Sys.time(), "%B %d, %Y at %H:%M %Z", tz = "America/Edmonton"), readme, fixed = TRUE)
writeLines(readme, "README.md")
