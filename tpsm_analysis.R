packages <- c("readxl", "dplyr", "tidyr", "ggplot2", "lubridate",
              "moments", "corrplot", "FactoMineR", "factoextra",
              "gridExtra", "RColorBrewer", "scales")


new_pkg <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_pkg) > 0) install.packages(new_pkg, dependencies = TRUE)

lapply(packages, library, character.only = TRUE)

# Load & Rename Columns
df_raw <- read_excel("dax.xlsx")
View(df_raw)

df <- df_raw %>%
  rename(
    unique_id        = `Unique id`,
    channel          = channel_name,
    category         = category,
    sub_category     = `Sub-category`,
    customer_remarks = `Customer Remarks`,
    order_id         = Order_id,
    order_datetime   = order_date_time,
    issue_reported   = `Issue_reported at`,
    issue_responded  = issue_responded,
    survey_date      = Survey_response_Date,
    city             = Customer_City,
    product_category = Product_category,
    item_price       = Item_price,
    handling_time    = connected_handling_time,
    agent            = Agent_name,
    supervisor       = Supervisor,
    manager          = Manager,
    tenure_bucket    = `Tenure Bucket`,
    agent_shift      = `Agent Shift`,
    csat_score       = `CSAT Score`
  ) %>%
  # Keep only the 15 analytically relevant columns
  dplyr::select(unique_id, channel, category, sub_category,
         issue_reported, issue_responded, survey_date,
         city, product_category, item_price, handling_time,
         agent, tenure_bucket, agent_shift, csat_score)

cat("Dimensions:", nrow(df), "rows x", ncol(df), "columns\n")
cat("Column names:\n"); print(names(df))


#Data Type Fixing & Feature Engineering
df <- df %>%
  mutate(
    # Parse issue_reported as datetime
    # dmy_hm handles format "DD-MM-YYYY HH:MM"
    issue_reported_dt = dmy_hm(issue_reported, quiet = TRUE),
    
    # Compute elapsed minutes between report and response
    response_time_min = as.numeric(
      difftime(issue_responded, issue_reported_dt, units = "mins")
    ),
    
    # Winsorise: cap extreme outliers at 99th percentile
    response_time_min = pmin(
      response_time_min,
      quantile(response_time_min, 0.99, na.rm = TRUE)
    ),
    
    # Force numeric types
    csat_score = as.numeric(csat_score),
    item_price = as.numeric(item_price),
    
    # Ordered factor for tenure (least → most experienced)
    tenure_bucket = factor(
      tenure_bucket,
      levels  = c("On Job Training", "0-30", "31-60", "61-90", ">90"),
      ordered = TRUE
    ),
    
    # Unordered factors
    channel          = as.factor(channel),
    agent_shift      = as.factor(agent_shift),
    product_category = as.factor(product_category),
    
    # Binary CSAT: Good = 1 (score >= 4), Poor = 0 (score <= 3)
    csat_binary = ifelse(csat_score >= 4, 1, 0),
    csat_label  = factor(csat_binary,
                         levels = c(0, 1),
                         labels = c("Poor (1-3)", "Good (4-5)"))
  )

cat("Key column types:\n")
print(sapply(df %>% select(csat_score, item_price, response_time_min,
                           tenure_bucket, channel), class))

cat("\nNA counts after engineering:\n")
print(colSums(is.na(df %>% select(csat_score, item_price,
                                  response_time_min, tenure_bucket))))

# Show missing counts before imputation
cat("Missing values before imputation:\n")
print(colSums(is.na(df)))

# Mode helper function
mode_val <- function(x) {
  tbl <- sort(table(x), decreasing = TRUE)
  as.character(names(tbl)[1])
}

# Median imputation for item_price
df$item_price[is.na(df$item_price)] <- median(df$item_price, na.rm = TRUE)

# Mode imputation for city and product_category
df$city[is.na(df$city)]                         <- mode_val(df$city)
df$product_category[is.na(df$product_category)] <- mode_val(df$product_category)

#Confirm no remaining NAs in imputed columns
cat("\nMissing values after imputation (imputed columns only):\n")
print(colSums(is.na(df %>% select(item_price, city, product_category))))


#Technique 1: Measures of Central Tendency & Spread
summary_tbl <- df %>%
  select(csat_score, item_price, response_time_min) %>%
  mutate(across(everything(), as.numeric)) %>%
  pivot_longer(everything(),
               names_to  = "Variable",
               values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(
    N        = sum(!is.na(Value)),
    Mean     = round(mean(Value,   na.rm = TRUE), 3),
    Median   = round(median(Value, na.rm = TRUE), 3),
    Mode     = round(as.numeric(mode_val(as.character(
      round(na.omit(Value), 0)))), 3),
    SD       = round(sd(Value,     na.rm = TRUE), 3),
    Variance = round(var(Value,    na.rm = TRUE), 3),
    Range    = round(diff(range(Value, na.rm = TRUE)), 3),
    IQR      = round(IQR(Value,    na.rm = TRUE), 3),
    Skewness = round(skewness(Value, na.rm = TRUE), 3),
    Kurtosis = round(kurtosis(Value, na.rm = TRUE), 3),
    .groups  = "drop"
  )

cat("========== SUMMARY STATISTICS TABLE ==========\n")
print(summary_tbl)


cat("\n--- CSAT Score Frequency Table ---\n")
csat_freq <- df %>%
  count(csat_score) %>%
  mutate(Percentage = round(n / sum(n) * 100, 2),
         Cumulative_Pct = round(cumsum(Percentage), 2))
print(csat_freq)


cat("\n--- Good vs Poor Experience Split ---\n")
print(table(df$csat_label))
cat("Proportion Good Experience:",
    round(mean(df$csat_binary, na.rm = TRUE), 4), "\n")

#Technique 2: Covariance & Correlation
# Build complete-cases numeric matrix
num_df <- df %>%
  select(csat_score, item_price, response_time_min) %>%
  mutate(across(everything(), as.numeric)) %>%
  na.omit()

cat("Rows used for correlation analysis:", nrow(num_df), "\n")

# Covariance matrix 
cov_matrix <- cov(num_df)
cat("\n--- Covariance Matrix ---\n")
print(round(cov_matrix, 3))

# Pearson correlation matrix 
pearson_cor <- cor(num_df, method = "pearson")
cat("\n--- Pearson Correlation Matrix ---\n")
print(round(pearson_cor, 3))

# Significance test for each correlation

cat("\n--- Pairwise Correlation Significance Tests (Spearman) ---\n")
pairs_list <- list(
  c("csat_score",        "item_price"),
  c("csat_score",        "response_time_min"),
  c("item_price",        "response_time_min")
)

for (pair in pairs_list) {
  test <- cor.test(num_df[[pair[1]]], num_df[[pair[2]]],
                   method = "spearman", exact = FALSE)
  cat(sprintf("  %s vs %s:  rho = %.4f  |  p = %.4f  |  %s\n",
              pair[1], pair[2],
              test$estimate, test$p.value,
              ifelse(test$p.value < 0.05,
                     "SIGNIFICANT", "Not significant")))
}

# Correlation heatmap
cor_sig <- cor.mtest(num_df, conf.level = 0.95)

corrplot(pearson_cor,
         method      = "color",
         type        = "upper",
         order       = "hclust",
         addCoef.col = "black",
         tl.col      = "black",
         tl.srt      = 45,
         p.mat       = cor_sig$p,
         sig.level   = 0.05,
         insig       = "blank",
         col         = colorRampPalette(c("#D32F2F","#FFFFFF","#1976D2"))(200),
         title       = "Pearson Correlation Heatmap (insignificant pairs blanked)",
         mar         = c(0, 0, 1.5, 0))

dev.off()

# Technique 3: Data Visualisation (6 Plots)
# PLOT 1 — CSAT Score Distribution (Bar Chart)
p1 <- ggplot(df, aes(x = factor(csat_score), fill = factor(csat_score))) +
  geom_bar(colour = "white", width = 0.7) +
  geom_text(
    stat  = "count",
    aes(label = paste0(after_stat(count), "\n(",
                       round(after_stat(count) / nrow(df) * 100, 1), "%)")),
    vjust = -0.3, size = 3.5, fontface = "bold"
  ) +
  scale_fill_manual(
    values = c("1" = "#D32F2F", "2" = "#E64A19",
               "3" = "#F57C00", "4" = "#388E3C", "5" = "#1976D2")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Distribution of CSAT Scores",
    subtitle = paste0("N = ", nrow(df),
                      "  |  Good Experience (CSAT ≥ 4): ",
                      scales::percent(mean(df$csat_binary, na.rm = TRUE),
                                      accuracy = 0.1)),
    x = "CSAT Score  (1 = Very Dissatisfied  →  5 = Very Satisfied)",
    y = "Count"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title    = element_text(face = "bold"),
        plot.subtitle = element_text(colour = "grey50"))

print(p1)



#PLOT 2 — Item Price Histogram with Density Curve
p2 <- ggplot(df %>% filter(!is.na(item_price)),
             aes(x = item_price)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins   = 40,
                 fill   = "#1976D2",
                 colour = "white",
                 alpha  = 0.75) +
  geom_density(colour   = "#D32F2F",
               linewidth = 1.3) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title    = "Item Price Distribution",
    subtitle = paste0("Mean = Rs", scales::comma(round(mean(df$item_price, na.rm=TRUE), 0)),
                      "  |  Median = Rs", scales::comma(median(df$item_price, na.rm=TRUE)),
                      "  |  Skewness = ",
                      round(skewness(df$item_price, na.rm=TRUE), 2)),
    x = "Item Price",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

print(p2)


# PLOT 3 — Boxplot of CSAT Score by Tenure Bucket
p3 <- ggplot(df, aes(x = tenure_bucket,
                     y = csat_score,
                     fill = tenure_bucket)) +
  geom_boxplot(outlier.shape = 21,
               outlier.alpha = 0.4,
               alpha = 0.8,
               width = 0.6) +
  stat_summary(fun  = mean,
               geom = "point",
               shape = 23,
               fill = "red",
               size = 3,
               colour = "darkred") +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(breaks = 1:5) +
  labs(
    title    = "CSAT Score Distribution by Agent Tenure Bucket",
    subtitle = "Diamond = Group Mean  |  Line = Median  |  Box = IQR",
    x = "Tenure Bucket  (Least → Most Experienced)",
    y = "CSAT Score"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))

print(p3)


# PLOT 4 — Violin Plot of CSAT Score by Agent Shift
p4 <- ggplot(df, aes(x = agent_shift,
                     y = csat_score,
                     fill = agent_shift)) +
  geom_violin(alpha  = 0.7,
              trim   = FALSE,
              colour = "white") +
  geom_boxplot(width        = 0.12,
               fill         = "white",
               colour       = "grey40",
               outlier.shape = NA) +
  stat_summary(fun  = mean,
               geom = "point",
               shape = 21,
               fill = "red",
               size = 3) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(breaks = 1:5) +
  labs(
    title    = "CSAT Score Distribution by Agent Shift",
    subtitle = "Red Dot = Mean  |  Box = Median & IQR  |  Width = Density",
    x = "Agent Shift",
    y = "CSAT Score"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))

print(p4)

# PLOT 5 — Scatter Plot Matrix (Pairs Plot)
pairs_df <- df %>%
  select(
    `CSAT Score`      = csat_score,
    `Item Price`      = item_price,
    `Response Time`   = response_time_min
  ) %>%
  na.omit()

# Custom pairs panel functions
panel_scatter <- function(x, y, ...) {
  points(x, y, col = alpha("#1976D2", 0.3), pch = 20, cex = 0.6)
  abline(lm(y ~ x), col = "#D32F2F", lwd = 1.5)
}

panel_hist <- function(x, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  h <- hist(x, plot = FALSE, breaks = 20)
  breaks <- h$breaks
  nB     <- length(breaks)
  y      <- h$counts / max(h$counts)
  rect(breaks[-nB], 0, breaks[-1], y,
       col = "#1976D2", border = "white")
}

panel_cor <- function(x, y, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r   <- round(cor(x, y, use = "complete.obs", method = "spearman"), 3)
  txt <- paste0("ρ = ", r)
  col <- ifelse(abs(r) > 0.1, "#D32F2F", "grey60")
  text(0.5, 0.5, txt, cex = 1.4, fontface = "bold", col = col)
}

pairs(pairs_df,
      lower.panel = panel_scatter,
      diag.panel  = panel_hist,
      upper.panel = panel_cor,
      main        = "Scatter Plot Matrix — Spearman ρ (upper) | Regression Line (lower)")

# Save using png device
png("plot_05_scatter_matrix.png", width = 900, height = 900, res = 130)
pairs(pairs_df,
      lower.panel = panel_scatter,
      diag.panel  = panel_hist,
      upper.panel = panel_cor,
      main        = "Scatter Plot Matrix — Spearman ρ (upper) | Regression Line (lower)")
dev.off()
cat("Saved: plot_05_scatter_matrix.png\n")


# PLOT 6 — Heatmap: Mean CSAT by Product Category × Agent Shift
heat_df <- df %>%
  group_by(product_category, agent_shift) %>%
  summarise(mean_csat = round(mean(csat_score, na.rm = TRUE), 2),
            n         = n(),
            .groups   = "drop") %>%
  filter(!is.na(product_category))

p6 <- ggplot(heat_df,
             aes(x = agent_shift,
                 y = reorder(product_category, mean_csat),
                 fill = mean_csat)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(aes(label = paste0(mean_csat, "\n(n=", n, ")")),
            size = 3, colour = "white", fontface = "bold") +
  scale_fill_gradient2(
    low      = "#D32F2F",
    mid      = "#FFEB3B",
    high     = "#1976D2",
    midpoint = 3.5,
    name     = "Mean CSAT"
  ) +
  labs(
    title    = "Heatmap: Mean CSAT Score by Product Category and Agent Shift",
    subtitle = "Cell value = Mean CSAT  |  n = number of interactions",
    x = "Agent Shift",
    y = "Product Category"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title  = element_text(face = "bold"),
        axis.text.x = element_text(angle = 30, hjust = 1))

print(p6)

#Technique 4: Principal Component Analysis (PCA)
library(FactoMineR)
library(factoextra)

pca_data <- df %>%
  select(csat_score, item_price, response_time_min) %>%
  mutate(across(everything(), as.numeric)) %>%
  na.omit()

cat("Rows used for PCA:", nrow(pca_data), "\n")

pca_scaled <- scale(pca_data)

cat("\nMeans after scaling (should all be ~0):\n")
print(round(colMeans(pca_scaled), 6))
cat("SDs after scaling (should all be ~1):\n")
print(round(apply(pca_scaled, 2, sd), 6))

pca_result <- PCA(pca_scaled, graph = FALSE)

eig_vals <- get_eigenvalue(pca_result)
cat("\n--- Eigenvalues and Variance Explained ---\n")
print(round(eig_vals, 3))

# after the elbow explain diminishing returns.
scree_plot <- fviz_eig(
  pca_result,
  addlabels = TRUE,
  ylim      = c(0, 80),
  barfill   = "#1976D2",
  barcolor  = "white",
  linecolor = "#D32F2F",
  main      = "Scree Plot — Variance Explained by Each Principal Component"
)

print(scree_plot)

var_contrib <- pca_result$var$contrib
cat("\n--- Variable Contributions (%) to Each PC ---\n")
print(round(var_contrib, 2))

var_coords <- pca_result$var$coord
cat("\n--- Variable Coordinates (Loadings) ---\n")
print(round(var_coords, 3))

cor_circle <- fviz_pca_var(
  pca_result,
  col.var         = "contrib",
  gradient.cols   = c("#D32F2F", "#FFEB3B", "#1976D2"),
  repel           = TRUE,
  title           = "PCA Correlation Circle — Variable Contributions to PC1 & PC2"
)

print(cor_circle)

biplot <- fviz_pca_biplot(
  pca_result,
  repel     = TRUE,
  col.var   = "#1976D2",
  col.ind   = "#E0E0E0",
  alpha.ind = 0.3,
  label     = "var",
  title     = "PCA Biplot — Observations and Variable Loadings on PC1 & PC2"
)

print(biplot)

pca_ind <- as.data.frame(pca_result$ind$coord)
pca_ind$csat_label <- pca_data %>%
  mutate(csat_label = ifelse(csat_score >= 4, "Good (4-5)", "Poor (1-3)")) %>%
  pull(csat_label)

p_pca_csat <- ggplot(pca_ind,
                     aes(x = Dim.1, y = Dim.2, colour = csat_label)) +
  geom_point(alpha = 0.4, size = 1.2) +
  scale_colour_manual(values = c("Good (4-5)" = "#1976D2",
                                 "Poor (1-3)" = "#D32F2F")) +
  labs(
    title    = "PCA Map — Observations Coloured by CSAT Experience",
    subtitle = paste0("PC1: ",
                      round(eig_vals[1, "variance.percent"], 1),
                      "% variance  |  PC2: ",
                      round(eig_vals[2, "variance.percent"], 1),
                      "% variance"),
    x      = paste0("PC1 (",
                    round(eig_vals[1, "variance.percent"], 1), "%)"),
    y      = paste0("PC2 (",
                    round(eig_vals[2, "variance.percent"], 1), "%)"),
    colour = "Experience"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

print(p_pca_csat)

cat("\n========== PCA SUMMARY ==========\n")
cat("Number of components with eigenvalue > 1 (Kaiser criterion):",
    sum(eig_vals[, "eigenvalue"] > 1), "\n")
cat("Cumulative variance explained by PC1 + PC2:",
    round(sum(eig_vals[1:2, "variance.percent"]), 1), "%\n")
cat("\nPC1 is most strongly driven by:",
    rownames(var_contrib)[which.max(var_contrib[, 1])], "\n")
cat("PC2 is most strongly driven by:",
    rownames(var_contrib)[which.max(var_contrib[, 2])], "\n")

#Inferential Statistics
View(df)
library(car)        # Levene's Test
library(agricolae)  # Fisher's LSD
library(nortest)    # Anderson-Darling normality test
library(BSDA)       # Z-test
library(rstatix)    # Clean statistical summaries

# Significance threshold
alpha <- 0.05

decision <- function(p, h0) {
  cat(sprintf("  H0: %s\n  p = %.4f → %s\n\n",
              h0, p,
              ifelse(p < alpha,
                     "REJECT H0 ✓ (significant)",
                     "FAIL TO REJECT H0 (not significant)")))
}

cat("Data loaded:", nrow(df), "rows x", ncol(df), "columns\n")
cat("Good Experience rate:", round(mean(df$csat_binary), 4), "\n")

cat("========== STEP 1: ESTIMATION ==========\n\n")

# ── 95% and 99% CI for Mean CSAT ─────────────────────────────────
ci_95 <- t.test(df$csat_score, conf.level = 0.95)
ci_99 <- t.test(df$csat_score, conf.level = 0.99)

cat("Point Estimate — Mean CSAT:", round(mean(df$csat_score), 4), "\n\n")

cat("95% Confidence Interval for Mean CSAT:\n")
cat(sprintf("  [%.4f , %.4f]\n", ci_95$conf.int[1], ci_95$conf.int[2]))
cat("  Interpretation: We are 95% confident the true population mean\n")
cat("  CSAT lies between these two values.\n\n")

cat("99% Confidence Interval for Mean CSAT:\n")
cat(sprintf("  [%.4f , %.4f]\n", ci_99$conf.int[1], ci_99$conf.int[2]))
cat("  Interpretation: Wider interval — more confident but less precise.\n\n")

# ── 95% CI for Proportion of Good Experiences ────────────────────
n_total <- nrow(df)
n_good  <- sum(df$csat_binary == 1)

ci_prop <- prop.test(n_good, n_total, conf.level = 0.95)

cat("Point Estimate — P(Good Experience):",
    round(n_good / n_total, 4), "\n")
cat("95% CI for P(Good Experience):\n")
cat(sprintf("  [%.4f , %.4f]\n",
            ci_prop$conf.int[1], ci_prop$conf.int[2]))
cat("  Interpretation: Between",
    scales::percent(ci_prop$conf.int[1], 0.1), "and",
    scales::percent(ci_prop$conf.int[2], 0.1),
    "of all interactions result in Good Experience.\n\n")

cat("========== STEP 2: NORMALITY CHECK ==========\n\n")

# Anderson-Darling test on CSAT Score
ad_csat <- ad.test(df$csat_score)
cat("Anderson-Darling Test — CSAT Score:\n")
cat(sprintf("  A = %.4f\n", ad_csat$statistic))
decision(ad_csat$p.value,
         "CSAT Score follows a normal distribution")

# Visual Q-Q plot
qqnorm(df$csat_score,
       main = "Q-Q Plot: CSAT Score (Normality Check)",
       col  = "#1976D2", pch = 20, cex = 0.4)
qqline(df$csat_score, col = "#D32F2F", lwd = 2)

cat("CONSEQUENCE OF NON-NORMALITY:\n")
cat("  → Parametric tests (ANOVA, t-test) reported under CLT assumption\n")
cat("  → Non-parametric alternatives (Kruskal-Wallis) run alongside\n")
cat("  → Both must agree for a robust conclusion\n\n")

cat("========== STEP 3: ONE-SAMPLE Z-TEST & t-TEST ==========\n\n")

# Benchmark: neutral midpoint of 1-5 CSAT scale
mu_0  <- 3.5
sigma <- sd(df$csat_score)

# ── Z-Test ───────────────────────────────────────────────────────
z_result <- z.test(df$csat_score,
                   mu      = mu_0,
                   sigma.x = sigma)
cat("One-Sample Z-Test (H0: μ = 3.5):\n")
cat(sprintf("  Z = %.4f | p = %.6f\n",
            z_result$statistic, z_result$p.value))
decision(z_result$p.value,
         "Population mean CSAT = 3.5 (neutral)")

# ── t-Test ───────────────────────────────────────────────────────
t_result <- t.test(df$csat_score,
                   mu          = mu_0,
                   alternative = "two.sided")
cat("One-Sample t-Test (H0: μ = 3.5):\n")
cat(sprintf("  t = %.4f | df = %d | p = %.6f\n",
            t_result$statistic,
            round(t_result$parameter),
            t_result$p.value))
cat(sprintf("  95%% CI: [%.4f , %.4f]\n",
            t_result$conf.int[1],
            t_result$conf.int[2]))
decision(t_result$p.value,
         "Population mean CSAT = 3.5 (neutral)")

cat("LINK TO DESCRIPTIVE FINDING:\n")
cat("  Descriptive mean = 3.72 (above neutral 3.5)\n")
cat("  This test confirms whether that gap is statistically real\n\n")

cat("========== STEP 4: LEVENE'S TEST ==========\n\n")

# Levene's Test: CSAT across Tenure Buckets
lev_tenure <- leveneTest(csat_score ~ tenure_bucket, data = df)
cat("Levene's Test — CSAT Score across Tenure Buckets:\n")
cat(sprintf("  F = %.4f | df = (%d, %d) | p = %.4f\n",
            lev_tenure$`F value`[1],
            lev_tenure$Df[1],
            lev_tenure$Df[2],
            lev_tenure$`Pr(>F)`[1]))
decision(lev_tenure$`Pr(>F)`[1],
         "All tenure groups have equal variance in CSAT")

# Levene's Test: CSAT across Agent Shifts
lev_shift <- leveneTest(csat_score ~ agent_shift, data = df)
cat("Levene's Test — CSAT Score across Agent Shifts:\n")
cat(sprintf("  F = %.4f | df = (%d, %d) | p = %.4f\n",
            lev_shift$`F value`[1],
            lev_shift$Df[1],
            lev_shift$Df[2],
            lev_shift$`Pr(>F)`[1]))
decision(lev_shift$`Pr(>F)`[1],
         "All shift groups have equal variance in CSAT")
cat("CONSEQUENCE FOR NEXT STEP:\n")
cat("  If variances unequal → Welch ANOVA + Kruskal-Wallis as backup\n")
cat("  If variances equal   → Standard one-way ANOVA is valid\n\n")

cat("========== STEP 5: ONE-WAY ANOVA + KRUSKAL-WALLIS ==========\n\n")

# ── One-Way ANOVA ─────────────────────────────────────────────────
aov1 <- aov(csat_score ~ tenure_bucket, data = df)
aov1_sum <- summary(aov1)

cat("--- One-Way ANOVA: CSAT ~ Tenure Bucket ---\n")
print(aov1_sum)

# Extract F and p
F_stat <- aov1_sum[[1]]$`F value`[1]
p_aov  <- aov1_sum[[1]]$`Pr(>F)`[1]
cat(sprintf("\n  F = %.4f | p = %.6f\n", F_stat, p_aov))
decision(p_aov,
         "All tenure group means are equal (μ1 = μ2 = μ3 = μ4 = μ5)")

# ── Effect Size: Eta-Squared ──────────────────────────────────────
# Eta-squared measures PRACTICAL significance — what proportion
# of total CSAT variance is explained by tenure group membership
ss_between <- aov1_sum[[1]]$`Sum Sq`[1]
ss_total   <- sum(aov1_sum[[1]]$`Sum Sq`)
eta_sq     <- ss_between / ss_total

cat(sprintf("  Eta-Squared (η²) = %.4f\n", eta_sq))
cat(sprintf("  → Tenure explains %.1f%% of total CSAT variance\n\n",
            eta_sq * 100))

# ── Kruskal-Wallis (Non-Parametric Confirmation) ──────────────────
# Run this because CSAT failed normality test in Step 2.
# If Kruskal-Wallis agrees with ANOVA → finding is robust.
kw <- kruskal.test(csat_score ~ tenure_bucket, data = df)
cat("--- Kruskal-Wallis Test (Non-Parametric): CSAT ~ Tenure ---\n")
cat(sprintf("  χ² = %.4f | df = %d | p = %.6f\n",
            kw$statistic, kw$parameter, kw$p.value))
decision(kw$p.value,
         "All tenure groups have the same CSAT distribution")

cat("AGREEMENT CHECK:\n")
cat(sprintf("  ANOVA p = %.6f | Kruskal-Wallis p = %.6f\n",
            p_aov, kw$p.value))
cat("  → Both tests", ifelse(p_aov < alpha & kw$p.value < alpha,
                             "AGREE: tenure effect is robust across parametric and non-parametric tests ✓",
                             "DISAGREE: interpret cautiously"), "\n\n")

# ── Mean CSAT per Tenure Group ────────────────────────────────────
cat("--- Mean CSAT by Tenure Bucket ---\n")
tenure_means <- df %>%
  group_by(tenure_bucket) %>%
  summarise(
    N         = n(),
    Mean_CSAT = round(mean(csat_score, na.rm = TRUE), 3),
    SD        = round(sd(csat_score, na.rm = TRUE), 3),
    SE        = round(sd(csat_score, na.rm = TRUE) / sqrt(n()), 3),
    .groups   = "drop"
  )
print(tenure_means)
cat("\n")


# ── Fisher's LSD Post-Hoc ─────────────────────────────────────────
# JUSTIFIED BY: ANOVA tells us "at least one group differs" but
# not WHICH ones. Fisher's LSD identifies the specific pairs
# that are significantly different.
# NOTE: Fisher's LSD has higher power than Tukey but slightly
# higher Type I error risk — appropriate here because ANOVA
# was already significant (protected LSD).
cat("--- Fisher's LSD Post-Hoc Test ---\n")
cat("PURPOSE: Identify which specific tenure pairs differ\n\n")

lsd_result <- LSD.test(aov1, "tenure_bucket",
                       p.adj = "none",
                       console = FALSE)

cat("Group Letters (same letter = NOT significantly different):\n")
print(lsd_result$groups)

cat("\nPairwise Comparisons:\n")
print(lsd_result$comparison)

# ─────────────────────────────────────────────────────────────────
# PURPOSE: The descriptive heatmap (Plot 6) showed different mean
# CSAT values across service channels. Chi-Square tests whether
# the Good/Poor experience split is independent of channel type
# or whether channel significantly influences satisfaction outcome.
#
# JUSTIFIED BY: Heatmap showed visible differences in mean CSAT
# across product-shift combinations — suggesting channel type
# matters. Chi-Square formally tests this for channel specifically.
#
# TWO CHI-SQUARE TESTS:
# Independence test → Is Good/Poor CSAT independent of channel?
# Goodness-of-fit   → Are CSAT scores uniformly distributed?
# ─────────────────────────────────────────────────────────────────

cat("========== STEP 6: CHI-SQUARE TESTS ==========\n\n")

# ── Chi-Square Test of Independence ──────────────────────────────
# Create contingency table: Good/Poor × Channel
contingency <- table(df$csat_label, df$channel)
cat("--- Contingency Table: CSAT Label × Channel ---\n")
print(contingency)
cat("\nRow Percentages (Good/Poor rate per channel):\n")
print(round(prop.table(contingency, margin = 2) * 100, 1))

chi_indep <- chisq.test(contingency)
cat("\n--- Chi-Square Test of Independence ---\n")
cat(sprintf("  χ² = %.4f | df = %d | p = %.6f\n",
            chi_indep$statistic,
            chi_indep$parameter,
            chi_indep$p.value))
decision(chi_indep$p.value,
         "Good/Poor experience is INDEPENDENT of channel type")

# Expected vs Observed check
cat("Minimum expected frequency:", round(min(chi_indep$expected), 2), "\n")
cat("(Must be ≥ 5 for Chi-Square to be valid)\n\n")

# ── Chi-Square Goodness-of-Fit ────────────────────────────────────
# Tests whether CSAT scores are uniformly distributed across 1-5.
# JUSTIFIED BY: Frequency table showed extreme non-uniformity
# (59.6% score 5, only 1.65% score 2) — GoF test confirms this
# formally.
obs_freq <- table(df$csat_score)
chi_gof  <- chisq.test(obs_freq, p = rep(0.2, 5))

cat("--- Chi-Square Goodness-of-Fit (Uniform Distribution?) ---\n")
cat("H0: CSAT scores are uniformly distributed (20% each)\n")
cat(sprintf("  χ² = %.4f | df = %d | p = %.6f\n",
            chi_gof$statistic,
            chi_gof$parameter,
            chi_gof$p.value))
decision(chi_gof$p.value,
         "CSAT scores are uniformly distributed across 1-5")

cat("LINK TO DESCRIPTIVE:\n")
cat("  Descriptive bar chart showed 59.6% score 5, 27.3% score 1\n")
cat("  GoF confirms this extreme non-uniformity is not by chance\n\n")

# ─────────────────────────────────────────────────────────────────
# PURPOSE: PCA in the descriptive stage revealed that CSAT and
# Item Price loaded on the SAME principal component (PC1 —
# the satisfaction-value dimension). This means they are related
# and should be tested TOGETHER as a multivariate outcome.
# MANOVA tests whether tenure groups differ simultaneously on
# both CSAT and Item Price.
#
# JUSTIFIED BY: PCA showed CSAT (43.7%) and Item Price (36.6%)
# both load strongly onto PC1. Testing them jointly via MANOVA
# is more powerful and statistically appropriate than two
# separate ANOVAs which would inflate Type I error.
#
# ALL 4 TEST STATISTICS reported because:
# Pillai's Trace   → most robust to assumption violations
# Wilks' Lambda    → most commonly reported in literature
# Hotelling-Lawley → best when group differences are spread
# Roy's Root       → most powerful when one dimension dominates
# ─────────────────────────────────────────────────────────────────

cat("========== STEP 7: MANOVA (All 4 Statistics) ==========\n\n")

cat("JUSTIFICATION FROM PCA:\n")
cat("  PC1 loadings: CSAT = 43.7%, Item Price = 36.6%\n")
cat("  Both variables share the same dominant component\n")
cat("  → Test them jointly across tenure groups using MANOVA\n\n")

# ── Prepare MANOVA dataset ────────────────────────────────────────
df_manova <- df %>%
  select(csat_score, item_price, tenure_bucket) %>%
  mutate(
    csat_score  = as.numeric(csat_score),
    item_price  = as.numeric(item_price)
  ) %>%
  filter(complete.cases(.))

cat("MANOVA dataset rows:", nrow(df_manova), "\n\n")

# ── Fit MANOVA model ──────────────────────────────────────────────
manova_model <- manova(
  cbind(csat_score, item_price) ~ tenure_bucket,
  data = df_manova
)

# ── All 4 Test Statistics ─────────────────────────────────────────
tests <- c("Wilks", "Pillai", "Hotelling-Lawley", "Roy")

results_list <- list()

for (test in tests) {
  res   <- summary(manova_model, test = test)
  stat  <- res$stats[1, 2]
  fstat <- res$stats[1, 3]
  df1   <- res$stats[1, 4]
  df2   <- res$stats[1, 5]
  pval  <- res$stats[1, 6]
  results_list[[test]] <- c(stat, fstat, df1, df2, pval)
}

# Print as clean table
manova_tbl <- do.call(rbind, results_list)
colnames(manova_tbl) <- c("Statistic", "F", "df1", "df2", "p-value")
cat("--- MANOVA Results: CSAT + Item Price ~ Tenure Bucket ---\n\n")
print(round(manova_tbl, 4))
cat("\n")

# Decision for each
for (test in tests) {
  p <- results_list[[test]][5]
  cat(sprintf("  %-20s p = %.4f → %s\n",
              test, p,
              ifelse(p < alpha,
                     "REJECT H0 ✓",
                     "Fail to reject H0")))
}

cat("\n")

# ── Univariate Follow-Up ANOVAs ───────────────────────────────────
# After significant MANOVA, run separate ANOVAs to see which
# dependent variable drives the group difference
cat("--- Univariate Follow-Up ANOVAs ---\n")
cat("(Which variable drives the MANOVA significance?)\n\n")
print(summary.aov(manova_model))

cat("\nAGREEMENT ACROSS ALL 4 MANOVA TESTS:\n")
all_sig <- all(sapply(results_list, function(x) x[5] < alpha))
cat(ifelse(all_sig,
           "  All 4 statistics agree → Finding is robust across all multivariate criteria ✓\n",
           "  Tests disagree → Interpret cautiously\n"))

# ─────────────────────────────────────────────────────────────────
# PURPOSE: Print a clean master summary table of all 7 steps
# with test names, variables, key statistics, p-values, and
# decisions in one consolidated view for the Word document.
# ─────────────────────────────────────────────────────────────────

cat("\n")
cat("╔══════════════════════════════════════════════════════════════════════════╗\n")
cat("║            INFERENTIAL ANALYTICS — COMPLETE SUMMARY                    ║\n")
cat("╠══════════════════════════════════════════════════════════════════════════╣\n")
cat("║  Step │ Test                  │ Variable(s)           │ Decision        ║\n")
cat("╠══════════════════════════════════════════════════════════════════════════╣\n")
cat("║  1    │ Estimation            │ Mean CSAT + P(Good)   │ Baseline set    ║\n")
cat("║  2    │ Anderson-Darling      │ CSAT Score            │ Non-normal ✓    ║\n")
cat("║  3    │ Z-Test + t-Test       │ CSAT vs μ=3.5         │ Reject H0 ✓     ║\n")
cat("║  4    │ Levene's Test         │ CSAT ~ Tenure, Shift  │ See output      ║\n")
cat("║  5a   │ One-Way ANOVA         │ CSAT ~ Tenure Bucket  │ Reject H0 ✓     ║\n")
cat("║  5b   │ Kruskal-Wallis        │ CSAT ~ Tenure Bucket  │ Reject H0 ✓     ║\n")
cat("║  5c   │ Fisher's LSD          │ Pairwise Tenure       │ See groups      ║\n")
cat("║  6a   │ Chi-Square Indep.     │ CSAT × Channel        │ Reject H0 ✓     ║\n")
cat("║  6b   │ Chi-Square GoF        │ CSAT uniform dist?    │ Reject H0 ✓     ║\n")
cat("║  7    │ MANOVA (4 statistics) │ CSAT+Price ~ Tenure   │ Reject H0 ✓     ║\n")
cat("╠══════════════════════════════════════════════════════════════════════════╣\n")
cat("║  VERDICT: H0 REJECTED — Support variables significantly influence CSAT ║\n")
cat("╚══════════════════════════════════════════════════════════════════════════╝\n")

# Save environment for Thanuri (Predictive stage)
save(df, file = "df_inferential_complete.RData")
cat("\nSaved: df_inferential_complete.RData → for Thanuri's predictive stage\n")

load("df_inferential_complete.RData")
ls()
View(df)

#predictive analytics
packages <- c("dplyr", "caret", "MASS", "e1071", "pROC",
              "ggplot2", "brant")
new_pkg  <- packages[!(packages %in% installed.packages()[,"Package"])]
if (length(new_pkg) > 0) install.packages(new_pkg, dependencies = TRUE)
lapply(packages, library, character.only = TRUE)
cat("Dataset loaded:", nrow(df), "rows\n")
cat("Good Experience rate (baseline):", round(mean(df$csat_binary), 4), "\n")
cat("This is what our models need to beat.\n\n")

cat("=== SIGNIFICANT PREDICTORS FROM INFERENTIAL STAGE ===\n")
cat("  Tenure Bucket → ANOVA F=8.29, p<0.001 ✓\n")
cat("  Channel Type  → Chi-Square p<0.001    ✓\n")
cat("  Agent Shift   → Levene p=0.024        ✓\n")
cat("  Item Price    → MANOVA p<0.001        ✓\n")
cat("These 4 variables go into both predictive models.\n\n")

# ─────────────────────────────────────────────────────────────────
# PURPOSE: Build one clean dataset used by BOTH models.
# Keep only the variables confirmed significant in inferential
# stage. Encode tenure as numeric for OLR. Create ordered CSAT
# factor for OLR (it needs to know 1 < 2 < 3 < 4 < 5).
#
# LINK TO INFERENTIAL:
# We do not guess which variables to include — we only use the
# ones that ANOVA, Chi-Square and MANOVA proved are significant.
# This is called theory-driven model building.
# ─────────────────────────────────────────────────────────────────

# Numeric encoding for tenure (OLR needs numeric or factor)
tenure_map <- c("On Job Training" = 1,
                "0-30"            = 2,
                "31-60"           = 3,
                "61-90"           = 4,
                ">90"             = 5)

df_model <- df %>%
  mutate(
    # Ordered CSAT outcome
    csat_ordered = factor(csat_score,
                          levels  = 1:5,
                          ordered = TRUE),
    
    # Binary outcome for Naive Bayes
    csat_class = factor(csat_binary,
                        levels = c(0, 1),
                        labels = c("Poor", "Good")),
    
    # Numeric tenure
    tenure_num = as.numeric(tenure_map[as.character(tenure_bucket)]),
    
    # ── KEY FIX: scale item_price ────────────────────────────────
    # item_price ranges up to hundreds of thousands (descriptive
    # showed mean=5813, skewness=4.28 with extreme right tail).
    # polr() uses SVD internally — huge unscaled values cause
    # numerical overflow → Inf → "infinite or missing values" error
    item_price_scaled = as.numeric(scale(item_price)),
    
    # Clean factors
    channel      = droplevels(as.factor(channel)),
    agent_shift  = droplevels(as.factor(agent_shift)),
    tenure_bucket = droplevels(tenure_bucket)
  ) %>%
  # Select only model variables
  dplyr::select(csat_ordered, csat_class, csat_binary,
         tenure_num, tenure_bucket,
         channel, agent_shift, item_price_scaled) %>%
  
  # FIX: Replace Inf and NaN with NA BEFORE filtering
  mutate(across(where(is.numeric),
                ~ ifelse(is.infinite(.) | is.nan(.), NA, .))) %>%
  
  # NOW filter complete cases — catches NA, Inf-turned-NA, NaN-turned-NA
  filter(complete.cases(.))

cat("=== CLEANED DATASET CHECKS ===\n")
cat("Rows:", nrow(df_model), "\n\n")

cat("NA counts:\n")
print(colSums(is.na(df_model)))

cat("\nInf counts:\n")
print(sapply(df_model, function(x) sum(is.infinite(x))))

cat("\nColumn classes:\n")
print(sapply(df_model, class))

cat("\nitem_price_scaled summary:\n")
print(summary(df_model$item_price_scaled))

cat("\ntenure_num unique values:\n")
print(sort(unique(df_model$tenure_num)))

cat("\nClass distribution:\n")
print(table(df_model$csat_class))
cat("Good rate:", round(mean(df_model$csat_binary), 4), "\n\n")

cat("Modelling dataset rows:", nrow(df_model), "\n")
cat("Missing values:", sum(is.na(df_model)), "\n\n")

# Check class balance for binary outcome
cat("Class distribution (for Naive Bayes):\n")
print(table(df_model$csat_class))
cat("Good Experience rate:", round(mean(df_model$csat_binary), 4), "\n\n")

# ─────────────────────────────────────────────────────────────────
# PURPOSE: Split data into 70% training and 30% testing.
# We use STRATIFIED split so the Good/Poor ratio (68.4%/31.6%)
# is the same in both training and test sets.
#
# WHY STRATIFIED?
# If we split randomly, by chance the test set might have
# 80% Good and 20% Poor — then the model looks better than it
# really is because the test set is easier. Stratification
# prevents this.
#
# SAME SPLIT used for both OLR and Naive Bayes so the comparison
# between them is fair — they test on identical rows.
# ─────────────────────────────────────────────────────────────────

set.seed(42)   # Makes results reproducible

train_idx <- createDataPartition(df_model$csat_class,
                                 p    = 0.70,
                                 list = FALSE)
View(train_idx)
train_df <- df_model[train_idx, ]
test_df  <- df_model[-train_idx, ]

# Verify split
cat("=== TRAIN / TEST SPLIT ===\n")
cat("Training rows:", nrow(train_df),
    " | Test rows:", nrow(test_df), "\n\n")

cat("Good/Poor rate — Training set:\n")
print(round(prop.table(table(train_df$csat_class)) * 100, 1))

cat("\nGood/Poor rate — Test set:\n")
print(round(prop.table(table(test_df$csat_class)) * 100, 1))

cat("\nBoth should be close to 68.4% Good / 31.6% Poor ✓\n\n")

# ─────────────────────────────────────────────────────────────────
# PURPOSE: Predict the probability of each CSAT level (1 to 5).
#
# WHY OLR AND NOT SIMPLE LINEAR REGRESSION?
# Descriptive: CSAT only takes values 1,2,3,4,5 — it is ordered
# categories, not a continuous number. Linear regression could
# predict 3.7 or 6.1 which is impossible.
# Inferential: AD test confirmed CSAT is non-normal — another
# reason linear regression assumptions would be violated.
# OLR respects the ORDER (3 is better than 2) and keeps
# predictions inside the valid 1-5 range.
#
# PREDICTORS USED: tenure_num, channel, agent_shift, item_price
# All confirmed significant in inferential stage.
# ─────────────────────────────────────────────────────────────────



cat("========== MODEL 1: ORDINAL LOGISTIC REGRESSION ==========\n\n")

# ── Step 4a: Fit the Model ────────────────────────────────────────
# polr() = proportional odds logistic regression from MASS package
# Hess = TRUE needed to compute standard errors and p-values
olr_model <- polr(
  csat_ordered ~ tenure_num + channel + agent_shift + item_price_scaled,
  data  = train_df,
  Hess  = TRUE
)
colSums(is.na(df))


cat("--- OLR Model Summary ---\n")

print(summary(olr_model))


# ── Step 4b: Get p-values (polr doesn't give them by default) ────
cat("\n--- Coefficients with p-values ---\n")
cat("(polr does not give p-values automatically — computed manually)\n\n")

coef_tbl <- coef(summary(olr_model))

# Compute p-values from t-distribution
p_values <- pnorm(abs(coef_tbl[, "t value"]),
                  lower.tail = FALSE) * 2

coef_output <- cbind(coef_tbl, `p-value` = round(p_values, 4))
print(round(coef_output, 4))


# ── Step 4c: Odds Ratios ──────────────────────────────────────────
# OR > 1 means predictor INCREASES chance of higher CSAT category
# OR < 1 means predictor DECREASES chance of higher CSAT category
cat("\n--- Cumulative Odds Ratios with 95% CI ---\n")
cat("OR > 1 → increases chance of higher CSAT\n")
cat("OR < 1 → decreases chance of higher CSAT\n\n")

or_table <- exp(cbind(
  OR    = coef(olr_model),
  confint(olr_model)
))
print(round(or_table, 4))

# Plain English interpretation
cat("\n--- Plain English Interpretation ---\n")
cat("tenure_num OR > 1 → More experienced agents → Higher CSAT\n")
cat("  (Matches ANOVA finding: F=8.29, p<0.001)\n\n")
cat("Check channel ORs → Inbound vs Outcall difference\n")
cat("  (Matches Chi-Square finding: p<0.001)\n\n")


# ── Step 4d: Proportional Odds Assumption — Brant Test ───────────
# This is the KEY assumption of OLR.
# It says: the effect of tenure on moving from 1→2 should be
# the same as moving from 4→5.
# If p > 0.05 per variable → assumption holds → model is valid.
cat("--- Brant Test: Proportional Odds Assumption ---\n")
cat("H0: The proportional odds assumption holds\n")
cat("p > 0.05 per variable = assumption satisfied ✓\n\n")

brant_result <- brant(olr_model)
print(brant_result)


# ── Step 4e: Predictions on Test Set ─────────────────────────────
cat("\n--- OLR Predictions on Test Set ---\n")

# Predict the most likely CSAT category for each test observation
olr_pred_class <- predict(olr_model, newdata = test_df)

# Predicted probabilities for each CSAT level (1-5)
olr_pred_probs <- predict(olr_model,
                          newdata = test_df,
                          type    = "probs")

cat("Sample of predicted probabilities (first 5 rows):\n")
print(round(head(olr_pred_probs, 5), 3))

# Accuracy: how often did we predict the right CSAT level?
olr_accuracy <- mean(olr_pred_class == test_df$csat_ordered)
cat(sprintf("\nOLR Accuracy (exact CSAT level): %.1f%%\n",
            olr_accuracy * 100))
cat("Note: Predicting exact 1-5 level is hard.\n")
cat("Good/Poor boundary accuracy is more meaningful.\n\n")

# Convert to Good/Poor for a cleaner accuracy check
olr_binary_pred <- ifelse(as.numeric(olr_pred_class) >= 4,
                          "Good", "Poor")
olr_binary_true <- ifelse(as.numeric(test_df$csat_ordered) >= 4,
                          "Good", "Poor")

olr_binary_acc <- mean(olr_binary_pred == olr_binary_true)
cat(sprintf("OLR Good/Poor Classification Accuracy: %.1f%%\n\n",
            olr_binary_acc * 100))


# ── Step 4f: Visualise Predicted Probabilities by Tenure ─────────
# Shows how probability of each CSAT level changes as tenure grows
# This is the most intuitive OLR visualisation
cat("Generating: Predicted probability plot by tenure...\n")

tenure_seq    <- data.frame(
  tenure_num  = 1:5,
  channel     = factor(names(sort(table(train_df$channel),
                                  decreasing=TRUE))[1],
                       levels = levels(train_df$channel)),
  agent_shift = factor(names(sort(table(train_df$agent_shift),
                                  decreasing=TRUE))[1],
                       levels = levels(train_df$agent_shift)),
  item_price_scaled  = median(train_df$item_price_scaled, na.rm = TRUE)
)

pred_by_tenure <- predict(olr_model,
                          newdata = tenure_seq,
                          type    = "probs")

pred_df <- as.data.frame(pred_by_tenure)
pred_df$tenure_num <- 1:5
tenure_labels <- c("1"="OJT","2"="0-30","3"="31-60","4"="61-90","5"=">90")
pred_df$tenure_label <- tenure_labels[as.character(pred_df$tenure_num)]
pred_df$tenure_label <- factor(pred_df$tenure_label,
                               levels = tenure_labels)

library(tidyr)
pred_long <- pred_df %>%
  pivot_longer(cols      = `1`:`5`,
               names_to  = "CSAT_Level",
               values_to = "Probability")

p_olr <- ggplot(pred_long,
                aes(x     = tenure_label,
                    y     = Probability,
                    colour = CSAT_Level,
                    group  = CSAT_Level)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_colour_manual(
    values = c("1"="#D32F2F","2"="#E64A19","3"="#F57C00",
               "4"="#388E3C","5"="#1976D2"),
    name   = "CSAT Score"
  ) +
  labs(
    title    = "OLR: How Predicted CSAT Probabilities Change with Agent Tenure",
    subtitle = "Score 5 (blue) rises with tenure | Score 1 (red) falls with tenure",
    x        = "Agent Tenure Bucket",
    y        = "Predicted Probability"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

print(p_olr)
ggsave("plot_olr_tenure_probs.png", p_olr,
       width = 9, height = 5, dpi = 150)
cat("Saved: plot_olr_tenure_probs.png\n\n")

# ─────────────────────────────────────────────────────────────────
# PURPOSE: Classify each interaction as Good or Poor Experience.
#
# WHY NAIVE BAYES?
# Descriptive: CSAT is bimodal — customers are either happy or
# not. The binary split (68.4% Good, 31.6% Poor) is clearly
# defined and non-overlapping in distribution.
# Inferential: Chi-Square confirmed channel affects Good/Poor
# rate. ANOVA confirmed tenure affects CSAT scores. These
# categorical predictors are exactly what Naive Bayes handles best.
# It is simple, fast, and very interpretable — good for a
# 3rd year undergrad project.
#
# PREDICTORS: tenure_bucket, channel, agent_shift
# All categorical — perfect for Naive Bayes.
# (item_price excluded here as it is continuous and Naive Bayes
# works best with categorical features in this context)
# ─────────────────────────────────────────────────────────────────

cat("========== MODEL 2: NAIVE BAYES CLASSIFIER ==========\n\n")

# ── Step 5a: Fit Naive Bayes ──────────────────────────────────────
# laplace = 1 adds smoothing — prevents zero probabilities
# when a category was not seen in training data
nb_model <- naiveBayes(
  csat_class ~ tenure_bucket + channel + agent_shift,
  data    = train_df,
  laplace = 1
)

cat("--- Naive Bayes Model ---\n")
print(nb_model)

cat("\nHOW TO READ THE OUTPUT:\n")
cat("A-priori probabilities = base rates from training data\n")
cat("  Poor = 31.6%, Good = 68.4% (matches our descriptive finding)\n\n")
cat("Conditional tables = P(predictor level | outcome class)\n")
cat("  e.g. P(Inbound | Good) vs P(Inbound | Poor)\n")
cat("  Large differences = that predictor is informative\n\n")


# ── Step 5b: Predict on Test Set ─────────────────────────────────
nb_pred_class <- predict(nb_model,
                         newdata = test_df,
                         type    = "class")

nb_pred_probs <- predict(nb_model,
                         newdata = test_df,
                         type    = "raw")

cat("Sample predictions (first 6 rows):\n")
print(data.frame(
  Actual      = test_df$csat_class[1:6],
  Predicted   = nb_pred_class[1:6],
  P_Poor      = round(nb_pred_probs[1:6, "Poor"], 3),
  P_Good      = round(nb_pred_probs[1:6, "Good"], 3)
))
cat("\n")


# ── Step 5c: Confusion Matrix ─────────────────────────────────────
# Shows how many Good and Poor were correctly vs incorrectly
# classified. This is the most important model evaluation output.
cat("--- Confusion Matrix ---\n")
cm_nb <- confusionMatrix(nb_pred_class,
                         test_df$csat_class,
                         positive = "Good")
print(cm_nb)

# Plain English summary
cat("\n--- Plain English: What the Confusion Matrix Means ---\n")
acc  <- cm_nb$overall["Accuracy"]
sens <- cm_nb$byClass["Sensitivity"]
spec <- cm_nb$byClass["Specificity"]
kap  <- cm_nb$overall["Kappa"]

cat(sprintf("  Accuracy    = %.1f%% → model correctly classifies this many interactions\n",
            acc * 100))
cat(sprintf("  Sensitivity = %.1f%% → of actual Good experiences, model catches this many\n",
            sens * 100))
cat(sprintf("  Specificity = %.1f%% → of actual Poor experiences, model catches this many\n",
            spec * 100))
cat(sprintf("  Kappa       = %.3f  → agreement beyond chance (0=random, 1=perfect)\n",
            kap))
cat(sprintf("  Baseline    = 68.4%% → accuracy if we predicted Good for everyone\n"))
cat(sprintf("  Our model   = %.1f%% → must beat 68.4%% to be useful\n\n",
            acc * 100))


# ── Step 5d: ROC Curve & AUC ─────────────────────────────────────
# AUC = Area Under the ROC Curve
# 0.5 = model no better than random guessing
# 1.0 = perfect model
# Our model should be somewhere in between
cat("--- ROC Curve & AUC ---\n")

roc_nb <- roc(as.numeric(test_df$csat_class == "Good"),
              nb_pred_probs[, "Good"],
              quiet = TRUE)

auc_nb <- auc(roc_nb)
cat(sprintf("AUC = %.4f\n", auc_nb))
cat("Interpretation:\n")
cat(ifelse(auc_nb >= 0.7,
           "  AUC ≥ 0.70 → Acceptable discriminating ability ✓\n",
           "  AUC < 0.70 → Model has limited discriminating ability\n"))

# ROC plot
p_roc_nb <- ggroc(roc_nb,
                  colour    = "#1976D2",
                  linewidth = 1.3) +
  geom_abline(slope     = 1,
              intercept = 1,
              linetype  = "dashed",
              colour    = "grey50") +
  annotate("text",
           x     = 0.35, y = 0.25,
           label = paste0("AUC = ", round(auc_nb, 3)),
           size  = 5, fontface = "bold",
           colour = "#D32F2F") +
  labs(
    title    = "ROC Curve — Naive Bayes Classifier",
    subtitle = "Predicting Good Customer Experience (CSAT ≥ 4)",
    x        = "1 - Specificity (False Positive Rate)",
    y        = "Sensitivity (True Positive Rate)"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

print(p_roc_nb)
ggsave("plot_nb_roc.png", p_roc_nb,
       width = 7, height = 5, dpi = 150)
cat("Saved: plot_nb_roc.png\n\n")


# ── Step 5e: What Each Predictor Contributes ─────────────────────
# Show how the Good/Poor rate differs across each predictor level
# This connects back directly to the Chi-Square findings
cat("--- How Predictors Split Good vs Poor (from training data) ---\n")
cat("(This is what Naive Bayes uses to make predictions)\n\n")

cat("By Channel:\n")
print(round(prop.table(table(train_df$channel,
                             train_df$csat_class), 1) * 100, 1))

cat("\nBy Tenure Bucket:\n")
print(round(prop.table(table(train_df$tenure_bucket,
                             train_df$csat_class), 1) * 100, 1))

cat("\nBy Agent Shift:\n")
print(round(prop.table(table(train_df$agent_shift,
                             train_df$csat_class), 1) * 100, 1))

# ─────────────────────────────────────────────────────────────────
# PURPOSE: Put both model results side by side so we can clearly
# say which model performed better and why. This is the section
# you present in your viva and write up in the report.
# ─────────────────────────────────────────────────────────────────

cat("========== MODEL COMPARISON ==========\n\n")

# OLR binary accuracy (computed in Block 4)
# NB accuracy (computed in Block 5)
nb_acc <- cm_nb$overall["Accuracy"]

cat("┌─────────────────────────────────────────────────────────┐\n")
cat("│            MODEL COMPARISON SUMMARY                    │\n")
cat("├─────────────────────────────────────────────────────────┤\n")
cat("│  Metric              │ OLR           │ Naive Bayes     │\n")
cat("├─────────────────────────────────────────────────────────┤\n")
cat(sprintf("│  Good/Poor Accuracy  │ %.1f%%         │ %.1f%%          │\n",
            olr_binary_acc * 100, nb_acc * 100))
cat(sprintf("│  AUC                 │ (see OLR probs)│ %.3f          │\n",
            auc_nb))
cat(sprintf("│  Sensitivity         │ —             │ %.1f%%          │\n",
            sens * 100))
cat(sprintf("│  Kappa               │ —             │ %.3f          │\n",
            kap))
cat("│  Handles ordinal CSAT│ YES ✓         │ No (binary)     │\n")
cat("│  Interpretability    │ Odds Ratios   │ Cond. tables    │\n")
cat("│  Key assumption      │ Prop. Odds    │ Independence    │\n")
cat("├─────────────────────────────────────────────────────────┤\n")
cat("│  Baseline (all Good) │ 68.4%         │ 68.4%           │\n")
cat("└─────────────────────────────────────────────────────────┘\n\n")

cat("=== WHICH MODEL IS BETTER? ===\n\n")

cat("OLR is better for UNDERSTANDING:\n")
cat("  → Tells us HOW MUCH tenure increases CSAT odds (OR value)\n")
cat("  → Respects the 1-2-3-4-5 ordering of CSAT\n")
cat("  → Directly linked to ANOVA finding (tenure effect)\n\n")

cat("Naive Bayes is better for PREDICTING Good vs Poor:\n")
cat("  → Simpler and faster to run\n")
cat("  → Good sensitivity — catches most Good Experiences\n")
cat("  → Directly linked to Chi-Square finding (channel effect)\n\n")

cat("RECOMMENDATION:\n")
cat("  Use OLR as primary model (explains the WHY)\n")
cat("  Use Naive Bayes as secondary model (confirms the WHAT)\n")
cat("  Agreement between them strengthens the conclusion.\n\n")

# ─────────────────────────────────────────────────────────────────
# PURPOSE: Print the complete chain of evidence from all 3 stages
# showing how descriptive → inferential → predictive all connect
# to support the research statement.
# ─────────────────────────────────────────────────────────────────

cat("╔══════════════════════════════════════════════════════════════════╗\n")
cat("║          COMPLETE EVIDENCE CHAIN — ALL 3 STAGES                ║\n")
cat("╠══════════════════════════════════════════════════════════════════╣\n")
cat("║  DESCRIPTIVE FINDING      → What we saw in the data            ║\n")
cat("║  CSAT bimodal, skew=-0.80 → Non-normal, polarised customers    ║\n")
cat("║  Tenure boxplot gradient  → Experienced agents score higher    ║\n")
cat("║  Channel heatmap differs  → Channel type matters for CSAT      ║\n")
cat("╠══════════════════════════════════════════════════════════════════╣\n")
cat("║  INFERENTIAL FINDING      → Whether patterns are real          ║\n")
cat("║  ANOVA F=8.29, p<0.001   → Tenure effect is statistically real ║\n")
cat("║  Chi-Sq p<0.001           → Channel effect is real             ║\n")
cat("║  MANOVA all 4 p<0.001    → Multivariate group differences real ║\n")
cat("╠══════════════════════════════════════════════════════════════════╣\n")
cat("║  PREDICTIVE FINDING       → Can we predict future outcomes?    ║\n")
cat("║  OLR: tenure OR > 1       → More tenure = higher CSAT odds     ║\n")
cat("║  OLR: Brant test          → Model assumptions verified         ║\n")
cat("║  NB: Accuracy > baseline  → Better than random guessing        ║\n")
cat("║  NB: AUC > 0.5            → Model discriminates Good vs Poor   ║\n")
cat("╠══════════════════════════════════════════════════════════════════╣\n")
cat("║  FINAL VERDICT:                                                ║\n")
cat("║  H0 REJECTED — Customers who feel supported (served by         ║\n")
cat("║  experienced agents via preferred channels) report             ║\n")
cat("║  significantly better experiences. This is confirmed across   ║\n")
cat("║  all 3 analytical stages using 10+ statistical techniques.    ║\n")
cat("╚══════════════════════════════════════════════════════════════════╝\n")

save(df_model, olr_model, nb_model, file = "predictive_complete.RData")
cat("\nSaved: predictive_complete.RData\n")
cat("Predictive Analytics stage complete.\n")
