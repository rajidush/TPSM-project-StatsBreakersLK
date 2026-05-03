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
    # Ordered factor for tenure
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
df$city[is.na(df$city)] <- mode_val(df$city)
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

# Technique 3: Data Visualisation 
# PLOT 1 — CSAT Score Distribution
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

# PLOT 5 — Scatter Plot Matrix 
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



# PLOT 6 — Mean CSAT by Product Category × Agent Shift
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
  main      = "Scree Plot - Variance Explained by Each Principal Component"
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
library(car)        
library(agricolae) 
library(nortest)    
library(BSDA)       
library(rstatix)    

# Significance threshold
alpha <- 0.05

decision <- function(p, h0) {
  cat(sprintf("  H0: %s\n  p = %.4f → %s\n\n",
              h0, p,
              ifelse(p < alpha,
                     "REJECT H0  (significant)",
                     "FAIL TO REJECT H0 (not significant)")))
}

cat("Data loaded:", nrow(df), "rows x", ncol(df), "columns\n")
cat("Good Experience rate:", round(mean(df$csat_binary), 4), "\n")


cat("========== STEP 1: LEVENE'S TEST ==========\n\n")

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
cat("  If variances unequal --> Welch ANOVA + Kruskal-Wallis as backup\n")
cat("  If variances equal   --> Standard one-way ANOVA is valid\n\n")

cat("========== STEP 2: ONE-WAY ANOVA + KRUSKAL-WALLIS ==========\n\n")

# One-Way ANOVA 
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

# Effect Size: Eta-Squared
ss_between <- aov1_sum[[1]]$`Sum Sq`[1]
ss_total   <- sum(aov1_sum[[1]]$`Sum Sq`)
eta_sq     <- ss_between / ss_total

cat(sprintf("  Eta-Squared (η²) = %.4f\n", eta_sq))
cat(sprintf("  --> Tenure explains %.1f%% of total CSAT variance\n\n",
            eta_sq * 100))

# Kruskal-Wallis (Non-Parametric Confirmation) 
kw <- kruskal.test(csat_score ~ tenure_bucket, data = df)
cat("--- Kruskal-Wallis Test (Non-Parametric): CSAT ~ Tenure ---\n")
cat(sprintf("  χ² = %.4f | df = %d | p = %.6f\n",
            kw$statistic, kw$parameter, kw$p.value))
decision(kw$p.value,
         "All tenure groups have the same CSAT distribution")

cat("AGREEMENT CHECK:\n")
cat(sprintf("  ANOVA p = %.6f | Kruskal-Wallis p = %.6f\n",
            p_aov, kw$p.value))
cat("  --> Both tests", ifelse(p_aov < alpha & kw$p.value < alpha,
                             "AGREE: tenure effect is robust across parametric and non-parametric tests ✓",
                             "DISAGREE: interpret cautiously"), "\n\n")

#Mean CSAT per Tenure Group
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


# Fisher's LSD Post-Hoc 
cat("--- Fisher's LSD Post-Hoc Test ---\n")
cat("PURPOSE: Identify which specific tenure pairs differ\n\n")

lsd_result <- LSD.test(aov1, "tenure_bucket",
                       p.adj = "none",
                       console = FALSE)

cat("Group Letters (same letter = NOT significantly different):\n")
print(lsd_result$groups)

cat("\nPairwise Comparisons:\n")
print(lsd_result$comparison)


cat("========== STEP 3: CHI-SQUARE TESTS ==========\n\n")

# Chi-Square Test of Independence 
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


cat("========== STEP 4: MANOVA (All 4 Statistics) ==========\n\n")

cat("JUSTIFICATION FROM PCA:\n")
cat("  PC1 loadings: CSAT = 43.7%, Item Price = 36.6%\n")
cat("  Both variables share the same dominant component\n")
cat("  --> Test them jointly across tenure groups using MANOVA\n\n")

# Prepare MANOVA dataset 
df_manova <- df %>%
  select(csat_score, item_price, tenure_bucket) %>%
  mutate(
    csat_score  = as.numeric(csat_score),
    item_price  = as.numeric(item_price)
  ) %>%
  filter(complete.cases(.))

cat("MANOVA dataset rows:", nrow(df_manova), "\n\n")

# Fit MANOVA model 
manova_model <- manova(
  cbind(csat_score, item_price) ~ tenure_bucket,
  data = df_manova
)

# All 4 Test Statistics 
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

#predictive analytics
install.packages(c(
  "readxl","dplyr","tidyr","ggplot2","MASS","lmtest",
  "brant","forecast","tseries","e1071","caret"
), repos = "https://cloud.r-project.org")

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(MASS)
library(lmtest)
library(brant)
library(forecast)
library(tseries)
library(e1071)
library(caret)

#load data
df_raw <- readxl::read_excel("dax.xlsx")

names(df_raw)
head(df_raw)
summary(df_raw)

#Clean data
data_clean <- df_raw %>%
  dplyr::select(
    csat_score = `CSAT Score`,
    item_price = Item_price,
    tenure_bucket = `Tenure Bucket`,
    agent_shift = `Agent Shift`,
    channel = channel_name,
    product_cat = Product_category,
    survey_date = Survey_response_Date
  )

head(data_clean)
colSums(is.na(data_clean))

#Convert variables
tenure_map <- c(
  "On Job Training" = 1,
  "0-30" = 2,
  "31-60" = 3,
  "61-90" = 4,
  ">90" = 5
)

data_clean <- data_clean %>%
  dplyr::mutate(
    tenure_num = tenure_map[as.character(tenure_bucket)],
    agent_shift = as.factor(agent_shift),
    channel = as.factor(channel),
    product_cat = as.factor(product_cat)
  )

table(data_clean$tenure_bucket, useNA = "ifany")
table(data_clean$tenure_num, useNA = "ifany")

#model1-Ordinal Logistic Regression
#Prepare OLR data
data_clean$csat_ordered <- factor(
  data_clean$csat_score,
  levels = 1:5,
  ordered = TRUE
)

df_olr <- data_clean %>%
  dplyr::select(csat_ordered, item_price, tenure_num, channel) %>%
  dplyr::filter(!is.na(csat_ordered),
                !is.na(item_price),
                !is.na(tenure_num)) %>%
  dplyr::filter(item_price > 0 & item_price < 50000)

df_olr$item_price_scaled <- as.numeric(scale(df_olr$item_price))

nrow(df_olr)
head(df_olr)

#Fit OLR model
olr_model <- MASS::polr(
  csat_ordered ~ item_price_scaled + tenure_num + channel,
  data = df_olr,
  Hess = TRUE
)

summary(olr_model)

#OLR p-values
coef_pvals <- lmtest::coeftest(olr_model)
coef_pvals

#OLR odds ratios
OR_table <- exp(cbind(
  OR = coef(olr_model),
  confint(olr_model)
))

round(OR_table, 4)

#Brant test
brant_result <- brant::brant(olr_model)
brant_result

#OLR accuracy
olr_pred <- predict(olr_model, type = "class")

table(Actual = df_olr$csat_ordered, Predicted = olr_pred)

olr_accuracy <- mean(
  as.character(olr_pred) == as.character(df_olr$csat_ordered)
)

olr_accuracy
olr_accuracy * 100

#OLR plot
graphics.off()

pred_probs <- predict(olr_model, type = "probs")
pred_df <- cbind(df_olr, as.data.frame(pred_probs))

pred_long <- pred_df %>%
  dplyr::select(tenure_num, `1`, `2`, `3`, `4`, `5`) %>%
  tidyr::pivot_longer(
    cols = `1`:`5`,
    names_to = "CSAT_Level",
    values_to = "Probability"
  )

p_olr <- ggplot(pred_long,
                aes(x = tenure_num,
                    y = Probability,
                    colour = CSAT_Level,
                    group = CSAT_Level)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(
    breaks = c(1,2,3,4,5),
    labels = c("OJT", "0-30", "31-60", "61-90", ">90")
  ) +
  labs(
    title = "Predicted Probability of CSAT Levels by Tenure",
    x = "Tenure Level",
    y = "Probability",
    colour = "CSAT Level"
  ) +
  theme_minimal()

p_olr

#save OLR outputs
ggsave("olr_probability_plot.png", plot = p_olr, width = 9, height = 5, dpi = 300)

coef_pvals_df <- data.frame(
  Variable = rownames(coef_pvals),
  Estimate = coef_pvals[, 1],
  Std_Error = coef_pvals[, 2],
  z_value = coef_pvals[, 3],
  p_value = coef_pvals[, 4],
  row.names = NULL
)

write.csv(coef_pvals_df, "olr_pvalues.csv", row.names = FALSE)

OR_df <- data.frame(
  Variable = rownames(OR_table),
  OR = OR_table[, 1],
  CI_Lower = OR_table[, 2],
  CI_Upper = OR_table[, 3],
  row.names = NULL
)

write.csv(OR_df, "olr_odds_ratios.csv", row.names = FALSE)

olr_accuracy_df <- data.frame(
  Model = "Ordinal Logistic Regression",
  Accuracy = olr_accuracy,
  Accuracy_Percentage = olr_accuracy * 100
)

write.csv(olr_accuracy_df, "olr_accuracy.csv", row.names = FALSE)

#model2-Time Series Analysis
#Check date format
head(df_raw$Survey_response_Date, 20)
unique(df_raw$Survey_response_Date)[1:20]

#Prepare time series data
df_ts <- df_raw %>%
  dplyr::mutate(
    date = as.Date(gsub(" UTC", "", Survey_response_Date))
  ) %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(
    mean_csat = mean(`CSAT Score`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(date)

nrow(df_ts)
head(df_ts)

#Create time series
csat_ts <- ts(df_ts$mean_csat, frequency = 7)

plot(csat_ts,
     main = "Daily Mean CSAT Score Over Time",
     ylab = "Mean CSAT",
     xlab = "Time")

#Decomposition
decomp <- decompose(csat_ts)

plot(decomp)

#ADF test
adf_result <- tseries::adf.test(na.omit(csat_ts))
adf_result

#ARIMA model
arima_model <- forecast::auto.arima(csat_ts)

arima_model

#Forecast
fc <- forecast::forecast(arima_model, h = 30)

plot(fc)

#Time series RMSE
ts_accuracy <- forecast::accuracy(arima_model)

ts_accuracy

ts_rmse <- ts_accuracy[1, "RMSE"]

ts_rmse

#Save time series outputs
png("time_series_csat.png", width = 1200, height = 700, res = 150)
plot(csat_ts,
     main = "Daily Mean CSAT Score Over Time",
     ylab = "Mean CSAT",
     xlab = "Time")
dev.off()

png("time_series_decomposition.png", width = 1200, height = 900, res = 150)
plot(decomp)
dev.off()

png("arima_forecast_30days.png", width = 1200, height = 700, res = 150)
plot(fc)
dev.off()

ts_metrics_df <- data.frame(
  Model = "ARIMA",
  RMSE = ts_rmse,
  AIC = arima_model$aic,
  BIC = arima_model$bic
)

write.csv(ts_metrics_df, "time_series_metrics.csv", row.names = FALSE)

#model3-Naive Bayes
#Prepare Naive Bayes data
df_nb <- data_clean %>%
  dplyr::mutate(
    csat_class = factor(ifelse(csat_score >= 4, "Good", "Poor"))
  ) %>%
  dplyr::select(csat_class, tenure_bucket, agent_shift, channel, product_cat) %>%
  dplyr::filter(complete.cases(.)) %>%
  dplyr::mutate(dplyr::across(where(is.character), as.factor))

table(df_nb$csat_class)
head(df_nb)

#Train-test split
set.seed(42)

train_idx <- caret::createDataPartition(
  df_nb$csat_class,
  p = 0.70,
  list = FALSE
)

train_nb <- df_nb[train_idx, ]
test_nb  <- df_nb[-train_idx, ]

nrow(train_nb)
nrow(test_nb)

#Fit Naive Bayes model
nb_model <- e1071::naiveBayes(
  csat_class ~ .,
  data = train_nb,
  laplace = 1
)

nb_model

#Predict and evaluate
nb_pred <- predict(nb_model, newdata = test_nb)

cm_nb <- caret::confusionMatrix(
  nb_pred,
  test_nb$csat_class,
  positive = "Good"
)

cm_nb

#Extract Naive Bayes metrics
nb_accuracy <- cm_nb$overall["Accuracy"]
nb_kappa <- cm_nb$overall["Kappa"]
nb_sensitivity <- cm_nb$byClass["Sensitivity"]
nb_specificity <- cm_nb$byClass["Specificity"]

nb_accuracy
nb_kappa
nb_sensitivity
nb_specificity

#Save Naive Bayes metrics
nb_metrics_df <- data.frame(
  Model = "Naive Bayes",
  Accuracy = as.numeric(nb_accuracy),
  Accuracy_Percentage = as.numeric(nb_accuracy) * 100,
  Kappa = as.numeric(nb_kappa),
  Sensitivity = as.numeric(nb_sensitivity),
  Specificity = as.numeric(nb_specificity)
)

nb_metrics_df

write.csv(nb_metrics_df, "naive_bayes_metrics.csv", row.names = FALSE)


#Final comparison table
final_comparison <- data.frame(
  Model = c("OLR", "Time Series ARIMA", "Naive Bayes"),
  Main_Task = c("Predict CSAT level", "Forecast future CSAT", "Classify Good/Poor CSAT"),
  Main_Metric = c("Derived Accuracy", "RMSE", "Accuracy"),
  Result = c(
    paste0(round(olr_accuracy * 100, 2), "%"),
    round(ts_rmse, 4),
    paste0(round(as.numeric(nb_accuracy) * 100, 2), "%")
  )
)

final_comparison

write.csv(final_comparison, "final_three_model_comparison.csv", row.names = FALSE)








