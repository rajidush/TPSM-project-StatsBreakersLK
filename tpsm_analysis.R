packages <- c("readxl", "dplyr", "tidyr", "ggplot2", "lubridate",
              "moments", "corrplot", "FactoMineR", "factoextra",
              "gridExtra", "RColorBrewer", "scales")


new_pkg <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_pkg) > 0) install.packages(new_pkg, dependencies = TRUE)

lapply(packages, library, character.only = TRUE)

# Load & Rename Columns
df_raw <- read_excel("C:/Users/User/Documents/3rd year/Y3S2/TPSM/dax.xlsx")

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
  select(unique_id, channel, category, sub_category,
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


