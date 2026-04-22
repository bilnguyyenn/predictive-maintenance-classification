# load all required packages
library(tidyverse)
library(ggplot2)
library(reshape2)
library(class)
library(caret)
library(gridExtra)
library(grid)
library(pROC)
library(scales)

# 1. Data Collection & Preprocessing

# import the raw dataset
df_raw <- read.csv(file.choose(), stringsAsFactors = FALSE)

# quick look at the data
print("Raw Data Dimensions:")
dim(df_raw)
names(df_raw)

print("Checking for NAs:")
colSums(is.na(df_raw))

# cleaning up the column names so they are easier to type
df <- df_raw %>%
  rename(
    UID = UDI,
    Product_ID = Product.ID,
    Type = Type,
    Air_Temp_K = Air.temperature..K.,
    Process_Temp_K = Process.temperature..K.,
    Rotational_Speed_rpm = Rotational.speed..rpm.,
    Torque_Nm = Torque..Nm.,
    Tool_Wear_min = Tool.wear..min.,
    Machine_Failure = Machine.failure
  ) %>%
  # fix factors and make the temperature diff column
  mutate(
    Type = as.factor(Type),
    Machine_Failure = as.factor(Machine_Failure),
    Temp_Diff_K = Process_Temp_K - Air_Temp_K
  ) %>%
  # drop columns we don't need for the models
  select(-UID, -Product_ID)

str(df)

# 2. Descriptive Stats

summary(df)

# get just the numeric columns
num_cols <- c("Air_Temp_K", "Process_Temp_K", "Rotational_Speed_rpm",
              "Torque_Nm", "Tool_Wear_min", "Temp_Diff_K")

# build the summary table for the png
desc_tbl <- data.frame(
  Variable = c("Air Temperature (K)", "Process Temperature (K)",
               "Rotational Speed (rpm)", "Torque (Nm)",
               "Tool Wear (min)", "Temp Differential (K)"),
  Mean = c(round(mean(df$Air_Temp_K), 3),
           round(mean(df$Process_Temp_K), 3),
           round(mean(df$Rotational_Speed_rpm), 3),
           round(mean(df$Torque_Nm), 3),
           round(mean(df$Tool_Wear_min), 3),
           round(mean(df$Temp_Diff_K), 3)),
  Median = c(round(median(df$Air_Temp_K), 3),
             round(median(df$Process_Temp_K), 3),
             round(median(df$Rotational_Speed_rpm), 3),
             round(median(df$Torque_Nm), 3),
             round(median(df$Tool_Wear_min), 3),
             round(median(df$Temp_Diff_K), 3)),
  Std_Dev = c(round(sd(df$Air_Temp_K), 3),
              round(sd(df$Process_Temp_K), 3),
              round(sd(df$Rotational_Speed_rpm), 3),
              round(sd(df$Torque_Nm), 3),
              round(sd(df$Tool_Wear_min), 3),
              round(sd(df$Temp_Diff_K), 3)),
  Variance = c(round(var(df$Air_Temp_K), 3),
               round(var(df$Process_Temp_K), 3),
               round(var(df$Rotational_Speed_rpm), 3),
               round(var(df$Torque_Nm), 3),
               round(var(df$Tool_Wear_min), 3),
               round(var(df$Temp_Diff_K), 3)),
  Min = c(round(min(df$Air_Temp_K), 3),
          round(min(df$Process_Temp_K), 3),
          round(min(df$Rotational_Speed_rpm), 3),
          round(min(df$Torque_Nm), 3),
          round(min(df$Tool_Wear_min), 3),
          round(min(df$Temp_Diff_K), 3)),
  Max = c(round(max(df$Air_Temp_K), 3),
          round(max(df$Process_Temp_K), 3),
          round(max(df$Rotational_Speed_rpm), 3),
          round(max(df$Torque_Nm), 3),
          round(max(df$Tool_Wear_min), 3),
          round(max(df$Temp_Diff_K), 3))
)

# save the table image
p_desc_tbl <- gridExtra::tableGrob(
  desc_tbl,
  rows = NULL,
  theme = gridExtra::ttheme_minimal(
    colhead = list(
      fg_params = list(col = "white", fontface = "bold", fontsize = 10),
      bg_params = list(fill = "#2c3e6b")
    ),
    core = list(
      fg_params = list(fontsize = 9.5),
      bg_params = list(fill = c("#f7f9fc", "white"))
    )
  )
)

title_grob <- grid::textGrob(
  "Descriptive Statistics - Operational Variables",
  gp = grid::gpar(fontsize = 13, fontface = "bold")
)
sub_grob <- grid::textGrob(
  "AI4I 2020 Predictive Maintenance Dataset | n = 10,000",
  gp = grid::gpar(fontsize = 9, col = "grey40")
)

png("desc_stats_table.png", width = 900, height = 320, res = 130)
gridExtra::grid.arrange(title_grob, sub_grob, p_desc_tbl,
                        ncol = 1, heights = c(0.12, 0.07, 0.81))
dev.off()


# Tool A: Box Plots by Failure Status
df_long <- df %>%
  select(Machine_Failure, all_of(num_cols)) %>%
  pivot_longer(-Machine_Failure, names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = recode(Variable,
                           Air_Temp_K = "Air Temp (K)",
                           Process_Temp_K = "Process Temp (K)",
                           Rotational_Speed_rpm = "Rot. Speed (rpm)",
                           Torque_Nm = "Torque (Nm)",
                           Tool_Wear_min = "Tool Wear (min)",
                           Temp_Diff_K = "Temp Diff (K)"
  ))

p_boxplot <- ggplot(df_long, aes(x = Machine_Failure, y = Value, fill = Machine_Failure)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.8) +
  facet_wrap(~Variable, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = c("0" = "#4E9BCD", "1" = "#E05C5C"),
                    labels = c("No Failure", "Failure")) +
  labs(
    title = "Box Plots of Operational Variables by Machine Failure Status",
    subtitle = "AI4I 2020 Predictive Maintenance Dataset (n = 10,000)",
    x = "Machine Failure (0 = No, 1 = Yes)",
    y = "Value",
    fill = "Status"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave("boxplots.png", p_boxplot, width = 10, height = 7, dpi = 200)


# Tool B: Correlation Heat Map
corr_df <- df %>%
  select(all_of(num_cols), Machine_Failure) %>%
  mutate(Machine_Failure = as.numeric(as.character(Machine_Failure)))

corr_mat <- round(cor(corr_df, use = "complete.obs"), 2)
corr_melt <- melt(corr_mat)

col_labels <- c(
  Air_Temp_K = "Air Temp",
  Process_Temp_K = "Proc Temp",
  Rotational_Speed_rpm = "Rot Speed",
  Torque_Nm = "Torque",
  Tool_Wear_min = "Tool Wear",
  Temp_Diff_K = "Temp Diff",
  Machine_Failure = "Failure"
)

p_heatmap <- ggplot(corr_melt, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = value), size = 3.5, fontface = "bold") +
  scale_fill_gradient2(
    low = "#2166AC", mid = "white", high = "#D6604D",
    midpoint = 0, limits = c(-1, 1), name = "Pearson\nCorrelation"
  ) +
  scale_x_discrete(labels = col_labels) +
  scale_y_discrete(labels = col_labels) +
  labs(
    title = "Correlation Heat Map of Operational Variables",
    subtitle = "Includes Machine Failure as numeric outcome (0/1)",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    axis.text.x = element_text(angle = 35, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    legend.position = "right"
  ) +
  coord_fixed()

ggsave("heatmap.png", p_heatmap, width = 8, height = 7, dpi = 200)

# 3. Statistical Inference (t-test)
# H0: Mean Tool Wear is equal for both groups
# H1: Mean Tool Wear is higher for failed machines

wear_fail <- df$Tool_Wear_min[df$Machine_Failure == 1]
wear_nofail <- df$Tool_Wear_min[df$Machine_Failure == 0]

t_result <- t.test(wear_fail, wear_nofail, alternative = "greater", var.equal = FALSE)
print(t_result)

# t-test density plot
p_ttest <- ggplot(df, aes(x = Tool_Wear_min, fill = Machine_Failure, color = Machine_Failure)) +
  geom_density(alpha = 0.45, linewidth = 0.8) +
  geom_vline(xintercept = mean(wear_fail), color = "#E05C5C", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = mean(wear_nofail), color = "#4E9BCD", linetype = "dashed", linewidth = 1) +
  scale_fill_manual(values = c("0" = "#4E9BCD", "1" = "#E05C5C"), labels = c("No Failure", "Failure")) +
  scale_color_manual(values = c("0" = "#4E9BCD", "1" = "#E05C5C"), labels = c("No Failure", "Failure")) +
  labs(
    title = "Distribution of Tool Wear by Machine Failure Status",
    subtitle = sprintf("Welch's t-test: t = %.2f, p = %.2e", t_result$statistic, t_result$p.value),
    x = "Tool Wear (min)", y = "Density", fill = "Status", color = "Status"
  ) +
  theme_bw(base_size = 11) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")

ggsave("ttest_density.png", p_ttest, width = 7, height = 5, dpi = 200)

# 4. Logistic Regression

# prep dummy variables for regression
df_model <- df %>%
  mutate(
    Failure_num = as.integer(as.character(Machine_Failure)),
    Type_L = ifelse(Type == "L", 1, 0),
    Type_M = ifelse(Type == "M", 1, 0)
  )

log_reg <- glm(
  Failure_num ~ Air_Temp_K + Process_Temp_K + Rotational_Speed_rpm +
    Torque_Nm + Tool_Wear_min + Type_L + Type_M,
  data = df_model,
  family = binomial(link = "logit")
)

summary(log_reg)

# generate odds ratio table
or_mat <- exp(cbind(OR = coef(log_reg), confint(log_reg)))
or_df <- data.frame(
  Predictor = rownames(or_mat),
  Estimate = round(coef(log_reg), 4),
  Std_Error = round(summary(log_reg)$coefficients[, 2], 4),
  z_value = round(summary(log_reg)$coefficients[, 3], 4),
  p_value = formatC(summary(log_reg)$coefficients[, 4], format = "e", digits = 3),
  OR = round(or_mat[, "OR"], 4),
  CI_2.5pct = round(or_mat[, "2.5 %"], 4),
  CI_97.5pct = round(or_mat[, "97.5 %"], 4),
  row.names = NULL
)

p_or_tbl <- gridExtra::tableGrob(
  or_df, rows = NULL,
  theme = gridExtra::ttheme_minimal(
    colhead = list(
      fg_params = list(col = "white", fontface = "bold", fontsize = 9),
      bg_params = list(fill = "#2c3e6b")
    ),
    core = list(
      fg_params = list(fontsize = 8.5),
      bg_params = list(fill = c("#f7f9fc", "white"))
    )
  )
)

lor_title <- grid::textGrob(
  "Logistic Regression — Coefficient & Odds Ratio Table",
  gp = grid::gpar(fontsize = 13, fontface = "bold")
)
lor_sub <- grid::textGrob(
  "Outcome: Machine Failure (0/1) | Reference Type: H (High quality)",
  gp = grid::gpar(fontsize = 9, col = "grey40")
)

png("logistic_regression_table.png", width = 1000, height = 360, res = 130)
gridExtra::grid.arrange(lor_title, lor_sub, p_or_tbl, ncol = 1, heights = c(0.10, 0.07, 0.83))
dev.off()

# check accuracy
pred_prob <- predict(log_reg, type = "response")
pred_class <- ifelse(pred_prob >= 0.5, 1, 0)
cm <- table(Predicted = pred_class, Actual = df_model$Failure_num)
print(cm)

# plot roc curve
roc_obj <- roc(df_model$Failure_num, pred_prob, quiet = TRUE)
p_roc <- ggroc(roc_obj, color = "#E05C5C", linewidth = 1.1) +
  geom_abline(slope = 1, intercept = 1, linetype = "dashed", color = "grey50") +
  annotate("text", x = 0.35, y = 0.15,
           label = sprintf("AUC = %.3f", auc(roc_obj)),
           size = 5, fontface = "bold", color = "#E05C5C") +
  labs(
    title = "ROC Curve — Logistic Regression Model",
    subtitle = "Predicting Machine Failure",
    x = "Specificity", y = "Sensitivity"
  ) +
  theme_bw(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave("roc_curve.png", p_roc, width = 6, height = 5, dpi = 200)


# 5. KNN Classification

set.seed(3030)

# isolate features and scale them
features <- c("Air_Temp_K", "Process_Temp_K", "Rotational_Speed_rpm",
              "Torque_Nm", "Tool_Wear_min", "Type_L", "Type_M")

X <- df_model %>% select(all_of(features))
y <- as.factor(df_model$Failure_num)

num_feat_idx <- 1:5
X[, num_feat_idx] <- scale(X[, num_feat_idx])

# split training and testing data 80/20
n <- nrow(X)
train_ix <- sample(seq_len(n), size = floor(0.8 * n))
X_train <- X[train_ix, ]
X_test <- X[-train_ix, ]
y_train <- y[train_ix]
y_test <- y[-train_ix]

# find best k value
k_vals <- seq(1, 25, by = 2)
cv_acc <- numeric(length(k_vals))

for (i in seq_along(k_vals)) {
  set.seed(3030)
  preds <- knn(X_train, X_train, y_train, k = k_vals[i])
  cv_acc[i] <- mean(preds == y_train)
}

best_k <- k_vals[which.max(cv_acc)]

# plot optimal k
k_df <- data.frame(k = k_vals, Accuracy = cv_acc)
p_knn_cv <- ggplot(k_df, aes(x = k, y = Accuracy)) +
  geom_line(color = "#4E9BCD", linewidth = 1) +
  geom_point(color = "#4E9BCD", size = 2.5) +
  geom_vline(xintercept = best_k, linetype = "dashed", color = "#E05C5C", linewidth = 0.9) +
  annotate("text", x = best_k + 1, y = min(cv_acc) + 0.002,
           label = paste("k =", best_k), color = "#E05C5C", size = 4) +
  scale_x_continuous(breaks = k_vals) +
  labs(
    title = "KNN: Training Accuracy vs. Number of Neighbors (k)",
    x = "k (Number of Neighbors)", y = "Training Accuracy"
  ) +
  theme_bw(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave("knn_k_selection.png", p_knn_cv, width = 7, height = 5, dpi = 200)

# run final knn model
set.seed(3030)
knn_pred <- knn(X_train, X_test, y_train, k = best_k, prob = TRUE)
cm_knn <- confusionMatrix(knn_pred, y_test, positive = "1")
print(cm_knn)

# model comparison table
log_test_prob <- predict(log_reg, newdata = df_model[-train_ix, ], type = "response")
log_test_class <- as.factor(ifelse(log_test_prob >= 0.5, 1, 0))
cm_log <- confusionMatrix(log_test_class, y_test, positive = "1")

compare_df <- data.frame(
  Method = c("Logistic Regression", paste0("KNN (k = ", best_k, ")")),
  Accuracy = round(c(cm_log$overall["Accuracy"], cm_knn$overall["Accuracy"]), 4),
  Sensitivity = round(c(cm_log$byClass["Sensitivity"], cm_knn$byClass["Sensitivity"]), 4),
  Specificity = round(c(cm_log$byClass["Specificity"], cm_knn$byClass["Specificity"]), 4),
  Balanced_Acc = round(c(cm_log$byClass["Balanced Accuracy"], cm_knn$byClass["Balanced Accuracy"]), 4),
  row.names = NULL
)

p_cmp_tbl <- gridExtra::tableGrob(
  compare_df, rows = NULL,
  theme = gridExtra::ttheme_minimal(
    colhead = list(
      fg_params = list(col = "white", fontface = "bold", fontsize = 10),
      bg_params = list(fill = "#2c3e6b")
    ),
    core = list(
      fg_params = list(fontsize = 10),
      bg_params = list(fill = c("#f7f9fc", "white"))
    )
  )
)

cmp_title <- grid::textGrob(
  "Model Comparison — Logistic Regression vs. KNN (Test Set)",
  gp = grid::gpar(fontsize = 13, fontface = "bold")
)
cmp_sub <- grid::textGrob(
  "80/20 train-test split | Positive class: Machine Failure = 1",
  gp = grid::gpar(fontsize = 9, col = "grey40")
)

png("model_comparison_table.png", width = 760, height = 230, res = 130)
gridExtra::grid.arrange(cmp_title, cmp_sub, p_cmp_tbl, ncol = 1, heights = c(0.14, 0.09, 0.77))
dev.off()

print(sessionInfo())