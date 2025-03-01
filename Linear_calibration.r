# Nepieciešamo bibliotēku ielāde
install.packages("ggplot2")
install.packages("dplyr")
install.packages("outliers")
install.packages("writexl")
install.packages("readxl")
install.packages("tidyr")

library(ggplot2)
library(dplyr)
library(outliers)
library(writexl)
library(readxl)
library(tidyr)

# 4. Ielādējam datus no Excel faila
data <- read_xlsx("calibration_data_AAS.xlsx")

# 5. Pārveidojam tabulu uz "long" formātu
data_long <- data %>%
  pivot_longer(cols = starts_with("signal"), names_to = "replicate", values_to = "signal")
#help("pivot_longer")

# 6. Izveidojam sākotnējo kalibrēšanas grafiku un kalibrēšanas līkni
plot_initial <- ggplot(data_long, aes(x = concentration, y = signal, color = replicate)) +
  geom_point() +
  theme_bw() + #noņem krāsu
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + #noņem grindline
  geom_smooth(method = "lm", se = TRUE, color = "#ff0000be") +
  labs(title = "Sākotnējais kalibrēšanas grafiks ar izkrītošo punktu",
       x = "Koncentrācija",
       y = "Signāls")
print(plot_initial)
#Saglabā grafiku kā jpg
jpeg("one.jpeg", width = 2000, height = 1600, res = 300)
print(plot_initial)
dev.off()

# 7. Kalibrēšanas līnijas modelis, lai izmantotu prognozēm un kļūdu joslām
initial_model <- lm(signal ~ concentration, data = data_long)
data_long <- data_long %>%
  mutate(predicted = predict(initial_model, newdata = data_long),
         residuals = abs(signal - predicted))

# 8. Rupjās kļūdas noteikšana, izmantojot dubultu kritēriju
threshold_sd <- 2 * sd(data_long$residuals) # Nosakām kļūdu robežu ar 2x standartnovirzi
data_long <- data_long %>%
  group_by(concentration) %>%
  mutate(is_outlier = residuals > threshold_sd) %>%
  ungroup()

# Parādām rupjās kļūdas punktus
outlier_data <- data_long %>% filter(is_outlier)
if (nrow(outlier_data) > 0) {
  cat("Identificētas rupjās kļūdas:\n")
  print(outlier_data)
  
  # Izkrītošās vērtības vizualizācija
  plot_outlier <- ggplot(data_long, aes(x = concentration, y = signal, color = replicate)) +
    geom_point() +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    geom_smooth(method = "lm", se = TRUE, color = "red") +
    geom_point(data = outlier_data, aes(x = concentration, y = signal), color = "red", size = 3) +
    labs(title = "Kalibrēšanas grafiks ar izkrītošo punktu",
         x = "Koncentrācija",
         y = "Signāls") +
    annotate("text", x = outlier_data$concentration, y = outlier_data$signal + 0.5,
             label = paste("Izkrītošs punkts:", round(outlier_data$signal, 2)),
             color = "red")
  print(plot_outlier)
}

jpeg("two.jpeg", width = 2000, height = 1600, res = 300)
print(plot_outlier)
dev.off()

# 9. Atmetam un izdzēšam izkrītošo vērtību no `data_long` tabulas
data_long <- data_long %>% filter(!is_outlier)

# 10. Uzzīmējam kalibrēšanas grafiku bez izkrītošās vērtības
plot_cleaned <- ggplot(data_long, aes(x = concentration, y = signal, color = replicate)) +
  geom_point() +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_smooth(method = "lm", se = TRUE, color = "#ff6600") +
  labs(title = "Kalibrēšanas līkne bez izkrītošā punkta",
       x = "Koncentrācija",
       y = "Signāls")
print(plot_cleaned)

jpeg("three.jpeg", width = 2000, height = 1600, res = 300)
print(plot_cleaned)
dev.off()

# 11. Vidējās vērtības un standartnovirzes aprēķināšana un pievienošana tabulai
data_summary <- data_long %>%
  group_by(concentration) %>%
  summarize(mean_signal = mean(signal), std_dev = sd(signal))

# 12. Uzzīmējam kalibrēšanas grafiku ar vidējām vērtībām un kļūdu intervāliem
plot_mean <- ggplot(data_summary, aes(x = concentration, y = mean_signal)) +
  geom_point(color = "#0077ff") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_errorbar(aes(ymin = mean_signal - std_dev, ymax = mean_signal + std_dev), width = 0.2) +
  labs(title = "Kalibrēšanas grafiks ar vidējām vērtībām un kļūdu intervāliem",
       x = "Koncentrācija",
       y = "Vidējais signāls")
print(plot_mean)

jpeg("four.jpeg", width = 2000, height = 1600, res = 300)
print(plot_mean)
dev.off()

# 13. Kalibrēšanas grafiks ar kļūdu intervāliem un vienādojumu ar kļūdu
model_cleaned <- lm(mean_signal ~ concentration, data = data_summary)
summary_stats <- summary(model_cleaned)
intercept <- coef(summary_stats)[1, 1]
slope <- coef(summary_stats)[2, 1]
intercept_se <- coef(summary_stats)[1, 2]
slope_se <- coef(summary_stats)[2, 2]
r_squared <- summary_stats$r.squared # Korelācijas koeficients

equation_with_uncertainty <- paste("y = (", round(slope, 3), "±", round(slope_se, 3), ")x",
                                   "+ (", round(intercept, 3), "±", round(intercept_se, 3), ")")
r_squared_label <- paste("R² =", format(round(r_squared, 3), nsmall = 3))

plot_final <- ggplot(data_summary, aes(x = concentration, y = mean_signal)) +
  geom_point(color = "black") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_errorbar(aes(ymin = mean_signal - std_dev, ymax = mean_signal + std_dev), width = 0.2) +
  labs(title = "Kalibrēšanas līkne ar kļūdu joslām",
       subtitle = paste(equation_with_uncertainty, "\n", r_squared_label),
       x = "Koncentrācija, mg/L",
       y = "Vidējais signāls")
print(plot_final)

jpeg("five.jpeg", width = 2000, height = 1600, res = 300)
print(plot_final)
dev.off()

# 14. Mainām līknes veidu, krāsu un confidence interval apgabalu
styled_plot <- plot_final +
  geom_smooth(method = "lm", linetype = "solid", color = "#20e9f0") +
  theme_classic() +
  theme(plot.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),#nonem grindline
        panel.grid.minor = element_blank(),
        text = element_text(family = "serif", face = "italic", size = 12),
        panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    size = 1)) #uzliek rāmi apkārt grafikam, bet ne bildei

print(styled_plot)

# 15. Saglabājam attēlu kā JPEG ar labu izšķirtspēju (300 dpi)
jpeg("calibration_curve_high_res.jpeg", width = 2000, height = 1600, res = 300)
print(styled_plot)
dev.off()
