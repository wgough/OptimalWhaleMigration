# This tests the differences between equatorward and poleward migration distances

#equatorward_distances <- MigrationDistancesLitAvgs$Migration_Distance_km[MigrationDistancesLitAvgs$Direction_of_Travel == "Equatorward"]
#poleward_distances <- MigrationDistancesLitAvgs$Migration_Distance_km[MigrationDistancesLitAvgs$Direction_of_Travel == "Poleward"]

# Performing a t-test

#t_test_result <- t.test(equatorward_distances, poleward_distances)

# Output the results

#print(t_test_result)

# This tests the differences between equatorward and poleward migration speeds

#equatorward_speeds <- MigrationDistancesLitAvgs$Migration_Speed_m_s[MigrationDistancesLitAvgs$Direction_of_Travel == "Equatorward"]
#poleward_speeds <- MigrationDistancesLitAvgs$Migration_Speed_m_s[MigrationDistancesLitAvgs$Direction_of_Travel == "Poleward"]

# Performing a t-test

#t_test_result2 <- t.test(equatorward_speeds, poleward_speeds)

# Output the results

#print(t_test_result2)


# This is to test the slopes of the linear relationshipsin Figure 4

#slope1 <- 0.00007252
#slope2 <- 0.00006071

#model1 <- lm(Migration_Distance_km ~ Migration_Speed_m_s, data = MigrationDistancesLitAvgs)
#model2 <- lm(Distance ~ OptSpeed, data = OptSpeedSummary)

# Extract the standard errors for the slopes

#se_slope1 <- summary(model1)$coefficients[2, "Std. Error"]

#se_slope2 <- summary(model2)$coefficients[2, "Std. Error"]

# Compute the t-statistic and p-value for the comparison of slopes

#t_stat <- (slope1 - slope2) / sqrt(se_slope1^2 + se_slope2^2)
#df <- sum(model1$df, model2$df)  # Total degrees of freedom
#p_value <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)

# Output the results

#if (p_value < 0.05) {
#  print("The slopes are statistically different.")
#} else {
#  print("The slopes are not statistically different.")
#}

# This is to test the intercepts of the linear relationships in Figure 4

# Extract the standard errors for the intercepts

#se_intercept1 <- summary(model1)$coefficients[1, "Std. Error"]
#se_intercept2 <- summary(model2)$coefficients[1, "Std. Error"]

# Compute the t-statistic and p-value for the comparison of intercepts

#t_stat_intercept <- (model1$coef[1] - model2$coef[1]) / sqrt(se_intercept1^2 + se_intercept2^2)
#df_intercept <- sum(model1$df, model2$df)  # Total degrees of freedom
#p_value_intercept <- 2 * pt(abs(t_stat_intercept), df = df_intercept, lower.tail = FALSE)

# Output the results for the intercept comparison

#if (p_value_intercept < 0.05) {
#  print("The intercepts are statistically different.")
#} else {
#  print("The intercepts are not statistically different.")
#}
