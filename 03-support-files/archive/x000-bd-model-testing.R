# Remove bulk density (bd) outliers, predict bd for 2021 values using total carbon value

## Remove 2021 bd values
infews_subset1 <- filter(infews, year != 2021)
unique(infews_subset1$year)

## Create linear model of bd and carbon
bd.lm <- lm(infews_subset1$bulk_density ~ infews_subset1$total_carbon)
summary(bd.lm)

## Filter out bd outliers; plot
infews_subset2 <- filter(infews_subset1, bulk_density <= 2.6 & bulk_density != 0)
plot(infews_subset2$total_carbon, infews_subset2$bulk_density)

## Compare filtered vs full data set to show that outliers were removed
par(mfrow = c(1,2))
plot(infews_subset1$total_carbon, infews_subset1$bulk_density)
abline(bd.lm, col = "red", lty = 2, lwd = 2)
plot(infews_subset2$total_carbon, infews_subset2$bulk_density, ylim = c(0,5))
bd.lm.2 <- lm(infews_subset2$bulk_density ~ infews_subset2$total_carbon)
summary(bd.lm.2)
abline(bd.lm.2, col = "red", lty = 2, lwd = 2)
curve((1.0462*exp(-0.1144*x) + 0.4601), col = "blue", lwd = 2, add = T)

## Create non-linear model
bd.nls <- nls(bulk_density ~ a*exp(-b*total_carbon) + c, start = list(a = 2, b = 0.2, c = 0.2), data = infews_subset2)
summary(bd.nls)

## Predict and create new bd variable for full data set
infews$bd_pred <- 1.0462*exp(-0.1144*infews$total_carbon) + 0.4601
infews_subset3 <- filter(infews, year != 2021)
plot(infews_subset3$bulk_density, infews_subset3$bd_pred, xlim = c(0, 3))
abline(0,1)

## See maximum bulk density value
max(infews_subset3$bulk_density, na.rm = T)

## Create new "complete" (comp) bd variable; uses existing bd values when present. Uses predicted variable when bd value not present in full data set OR when year = 2021
infews$bd_pred <- 1.0462*exp(-0.1144*infews$total_carbon) + 0.4601
infews$bd_meas <- infews$bulk_density

infews$bd_meas[infews$bd_meas == 0 | infews$bd_meas >= 2.6 ] <- NA

infews$bd_comp <- ifelse(is.na(infews$bd_meas), infews$bd_pred, infews$bd_meas)
infews$bd_comp <- ifelse(infews$year == 2021, infews$bd_pred, infews$bd_comp)

unique(infews$bd_comp)
max(infews$bd_comp, na.rm = T)
