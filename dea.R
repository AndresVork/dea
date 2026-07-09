#Benchmarking	- Standard DEA in teaching and empirical work	
# Very clean syntax; CRS, VRS, DRS, IRS, FDH; input/output/directional/additive/super-efficiency; 
# peers; slacks; cost/revenue/profit efficiency; Malmquist index; also SFA	
#Less focused on fuzzy DEA, network DEA, and some newer special models

library(Benchmarking)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(forcats)

load(file = "Data/dfdata.RData")
head(dfdata)

#Potential inputs
unique(dfdata$measure[dfdata$type =="input"])
#"MINU"       "PHYS"       "VQ"         "EXP_HEALTH"

#Potential outputs
unique(dfdata$measure[dfdata$type =="output"])

# c("LFEXP" - "Life expectancy"
# "TRTM"  - "Treatable mortality (inverse)"
# "AVM" - "Avoidable mortality (inverse)"
# "PREVM" - "Preventable mortality (inverse)"
# "MINU"  - "Nurses"
# "PHYS" - "Doctors"
# "VQ"  - "Health and social workers"
# "EXP_HEALTH" - "Health expenditure, USD PPP pc")

#Potential outputs
unique(dfdata$measure[dfdata$type =="output"])
#[1] "LFEXP" "TRTM"  "AVM"   "PREVM"



#Select inputs and outputs from the list
inputs <- c("MINU", "EXP_HEALTH", "PHYS")
outputs <- c("LFEXP", "TRTM")

#Select year
myyear <- 2022

dfmodel <- dfdata |> 
  filter(measure %in% c(inputs, outputs)) |> 
  filter(obstime %in% c(myyear)) |> 
  pivot_wider(id_cols = c(ref_area, obstime), names_from = measure, values_from = obsvalue) |> 
  # Keep only countries with complete input and output data
  filter(if_all(all_of(c(inputs, outputs)), ~ !is.na(.x)))

# X = inputs matrix, Y = outputs matrix
X <- as.matrix(dfmodel[, inputs])
Y <- as.matrix(dfmodel[, outputs])

# Output-oriented CRS DEA - constant returns to scale
dea_crs <- dea( X, Y, RTS = "crs", ORIENTATION = "out")
summary(dea_crs)

# Output-oriented VRS DEA
dea_vrs <- dea(  X, Y, RTS = "vrs", ORIENTATION = "out")
summary(dea_vrs)

# Output-oriented efficiency scores
# In Benchmarking, output-oriented scores are usually >= 1.
# 1 = efficient; values > 1 mean outputs could be expanded.
eff_crs_raw <- efficiencies(dea_crs)
eff_vrs_raw <- efficiencies(dea_vrs)

# Convert to 0-1 efficiency index for easier interpretation
# 1 = efficient; lower values = less efficient
eff_crs <- 1 / eff_crs_raw
eff_vrs <- 1 / eff_vrs_raw

# Scale efficiency
# For output-oriented raw scores, use VRS / CRS.
scale_eff <- eff_vrs_raw / eff_crs_raw


# Results table
eff_table <- dfmodel |>
  mutate(
    eff_crs_raw = eff_crs_raw,
    eff_vrs_raw = eff_vrs_raw,
    eff_crs = eff_crs,
    eff_vrs = eff_vrs,
    scale_eff = scale_eff
  ) |>
  arrange(desc(eff_vrs)) |>
  mutate(rank_vrs = row_number()) |>
  select(
    rank_vrs, 
    ref_area,
    obstime,
    eff_vrs,
    eff_crs,
    scale_eff,
    eff_vrs_raw,
    eff_crs_raw,
    all_of(inputs),
    all_of(outputs)
  )

# Print ranking table
eff_table |> 
  print(n = Inf)

# Plot countries ranked by VRS efficiency
p_eff <- eff_table |>
  ggplot(aes(
    x = fct_reorder(ref_area, eff_vrs),
    y = eff_vrs
  )) +
  geom_col() +
  geom_text(
    aes(label = percent(eff_vrs, accuracy = 0.1)),
    hjust = 1.05,
    size = 3
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  labs(
    x = NULL,
    y = "VRS efficiency score",
    title = paste0("Output-oriented DEA efficiency by country, ", myyear),
    subtitle = "VRS model; 1 = efficient frontier"
  ) +
  theme_minimal()

p_eff
