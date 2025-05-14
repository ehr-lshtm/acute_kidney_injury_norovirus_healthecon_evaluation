# example of discounting QALYs lost from mortality; FGS 11.10.2024

#-------------------------------
# QALYs lost per premature death
#-------------------------------

#load library
library(tidyverse)

path = "docs/" ### add your file path here for where you saved the file below 

##### optional adjustment for changes in mortality (and comorbidity)
# adjust mortality rate in life-expectancy
SMR <- 1.44 # taking reports of 44% excess deaths
# check this! Better value needed https://www.sciencedirect.com/science/article/abs/pii/S0883944123000291 

# get most recent life expectancy for UK from ONS as 3-year average over 2017-2019
## https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/nationallifetablesunitedkingdomreferencetables
LE_UK <- readxl::read_excel(paste0(path, "nltuk198020203.xlsx"), 
                            sheet="2017-2019", range = "A7:F108") %>%
  setNames(c("age", "mx_M", "qx_M", "lx_M", "dx_M", "ex_M")) %>%
  bind_cols(readxl::read_excel(paste0(path, "nltuk198020203.xlsx"), 
                               sheet="2017-2019", range = "I7:M108") %>%
              setNames(c("mx_F", "qx_F", "lx_F", "dx_F", "ex_F")) )

# adjust by standardised mortality rate (SMR)
LE_UK$adjSurvM <- c(100000, c(LE_UK$lx_M * exp(-LE_UK$qx_M * SMR))[-101])
LE_UK$adjSurvF <- c(100000, c(LE_UK$lx_F * exp(-LE_UK$qx_F * SMR))[-101])

suppressWarnings({
  LE_UK  <- LE_UK %>% 
    mutate(YrsAliveM = c(c((adjSurvM+lag(adjSurvM) )/2)[-1],
                         last(c((adjSurvM+lag(adjSurvM) )/2)[-1]) ),
           YrsAliveF = c(c((adjSurvF+lag(adjSurvF) )/2)[-1],
                         last(c((adjSurvF+lag(adjSurvF) )/2)[-1]) ) )
  
  LE_UK$adjLEmale   <- sapply(seq_len(nrow(LE_UK)), 
                              function(x){ sum(LE_UK$YrsAliveM[x:nrow(LE_UK)])/LE_UK$YrsAliveM[x] })
  LE_UK$adjLEfemale <- sapply(seq_len(nrow(LE_UK)), 
                              function(x){ sum(LE_UK$YrsAliveF[x:nrow(LE_UK)])/LE_UK$YrsAliveF[x] })
})


# use age groups
LE_UK <- LE_UK %>%
  dplyr::select(age, male=adjLEmale, female=adjLEfemale) %>%
  dplyr::mutate(age = ifelse(age %in% 0:4, "0-4",
                             ifelse(age %in% 5:14, "5-14",
                                    ifelse(age %in% 15:64, "15-64", "65+")))) %>%
  dplyr::group_by(age) %>%
  dplyr::summarise(male   = mean(male),
                   female = mean(female)) %>%
  dplyr::mutate(age = factor(age, levels= c("0-4", "5-14", "15-64", "65+"))) %>%
  dplyr::arrange(age)

## function for discounting life years (quality-adjusted or not)
discount_LY <- function(target_value, timeframe, comorb_reduc_val, disc_rate, female=TRUE) {
  
  # adjust for round numbers; "ceiling" for decimals
  x <- ifelse(timeframe-floor(timeframe)==0, 
              ceiling(timeframe)+1, 
              ceiling(timeframe))
  
  # create table
  y <- data.frame(Year = seq_len(x), Value = target_value )
  
  # add age-/sex-specific QALY value depending on UK norms of EQ-5D (Ara and Brazier, 2010)
  if(female==TRUE) { sex = 0} else { sex = 1}
  
  y <- y %>% dplyr::mutate(Value = 0.9508566 + 0.0212126*sex - 0.0002587*Year - 0.0000332*Year^2)
  
  # adjust for comorbidities, if wanted to
  if(!is.null(comorb_reduc_val)){ 
    y <- y %>% dplyr::mutate(Value = Value * comorb_reduc_val)
  }
  
  # discount after 1st year
  y <- y %>% dplyr::mutate(Disc_value = Value * (1+disc_rate)^-(Year-1) )
  
  # correct for fraction in last year
  y[x,"Disc_value"] <- (timeframe-floor(timeframe))*y[x,"Value"] * (1+disc_rate)^-y[x-1,"Year"]
  
  #print result
  return( round(sum(y$Disc_value), 2) )
}


# print QALYs
QALY_UK = LE_UK

# discount quality-adjusted life-expectancy (QALE); different utilities by sex
for(j in c("female","male")){
  if(j == "female") {fem=TRUE} else {fem=FALSE}
  
  # discounting
  QALY_UK[[paste0(j, "_QALE_disc", sep="")]] <- 
    QALY_UK[, j] %>% unlist() %>% 
    purrr::map_dbl(function(x) discount_LY(x, x, 0.9, 0.035, female=fem) )
}

# mean QALYs total by age
QALY_UK <- QALY_UK %>%
  transmute(group  = age,
            undisc = (female           +male)    /2, 
            disc   = (female_QALE_disc +male_QALE_disc)/2 )
