# =============================================================================
# R/data_gen.R
# Synthetic ADaM Dataset Generator
# =============================================================================
# Purpose : Generate realistic CDISC ADaM-style datasets for the TFL Viewer
#           demonstration. All data is purely simulated and does not represent
#           any real clinical trial, sponsor, or patient population.
#
# Datasets produced:
#   adsl  - Subject Level Analysis Dataset        (1 row per subject)
#   adae  - Adverse Events Analysis Dataset       (1+ rows per subject)
#   adlb  - Laboratory Data Analysis Dataset      (1 row per subject/param/visit)
#   adtte - Time-to-Event Analysis Dataset        (1 row per subject/parameter)
#
# Usage:
#   adam <- generate_adam_data()
#   adsl  <- adam$adsl
#   adae  <- adam$adae
#   adlb  <- adam$adlb
#   adtte <- adam$adtte
# =============================================================================

#' Generate synthetic CDISC ADaM datasets
#'
#' @param n_subjects Integer. Number of subjects to simulate. Default 200.
#' @param seed       Integer. RNG seed for reproducibility. Default 4729.
#'
#' @return A named list with elements: adsl, adae, adlb, adtte.
generate_adam_data <- function(n_subjects = 200, seed = 4729) {

  set.seed(seed)
  n         <- n_subjects
  study_id  <- "STUDY001"

  # ---------------------------------------------------------------------------
  # 1. ADSL – Subject Level Analysis Dataset
  # ---------------------------------------------------------------------------

  # Unique subject IDs: STUDY001-SSS-NNNN (site-subject)
  site_id  <- sample(101:115, n, replace = TRUE)
  usubjid  <- sprintf("%s-%03d-%04d", study_id, site_id, seq_len(n))

  # Treatment assignment (roughly balanced 3 arms)
  trt <- sample(
    c("Placebo", "Active 10mg", "Active 20mg"),
    n, replace = TRUE, prob = c(0.34, 0.33, 0.33)
  )

  # Demographics
  age   <- as.integer(pmax(18, pmin(85, round(rnorm(n, 52, 12)))))
  sex   <- sample(c("M", "F"), n, replace = TRUE, prob = c(0.52, 0.48))
  race  <- sample(
    c("WHITE", "BLACK OR AFRICAN AMERICAN", "ASIAN", "OTHER"),
    n, replace = TRUE, prob = c(0.65, 0.15, 0.15, 0.05)
  )
  bmi   <- round(rnorm(n, 26.5, 4.2), 1)

  # Disposition: most subjects complete; small fractions discontinue
  dcsreas <- sample(
    c("COMPLETED", "ADVERSE EVENT", "WITHDRAWAL BY SUBJECT",
      "LOST TO FOLLOW-UP", "PROTOCOL DEVIATION"),
    n, replace = TRUE, prob = c(0.72, 0.10, 0.09, 0.05, 0.04)
  )

  # Study dates
  rand_day <- as.Date("2022-01-10") + sample(0:180, n, replace = TRUE)
  trt_dur  <- ifelse(
    dcsreas == "COMPLETED",
    sample(168:182, n, replace = TRUE),
    sample(14:150,  n, replace = TRUE)
  )

  adsl <- data.frame(
    STUDYID = study_id,
    USUBJID = usubjid,
    SUBJID  = sprintf("%04d", seq_len(n)),
    SITEID  = sprintf("%03d", site_id),
    AGE     = age,
    AGEGR1  = ifelse(age < 40, "<40", ifelse(age < 65, "40-64", ">=65")),
    SEX     = sex,
    SEXN    = ifelse(sex == "M", 1L, 2L),
    RACE    = race,
    BMI     = bmi,
    BMIGR1  = ifelse(bmi < 18.5, "Underweight",
              ifelse(bmi < 25,   "Normal",
              ifelse(bmi < 30,   "Overweight", "Obese"))),
    ARM     = trt,
    ARMCD   = ifelse(trt == "Placebo",      "PBO",
              ifelse(trt == "Active 10mg",  "ACT10", "ACT20")),
    TRT01A  = trt,
    TRT01P  = trt,
    RANDDT  = format(rand_day, "%Y-%m-%d"),
    TRTSDT  = format(rand_day, "%Y-%m-%d"),
    TRTEDT  = format(rand_day + trt_dur, "%Y-%m-%d"),
    TRTDUR  = as.integer(trt_dur),
    DCSREAS = dcsreas,
    # Analysis population flags (all Y = randomised study)
    RANDFL  = "Y",
    SAFFL   = ifelse(trt_dur >= 1, "Y", "N"),   # received ≥1 dose
    ITTFL   = "Y",
    PPROTFL = ifelse(
      dcsreas == "COMPLETED" &
        sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.90, 0.10)),
      "Y", "N"
    ),
    stringsAsFactors = FALSE
  )

  # ---------------------------------------------------------------------------
  # 2. ADAE – Adverse Events Analysis Dataset
  # ---------------------------------------------------------------------------

  # MedDRA SOC → Preferred Term lookup
  soc_pt <- list(
    "INFECTIONS AND INFESTATIONS"  = c("NASOPHARYNGITIS",
                                        "UPPER RESPIRATORY TRACT INFECTION",
                                        "URINARY TRACT INFECTION", "INFLUENZA"),
    "GASTROINTESTINAL DISORDERS"   = c("NAUSEA", "DIARRHOEA", "VOMITING",
                                        "ABDOMINAL PAIN", "CONSTIPATION"),
    "NERVOUS SYSTEM DISORDERS"     = c("HEADACHE", "DIZZINESS",
                                        "SOMNOLENCE", "PARAESTHESIA"),
    "MUSCULOSKELETAL DISORDERS"    = c("BACK PAIN", "ARTHRALGIA",
                                        "MYALGIA", "PAIN IN EXTREMITY"),
    "GENERAL DISORDERS"            = c("FATIGUE", "PYREXIA",
                                        "OEDEMA PERIPHERAL", "ASTHENIA")
  )
  soc_names <- names(soc_pt)

  # Simulate Poisson number of AEs per subject (mean ≈ 2.5)
  n_ae_per <- pmax(0L, rpois(n, lambda = 2.5))

  ae_list <- lapply(seq_len(n), function(i) {
    k <- n_ae_per[i]
    if (k == 0L) return(NULL)
    soc <- sample(soc_names, k, replace = TRUE)
    pt  <- mapply(function(s) sample(soc_pt[[s]], 1), soc)
    data.frame(
      STUDYID  = study_id,
      USUBJID  = adsl$USUBJID[i],
      AEBODSYS = soc,
      AEDECOD  = pt,
      AETERM   = pt,   # verbatim == coded for simplicity
      AESEV    = sample(c("MILD", "MODERATE", "SEVERE"),
                        k, replace = TRUE, prob = c(0.55, 0.35, 0.10)),
      AESER    = sample(c("N", "Y"),
                        k, replace = TRUE, prob = c(0.92, 0.08)),
      AEREL    = sample(c("NOT RELATED", "UNLIKELY RELATED",
                          "POSSIBLY RELATED", "RELATED"),
                        k, replace = TRUE, prob = c(0.35, 0.25, 0.25, 0.15)),
      AEOUT    = sample(c("RECOVERED/RESOLVED", "RECOVERING/RESOLVING",
                          "NOT RECOVERED/NOT RESOLVED", "FATAL"),
                        k, replace = TRUE, prob = c(0.70, 0.15, 0.12, 0.03)),
      AESTDY   = sample(1:180, k, replace = TRUE),
      stringsAsFactors = FALSE
    )
  })

  adae_raw <- do.call(rbind, ae_list[!sapply(ae_list, is.null)])

  # Merge ADSL flags onto ADAE rows
  adae <- merge(
    adae_raw,
    adsl[, c("USUBJID", "TRT01A", "ARMCD", "SAFFL", "ITTFL")],
    by = "USUBJID", all.x = TRUE
  )
  adae <- adae[order(adae$USUBJID, adae$AESTDY), ]
  row.names(adae) <- NULL

  # ---------------------------------------------------------------------------
  # 3. ADLB – Laboratory Data Analysis Dataset
  # ---------------------------------------------------------------------------

  # Lab parameter reference ranges (rough physiological means)
  params <- data.frame(
    PARAMCD = c("ALT",  "AST",  "BILI", "CREAT", "HGB",  "WBC",  "PLAT"),
    PARAM   = c(
      "Alanine Aminotransferase (U/L)",
      "Aspartate Aminotransferase (U/L)",
      "Bilirubin (umol/L)",
      "Creatinine (umol/L)",
      "Haemoglobin (g/dL)",
      "White Blood Cell Count (10^9/L)",
      "Platelet Count (10^9/L)"
    ),
    mean_bl = c(28,  26,  10,  75,  13.5, 6.5, 250),
    sd_bl   = c(12,  10,   4,  15,   1.5, 1.5,  60),
    stringsAsFactors = FALSE
  )

  visits <- data.frame(
    AVISIT  = c("Baseline", "Week 4", "Week 8",
                "Week 12",  "Week 24", "End of Treatment"),
    AVISITN = c(0, 4, 8, 12, 24, 99),
    stringsAsFactors = FALSE
  )

  lb_list <- lapply(seq_len(nrow(params)), function(pi) {
    lapply(seq_len(n), function(i) {
      bl  <- max(rnorm(1, params$mean_bl[pi], params$sd_bl[pi]), 0.1)
      nv  <- nrow(visits)
      # Active arms show a modest downward trend (treatment effect)
      slope <- ifelse(adsl$TRT01A[i] == "Placebo",      0,
               ifelse(adsl$TRT01A[i] == "Active 10mg", -0.05, -0.10))
      aval  <- pmax(bl * (1 + slope * seq(0, 1, length.out = nv) +
                            rnorm(nv, 0, 0.08)), 0.1)
      data.frame(
        STUDYID = study_id,
        USUBJID = adsl$USUBJID[i],
        PARAMCD = params$PARAMCD[pi],
        PARAM   = params$PARAM[pi],
        AVISIT  = visits$AVISIT,
        AVISITN = visits$AVISITN,
        AVAL    = round(aval, 2),
        BASE    = round(bl, 2),
        CHG     = round(aval - bl, 2),
        PCHG    = round((aval - bl) / bl * 100, 1),
        ANL01FL = "Y",
        stringsAsFactors = FALSE
      )
    })
  })

  adlb_raw <- do.call(rbind, lapply(lb_list, function(x) do.call(rbind, x)))
  adlb <- merge(
    adlb_raw,
    adsl[, c("USUBJID", "TRT01A", "ARMCD", "SAFFL", "ITTFL")],
    by = "USUBJID", all.x = TRUE
  )
  adlb <- adlb[order(adlb$USUBJID, adlb$PARAMCD, adlb$AVISITN), ]
  row.names(adlb) <- NULL

  # ---------------------------------------------------------------------------
  # 4. ADTTE – Time-to-Event Analysis Dataset
  # ---------------------------------------------------------------------------

  # Three endpoints; active arms have longer (better) event-free times
  tte_params <- data.frame(
    PARAM   = c("TIME TO FIRST ADVERSE EVENT",
                "OVERALL SURVIVAL",
                "PROGRESSION FREE SURVIVAL"),
    PARAMCD = c("TTFAE", "OS", "PFS"),
    # Exponential scale parameter (mean days) by arm
    sc_pbo  = c(90,  120, 100),
    sc_a10  = c(120, 180, 150),
    sc_a20  = c(150, 240, 200),
    stringsAsFactors = FALSE
  )

  max_fu <- 180   # maximum follow-up days

  tte_list <- lapply(seq_len(nrow(tte_params)), function(pi) {
    scale <- ifelse(adsl$TRT01A == "Placebo",     tte_params$sc_pbo[pi],
             ifelse(adsl$TRT01A == "Active 10mg", tte_params$sc_a10[pi],
                                                   tte_params$sc_a20[pi]))
    aval <- round(rexp(n, rate = 1 / scale), 1)
    cnsr <- as.integer(aval > max_fu)
    aval <- pmin(aval, max_fu)

    data.frame(
      STUDYID  = study_id,
      USUBJID  = adsl$USUBJID,
      PARAM    = tte_params$PARAM[pi],
      PARAMCD  = tte_params$PARAMCD[pi],
      AVAL     = aval,
      CNSR     = cnsr,
      EVNTDESC = ifelse(cnsr == 0, "Event Observed", "Censored"),
      stringsAsFactors = FALSE
    )
  })

  adtte_raw <- do.call(rbind, tte_list)
  adtte <- merge(
    adtte_raw,
    adsl[, c("USUBJID", "TRT01A", "ARMCD", "SAFFL", "ITTFL")],
    by = "USUBJID", all.x = TRUE
  )
  adtte <- adtte[order(adtte$USUBJID, adtte$PARAM), ]
  row.names(adtte) <- NULL

  list(adsl = adsl, adae = adae, adlb = adlb, adtte = adtte)
}
