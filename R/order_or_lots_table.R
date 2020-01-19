
order_or_lots_table <- tribble(
    ~ID,	~Name,          ~PhaseOrMilestoneID, ~CustomerName,
    "1_BA", "Base Award",   "C_LRIP",            NA_character_,
    "2_L1", "LRIP Order 1", "C_LRIP",            NA_character_,
    "3_L2", "LRIP Order 2", "C_LRIP",            NA_character_,
    "4_F1", "FRP Order 1",  "C_FRP",             NA_character_,
    "5_F2", "FRP Order 2",  "C_FRP",             NA_character_
)

order_or_lots_table <- order_or_lots_table %>%
    mutate(ContractTypeID = factor("MULTIPLE",
        levels = c("CS", "CPAF", "CPFF", "CPIF", "CPIF_PI", "FFP", "FPIF",
                   "FPIST", "FPIST_PI", "FPIFT_PI", "FPAF", "FP_EPA", "FP_PPR",
                   "FCP_RPR", "FFP_LOET", "IDIQ", "LC", "TM", "OTHER",
                   "MULTIPLE"))
)

order_or_lots_table <- order_or_lots_table %>%
    mutate(AppropriationTypeID = factor(
        "PROCUREMENT",
        levels = c("RDTE", "PROCUREMENT", "O_AND_M"))
)
