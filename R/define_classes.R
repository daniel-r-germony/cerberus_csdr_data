
flexfile <- structure(
    list(
        ReportConfiguration = list(
            DetailedStandardCategory = as.logical(),
            GA_AsStandardCategory = as.logical(),
            FCCM_AsStandardCategory = as.logical(),
            ForecastAtCompletion_ByNonrecurringOrRecurring = as.logical(),
            ForecastAtCompletion_ByStandardCategory = as.logical()
        ),
        ReportMetadata = list(
            SecurityClassification = as.character(),
            ProprietaryStatement = as.character(),
            ProgramName = as.character(),
            PhaseOrMilestoneID = as.character(),
            PrimeMissionProduct = as.character(),
            CommodityType = as.character(),
            ReportingOrganization_OrganizationName = as.character(),
            ReportingOrganization_DivisionName = as.character(),
            ReportingOrganization_CageCode = as.character(),
            ReportingOrganization_Location_Street = as.character(),
            ReportingOrganization_Location_City = as.character(),
            ReportingOrganization_Location_State = as.character(),
            ReportingOrganization_Location_ZipCode = as.character(),
            ReportingOrganization_Location_Country = as.character(),
            ApprovedPlanNumber = as.character(),
            ApprovedPlanRevisionNumber = as.character(),
            CustomerName = as.character(),
            ContractTypeID = as.character(),
            ContractPrice = as.numeric(),
            ContractCeiling = as.numeric(),
            ContractNumber = as.character(),
            PeriodOfPerformance_StartDate = lubridate::ymd(),
            PeriodOfPerformance_EndDate = lubridate::ymd(),
            ReportCycleID = as.character(),
            SubmissionEvent_Number = as.integer(),
            SubmissionEvent_Name = as.character(),
            SubmissionEvent_IsWildcard = as.logical(),
            ResubmissionNumber = as.integer(),
            ReportAsOf = lubridate::ymd(),
            PointOfContact_Name = as.character(),
            PointOfContact_Department = as.character(),
            PointOfContact_TelephoneNumber = as.character(),
            PointOfContact_EmailAddress = as.character(),
            DatePrepared = lubridate::ymd(),
            ReportingPeriodID = as.integer()
        ),
        OrdersOrLots = list(
            ID = as.character(),
            Name = as.character(),
            PhaseOrMilestoneID = as.character(),
            CustomerName = as.character(),
            ContractTypeID = as.character(),
            ContractPrice = as.numeric(),
            ContractCeiling = as.numeric(),
            PeriodOfPerformance_StartDate = lubridate::ymd(),
            PeriodOfPerformance_EndDate = lubridate::ymd(),
            AppropriationTypeID = as.character()
        ),
        CLINs = list(
            ID = as.character(),
            Name = as.character(),
            ContractTypeID = as.character()
        ),
        EndItems = list(
            ID = as.character(),
            Name = as.character()
        ),
        WBS = list(
            Level = as.integer(),
            ID = as.character(),
            Name = as.character(),
            ParentID = as.character()
        ),
        Accounts = list(
            ID = as.character(),
            Name = as.character()
        ),
        FunctionalCategories = list(
            ID = as.character(),
            Name = as.character()
        ),
        FunctionalOverheadCategories = list(
            ID = as.character(),
            Name = as.character()
        ),
        UnitsOrSublots = list(
            ID = as.character(),
            EndItemID = as.character(),
            FirstUnitNumber = as.integer(),
            LastUnitNumber = as.integer(),
            OrderOrLotID = as.character()
        ),
        ReportingCalendar = list(
            ID = as.integer(),
            StartDate = lubridate::ymd(),
            EndDate = lubridate::ymd()
        ),
        SummaryCostData = list(
            OrderOrLotID = as.character(),
            Subtotal_TD = as.numeric(),
            Subtotal_AC = as.numeric(),
            GA_TD = as.numeric(),
            GA_AC = as.numeric(),
            UB = as.numeric(),
            MR = as.numeric(),
            FCCM_TD = as.numeric(),
            FCCM_AC = as.numeric(),
            Fee_TD = as.numeric(),
            Fee_AC = as.numeric(),
            Price_TD = as.numeric(),
            Price_AC = as.numeric()
        ),
        ActualCostHourData = list(
            OrderOrLotID = as.character(),
            CLIN_ID = as.character(),
            EndItemID = as.character(),
            WBSElementID = as.character(),
            AccountID = as.character(),
            NonrecurringOrRecurringID = as.character(),
            FunctionalCategoryID = as.character(),
            FunctionalOverheadCategoryID = as.character(),
            StandardCategoryID = as.character(),
            DetailedStandardCategoryID = as.character(),
            UnitOrSublotID = as.character(),
            AllocationMethodID = as.character(),
            ReportingPeriodID = as.integer(),
            Tag1 = as.character(),
            Tag2 = as.character(),
            Tag3 = as.character(),
            Tag4 = as.character(),
            Tag5 = as.character(),
            Tag6 = as.character(),
            Tag7 = as.character(),
            Tag8 = as.character(),
            Tag9 = as.character(),
            Tag10 = as.character(),
            Tag11 = as.character(),
            Tag12 = as.character(),
            Tag13 = as.character(),
            Tag14 = as.character(),
            Tag15 = as.character(),
            Tag16 = as.character(),
            Tag17 = as.character(),
            Tag18 = as.character(),
            Tag19 = as.character(),
            Tag20 = as.character(),
            Tag21 = as.character(),
            Tag22 = as.character(),
            Tag23 = as.character(),
            Tag24 = as.character(),
            Tag25 = as.character(),
            Value_Dollars = as.numeric(),
            Value_Hours = as.numeric()
        ),
        ForecastAtCompletionCostHourData = list(
            OrderOrLotID = as.character(),
            WBSElementID = as.character(),
            NonrecurringOrRecurringID = as.character(),
            StandardCategoryID = as.character(),
            DetailedStandardCategoryID = as.character(),
            Value_Dollars = as.numeric(),
            Value_Hours = as.numeric()
        ),
        AllocationMethods = list(
            ID = as.character(),
            AllocationMethodTypeID = as.character(),
            IsUnitOrSublotAllocationMethod = as.logical(),
            Name = as.character()
        ),
        AllocationComponents = list(
            AllocationMethodID = as.character(),
            OrderOrLotID = as.character(),
            EndItemID = as.character(),
            WBSElementID = as.character(),
            UnitOrSublotID = as.character(),
            PercentValue = as.numeric()
        ),
        SummaryRemarks = list(
            OrderOrLotID = as.character(),
            Text = as.character()
        ),
        WBSElementRemarks = list(
            OrderOrLotID = as.character(),
            WBSElementID = as.character(),
            Text = as.character()
        ),
        WBSDictionaryDefinitions = list(
            WBSElementID = as.character(),
            Text = as.character()
        ),
        CostHourTagDefinitions = list(
            CostHourTagID = as.character(),
            Name = as.character(),
            Text = as.character()
        )
    ),
    class = "flexfile"
)

qty_rpt <- structure(
    list(
        ReportMetadata = list(
            SecurityClassification = as.character(),
            ProprietaryStatement = as.character(),
            ProgramName = as.character(),
            PhaseOrMilestoneID = as.character(),
            PrimeMissionProduct = as.character(),
            CommodityType = as.character(),
            ReportingOrganization_OrganizationName = as.character(),
            ReportingOrganization_DivisionName = as.character(),
            ReportingOrganization_CageCode = as.character(),
            ReportingOrganization_Location_Street = as.character(),
            ReportingOrganization_Location_City = as.character(),
            ReportingOrganization_Location_State = as.character(),
            ReportingOrganization_Location_ZipCode = as.character(),
            ReportingOrganization_Location_Country = as.character(),
            ApprovedPlanNumber = as.character(),
            ApprovedPlanRevisionNumber = as.character(),
            CustomerName = as.character(),
            ContractTypeID = as.character(),
            ContractPrice = as.numeric(),
            ContractCeiling = as.numeric(),
            ContractNumber = as.character(),
            PeriodOfPerformance_StartDate = lubridate::ymd(),
            PeriodOfPerformance_EndDate = lubridate::ymd(),
            ReportCycleID = as.character(),
            SubmissionEvent_Number = as.integer(),
            SubmissionEvent_Name = as.character(),
            SubmissionEvent_IsWildcard = as.logical(),
            ResubmissionNumber = as.integer(),
            ReportAsOf = lubridate::ymd(),
            PointOfContact_Name = as.character(),
            PointOfContact_Department = as.character(),
            PointOfContact_TelephoneNumber = as.character(),
            PointOfContact_EmailAddress = as.character(),
            DatePrepared = lubridate::ymd()
        ),
        OrdersOrLots = list(
            ID = as.character(),
            Name = as.character(),
            PhaseOrMilestoneID = as.character(),
            CustomerName = as.character(),
            ContractTypeID = as.character(),
            ContractPrice = as.numeric(),
            ContractCeiling = as.numeric(),
            PeriodOfPerformance_StartDate = lubridate::ymd(),
            PeriodOfPerformance_EndDate = lubridate::ymd(),
            AppropriationTypeID = as.character()
        ),
        EndItems = list(
            ID = as.character(),
            Name = as.character()
        ),
        WBS = list(
            Level = as.integer(),
            ID = as.character(),
            Name = as.character(),
            ParentID = as.character()
        ),
        QuantitiesAtCompletion = list(
            OrderOrLotID = as.character(),
            EndItemID = as.character(),
            WBSElementID = as.character(),
            DeliveredQuantityAtCompletion = as.numeric(),
            InternalQuantityAtCompletion = as.numeric(),
            CoproductionOrConcurrentQuantityAtCompletion = as.numeric(),
            GFEQuantityAtCompletion = as.numeric()
        ),
        QuantitiesToDate = list(
            OrderOrLotID = as.character(),
            WBSElementID = as.character(),
            CompletedQuantityToDate = as.numeric(),
            InProcessQuantity = as.numeric()
        ),
        ProductionSequence = list(
            EndItemID = as.character(),
            FirstUnitNumber = as.integer(),
            LastUnitNumber = as.integer(),
            OrderOrLotID = as.character(),
            IsInternal = as.logical()
        ),
        SummaryRemarks = list(
            OrderOrLotID = as.character(),
            Text = as.character()
        ),
        WBSElementRemarks = list(
            OrderOrLotID = as.character(),
            WBSElementID = as.character(),
            Text = as.character()
        )
    ),
    class = "qty_rpt"
)
