
## At first I thought I would use allocation tables to split material costs by
## WBS but I don't think that's what the allocation tables are setup for. I've
## commented all that code out for now and just used left_joins to modify the
## material cost tables "pre-submission" on material_cost_by_unit.R.
# allocation_methods_table <- tibble::tribble(
#     ~ID, ~AllocationMethodTypeID, ~IsUnitOrSublotAllocationMethod, ~Name,
#     1,   "PERCENT",               TRUE,                            "General Purpose Variant Allocation",
#     2,   "PERCENT",               TRUE,                            "Command & Control Variant Allocation",
#     3,   "PERCENT",               TRUE,                            "Heavy Weapons Variant Allocation"
# )
