*** |  (C) 2008-2018 Potsdam Institute for Climate Impact Research (PIK),
*** |  authors, and contributors see AUTHORS file
*** |  This file is part of MAgPIE and licensed under GNU AGPL Version 3
*** |  or later. See LICENSE file or go to http://www.gnu.org/licenses/
*** |  Contact: magpie@pik-potsdam.de


positive variables
 vm_nr_inorg_fert_reg(i,land_ag)  Inorganic fertilizer application (Tg N per yr)
 vm_nr_inorg_fert_costs(i)        Cost of inorganic fertilizers (mio. USD per yr)
 v50_nr_eff(i)                    Cropland nutrient uptake efficiency (Tg N per yr)
 v50_nr_eff_pasture(i)            Pasture nutrient uptake efficiency (Tg N per yr)
 v50_nr_withdrawals(i,kcr)        Withdrawals of Nr from soils (Tg N per yr)
 v50_nr_deposition(i,land)        Atmospheric deposition (Tg N per yr)
;

equations
 q50_nr_cost_fert(i)        Fertilizer costs (mio. USD per yr)
 q50_nr_bal_crp(i)          Cropland nutrient inputs have to equal withdrawals and losses (Tg N per yr)
 q50_nr_withdrawals(i,kcr)  Calculating nr withdrawals (Tg N per yr)
 q50_nr_bal_pasture(i)      Nitrogen balance pasture lands (Tg N per yr)
 q50_nr_deposition(i,land)  Atmospheric deposition (Tg N per yr)
;

parameters
 ic50_atmospheric_deposition_rates(i,land)   Atmospheric deposition rate (t N per ha)
;

*#################### R SECTION START (OUTPUT DECLARATIONS) ####################
parameters
 ov_nr_inorg_fert_reg(t,i,land_ag,type) inorganic fertilizer application (Tg N per yr)
 ov_nr_inorg_fert_costs(t,i,type)       cost of inorganic fertilizers (mio. USD per yr)
 ov50_nr_eff(t,i,type)                  cropland nutrient uptake efficiency (Tg N per yr)
 ov50_nr_eff_pasture(t,i,type)          pasture nutrient uptake efficiency (Tg N per yr)
 ov50_nr_withdrawals(t,i,kcr,type)      withdrawals of Nr from soils (Tg N per yr)
 ov50_nr_deposition(t,i,land,type)      atmospheric deposition (Tg N per yr)
 oq50_nr_cost_fert(t,i,type)            fertilizer costs (mio. USD per yr)
 oq50_nr_bal_crp(t,i,type)              cropland nutrient inputs have to equal withdrawals and losses (Tg N per yr)
 oq50_nr_withdrawals(t,i,kcr,type)      calculating nr withdrawals (Tg N per yr)
 oq50_nr_bal_pasture(t,i,type)          nitrogen balance pasture lands (Tg N per yr)
 oq50_nr_deposition(t,i,land,type)      atmospheric deposition (Tg N per yr)
;
*##################### R SECTION END (OUTPUT DECLARATIONS) #####################
