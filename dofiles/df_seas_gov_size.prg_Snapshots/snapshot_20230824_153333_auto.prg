'Opens a workfile
wfcreate(wf=eviews_gov_size) q 2006.1 2022.4

'Working directories
'Gauss working directory: 'C:/Users/MatildeCerdaRuiz/Documents/GitHub/govsize_2023'
'Euler working directory: '/Users/axelcanales/Documents/GitHub/govsize_2023'
'Euler working directory WINDOWS: 'C:/Users/Axel Canales/Documents/GitHub/govsize_2023'

'Read data from csv created in Rstudio
import "C:\Users\MatildeCerdaRuiz\Documents\GitHub\govsize_2023\raw_data.csv" ftype=ascii rectype=crlf skip=0 fieldtype=delimited delim=comma colhead=1 eoltype=pad badfield=NA @freq Q @id @date(date) @destid @date @smpl @all

'dummy variables
series d_2008 = @recode(@date<@dateval("2008q3") or @date>@dateval("2009q1"),0,1)

series d_2018= @recode(@date>@dateval("2018q2"),1,0)

'Rename variables  log_gdp_pc

rename gov_gdp x1
rename priv_inv_gdp x2
rename tr_op x3

'Desestacionalizar
log_gdp_pc.stl log_gdp_pc_saf log_gdp_pc_trend log_gdp_pc_sa
delete log_gdp_pc_saf log_gdp_pc_trend

pub_inv_gdp.stl PUB_INV_GDP_SAf pub_inv_gdp_trend PUB_INV_GDP_SA
delete PUB_INV_GDP_SAf pub_inv_gdp_trend

x1.stl x1_saf x1_trend x1_sa
delete x1_saf x1_trend

x2.stl x2_saf x2_trend x2_sa
delete x2_saf x2_trend

x3.stl x3_saf x3_trend x3_sa
delete x3_saf x3_trend

'============ Unit Root test ============
'ADF
'diferencias	

freeze(adftable_dif_log_gdp_pc_sa_c) log_gdp_pc_sa.uroot(dif=1)
freeze(adftable_dif_log_gdp_pc_sa_ct) log_gdp_pc_sa.uroot(exog=trend, dif=1)
freeze(adftable_dif_log_gdp_pc_sa_nc) log_gdp_pc_sa.uroot(exog=none, dif=1)

freeze(adftable_dif_PUB_INV_GDP_SA_c) x1_sa.uroot(dif=1)
freeze(adftable_dif_PUB_INV_GDP_SA_ct) x1_sa.uroot(exog=trend, dif=1)
freeze(adftable_dif_lPUB_INV_GDP_SA_nc) x1_sa.uroot(exog=none, dif=1)

for !i=1 to 3
freeze(adftable_dif_{!i}_c) x{!i}_sa.uroot(dif=1)
freeze(adftable_dif_{!i}_ct) x{!i}_sa.uroot(exog=trend, dif=1)
freeze(adftable_dif_{!i}_nc) x{!i}_sa.uroot(exog=none, dif=1)

next

'Niveles

freeze(adftable_log_gdp_pc_sa_c) log_gdp_pc_sa.uroot
freeze(adftable_log_gdp_pc_sa_ct) log_gdp_pc_sa.uroot(exog=trend)
freeze(adftable_log_gdp_pc_sa_nc) log_gdp_pc_sa.uroot(exog=none)

freeze(adftable_PUB_INV_GDP_SA_c) PUB_INV_GDP_SA.uroot
freeze(adftable_PUB_INV_GDP_SA_ct) PUB_INV_GDP_SA.uroot(exog=trend)
freeze(adftable_lPUB_INV_GDP_SA_nc) PUB_INV_GDP_SA.uroot(exog=none)

for !i=1 to 3
freeze(adftable_{!i}_c) x{!i}_sa.uroot
freeze(adftable_{!i}_ct) x{!i}_sa.uroot(exog=trend)
freeze(adftable_{!i}_nc) x{!i}_sa.uroot(exog=none)

next

'Phillips-perron 
'Niveles

freeze(pptable_log_gdp_pc_sa_c) log_gdp_pc_sa.uroot(pp)
freeze(pptable_log_gdp_pc_sa_ct) log_gdp_pc_sa.uroot(exog=trend, pp)
freeze(pptable_log_gdp_pc_sa_nc) log_gdp_pc_sa.uroot(exog=none, pp)

freeze(pptable_pub_inv_gdpc_s_c) PUB_INV_GDP_SA.uroot(pp)
freeze(pptable_PUB_INV_GDP_SA_ct) PUB_INV_GDP_SA.uroot(exog=trend, pp)
freeze(pptable_PUB_INV_GDP_SA_nc) PUB_INV_GDP_SA.uroot(exog=none, pp)

for !i=1 to 3
freeze(pptable{!i}_c) x{!i}_sa.uroot(pp)
freeze(pptable{!i}_ct) x{!i}_sa.uroot(exog=trend, pp)
freeze(pptable{!i}_nc) x{!i}_sa.uroot(exog=none, pp)

next

'Diferencias
freeze(pptable_dif_log_gdp_pc_sa_c) log_gdp_pc_sa.uroot(dif=1, pp)
freeze(pptable_dif_log_gdp_pc_sa_ct) log_gdp_pc_sa.uroot(exog=trend, dif=1, pp)
freeze(pptable_dif_log_gdp_pc_sa_nc) log_gdp_pc_sa.uroot(exog=none,dif=1, pp)

freeze(pptable_dif_pub_inv_gdpc_s_c) PUB_INV_GDP_SA.uroot(dif=1,pp)
freeze(pptable_dif_PUB_INV_GDP_SA_ct) PUB_INV_GDP_SA.uroot(exog=trend, dif=1, pp)
freeze(pptable_dif_PUB_INV_GDP_SA_nc) PUB_INV_GDP_SA.uroot(exog=none, dif=1, pp)

for !i=1 to 3
freeze(pptable_dif_{!i}_c) x{!i}_sa.uroot(dif=1, pp)
freeze(pptable_dif_{!i}_ct) x{!i}_sa.uroot(exog=trend, dif=1, pp)
freeze(pptable_dif_{!i}_nc) x{!i}_sa.uroot(exog=none, dif=1, pp)

next

'===========================
'Long Rung models
'===========================

'------------------------------------------------------------------------------------------------------------------------------
'Variable independiente del gobierno: Inversion fija pública
'------------------------------------------------------------------------------------------------------------------------------

'Modelos lineales  con todas las metodologias en muestra completa

smpl @all
equation eq_lin_inv_1.ls(cov=hac) log_gdp_pc_sa c d_2008 d_2018   x2_sa x3_sa PUB_INV_GDP_SA 


smpl @all
equation eq_lin_inv_2.cointreg log_gdp_pc_sa x2_sa x3_sa PUB_INV_GDP_SA @determ d_2008 d_2018 

smpl @all
equation eq_lin_inv_3.cointreg(method=ccr) log_gdp_pc_sa x2_sa x3_sa PUB_INV_GDP_SA @determ d_2008 d_2018 


'Modelos cuadráticos con todas las metodologias en muestra completa

smpl @all
equation eq_quad_inv_1.ls(cov=hac) log_gdp_pc_sa c d_2008 d_2018  x2_sa x3_sa PUB_INV_GDP_SA  (PUB_INV_GDP_SA )^2

smpl @all
equation eq_quad_inv_2.cointreg log_gdp_pc_sa x2_sa x3_sa  PUB_INV_GDP_SA  (PUB_INV_GDP_SA )^2 @determ d_2008 d_2018 

smpl @all
equation eq_quad_inv_3.cointreg(method=ccr) log_gdp_pc_sa x2_sa x3_sa PUB_INV_GDP_SA  (PUB_INV_GDP_SA )^2 @determ d_2008 d_2018 

'Modelos cúbicos con todas las metodologias en muestra completa

smpl @all
equation eq_cub_inv_1.ls(cov=hac) log_gdp_pc_sa c d_2008 d_2018  x2_sa x3_sa PUB_INV_GDP_SA  (PUB_INV_GDP_SA )^2 (PUB_INV_GDP_SA )^3

smpl @all
equation eq_cub_inv_2.cointreg log_gdp_pc_sa  x2_sa x3_sa PUB_INV_GDP_SA  (PUB_INV_GDP_SA )^2 (PUB_INV_GDP_SA )^3 @determ d_2008 d_2018 

smpl @all
equation eq_cub_inv_3.cointreg(method=ccr) log_gdp_pc_sa  x2_sa x3_sa PUB_INV_GDP_SA  (PUB_INV_GDP_SA )^2 (PUB_INV_GDP_SA )^3 @determ d_2008 d_2018 

'------------------------------------------------------------------------------------------------------------------------------
'Variable independiente del gobierno: Gasto Agregado
'------------------------------------------------------------------------------------------------------------------------------

'Modelos lineales  con todas las metodologias en muestra completa

smpl @all
equation eq_lin_ag_1.ls(cov=hac) log_gdp_pc_sa c  d_2008 d_2018  x2_sa x3_sa x1_sa 


smpl @all
equation eq_lin_ag_2.cointreg log_gdp_pc_sa  x2_sa x3_sa  x1_sa @determ d_2008 d_2018

smpl @all
equation eq_lin_ag_3.cointreg(method=ccr) log_gdp_pc_sa  x2_sa x3_sa  x1_sa  @determ d_2008 d_2018 


'Modelos cuadráticos con todas las metodologias en muestra completa

smpl @all
equation eq_quad_ag_1.ls(cov=hac) log_gdp_pc_sa c d_2008 d_2018  x2_sa x3_sa x1_sa  (x1_sa )^2

smpl @all
equation eq_quad_ag_2.cointreg log_gdp_pc_sa   x2_sa x3_sa x1_sa  (x1_sa )^2 @determ d_2008 d_2018

smpl @all
equation eq_quad_ag_3.cointreg(method=ccr) log_gdp_pc_sa x2_sa x3_sa  x1_sa  (x1_sa )^2  @determ d_2008 d_2018

'Modelos cúbicos con todas las metodologias en muestra completa

smpl @all
equation eq_cub_ag_1.ls(cov=hac) log_gdp_pc_sa c d_2008 d_2018 x2_sa x3_sa  x1_sa  (x1_sa )^2 (x1_sa )^3

smpl @all
equation eq_cub_ag_2.cointreg log_gdp_pc_sa  x2_sa x3_sa  x1_sa  (x1_sa )^2 (x1_sa )^3 @determ d_2008 d_2018

smpl @all
equation eq_cub_ag_3.cointreg(method=ccr) log_gdp_pc_sa  x2_sa x3_sa x1_sa  (x1_sa )^2 (x1_sa )^3 @determ d_2008 d_2018 

'=============================
'Bootstrap
'=============================
'Gasto agregado

for !x=1 to 30
if !x<13 then
smpl @first+{!x} @last
equation reg_agregado{!x}.COINTREG log_gdp_pc_sa X1_SA (X1_SA)^2 X2_SA X3_SA @DETERM D_2008 D_2018
else
smpl @first+{!x} @last
equation reg_agregado{!x}.COINTREG log_gdp_pc_sa X1_SA (X1_SA)^2 X2_SA X3_SA @DETERM D_2018
endif

next

for !x=1 to 30
table(30,1) lineales_agregado
table(30,1) cuadraticos_agregado
lineales_agregado({!x},1) = reg_agregado{!x}.@coef(1)
cuadraticos_agregado({!x},1) = reg_agregado{!x}.@coef(2)
next

'inversion Fija publica

for !x=1 to 30
if !x<13 then

smpl @first+{!x} @last
equation reg_inversion{!x}.COINTREG log_gdp_pc_sa PUB_INV_GDP_SA  (PUB_INV_GDP_SA )^2 X2_SA X3_SA @DETERM D_2008 D_2018
else
smpl @first+{!x} @last
equation reg_inversion{!x}.COINTREG log_gdp_pc_sa PUB_INV_GDP_SA  (PUB_INV_GDP_SA )^2 X2_SA X3_SA @DETERM D_2018
endif
next

for !x=1 to 30
table(30,1) lineales_inversion
table(30,1) cuadraticos_inversion
lineales_inversion({!x},1) = reg_inversion{!x}.@coef(1)
cuadraticos_inversion({!x},1) = reg_inversion{!x}.@coef(2)
next


'=============================
'Tabla Gasto Agregado
'=============================
'Modelos OLS
'lineales
for !x=1 to 6
table(17, 9) tabla_modelos_agregado
tabla_modelos_agregado(2*{!x}-1,1) = eq_lin_ag_1.@coef({!x})
tabla_modelos_agregado(2*{!x},1) = eq_lin_ag_1.@pval({!x})
next
'cuadraticos
for !x=1 to 7
tabla_modelos_agregado(2*{!x}-1,2) = eq_quad_ag_1.@coef({!x})
tabla_modelos_agregado(2*{!x},2) = eq_quad_ag_1.@pval({!x})
next
'cubicos
for !x=1 to 8
tabla_modelos_agregado(2*{!x}-1,3) = eq_cub_ag_1.@coef({!x})
tabla_modelos_agregado(2*{!x},3) = eq_cub_ag_1.@pval({!x})
next
'cubicos valor R2
tabla_modelos_agregado(17,1) = eq_lin_ag_1.@r2
tabla_modelos_agregado(17,2) = eq_quad_ag_1.@r2
tabla_modelos_agregado(17,3) = eq_cub_ag_1.@r2

'Modelos FMOLS
'Lineales
for !x=1 to 6
if !x<4 then
tabla_modelos_agregado(2*(!x+3)-1,4) = eq_lin_ag_2.@coef({!x})
tabla_modelos_agregado(2*(!x+3),4) = eq_lin_ag_2.@pval({!x})
else
tabla_modelos_agregado(2*(!x-3) -1,4) = eq_lin_ag_2.@coef({!x})
tabla_modelos_agregado(2*(!x-3),4) = eq_lin_ag_2.@pval({!x})
endif
next 

'cuadraticos
for !x=1 to 7
if !x<5 then
tabla_modelos_agregado(2*(!x + 3) - 1,5) = eq_quad_ag_2.@coef({!x})
tabla_modelos_agregado(2*(!x+ 3),5) = eq_quad_ag_2.@pval({!x})
else
tabla_modelos_agregado(2*(!x - 4) - 1,5) = eq_quad_ag_2.@coef({!x})
tabla_modelos_agregado(2*(!x - 4),5) = eq_quad_ag_2.@pval({!x})
endif
next 

'cubicos
for !x=1 to 8
if !x<6 then
tabla_modelos_agregado(2*(!x + 3) - 1,6) = eq_cub_ag_2.@coef({!x})
tabla_modelos_agregado(2*(!x+ 3),6) = eq_cub_ag_2.@pval({!x})
else
tabla_modelos_agregado(2*(!x - 5) - 1,6) = eq_cub_ag_2.@coef({!x})
tabla_modelos_agregado(2*(!x - 5),6) = eq_cub_ag_2.@pval({!x})
endif
next 

tabla_modelos_agregado(17,4) = eq_lin_ag_2.@r2
tabla_modelos_agregado(17,5) = eq_quad_ag_2.@r2
tabla_modelos_agregado(17,6) = eq_cub_ag_2.@r2


'Modelos CCR
'Lineales
for !x=1 to 6
if !x<4 then
tabla_modelos_agregado(2*(!x+3)-1,7) = eq_lin_ag_3.@coef({!x})
tabla_modelos_agregado(2*(!x+3),7)= eq_lin_ag_3.@pval({!x})
else
tabla_modelos_agregado(2*(!x-3) -1,7) = eq_lin_ag_3.@coef({!x})
tabla_modelos_agregado(2*(!x-3),7) = eq_lin_ag_3.@pval({!x})
endif
next 

'cuadraticos
for !x=1 to 7
if !x<5 then
tabla_modelos_agregado(2*(!x + 3) - 1,8) = eq_quad_ag_3.@coef({!x})
tabla_modelos_agregado(2*(!x+ 3),8) = eq_quad_ag_3.@pval({!x})
else
tabla_modelos_agregado(2*(!x - 4) - 1,8) = eq_quad_ag_3.@coef({!x})
tabla_modelos_agregado(2*(!x - 4),8) = eq_quad_ag_3.@pval({!x})
endif
next 

'cubicos
for !x=1 to 8
if !x<6 then
tabla_modelos_agregado(2*(!x + 3) - 1,9) = eq_cub_ag_3.@coef({!x})
tabla_modelos_agregado(2*(!x+ 3),9) = eq_cub_ag_3.@pval({!x})
else
tabla_modelos_agregado(2*(!x - 5) - 1,9) = eq_cub_ag_3.@coef({!x})
tabla_modelos_agregado(2*(!x - 5),9) = eq_cub_ag_3.@pval({!x})
endif
next 

tabla_modelos_agregado(17,7) = eq_lin_ag_3.@r2
tabla_modelos_agregado(17,8) = eq_quad_ag_3.@r2
tabla_modelos_agregado(17,9) = eq_cub_ag_3.@r2


'jb test
for !x=1 to 3
eq_lin_ag_{!x}.makeresids resid_eq_lin_ag_{!x}
freeze(jb_eq_lin_ag_{!x}) resid_eq_lin_ag_{!x}.stats
next
for !x=1 to 3
eq_quad_ag_{!x}.makeresids resid_eq_quad_ag_{!x}
freeze(jb_eq_quad_ag_{!x}) resid_eq_quad_ag_{!x}.stats
next
for !x=1 to 3
eq_cub_ag_{!x}.makeresids resid_eq_cub_ag_{!x}
freeze(jb_eq_cub_ag_{!x}) resid_eq_cub_ag_{!x}.stats
next
tabla_modelos_agregado(18,1) = @val(jb_eq_lin_ag_1(14,2))
tabla_modelos_agregado(18,2) = @val(jb_eq_quad_ag_1(14,2))
tabla_modelos_agregado(18,3) = @val(jb_eq_cub_ag_1(14,2))
tabla_modelos_agregado(18,4) = @val(jb_eq_lin_ag_2(14,2))
tabla_modelos_agregado(18,5) = @val(jb_eq_quad_ag_2(14,2))
tabla_modelos_agregado(18,6) = @val(jb_eq_cub_ag_2(14,2))
tabla_modelos_agregado(18,7) = @val(jb_eq_lin_ag_3(14,2))
tabla_modelos_agregado(18,8) = @val(jb_eq_quad_ag_3(14,2))
tabla_modelos_agregado(18,9) = @val(jb_eq_cub_ag_3(14,2))

'auto LM test

freeze(auto_eq_lin_ag_1) eq_lin_ag_1.auto
freeze(auto_eq_quad_ag_1) eq_quad_ag_1.auto
freeze(auto_eq_cub_ag_1) eq_cub_ag_1.auto
tabla_modelos_agregado(19,1) = @val(auto_eq_lin_ag_1(4,5))
tabla_modelos_agregado(19,2) = @val(auto_eq_quad_ag_1(4,5))
tabla_modelos_agregado(19,3) = @val(auto_eq_cub_ag_1(4,5))

'heteroskedasticity BPG test
freeze(bpg_eq_lin_ag_1) eq_lin_ag_1.hettest c x2_sa x3_sa d_2008 d_2018 x1_sa
freeze(bpg_eq_quad_ag_1) eq_lin_ag_1.hettest c x2_sa x3_sa d_2008 d_2018 x1_sa (x1_sa)^2
freeze(bpg_eq_cub_ag_1) eq_lin_ag_1.hettest c x2_sa x3_sa d_2008 d_2018 x1_sa (x1_sa)^3
tabla_modelos_agregado(20,1) = @val(bpg_eq_lin_ag_1(4,5))
tabla_modelos_agregado(20,2) = @val(bpg_eq_quad_ag_1(4,5))
tabla_modelos_agregado(20,3) = @val(bpg_eq_cub_ag_1(4,5))

'export
tabla_modelos_agregado.save(t=csv) "C:\Users\MatildeCerdaRuiz\Documents\GitHub\govsize_2023\dofiles\tabla_modelos_agregado.csv"




'=============================
'Tabla Inversion Fija Publica
'=============================
'Modelos OLS
'lineales
for !x=1 to 6
table(17, 9) tabla_modelos_inv
tabla_modelos_inv(2*{!x}-1,1) = eq_lin_inv_1.@coef({!x})
tabla_modelos_inv(2*{!x},1) = eq_lin_inv_1.@pval({!x})
next
'cuadraticos
for !x=1 to 7
tabla_modelos_inv(2*{!x}-1,2) = eq_quad_inv_1.@coef({!x})
tabla_modelos_inv(2*{!x},2) = eq_quad_inv_1.@pval({!x})
next
'cubicos
for !x=1 to 8
tabla_modelos_inv(2*{!x}-1,3) = eq_cub_inv_1.@coef({!x})
tabla_modelos_inv(2*{!x},3) = eq_cub_inv_1.@pval({!x})
next
tabla_modelos_inv(17,1) = eq_lin_inv_1.@r2
tabla_modelos_inv(17,2) = eq_quad_inv_1.@r2
tabla_modelos_inv(17,3) = eq_cub_inv_1.@r2

'Modelos FMOLS
'Lineales
for !x=1 to 6
if !x<4 then
tabla_modelos_inv(2*(!x+3)-1,4) = eq_lin_inv_2.@coef({!x})
tabla_modelos_inv(2*(!x+3),4) = eq_lin_inv_2.@pval({!x})
else
tabla_modelos_inv(2*(!x-3) -1,4) = eq_lin_inv_2.@coef({!x})
tabla_modelos_inv(2*(!x-3),4) = eq_lin_inv_2.@pval({!x})
endif
next 

'cuadraticos
for !x=1 to 7
if !x<5 then
tabla_modelos_inv(2*(!x + 3) - 1,5) = eq_quad_inv_2.@coef({!x})
tabla_modelos_inv(2*(!x+ 3),5) = eq_quad_inv_2.@pval({!x})
else
tabla_modelos_agregado(2*(!x - 4) - 1,5) = eq_quad_inv_2.@coef({!x})
tabla_modelos_inv(2*(!x - 4),5) = eq_quad_inv_2.@pval({!x})
endif
next 

'cubicos
for !x=1 to 8
if !x<6 then
tabla_modelos_inv(2*(!x + 3) - 1,6) = eq_cub_inv_2.@coef({!x})
tabla_modelos_inv(2*(!x+ 3),6) = eq_cub_inv_2.@pval({!x})
else
tabla_modelos_inv(2*(!x - 5) - 1,6) = eq_cub_inv_2.@coef({!x})
tabla_modelos_inv(2*(!x - 5),6) = eq_cub_inv_2.@pval({!x})
endif
next 

tabla_modelos_inv(17,4) = eq_lin_inv_2.@r2
tabla_modelos_inv(17,5) = eq_quad_inv_2.@r2
tabla_modelos_inv(17,6) = eq_cub_inv_2.@r2


'Modelos CCR
'Lineales
for !x=1 to 6
if !x<4 then
tabla_modelos_inv(2*(!x+3)-1,7) = eq_lin_inv_3.@coef({!x})
tabla_modelos_inv(2*(!x+3),7)= eq_lin_inv_3.@pval({!x})
else
tabla_modelos_inv(2*(!x-3) -1,7) = eq_lin_inv_3.@coef({!x})
tabla_modelos_inv(2*(!x-3),7) = eq_lin_inv_3.@pval({!x})
endif
next 

'cuadraticos
for !x=1 to 7
if !x<5 then
tabla_modelos_inv(2*(!x + 3) - 1,8) = eq_quad_inv_3.@coef({!x})
tabla_modelos_inv(2*(!x+ 3),8) = eq_quad_inv_3.@pval({!x})
else
tabla_modelos_inv(2*(!x - 4) - 1,8) = eq_quad_inv_3.@coef({!x})
tabla_modelos_inv(2*(!x - 4),8) = eq_quad_inv_3.@pval({!x})
endif
next 

'cubicos
for !x=1 to 8
if !x<6 then
tabla_modelos_inv(2*(!x + 3) - 1,9) = eq_cub_inv_3.@coef({!x})
tabla_modelos_inv(2*(!x+ 3),9) = eq_cub_inv_3.@pval({!x})
else
tabla_modelos_inv(2*(!x - 5) - 1,9) = eq_cub_inv_3.@coef({!x})
tabla_modelos_inv(2*(!x - 5),9) = eq_cub_inv_3.@pval({!x})
endif
next 
tabla_modelos_inv(17,7) = eq_lin_inv_3.@r2
tabla_modelos_inv(17,8) = eq_quad_inv_3.@r2
tabla_modelos_inv(17,9) = eq_cub_inv_3.@r2

'jb test
for !x=1 to 3
eq_lin_inv_{!x}.makeresids resid_eq_lin_inv_{!x}
freeze(jb_eq_lin_inv_{!x}) resid_eq_lin_inv_{!x}.stats
next
for !x=1 to 3
eq_quad_inv_{!x}.makeresids resid_eq_quad_inv_{!x}
freeze(jb_eq_quad_inv_{!x}) resid_eq_quad_inv_{!x}.stats
next
for !x=1 to 3
eq_cub_inv_{!x}.makeresids resid_eq_cub_inv_{!x}
freeze(jb_eq_cub_inv_{!x}) resid_eq_cub_inv_{!x}.stats
next
tabla_modelos_inv(18,1) = @val(jb_eq_lin_inv_1(14,2))
tabla_modelos_inv(18,2) = @val(jb_eq_quad_inv_1(14,2))
tabla_modelos_inv(18,3) = @val(jb_eq_cub_inv_1(14,2))
tabla_modelos_inv(18,4) = @val(jb_eq_lin_inv_2(14,2))
tabla_modelos_inv(18,5) = @val(jb_eq_quad_inv_2(14,2))
tabla_modelos_inv(18,6) = @val(jb_eq_cub_inv_2(14,2))
tabla_modelos_inv(18,7) = @val(jb_eq_lin_inv_3(14,2))
tabla_modelos_inv(18,8) = @val(jb_eq_quad_inv_3(14,2))
tabla_modelos_inv(18,9) = @val(jb_eq_cub_inv_3(14,2))

'auto LM test

freeze(auto_eq_lin_inv_1) eq_lin_inv_1.auto
freeze(auto_eq_quad_inv_1) eq_quad_inv_1.auto
freeze(auto_eq_cub_inv_1) eq_cub_inv_1.auto
tabla_modelos_inv(19,1) = @val(auto_eq_lin_inv_1(3,2))
tabla_modelos_inv(19,2) = @val(auto_eq_quad_inv_1(3,2))
tabla_modelos_inv(19,3) = @val(auto_eq_cub_inv_1(3,2))

'heteroskedasticity BPG test
freeze(bpg_eq_lin_inv_1) eq_lin_inv_1.hettest c x2_sa x3_sa d_2008 d_2018 PUB_INV_GDP_SA
freeze(bpg_eq_quad_inv_1) eq_lin_inv_1.hettest c x2_sa x3_sa d_2008 d_2018 PUB_INV_GDP_SA (PUB_INV_GDP_SA)^2
freeze(bpg_eq_cub_inv_1) eq_lin_inv_1.hettest c x2_sa x3_sa d_2008 d_2018 PUB_INV_GDP_SA (PUB_INV_GDP_SA)^3
tabla_modelos_inv(20,1) = @val(bpg_eq_lin_inv_1(3,2))
tabla_modelos_inv(20,2) = @val(bpg_eq_quad_inv_1(3,2))
tabla_modelos_inv(20,3) = @val(bpg_eq_cub_inv_1(3,2))

'export
tabla_modelos_inv.save(t=csv) "C:\Users\MatildeCerdaRuiz\Documents\GitHub\govsize_2023\dofiles\tabla_modelos_inv.csv"

'Johansen
'Gasto agregado
group g1
g1.add log_gdp_pc_sa x1_sa x2_sa x3_sa
g1.coint(s,4)
freeze(johansen_test_ag) g1.coint(s, 1 4) 

'Inversion Fija Publica 
group g2
g2.add log_gdp_pc_sa PUB_INV_GDP_SA x2_sa x3_sa
g2.coint(s,4)
freeze(johansen_test_inv) g1.coint(s, 1 4) 

table(4,5) tabla_johansen
tabla_johansen(1,1)=@val(johansen_test_ag(12,2))
tabla_johansen(1,2)=@val(johansen_test_ag(12,3))
tabla_johansen(1,3)=@val(johansen_test_ag(12,4))
tabla_johansen(1,4)=@val(johansen_test_ag(12,5))
tabla_johansen(1,5)=@val(johansen_test_ag(12,6))
tabla_johansen(2,1)=@val(johansen_test_ag(13,2))
tabla_johansen(2,2)=@val(johansen_test_ag(13,3))
tabla_johansen(2,3)=@val(johansen_test_ag(13,4))
tabla_johansen(2,4)=@val(johansen_test_ag(13,5))
tabla_johansen(2,5)=@val(johansen_test_ag(13,6))
tabla_johansen(3,1)=@val(johansen_test_inv(12,2))
tabla_johansen(3,2)=@val(johansen_test_inv(12,3))
tabla_johansen(3,3)=@val(johansen_test_inv(12,4))
tabla_johansen(3,4)=@val(johansen_test_inv(12,5))
tabla_johansen(3,5)=@val(johansen_test_inv(12,6))
tabla_johansen(4,1)=@val(johansen_test_inv(13,2))
tabla_johansen(4,2)=@val(johansen_test_inv(13,3))
tabla_johansen(4,3)=@val(johansen_test_inv(13,4))
tabla_johansen(4,4)=@val(johansen_test_inv(13,5))
tabla_johansen(4,5)=@val(johansen_test_inv(13,6))
tabla_johansen.save(t=csv) "C:\Users\MatildeCerdaRuiz\Documents\GitHub\govsize_2023\dofiles\tabla_johansen.csv"


'bootstrap
lineales_agregado.save(t=csv) "C:\Users\MatildeCerdaRuiz\Documents\GitHub\govsize_2023\dofiles\lineales_agregado.csv"
cuadraticos_agregado.save(t=csv) "C:\Users\MatildeCerdaRuiz\Documents\GitHub\govsize_2023\dofiles\cuadraticos_agregado.csv"
lineales_inversion.save(t=csv) "C:\Users\MatildeCerdaRuiz\Documents\GitHub\govsize_2023\dofiles\lineales_inversion.csv"
cuadraticos_inversion.save(t=csv) "C:\Users\MatildeCerdaRuiz\Documents\GitHub\govsize_2023\dofiles\cuadraticos_inversion.csv"


'Cuadro 8 Estimacion de tamano optimo del gobierno
table(3, 7) cuadro8
cuadro8(1,4) = -100*eq_quad_ag_1.@coef(6)/(2*eq_quad_ag_1.@coef(7))
cuadro8(2,4) = -100*eq_quad_ag_2.@coef(1)/(2*eq_quad_ag_2.@coef(2))
cuadro8(3,4) = -100*eq_quad_ag_3.@coef(6)/(2*eq_quad_ag_3.@coef(7))
cuadro8(1,5) = -100*eq_quad_inv_1.@coef(6)/(2*eq_quad_inv_1.@coef(7))
cuadro8(2,5) = -100*eq_quad_inv_2.@coef(6)/(2*eq_quad_inv_2.@coef(7))
cuadro8(3,5) = -100*eq_quad_inv_3.@coef(6)/(2*eq_quad_inv_3.@coef(7))

cuadro8.save(t=csv) "C:\Users\MatildeCerdaRuiz\Documents\GitHub\govsize_2023\dofiles\cuadro8.csv"


