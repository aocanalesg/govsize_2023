'Opens a workfile
wfcreate(wf=eviews_gov_size) q 2006.1 2022.4

'Read data from csv created in Rstudio
import "C:\Users\Axel Canales\Documents\GitHub\govsize_2023\dofiles\df_seas.csv" ftype=ascii rectype=crlf skip=0 fieldtype=delimited delim=comma colhead=1 eoltype=pad badfield=NA @freq Q @id @date(date) @destid @date @smpl @all

'dummy variables
series d_2008 = @recode(@date<@dateval("2008q3") or @date>@dateval("2009q1"),1,0)

series d_2018= @recode(@date>@dateval("2018q2"),1,0)

'Rename variables

rename GOV_GDP_S x1_sa
rename PRIV_INV_GDP_S x2_sa
rename TR_OP_S x3_sa

'============ Unit Root test ============
'ADF
'diferencias	

freeze(adftable_dif_log_gdp_pc_s_c) log_gdp_pc_s.uroot(dif=1)
freeze(adftable_dif_log_gdp_pc_s_ct) log_gdp_pc_s.uroot(exog=trend, dif=1)
freeze(adftable_dif_log_gdp_pc_s_nc) log_gdp_pc_s.uroot(exog=none, dif=1)

freeze(adftable_dif_pub_inv_gdp_s_c) pub_inv_gdp_s.uroot(dif=1)
freeze(adftable_dif_pub_inv_gdp_s_ct) pub_inv_gdp_s.uroot(exog=trend, dif=1)
freeze(adftable_dif_lpub_inv_gdp_s_nc) pub_inv_gdp_s.uroot(exog=none, dif=1)

for !i=1 to 3
freeze(adftable_dif_{!i}_c) x{!i}_sa.uroot(dif=1)
freeze(adftable_dif_{!i}_ct) x{!i}_sa.uroot(exog=trend, dif=1)
freeze(adftable_dif_{!i}_nc) x{!i}_sa.uroot(exog=none, dif=1)

next

'Niveles

freeze(adftable_log_gdp_pc_s_c) log_gdp_pc_s.uroot
freeze(adftable_log_gdp_pc_s_ct) log_gdp_pc_s.uroot(exog=trend)
freeze(adftable_log_gdp_pc_s_nc) log_gdp_pc_s.uroot(exog=none)

freeze(adftable_pub_inv_gdp_s_c) pub_inv_gdp_s.uroot
freeze(adftable_pub_inv_gdp_s_ct) pub_inv_gdp_s.uroot(exog=trend)
freeze(adftable_lpub_inv_gdp_s_nc) pub_inv_gdp_s.uroot(exog=none)

for !i=1 to 3
freeze(adftable_{!i}_c) x{!i}_sa.uroot
freeze(adftable_{!i}_ct) x{!i}_sa.uroot(exog=trend)
freeze(adftable_{!i}_nc) x{!i}_sa.uroot(exog=none)

next

'Phillips-perron 
'Niveles

freeze(pptable_log_gdp_pc_s_c) log_gdp_pc_s.uroot(pp)
freeze(pptable_log_gdp_pc_s_ct) log_gdp_pc_s.uroot(exog=trend, pp)
freeze(pptable_log_gdp_pc_s_nc) log_gdp_pc_s.uroot(exog=none, pp)

freeze(pptable_pub_inv_gdpc_s_c) pub_inv_gdp_s.uroot(pp)
freeze(pptable_pub_inv_gdp_s_ct) pub_inv_gdp_s.uroot(exog=trend, pp)
freeze(pptable_pub_inv_gdp_s_nc) pub_inv_gdp_s.uroot(exog=none, pp)

for !i=1 to 3
freeze(pptable{!i}_c) x{!i}_sa.uroot(pp)
freeze(pptable{!i}_ct) x{!i}_sa.uroot(exog=trend, pp)
freeze(pptable{!i}_nc) x{!i}_sa.uroot(exog=none, pp)

next

'Diferencias
freeze(pptable_dif_log_gdp_pc_s_c) log_gdp_pc_s.uroot(dif=1, pp)
freeze(pptable_dif_log_gdp_pc_s_ct) log_gdp_pc_s.uroot(exog=trend, dif=1, pp)
freeze(pptable_dif_log_gdp_pc_s_nc) log_gdp_pc_s.uroot(exog=none,dif=1, pp)

freeze(pptable_dif_pub_inv_gdpc_s_c) pub_inv_gdp_s.uroot(dif=1,pp)
freeze(pptable_dif_pub_inv_gdp_s_ct) pub_inv_gdp_s.uroot(exog=trend, dif=1, pp)
freeze(pptable_dif_pub_inv_gdp_s_nc) pub_inv_gdp_s.uroot(exog=none, dif=1, pp)

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
equation eq_lin_inv_1.ls(cov=hac) log_gdp_pc_s c  x2_sa x3_sa d_2008 d_2018 PUB_INV_GDP_S 


smpl @all
equation eq_lin_inv_2.cointreg log_gdp_pc_s x2_sa x3_sa @determ d_2008 d_2018 PUB_INV_GDP_S

smpl @all
equation eq_lin_inv_3.cointreg(method=ccr) log_gdp_pc_s x2_sa x3_sa @determ d_2008 d_2018 PUB_INV_GDP_S


'Modelos cuadráticos con todas las metodologias en muestra completa

smpl @all
equation eq_quad_inv_1.ls(cov=hac) log_gdp_pc_s c x2_sa x3_sa d_2008 d_2018 PUB_INV_GDP_S  (PUB_INV_GDP_S )^2

smpl @all
equation eq_quad_inv_2.cointreg log_gdp_pc_s x2_sa x3_sa @determ d_2008 d_2018 PUB_INV_GDP_S  (PUB_INV_GDP_S )^2

smpl @all
equation eq_quad_inv_3.cointreg(method=ccr) log_gdp_pc_s x2_sa x3_sa @determ d_2008 d_2018 PUB_INV_GDP_S  (PUB_INV_GDP_S )^2

'Modelos cúbicos con todas las metodologias en muestra completa

smpl @all
equation eq_cub_inv_1.ls(cov=hac) log_gdp_pc_s c x2_sa x3_sa d_2008 d_2018 PUB_INV_GDP_S  (PUB_INV_GDP_S )^2 (PUB_INV_GDP_S )^3

smpl @all
equation eq_cub_inv_2.cointreg log_gdp_pc_s  x2_sa x3_sa @determ d_2008 d_2018 PUB_INV_GDP_S  (PUB_INV_GDP_S )^2 (PUB_INV_GDP_S )^3

smpl @all
equation eq_cub_inv_3.cointreg(method=ccr) log_gdp_pc_s  x2_sa x3_sa @determ d_2008 d_2018 PUB_INV_GDP_S  (PUB_INV_GDP_S )^2 (PUB_INV_GDP_S )^3

'------------------------------------------------------------------------------------------------------------------------------
'Variable independiente del gobierno: Gasto Agregado
'------------------------------------------------------------------------------------------------------------------------------

'Modelos lineales  con todas las metodologias en muestra completa

smpl @all
equation eq_lin_ag_1.ls(cov=hac) log_gdp_pc_s c  x2_sa x3_sa d_2008 d_2018 x1_sa 


smpl @all
equation eq_lin_ag_2.cointreg log_gdp_pc_s x2_sa x3_sa @determ d_2008 d_2018 x1_sa

smpl @all
equation eq_lin_ag_3.cointreg(method=ccr) log_gdp_pc_s x2_sa x3_sa @determ d_2008 d_2018 x1_sa


'Modelos cuadráticos con todas las metodologias en muestra completa

smpl @all
equation eq_quad_ag_1.ls(cov=hac) log_gdp_pc_s c x2_sa x3_sa d_2008 d_2018 x1_sa  (x1_sa )^2

smpl @all
equation eq_quad_ag_2.cointreg log_gdp_pc_s x2_sa x3_sa @determ d_2008 d_2018 x1_sa  (x1_sa )^2

smpl @all
equation eq_quad_ag_3.cointreg(method=ccr) log_gdp_pc_s x2_sa x3_sa @determ d_2008 d_2018 x1_sa  (x1_sa )^2

'Modelos cúbicos con todas las metodologias en muestra completa

smpl @all
equation eq_cub_ag_1.ls(cov=hac) log_gdp_pc_s c x2_sa x3_sa d_2008 d_2018 x1_sa  (x1_sa )^2 (x1_sa )^3

smpl @all
equation eq_cub_ag_2.cointreg log_gdp_pc_s  x2_sa x3_sa @determ d_2008 d_2018 x1_sa  (x1_sa )^2 (x1_sa )^3

smpl @all
equation eq_cub_ag_3.cointreg(method=ccr) log_gdp_pc_s  x2_sa x3_sa @determ d_2008 d_2018 x1_sa  (x1_sa )^2 (x1_sa )^3




'=============================
'Bootstrap
'=============================
'Gasto agregado

for !x=1 to 30
if !x<13 then
smpl @first+{!x} @last
equation reg_agregado{!x}.COINTREG log_gdp_pc_s X1_SA (X1_SA)^2 X2_SA X3_SA @DETERM D_2008 D_2018
else
smpl @first+{!x} @last
equation reg_agregado{!x}.COINTREG log_gdp_pc_s X1_SA (X1_SA)^2 X2_SA X3_SA @DETERM D_2018
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
equation reg_inversion{!x}.COINTREG log_gdp_pc_s PUB_INV_GDP_S  (PUB_INV_GDP_S )^2 X2_SA X3_SA @DETERM D_2008 D_2018
else
smpl @first+{!x} @last
equation reg_inversion{!x}.COINTREG log_gdp_pc_s PUB_INV_GDP_S  (PUB_INV_GDP_S )^2 X2_SA X3_SA @DETERM D_2018
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
tabla_modelos_agregado(17,1) = eq_lin_ag_1.@r2
tabla_modelos_agregado(17,2) = eq_quad_ag_1.@r2
tabla_modelos_agregado(17,3) = eq_cub_ag_1.@r2



'Modelos FMOLS
for !x=1 to 6
tabla_modelos_agregado(2*{!x}-1,4) = eq_lin_ag_2.@coef({!x})
tabla_modelos_agregado(2*{!x},4) = eq_lin_ag_2.@pval({!x})
next
'cuadraticos
for !x=1 to 7
tabla_modelos_agregado(2*{!x}-1,5) = eq_quad_ag_2.@coef({!x})
tabla_modelos_agregado(2*{!x},5) = eq_quad_ag_2.@pval({!x})
next
'cubicos
for !x=1 to 8
tabla_modelos_agregado(2*{!x}-1,6) = eq_cub_ag_2.@coef({!x})
tabla_modelos_agregado(2*{!x},6) = eq_cub_ag_2.@pval({!x})
next
tabla_modelos_agregado(17,4) = eq_lin_ag_2.@r2
tabla_modelos_agregado(17,5) = eq_quad_ag_2.@r2
tabla_modelos_agregado(17,6) = eq_cub_ag_2.@r2


'Modelos CCR
for !x=1 to 6
tabla_modelos_agregado(2*{!x}-1,7) = eq_lin_ag_3.@coef({!x})
tabla_modelos_agregado(2*{!x},7) = eq_lin_ag_3.@pval({!x})
next
'cuadraticos
for !x=1 to 7
tabla_modelos_agregado(2*{!x}-1,8) = eq_quad_ag_3.@coef({!x})
tabla_modelos_agregado(2*{!x},8) = eq_quad_ag_3.@pval({!x})
next
'cubicos
for !x=1 to 8
tabla_modelos_agregado(2*{!x}-1,9) = eq_cub_ag_3.@coef({!x})
tabla_modelos_agregado(2*{!x},9) = eq_cub_ag_3.@pval({!x})
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
tabla_modelos_agregado(19,1) = @val(auto_eq_lin_ag_1(3,2))
tabla_modelos_agregado(19,2) = @val(auto_eq_quad_ag_1(3,2))
tabla_modelos_agregado(19,3) = @val(auto_eq_cub_ag_1(3,2))

'heteroskedasticity BPG test
freeze(bpg_eq_lin_ag_1) eq_lin_ag_1.hettest c x2_sa x3_sa d_2008 d_2018 x1_sa
freeze(bpg_eq_quad_ag_1) eq_lin_ag_1.hettest c x2_sa x3_sa d_2008 d_2018 x1_sa (x1_sa)^2
freeze(bpg_eq_cub_ag_1) eq_lin_ag_1.hettest c x2_sa x3_sa d_2008 d_2018 x1_sa (x1_sa)^3
tabla_modelos_agregado(20,1) = @val(bpg_eq_lin_ag_1(3,2))
tabla_modelos_agregado(20,2) = @val(bpg_eq_quad_ag_1(3,2))
tabla_modelos_agregado(20,3) = @val(bpg_eq_cub_ag_1(3,2))

'export
tabla_modelos_agregado.save(t=csv) "C:\Users\Axel Canales\Documents\GitHub\govsize_2023\dofiles\tabla_modelos_agregado.csv"




'=============================
'Tabla Inversion Fija Publica
'=============================
'Modelos OLS
'lineales
for !x=1 to 6
table(17, 9) tabla_modelos
tabla_modelos(2*{!x}-1,1) = eq_lin_inv_1.@coef({!x})
tabla_modelos(2*{!x},1) = eq_lin_inv_1.@pval({!x})
next
'cuadraticos
for !x=1 to 7
tabla_modelos(2*{!x}-1,2) = eq_quad_inv_1.@coef({!x})
tabla_modelos(2*{!x},2) = eq_quad_inv_1.@pval({!x})
next
'cubicos
for !x=1 to 8
tabla_modelos(2*{!x}-1,3) = eq_cub_inv_1.@coef({!x})
tabla_modelos(2*{!x},3) = eq_cub_inv_1.@pval({!x})
next
tabla_modelos(17,1) = eq_lin_inv_1.@r2
tabla_modelos(17,2) = eq_quad_inv_1.@r2
tabla_modelos(17,3) = eq_cub_inv_1.@r2



'Modelos FMOLS
for !x=1 to 6
tabla_modelos(2*{!x}-1,4) = eq_lin_inv_2.@coef({!x})
tabla_modelos(2*{!x},4) = eq_lin_inv_2.@pval({!x})
next
'cuadraticos
for !x=1 to 7
tabla_modelos(2*{!x}-1,5) = eq_quad_inv_2.@coef({!x})
tabla_modelos(2*{!x},5) = eq_quad_inv_2.@pval({!x})
next
'cubicos
for !x=1 to 8
tabla_modelos(2*{!x}-1,6) = eq_cub_inv_2.@coef({!x})
tabla_modelos(2*{!x},6) = eq_cub_inv_2.@pval({!x})
next
tabla_modelos(17,4) = eq_lin_inv_2.@r2
tabla_modelos(17,5) = eq_quad_inv_2.@r2
tabla_modelos(17,6) = eq_cub_inv_2.@r2


'Modelos CCR
for !x=1 to 6
tabla_modelos(2*{!x}-1,7) = eq_lin_inv_3.@coef({!x})
tabla_modelos(2*{!x},7) = eq_lin_inv_3.@pval({!x})
next
'cuadraticos
for !x=1 to 7
tabla_modelos(2*{!x}-1,8) = eq_quad_inv_3.@coef({!x})
tabla_modelos(2*{!x},8) = eq_quad_inv_3.@pval({!x})
next
'cubicos
for !x=1 to 8
tabla_modelos(2*{!x}-1,9) = eq_cub_inv_3.@coef({!x})
tabla_modelos(2*{!x},9) = eq_cub_inv_3.@pval({!x})
next
tabla_modelos(17,7) = eq_lin_inv_3.@r2
tabla_modelos(17,8) = eq_quad_inv_3.@r2
tabla_modelos(17,9) = eq_cub_inv_3.@r2


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
tabla_modelos(18,1) = @val(jb_eq_lin_inv_1(14,2))
tabla_modelos(18,2) = @val(jb_eq_quad_inv_1(14,2))
tabla_modelos(18,3) = @val(jb_eq_cub_inv_1(14,2))
tabla_modelos(18,4) = @val(jb_eq_lin_inv_2(14,2))
tabla_modelos(18,5) = @val(jb_eq_quad_inv_2(14,2))
tabla_modelos(18,6) = @val(jb_eq_cub_inv_2(14,2))
tabla_modelos(18,7) = @val(jb_eq_lin_inv_3(14,2))
tabla_modelos(18,8) = @val(jb_eq_quad_inv_3(14,2))
tabla_modelos(18,9) = @val(jb_eq_cub_inv_3(14,2))

'auto LM test

freeze(auto_eq_lin_inv_1) eq_lin_inv_1.auto
freeze(auto_eq_quad_inv_1) eq_quad_inv_1.auto
freeze(auto_eq_cub_inv_1) eq_cub_inv_1.auto
tabla_modelos(19,1) = @val(auto_eq_lin_inv_1(3,2))
tabla_modelos(19,2) = @val(auto_eq_quad_inv_1(3,2))
tabla_modelos(19,3) = @val(auto_eq_cub_inv_1(3,2))

'heteroskedasticity BPG test
freeze(bpg_eq_lin_inv_1) eq_lin_inv_1.hettest c x2_sa x3_sa d_2008 d_2018 pub_inv_gdp_s
freeze(bpg_eq_quad_inv_1) eq_lin_inv_1.hettest c x2_sa x3_sa d_2008 d_2018 pub_inv_gdp_s (pub_inv_gdp_s)^2
freeze(bpg_eq_cub_inv_1) eq_lin_inv_1.hettest c x2_sa x3_sa d_2008 d_2018 pub_inv_gdp_s (pub_inv_gdp_s)^3
tabla_modelos(20,1) = @val(bpg_eq_lin_inv_1(3,2))
tabla_modelos(20,2) = @val(bpg_eq_quad_inv_1(3,2))
tabla_modelos(20,3) = @val(bpg_eq_cub_inv_1(3,2))

'export
tabla_modelos.save(t=csv) "C:\Users\Axel Canales\Documents\GitHub\govsize_2023\dofiles\tabla_modelos_inv.csv"

'bootstrap
lineales_agregado.save(t=csv) "C:\Users\Axel Canales\Documents\GitHub\govsize_2023\dofiles\lineales_agregado.csv"
cuadraticos_agregado.save(t=csv) "C:\Users\Axel Canales\Documents\GitHub\govsize_2023\dofiles\cuadraticos_agregado.csv"
lineales_inversion.save(t=csv) "C:\Users\Axel Canales\Documents\GitHub\govsize_2023\dofiles\lineales_inversion.csv"
cuadraticos_inversion.save(t=csv) "C:\Users\Axel Canales\Documents\GitHub\govsize_2023\dofiles\cuadraticos_inversion.csv"


