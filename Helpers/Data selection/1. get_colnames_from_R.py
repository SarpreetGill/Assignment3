import re

# paste output form 0. get_column_names.R
var_names = ''' [1] "SEQN"       "URXUMA"     "URXUMS"     "URXUCR.x"   "URXCRS"     "URDACT"     "LBXWBCSI"   "LBXLYPCT"   "LBXMOPCT"   "LBXNEPCT"   "LBXEOPCT"   "LBXBAPCT"   "LBDLYMNO"   "LBDMONO"    "LBDNENO"   
[16] "LBDEONO"    "LBDBANO"    "LBXRBCSI"   "LBXHGB"     "LBXHCT"     "LBXMCVSI"   "LBXMCHSI"   "LBXMC"      "LBXRDW"     "LBXPLTSI"   "LBXMPSI"    "PHQ020"     "PHQ030"     "PHQ040"     "PHQ050"    
[31] "PHQ060"     "PHAFSTHR.x" "PHAFSTMN.x" "PHDSESN"    "LBDHDD"     "LBDHDDSI"   "LBXHA"      "LBXHBS"     "LBXHBC"     "LBDHBG"     "LBDHD"      "LBDHEG"     "LBDHEM"     "LBXTC"      "LBDTCSI"   
[46] "LBXTTG"     "URXVOL1"  '''

var_names = list(i for i in re.findall('([a-zA-Z0-9.]+)', var_names) if not i.isdigit())

print(var_names)

# paste result from 2. get_variable_descriptions.js here
var_descs = ["Respondent sequence number.", "Albumin, urine (ug/mL)", "Albumin, urine (mg/L)", "", "Creatinine, urine (umol/L)", "Albumin creatinine ratio (mg/g)", "White blood cell count (1000 cells/uL)", "Lymphocyte percent (%)", "Monocyte percent (%)", "Segmented neutrophils percent (%)", "Eosinophils percent (%)", "Basophils percent (%)", "Lymphocyte number (1000 cells/uL)", "Monocyte number (1000 cells/uL)", "Segmented neutrophils num (1000 cell/uL)", "Eosinophils number (1000 cells/uL)", "Basophils number (1000 cells/uL)", "Red blood cell count (million cells/uL)", "Hemoglobin (g/dL)", "Hydroxycotinine, Serum (ng/mL)", "Mean cell volume (fL)", "Mean cell hemoglobin (pg)", "Mean cell hemoglobin concentration (g/dL)", "Red cell distribution width (%)", "Platelet count (1000 cells/uL)", "Mean platelet volume (fL)", "Coffee or tea with cream or sugar? [Include milk or non-dairy creamers.]", "Alcohol, such as beer, wine, or liquor?", "Gum, breath mints, lozenges or cough drops, or other cough or cold remedies?", "Antacids, laxatives, or anti-diarrheals?", "Dietary supplements such as vitamins and minerals?…e multivitamins and single nutrient supplements.]", "", "", "Session in which SP was examined", "Direct HDL-Cholesterol (mg/dL)", "Direct HDL-Cholesterol (mmol/L)", "Hepatitis A antibody", "Hepatitis B Surface Antibody", "Hepatitis B core antibody", "Hepatitis B surface antigen", "Hepatitis D (anti-HDV)", "Hepatitis E IgG (anti-HEV)", "Hepatitis E IgM (anti-HEV)", "Total Cholesterol( mg/dL)", "Total Cholesterol( mmol/L)", "Tissue transglutaminase(IgA-TTG)", "The volume of urine collection #1 (mL)"]

for n in range(len(var_names)):
    print('{} {}\t{}'.format(n, var_names[n], var_descs[n]))

# select which columns to keep
selected = {0: 'ID',
            6: 'White_cells_count',
            17: 'Red_cells_count',
            26: 'Caffeine',
            27: 'Alcohol',
            30: 'Supplements',
            43: 'Cholesterol'} #{n: new_name}

print('')
for n in sorted(selected.keys()):
    new_name = selected[n]
    print('* **{} ({})** - {}.'.format(new_name, var_names[n], var_descs[n]))
