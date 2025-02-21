import re

def extract_var_names(data_dict):
    data = data_dict['data']
    var_names = list(i.replace('.','_').upper()
                     for i in re.findall('([a-zA-Z0-9.]+)', data)
                     if not i.isdigit())
    return var_names

def select_relevant_vars(var_names, data_dict):
    var_descs = data_dict['var_descs']

    if 'selected' not in data_dict:
        for n in range(len(var_names)):
            print('{} {}\t{}'.format(n, var_names[n], var_descs[n]))

        print('')
    else:
        selected = data_dict['selected']
        for n in sorted(selected.keys()):
            new_name = selected[n]
            print('* **{} ({})** - {}.'.format(new_name, var_names[n], var_descs[n]))

def print_selected_vars(data_dict):
    print(list(i for i in var_names
               if var_names.index(i)
               in data_dict['selected'].keys()))

def print_rename_vars(data_dict, suffix=''):
    for index, new_name in data_dict['selected'].items():
        var_name = var_names[index]
        new_name = new_name + suffix
        print('"{}" = "{}",'.format(new_name, var_name))
        #print('#{} - '.format(new_name))
    
labs = {}
diet = {}
quest = {}
exam = {}
demo = {}

# paste output form 0. get_column_names.R
labs['data'] = '''[1] "SEQN"       "URXUMA"     "URXUMS"     "URXUCR.x"   "URXCRS"     "URDACT"     "LBXWBCSI"   "LBXLYPCT"   "LBXMOPCT"   "LBXNEPCT"   "LBXEOPCT"   "LBXBAPCT"   "LBDLYMNO"   "LBDMONO"    "LBDNENO"   
[16] "LBDEONO"    "LBDBANO"    "LBXRBCSI"   "LBXHGB"     "LBXHCT"     "LBXMCVSI"   "LBXMCHSI"   "LBXMC"      "LBXRDW"     "LBXPLTSI"   "LBXMPSI"    "PHQ020"     "PHQ030"     "PHQ040"     "PHQ050"    
[31] "PHQ060"     "PHAFSTHR.x" "PHAFSTMN.x" "PHDSESN"    "LBDHDD"     "LBDHDDSI"   "LBXHA"      "LBXHBS"     "LBXHBC"     "LBDHBG"     "LBDHD"      "LBDHEG"     "LBDHEM"     "LBXTC"      "LBDTCSI"   
[46] "LBXTTG"     "URXVOL1"   '''
diet['data'] = '''[1] "SEQN"     "WTDRD1"   "WTDR2D"   "DR1DRSTZ" "DR1EXMER" "DRABF"    "DRDINT"   "DR1DBIH"  "DR1DAY"   "DR1LANG"  "DR1MNRSP" "DR1HELPD" "DBQ095Z"  "DRQSPREP" "DR1STY"   "DRQSDIET" "DR1TNUMF" "DR1TKCAL"
[19] "DR1TPROT" "DR1TCARB" "DR1TSUGR" "DR1TFIBE" "DR1TTFAT" "DR1TSFAT" "DR1TMFAT" "DR1TPFAT" "DR1TCHOL" "DR1TATOC" "DR1TATOA" "DR1TRET"  "DR1TVARA" "DR1TACAR" "DR1TBCAR" "DR1TCRYP" "DR1TLYCO" "DR1TLZ"  
[37] "DR1TVB1"  "DR1TVB2"  "DR1TNIAC" "DR1TVB6"  "DR1TFOLA" "DR1TFA"   "DR1TFF"   "DR1TFDFE" "DR1TCHL"  "DR1TVB12" "DR1TB12A" "DR1TVC"   "DR1TVD"   "DR1TVK"   "DR1TCALC" "DR1TPHOS" "DR1TMAGN" "DR1TIRON"
[55] "DR1TZINC" "DR1TCOPP" "DR1TSODI" "DR1TPOTA" "DR1TSELE" "DR1TCAFF" "DR1TTHEO" "DR1TALCO" "DR1TMOIS" "DR1TS040" "DR1TS060" "DR1TS080" "DR1TS100" "DR1TS120" "DR1TS140" "DR1TS160" "DR1TS180" "DR1TM161"
[73] "DR1TM181" "DR1TM201" "DR1TM221" "DR1TP182" "DR1TP183" "DR1TP184" "DR1TP204" "DR1TP205" "DR1TP225" "DR1TP226" "DR1.300"  "DR1.320Z" "DR1.330Z" "DR1BWATZ" "DR1TWS"   "DRD340"   "DRD360"  '''
quest['data'] =  '''[1] "SEQN"      "CBD070"    "CBD090"    "CBD110"    "CBD120"    "CBD130"    "HSQ500"    "HSQ510"    "HSQ520"    "HSAQUEX"   "DIQ010"    "DIQ050"    "DBQ197"    "DBD895"    "DBD905"    "DBD910"   
[17] "DLQ010"    "DLQ020"    "DLQ040"    "DLQ050"    "DLQ060"    "FSD032A"   "FSD032B"   "FSD032C"   "FSDHH"     "FSDAD"     "FSD151"    "FSQ165"    "FSQ162"    "HEQ010"    "HEQ030"    "HIQ011"   
[33] "HIQ270"    "HIQ210"    "HOD050"    "HOQ065"    "HUQ010"    "HUQ020"    "HUQ030"    "HUQ041"    "HUQ051"    "HUQ071"    "HUQ090"    "IMQ011"    "IMQ020"    "INQ020"    "INQ012"    "INQ030"   
[49] "INQ060"    "INQ080"    "INQ090"    "INQ132"    "INQ140"    "INQ150"    "IND235"    "INDFMMPI"  "INDFMMPC"  "MCQ010"    "MCQ053"    "MCQ082"    "MCQ086"    "MCQ092"    "MCQ203"    "MCQ300B"  
[65] "OHQ030"    "OHQ033"    "OHQ770"    "OHQ845"    "PAQ710"    "PAQ715"    "PAAQUEX"   "PUQ100"    "PUQ110"    "SMD460"    "SMQ860"    "SMQ870"    "SMQ872"    "SMQ874"    "SMQ878"    "SMAQUEX.x"'''
exam['data'] = '''[1] "SEQN"     "PEASCST1" "PEASCTM1" "BPAARM"   "BPACSZ"   "BPXPLS"   "BPXPULS"  "BPXPTY"   "BPXML1"   "BPAEN1"   "BPXSY2"   "BPXDI2"   "BPAEN2"   "BPXSY3"   "BPXDI3"   "BPAEN3"   "BMDSTATS" "BMXWT"   
 [19] "BMXHT"    "BMXBMI"   "BMXLEG"   "BMXARML"  "BMXARMC"  "BMXWAIST" "MGDEXSTS" "MGQ070"   "MGQ100"   "MGD130"   "MGQ90DG"  "MGAPHAND" "MGATHAND" "MGXH1T1"  "MGXH1T1E" "MGXH2T1"  "MGXH2T1E" "MGXH1T2" 
 [37] "MGXH1T2E" "MGXH2T2"  "MGXH2T2E" "MGXH1T3"  "MGXH1T3E" "MGXH2T3"  "MGXH2T3E" "MGDCGSZ"  "OHDEXSTS" "OHDDESTS" "OHX01TC"  "OHX02TC"  "OHX03TC"  "OHX04TC"  "OHX05TC"  "OHX06TC"  "OHX07TC"  "OHX08TC" 
 [55] "OHX09TC"  "OHX10TC"  "OHX11TC"  "OHX12TC"  "OHX13TC"  "OHX14TC"  "OHX15TC"  "OHX16TC"  "OHX17TC"  "OHX18TC"  "OHX19TC"  "OHX20TC"  "OHX21TC"  "OHX22TC"  "OHX23TC"  "OHX24TC"  "OHX25TC"  "OHX26TC" 
 [73] "OHX27TC"  "OHX28TC"  "OHX29TC"  "OHX30TC"  "OHX31TC"  "OHX32TC"  "OHX02CTC" "OHX03CTC" "OHX04CTC" "OHX05CTC" "OHX06CTC" "OHX07CTC" "OHX08CTC" "OHX09CTC" "OHX10CTC" "OHX11CTC" "OHX12CTC" "OHX13CTC"
 [91] "OHX14CTC" "OHX15CTC" "OHX18CTC" "OHX19CTC" "OHX20CTC" "OHX21CTC" "OHX22CTC" "OHX23CTC" "OHX24CTC" "OHX25CTC" "OHX26CTC" "OHX27CTC" "OHX28CTC" "OHX29CTC" "OHX30CTC" "OHX31CTC"'''
demo['data'] = '''[1] "SEQN"     "SDDSRVYR" "RIDSTATR" "RIAGENDR" "RIDAGEYR" "RIDRETH1" "RIDRETH3" "RIDEXMON" "DMDBORN4" "DMDCITZN" "SIALANG"  "SIAPROXY" "SIAINTRP" "FIALANG"  "FIAPROXY" "FIAINTRP" "DMDHHSIZ" "DMDFMSIZ"
[19] "DMDHHSZA" "DMDHHSZB" "DMDHHSZE" "DMDHRGND" "DMDHRAGE" "DMDHRBR4" "DMDHREDU" "DMDHRMAR" "WTINT2YR" "WTMEC2YR" "SDMVPSU"  "SDMVSTRA" "INDHHIN2" "INDFMIN2" "INDFMPIR"'''

##################

SELECTED = demo

##################


# get variable names for 2. get_variable_descriptions.js
var_names = extract_var_names(SELECTED)
print(var_names)
print('')

# paste result from 2. get_variable_descriptions.js here
labs['var_descs'] = ["Respondent sequence number.","Albumin, urine (ug/mL)","Albumin, urine (mg/L)","","Creatinine, urine (umol/L)","Albumin creatinine ratio (mg/g)","White blood cell count (1000 cells/uL)","Lymphocyte percent (%)","Monocyte percent (%)","Segmented neutrophils percent (%)","Eosinophils percent (%)","Basophils percent (%)","Lymphocyte number (1000 cells/uL)","Monocyte number (1000 cells/uL)","Segmented neutrophils num (1000 cell/uL)","Eosinophils number (1000 cells/uL)","Basophils number (1000 cells/uL)","Red blood cell count (million cells/uL)","Hemoglobin (g/dL)","Hydroxycotinine, Serum (ng/mL)","Mean cell volume (fL)","Mean cell hemoglobin (pg)","Mean cell hemoglobin concentration (g/dL)","Red cell distribution width (%)","Platelet count (1000 cells/uL)","Mean platelet volume (fL)","Coffee or tea with cream or sugar? [Include milk or non-dairy creamers.]","Alcohol, such as beer, wine, or liquor?","Gum, breath mints, lozenges or cough drops, or other cough or cold remedies?","Antacids, laxatives, or anti-diarrheals?","Dietary supplements such as vitamins and minerals? [Include multivitamins and single nutrient supplements.]","","","Session in which SP was examned","Direct HDL-Cholesterol (mg/dL)","Direct HDL-Cholesterol (mmol/L)","Hepatitis A antibody","Hepatitis B Surface Antibody","Hepatitis B core antibody","Hepatitis B surface antigen","Hepatitis D (anti-HDV)","Hepatitis E IgG (anti-HEV)","Hepatitis E IgM (anti-HEV)","Total Cholesterol( mg/dL)","Total Cholesterol( mmol/L)","Tissue transglutaminase(IgA-TTG)","The volume of urine collection #1 (mL)"]
diet['var_descs'] = [" ","Dietary day one sample weight","Dietary two-day sample weight","Dietary recall status","Interviewer ID code","Indicates whether the sample person was an infant who was breast fed on either of the two recall days.","Indicates whether the sample person has intake data for one or two days.","# of days b/w intake and HH interview","Intake day of the week","The respondent spoke mostly:","Who was the main respondent for this interview?","Who helped in responding for this interview","What type of salt {do you/does SP} usually add to {your/his/her/SP's} food at the table? Would you say . . .","How often is ordinary salt or seasoned salt added in cooking or preparing foods in your household? Is it never, rarely, occasionally, or very often?","Did {you/SP} add any salt to {your/her/his} food at the table yesterday? Salt includes ordinary or seasoned salt, lite salt, or a salt substitute.","Are you currently on any kind of diet, either to lose weight or for some other health-related reason?","Total number of foods/beverages reported in the individual foods file","Energy (kcal)","Protein (gm)","Carbohydrate (gm)","Total sugars (gm)","Dietary fiber (gm)","Total fat (gm)","Total saturated fatty acids (gm)","Total monounsaturated fatty acids (gm)","Total polyunsaturated fatty acids (gm)","Cholesterol (mg)","Vitamin E as alpha-tocopherol (mg)","Added alpha-tocopherol (Vitamin E) (mg)","Retinol (mcg)","Vitamin A as retinol activity equivalents (mcg)","Alpha-carotene (mcg)","Beta-carotene (mcg)","Beta-cryptoxanthin (mcg)","Lycopene (mcg)","Lutein + zeaxanthin (mcg)","Thiamin (Vitamin B1) (mg)","Riboflavin (Vitamin B2) (mg)","Niacin (mg)","Vitamin B6 (mg)","Total folate (mcg)","Folic acid (mcg)","Food folate (mcg)","Folate as dietary folate equivalents (mcg)","Total choline (mg)","Vitamin B12 (mcg)","Added vitamin B12 (mcg)","Vitamin C (mg)","Vitamin D (D2 + D3) (mcg)","Vitamin K (mcg)","Calcium (mg)","Phosphorus (mg)","Magnesium (mg)","Iron (mg)","Zinc (mg)","Copper (mg)","Sodium (mg)","Potassium (mg)","Selenium (mcg)","Caffeine (mg)","Theobromine (mg)","Alcohol (gm)","Moisture (gm)","SFA 4:0 (Butanoic) (gm)","SFA 6:0 (Hexanoic) (gm)","SFA 8:0 (Octanoic) (gm)","SFA 10:0 (Decanoic) (gm)","SFA 12:0 (Dodecanoic) (gm)","SFA 14:0 (Tetradecanoic) (gm)","SFA 16:0 (Hexadecanoic) (gm)","SFA 18:0 (Octadecanoic) (gm)","MFA 16:1 (Hexadecenoic) (gm)","MFA 18:1 (Octadecenoic) (gm)","MFA 20:1 (Eicosenoic) (gm)","MFA 22:1 (Docosenoic) (gm)","PFA 18:2 (Octadecadienoic) (gm)","PFA 18:3 (Octadecatrienoic) (gm)","PFA 18:4 (Octadecatetraenoic) (gm)","PFA 20:4 (Eicosatetraenoic) (gm)","PFA 20:5 (Eicosapentaenoic) (gm)","PFA 22:5 (Docosapentaenoic) (gm)","PFA 22:6 (Docosahexaenoic) (gm)","Was the amount of food that {you/NAME} ate yesterday much more than usual, usual, or much less than usual?","Total plain water drank yesterday - including plain tap water, water from a drinking fountain, water from a water cooler, bottled water, and spring water.","Total tap water drank yesterday - including filtered tap water and water from a drinking fountain.","Total bottled water drank yesterday (gm)","When you drink tap water, what is the main source of the tap water? Is the city water supply (community water supply); a well or rain cistern; a spring; or something else?","Please look at this list of shellfish. During the past 30 days did you eat any types of shellfish listed on this card? Include any foods that had shellfish in them such as sandwiches, soups, or salads.","Please look at this list of fish. During the past 30 days did you eat any types of fish listed on this card? Include any foods that had fish in them such as sandwiches, soups, or salads."]
quest['var_descs'] = ["Respondent sequence number","The next questions are about how much money {your family spends/you spend} on food. First I'll ask you about money spent at supermarkets or grocery stores. Then we will talk about money spent at other types of stores. During the past 30 days, how much money {did your family/did you} spend at supermarkets or grocery stores? Please include purchases made with food stamps.","About how much money was spent on nonfood items?","About how much money {did your family/did you} spend on food at these types of stores? (Please do not include any stores you have already told me about.)","During the past 30 days, how much money {did your family/did you} spend on eating out? Please include money spent in cafeterias at work or at school or on vending machines, for all family members.","During the past 30 days, how much money {did your family/did you} spend on food carried out or delivered? Please do not include money you have already told me about.","Did {you/SP} have a head cold or chest cold that started during those 30 days?","Did {you/SP} have a stomach or intestinal illness with vomiting or diarrhea that started during those 30 days?","Did {you/SP} have flu, pneumonia, or ear infections that started during those 30 days?","Source of Health Status Data","The next questions are about specific medical conditions. {Other than during pregnancy, {have you/has SP}/{Have you/Has SP}} ever been told by a doctor or health professional that {you have/{he/she/SP} has} diabetes or sugar diabetes?","{Is SP/Are you} now taking insulin","Now I'm going to ask a few questions about milk products. Do not include their use in cooking. In the past 30 days, how often did {you/SP} have milk to drink or on {your/his/her} cereal? Please include chocolate and other flavored milks as well as hot cocoa made with milk. Do not count small amounts of milk added to coffee or tea. Would you say...","Next I'm going to ask you about meals. By meal, I mean breakfast, lunch and dinner. During the past 7 days, how many meals {did you/did SP} get that were prepared away from home in places such as restaurants, fast food places, food stands, grocery stores, or from vending machines? {Please do not include meals provided as part of the school lunch or school breakfast./Please do not include meals provided as part of the community programs you reported earlier.}","Some grocery stores sell \"ready to eat\" foods such as salads, soups, chicken, sandwiches and cooked vegetables in their salad bars and deli counters. During the past 30 days, how often did {you/SP} eat \"ready to eat\" foods from the grocery store? Please do not include sliced meat or cheese you buy for sandwiches and frozen or canned foods.","During the past 30 days, how often did you {SP} eat frozen meals or frozen pizzas? Here are some examples of frozen meals and frozen pizzas.","With this next set of questions, we want to learn about people who have physical, mental, or emotional conditions that cause serious difficulties with their daily activities. Though different, these questions may sound similar to ones I asked earlier. {Are you/Is SP} deaf or {do you/does he/does she} have serious difficulty hearing?","{Are you/Is SP} blind or {do you/does he/does she} have serious difficulty seeing even when wearing glasses?","Because of a physical, mental, or emotional condition, {do you/does he/does she} have serious difficulty concentrating, remembering, or making decisions?","{Do you/Does SP} have serious difficulty walking or climbing stairs?","{Do you/Does SP} have difficulty dressing or bathing?","Now I am going to read you several statements that people have made about their food situation. For these statements, please tell me whether the statement was often true, sometimes true, or never true for {you/your household} in the last 12 months, that is since last {DISPLAY CURRENT MONTH}. The first statement is . . . {I/we} worried whether {my/our} food would run out before {I/we} got money to buy more.","[The next statement is . . .] The food that {I/we} bought just didn't last, and {I/we} didn't have money to get more.","[The next statement is . . .] {I/we} couldn't afford to eat balanced meals.","Household food security category for last 12 months","Adult food security category for last 12 months","In the last 12 months, did {you/you or any member of your household} ever get emergency food from a church, a food pantry, or a food bank, or eat in a soup kitchen?","The next questions are about SNAP, the Supplemental Nutrition Assistance Program, formerly known as the Food Stamp Program. SNAP benefits are provided on an electronic debit card {or EBT card} {called the DISPLAY STATE NAME FOR EBT CARD}} card in STATE}. Have {you/you or anyone in your household} ever received SNAP or Food Stamp benefits?","In the last 12 months, did {you/you or any member of your household} receive benefits from the WIC program, that is, the Women, Infants and Children program?","Has a doctor or other health professional ever told {you/SP} that {you have/s/he/SP has} Hepatitis B? (Hepatitis is a form of liver disease. Hepatitis B is an infection of the liver from the Hepatitis B virus (HBV).)","Has a doctor or other health professional ever told {you/SP} that {you have/s/he/SP has} Hepatitis C? (Hepatitis is a form of liver disease. Hepatitis C is an infection of the liver from the Hepatitis C virus (HCV).)","The (first/next) questions are about health insurance. {Are you/Is SP} covered by health insurance or some other kind of health care plan? [Include health insurance obtained through employment or purchased directly as well as government programs like Medicare and Medicaid that provide medical care or help pay medical bills.]","{Does this plan/Do any of these plans} cover any part of the cost of prescriptions?","In the past 12 months, was there any time when {you/SP} did not have any health insurance coverage?","How many rooms are in this home? Count the kitchen but not the bathroom.","Is this {mobile home/house/apartment} owned, being bought, rented, or occupied by some other arrangement by {you/you or someone else in your family}?","{First/Next} I have some general questions about {your/SP's} health. Would you say {your/SP's} health in general is . . .","Compared with 12 months ago, would you say {your/SP's} health is now . . .","Is there a place that {you/SP} usually {go/goes} when {you are/he/she is} sick or {you/s/he} need{s} advice about {your/his/her} health?","{What kind of place is it - a clinic, doctor's office, emergency room, or some other place?} {What kind of place {do you/does SP} go to most often - a clinic, doctor's office, emergency room, or some other place?}","{During the past 12 months, how/How} many times {have you/has SP} seen a doctor or other health care professional about {your/his/her} health at a doctor's office, a clinic or some other place? Do not include times {you were/s/he was} hospitalized overnight, visits to hospital emergency rooms, home visits or telephone calls.","{During the past 12 months, were you/{was} SP} a patient in a hospital overnight? Do not include an overnight stay in the emergency room.","During the past 12 months, that is since {DISPLAY CURRENT MONTH} of {DISPLAY LAST YEAR}, {have you/has SP} seen or talked to a mental health professional such as a psychologist, psychiatrist, psychiatric nurse or clinical social worker about {your/his/her} health?","Hepatitis (Hep-a-ti-tis) A vaccine is given as a two dose series to some children older than 2 years and also to some adults, especially people who travel outside the United States. It has only been available since 1995. {Have you/Has SP} ever received hepatitis A vaccine?","Hepatitis (Hep-a-ti-tis) B vaccine is given in three separate doses and has been recommended for all newborn infants since 1991. In 1995, it was recommended that adolescents be given the vaccine. Persons who may be exposed to other people's blood, such as health care workers, also may have received the vaccine. {Have you/Has SP} ever received the 3-dose series of the hepatitis B vaccine?","The next questions are about {your/your combined family} income. When answering these questions, please remember that by {\"income/combined family income\"}, I mean {your income/your income plus the income of {NAMES OF OTHER NHANES FAMILY MEMBERS} for {LAST CALENDAR YEAR}. Did {you/you and OTHER NHANES FAMILY MEMBERS 16+} receive income in {LAST CALENDAR YEAR} from wages and salaries? [Did {you/you or OTHER FAMILY MEMBERS 16+} get paid for work in {LAST CALENDAR YEAR}.]","Did {you/you or any family members 16 and older} receive income in {LAST CALENDAR YEAR} from self-employment including business and farm income? [Self-employment means you worked for yourself.]","When answering the next questions about different kinds of income members of your family might have received in {LAST CALENDAR YEAR}, please consider that we also want to know about family members less than 16 years old. Did {you/you or any family members living here, that is: you or NAME(S) OF OTHER NHANES FAMILY MEMBERS} receive income in {LAST CALENDAR YEAR} from Social Security or Railroad Retirement?","Did {you/you or any family members living here} receive any disability pension [other than Social Security or Railroad Retirement] in {LAST CALENDAR YEAR}?","Did {you/you or any family members living here} receive retirement or survivor pension [other than Social Security or Railroad Retirement or disability pension] in {LAST CALENDAR YEAR}?","Did {you/you or any family members living here} receive Supplemental Security Income [SSI] in {LAST CALENDAR YEAR}?","Did {you/you or any family members living here} receive any cash assistance from a state or county welfare program such as welfare, public assistance, AFDC, or some other program in {LAST CALENDAR YEAR}?","Did {you/you or any family members living here} receive interest from savings or other bank accounts or income from dividends received from stocks or mutual funds or net rental income from property, royalties, estates, or trusts in {LAST CALENDAR YEAR}?","Did {you/you or any family members living here} receive income in {LAST CALENDAR YEAR} from child support, alimony, contributions from family or others, VA payments, worker's compensation, or unemployment compensation?","Monthly family income (reported as a range value in dollars).","Family monthly poverty level index, a ratio of monthly family income to the HHS poverty guidelines specific to family size.","Family monthly poverty level index categories.","The following questions are about different medical conditions. Has a doctor or other health professional ever told {you/SP} that {you have/s/he/SP has} asthma (az-ma)?","During the past 3 months, {have you/has SP} been on treatment for anemia (a-nee-me-a), sometimes called \"tired blood\" or \"low blood\"? [Include diet, iron pills, iron shots, transfusions as treatment.]","Has a doctor or other health professional ever told {you/SP} that {you have/s/he/SP has} celiac (sele-ak) disease, also called or sprue (sproo)?","{Are you/is SP} on a gluten-free diet?","{Have you/Has SP} ever received a blood transfusion?","Has anyone ever told {you/SP} that {you/she/he/SP} had yellow skin, yellow eyes or jaundice? Please do not include infant jaundice, which is common during the first weeks after birth.","Including living and deceased, were any of {SP's/your} close biological that is, blood relatives including father, mother, sisters or brothers, ever told by a health professional that they had asthma (az-ma)?","The next questions are about {your/SP's} teeth and gums. About how long has it been since {you/SP} last visited a dentist? Include all types of dentists, such as, orthodontists, oral surgeons, and all other dental specialists, as well as dental hygienists.","What was the main reason {you/SP} last visited the dentist?","During the past 12 months was there a time when (you/SP) needed dental care but could not get it at that time?","Overall, how would {you/SP} rate the health of {your/his/her} teeth and gums?","Now I will ask you first about TV watching and then about computer use. Over the past 30 days, on average how many hours per day did {you/SP} sit and watch TV or videos? Would you say . . .","Over the past 30 days, on average how many hours per day did {you/SP} use a computer or play computer games outside of school? Include Playstation, Nintendo DS, or other portable video games Would you say . . .","Questionnaire source flag for weighting","In the past 7 days, were any chemical products used in {your/his/her} home to control fleas, roaches, ants, termites, or other insects?","In the past 7 days, were any chemical products used in {your/his/her} lawn or garden to kill weeds?","Now I would like to ask you a few questions about smoking in this home. How many people who live here smoke cigarettes, cigars, little cigars, pipes, water pipes, hookah, or any other tobacco product?","{I will now ask you about smoking in other places.} During the last 7 days, did {you/SP} spend time in a restaurant?","During the last 7 days, did {you/SP} ride in a car or motor vehicle?","While {you were/SP was} riding in a car or motor vehicle, did someone else smoke cigarettes or other tobacco products?","During the last 7 days, did {you/SP} spend time in a home other than {your/his/her} own?","During the last 7 days,{were you/was SP} in any other indoor area?",""]
exam['var_descs'] = ["Respondent sequence number.","Blood Pressure Status","Blood Pressure Time in Seconds","Arm selected:","Cuff size (cm) (width X length)","60 sec. pulse (30 sec. pulse * 2)","Pulse regular or irregular?","Pulse type","MIL: maximum inflation levels (mm Hg)","Enhancement used first reading","Systolic: Blood pressure (second reading) mm Hg","Diastolic: Blood pressure (second reading) mm Hg","Enhancement used second reading","Systolic: Blood pressure (third reading) mm Hg","Diastolic: Blood pressure (third reading) mm Hg","Enhancement used third reading","Body Measures Component status Code","Weight (kg)","Standing Height (cm)","Body Mass Index (kg/m**2)","Upper Leg Length (cm)","Upper Arm Length (cm)","Arm Circumference (cm)","Waist Circumference (cm)","Grip test status","Have you had any pain, aching or stiffness in your right hand in the past 7 days?","Have you had any pain, aching or stiffness in your left hand in the past 7 days?","Are you right-handed, left-handed, or do you use both hands equally?","Was the participant able to achieve a 90 degree angle with the index finger on the eligible hand(s)?","The hand assigned for the practice trial","Begin the test with this hand.","Grip strength (kg), hand 1, test 1","Whether the participant exerted a maximal or questionable effort during the test 1 on hand 1, as assessed by the technician.","Grip strength (kg), hand 2, test 1","Whether the participant exerted a maximal or questionable effort during the test 1 on hand 2, as assessed by the technician.","Grip strength (kg), hand 1, test 2","Whether the participant exerted a maximal or questionable effort during the test 2 on hand 1, as assessed by the technician.","Grip strength (kg), hand 2, test 2","Whether the participant exerted a maximal or questionable effort during the test 2 on hand 2, as assessed by the technician.","Grip strength (kg), hand 1, test 3","Whether the participant exerted a maximal or questionable effort during the test 3 on hand 1, as assessed by the technician.","Grip strength (kg), hand 2, test 3","Whether the participant exerted a maximal or questionable effort during the test 3 on hand 2, as assessed by the technician.","Combined grip strength (kg): the sum of the largest reading from each hand.","Overall Oral Health Exam Status","Dentition Status Code","Tooth Count: Upper right 3rd molar (3M)","Tooth Count: Upper right 2nd molar (2M)","Tooth Count: Upper right 1st molar (1M)","Tooth Count: Upper right 2nd bicuspid/2nd primary molar (2B)","Tooth Count: Upper right 1st bicuspid/1st primary molar (1B)","Tooth Count: Upper right cuspid (C)","Tooth Count: Upper right lateral incisor (LI)","Tooth Count: Upper right central incisor (CI)","Tooth Count: Upper left central incisor (CI)","Tooth Count: Upper left lateral incisor (LI)","Tooth Count: Upper left cuspid (C)","Tooth Count: Upper left 1st bicuspid/1st primary molar (1B)","Tooth Count: Upper left 2nd bicuspid/2nd primary molar (2B)","Tooth Count: Upper left 1st molar (1M)","Tooth Count: Upper left 2nd molar (2M)","Tooth Count: Upper left 3rd molar (3M)","Tooth Count: Lower left 3rd molar (3M)","Tooth Count: Lower left 2nd molar (2M)","Tooth Count: Lower left 1st molar (1M)","Tooth Count: Lower left 2nd bicuspid/2nd primary molar (2B)","Tooth Count: Lower left 1st bicuspid/1st primary molar (1B)","Tooth Count: Lower left cuspid (C)","Tooth Count: Lower left lateral incisor (LI)","Tooth Count: Lower left central incisor (CI)","Tooth Count: Lower right central incisor (CI)","Tooth Count: Lower right lateral incisor (LI)","Tooth Count: Lower right cuspid (C)","Tooth Count: Lower right 1st bicuspid/1st primary molar (1B)","Tooth Count: Lower right 2nd bicuspid/2nd primary molar (2B)","Tooth Count: Lower right 1st molar (1M)","Tooth Count: Lower right 2nd molar (2M)","Tooth Count: Lower right 3rd molar (3M)","Coronal Caries: Upper right 2nd molar (2M) tooth code","Coronal Caries: Upper right 1st molar (1M) tooth code","Coronal Caries: Upper right 2nd bicuspid/2nd primary molar (2B) tooth code","Coronal Caries: Upper right 1st bicuspid/1st primary molar (1B) tooth code","Coronal Caries: Upper right cuspid (C) tooth code","Coronal Caries: Upper right lateral incisor (LI) tooth code","Coronal Caries: Upper right central incisor (CI) tooth code","Coronal Caries: Upper left central incisor (CI) tooth code","Coronal Caries: Upper left lateral incisor (LI) tooth code","Coronal Caries: Upper left cuspid (C) tooth code","Coronal Caries: Upper left 1st bicuspid/1st primary molar (1B) tooth code","Coronal Caries: Upper left 2nd bicuspid/2nd primary molar (2B) tooth code","Coronal Caries: Upper left 1st molar (1M) tooth code","Coronal Caries: Upper left 2nd molar (2M) tooth code","Coronal Caries: Lower left 2nd molar (2M) tooth code","Coronal Caries: Lower left 1st molar (1M) tooth code","Coronal Caries: Lower left 2nd bicuspid/2nd primary molar (2B) tooth code","Coronal Caries: Lower left 1st bicuspid/1st primary molar (1B) tooth code","Coronal Caries: Lower left cuspid (C) tooth code","Coronal Caries: Lower left lateral incisor (LI) tooth code","Coronal Caries: Lower left central incisor (CI) tooth code","Coronal Caries: Lower right central incisor (CI) tooth code","Coronal Caries: Lower right lateral incisor (LI) tooth code","Coronal Caries: Lower right cuspid (C) tooth code","Coronal Caries: Lower right 1st bicuspid/1st primary molar (1B) tooth code","Coronal Caries: Lower right 2nd bicuspid/2nd primary molar (2B) tooth code","Coronal Caries: Lower right 1st molar (1M) tooth code","Coronal Caries: Lower right 2nd molar (2M) tooth code"]
demo['var_descs'] = ["Respondent sequence number.","Data release cycle","Interview and examination status of the participant.","Gender of the participant.","Age in years of the participant at the time of screening. Individuals 80 and over are topcoded at 80 years of age.","Recode of reported race and Hispanic origin information","Recode of reported race and Hispanic origin information, with Non-Hispanic Asian Category","Six month time period when the examination was performed - two categories: November 1 through April 30, May 1 through October 31.","In what country {were you/was SP} born?","{Are you/Is SP} a citizen of the United States? [Information about citizenship is being collected by the U.S. Public Health Service to perform health related research. Providing this information is voluntary and is collected under the authority of the Public Health Service Act. There will be no effect on pending immigration or citizenship petitions.]","Language of the Sample Person Interview Instrument","Was a Proxy respondent used in conducting the Sample Person (SP) interview?","Was an interpreter used to conduct the Sample Person (SP) interview?","Language of the Family Interview Instrument","Was a Proxy respondent used in conducting the Family Interview?","Was an interpreter used to conduct the Family interview?","Total number of people in the Household","Total number of people in the Family","Number of children aged 5 years or younger in the household","Number of children aged 6-17 years old in the household","Number of adults aged 60 years or older in the household","HH reference person's gender","HH reference person's age in years","HH reference person's country of birth","HH reference person's education level","HH reference person's marital status","Full sample 2 year interview weight.","Full sample 2 year MEC exam weight.","Masked variance unit pseudo-PSU variable for variance estimation","Masked variance unit pseudo-stratum variable for variance estimation","Total household income (reported as a range value in dollars)","Total family income (reported as a range value in dollars)","A ratio of family income to poverty guidelines."]

# select which columns to keep
labs['selected'] = {0: 'ID',
                    6: 'White_cells_count',
                    17: 'Red_cells_count',
                    26: 'Caffeine',
                    27: 'Alcohol',
                    30: 'Supplements',
                    36: 'Hepatitis_a',
                    38: 'Hepatitis_b',
                    43: 'Cholesterol'}

diet['selected'] = {0: 'ID',
                    16: 'Number_of_foods',
                    17: 'Energy_kcal',
                    18: 'Protein',
                    19: 'Carbohydrate',
                    20: 'Sugar',
                    21: 'Fiber',
                    22: 'Total_fat',
                    23: 'Sat_fats',
                    24: 'Mono_fats',
                    25: 'Poly_fats',
                    26: 'Cholesterol',
                    27: 'Tocopherol',
                    28: 'Tocopherol_alpha',
                    29: 'Retinol',
                    30: 'Retinol_a',
                    31: 'Carotene_alpha',
                    32: 'Carotene_beta',
                    33: 'Cryptoxanthin',
                    34: 'Lycopene',
                    35: 'Lutein_zeaxanthin',
                    36: 'Thiamin',
                    37: 'Riboflavin',
                    38: 'Niacin',
                    39: 'Vitamin_b6',
                    40: 'Folate',
                    41: 'Folic_acid',
                    42: 'Folate_food',
                    43: 'Folate_equivs',
                    44: 'Choline',
                    45: 'Vitamine_b12',
                    46: 'Vitamine_b12_added',
                    47: 'Vitamine_c',
                    48: 'Vitamine_d',
                    49: 'Vitamine_k',
                    50: 'Calcium',
                    51: 'Phosphorus',
                    52: 'Magnesium',
                    53: 'Iron',
                    54: 'Zinc',
                    55: 'Copper',
                    56: 'Sodium',
                    57: 'Potassium',
                    58: 'Selenium',
                    59: 'Caffeine',
                    60: 'Theobromine',
                    61: 'Alcohol',
                    62: 'Moisture',
                    63: 'Butanoic',
                    64: 'Hexanoic',
                    65: 'Octanoic',
                    66: 'Decanoic',
                    67: 'Dodecanoic',
                    68: 'Tetradecanoic',
                    69: 'Hexadecanoic',
                    70: 'Octadecanoic',
                    71: 'Hexadecenoic',
                    72: 'Octadecenoic',
                    73: 'Eicosenoic',
                    74: 'Docosenoic',
                    75: 'Octadecadienoic',
                    76: 'Octadecatrienoic',
                    77: 'Octadecatetraenoic',
                    78: 'Eicosapentaenoic',
                    79: 'Eicosapentaenoic',
                    80: 'Docosapentaenoic',
                    81: 'Docosahexaenoic',
                    82: 'Food_yesterday',
                    83: 'Water_yesterday'}

exam['selected'] = {0: 'ID',
                     2: 'BP_test_time',
                     3: 'BP_arm',
                     10: 'BP_Systolic',
                     11: 'BP_Diastolic',
                     17: 'Weight',
                     18: 'Height',
                     19: 'Leg_length',
                     20: 'Arm_length',
                     23: 'Waist_circumference',
                     27: 'Dominant_hand',
                     43: 'Grip_strength'}

quest['selected'] = {0: 'ID',
                     1: 'Spent_total',
                     3: 'Spent_groceries',
                     4: 'Spent_vending',
                     5: 'Spent_delivered',
                     6: 'Cold_30',
                     7: 'Stomach_30',
                     8: 'Flu_30',
                     10: 'Diabetes',
                     11: 'Taking_insuline',
                     12: 'Milk_30',
                     13: 'Meals_outside',
                     14: 'Meals_premade',
                     15: 'Meals_frozen',
                     16: 'Deafness',
                     17: 'Blindness',
                     18: 'Forgetfulness',
                     26: 'Food_assistance',
                     28: 'WIC_assistance',
                     29: 'Hepatitis_b',
                     30: 'Hepatitis_c',
                     31: 'Insurance_current',
                     33: 'Insurance_lapse_12',
                     34: 'House_rooms',
                     36: 'Health_current',
                     39: 'Health_institution',
                     40: 'Doctor_visits_12',
                     42: 'Health_mental_12',
                     54: 'Family_income_mo',
                     57: 'Asthma',
                     58: 'Anemia',
                     59: 'Celiac',
                     60: 'Gluten_free_diet',
                     62: 'Jaundice',
                     63: 'Asthma_relatives',
                     64: 'Dentist_visit_since',
                     68: 'TV_30',
                     69: 'Gaming_hours',
                     73: 'Smoking_relatives',
                     75: 'Ride_motor_vehicle'}

demo['selected'] = {0: 'ID',
                    3: 'Gender',
                    4: 'Age',
                    6: 'Race',
                    8: 'Country_of_birth',
                    9: 'Citizenship_status',
                    17: 'Family_members',
                    25: 'Marital_status',
                    31: 'Family_income'}
                    

                     
# Modify SELECTED at Ln53

if 'var_descs' in SELECTED:
    select_relevant_vars(var_names, SELECTED)
    print('')
    print_selected_vars(SELECTED)
    print('')
    print_rename_vars(SELECTED)
