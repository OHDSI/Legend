Definition process
==================

We originally identified outcomes for LEGEND from clinical trial
endpoints from clinical guidelines and systematic reviews. We augmented
these with adverse events from US structured product labels of
hypertension drugs (<https://dailymed.nlm.nih.gov/dailymed/>). For each
outcome, we developed an operational phenotype definition to determine
if observational data could in fact support evaluation of the outcome.
We used the same approach to design, implement, and evaluate all
phenotypes. Specifically, we conducted a PubMed literature review to
identify prior observational studies that used the phenotype as an
outcome, looking especially for studies where source record verification
or other approaches validated the outcome. In addition, we reviewed
eMERGE PheKB phenotype entries (<https://phekb.org/phenotypes>).
Clinical guidelines and systematic review of clinical trials of
hypertension treatments informed our clinical definitions of
cardiovascular outcomes ([1](#ref-Reboussin2018) [2](#ref-Whelton2018)
[3](#ref-Williams2018)). Where possible, conceptsets originated with
published codelists (e.g. ICD-9-CM and ICD-10). We augmented these with
lexical search and semantic exploration of the OHDSI standardized
vocabularies. A clinical adjudicator then reviewed the cohort
definitions and associated conceptsets. We developed concept definitions
using ATLAS, the OHDSI open-source platform
(<https://github.com/OHDSI/atlas>). We initially executed these
definitions across 7 databases (CCAE, MDCR, MDCD, Optum, Panther, JMDC,
IMS Germany) to identify qualifying patients. Because the databases used
in this study do not all consistently contain laboratory values,
diagnosis records alone identified outcomes involving electrolyte
imbalance (hypokalemia, hypomagnesemia, hyponatremia). To assess
consistency across data sources as well as general clinical
reasonableness, we utilized these cohorts to characterize outcome
incidence, stratifying by age decile, gender, and index year. We did not
perform source record verification or other validation methods.

<table>
<thead>
<tr class="header">
<th align="left">Phenotype</th>
<th align="left">Logical description</th>
<th align="left">Supporting references</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Abdominal%20pain.json">Abdominal pain</a></td>
<td align="left">Abdominal pain condition record of any type; successive records with &gt; 90 day gap are considered independent episodes</td>
<td align="left"><span class="citation"><a href="#ref-Goldstein2003">4</a></span> <span class="citation"><a href="#ref-Rao2018">5</a></span> <span class="citation"><a href="#ref-Saps2013">6</a></span></td>
</tr>
<tr class="even">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Abnormal%20weight%20gain.json">Abnormal weight gain</a></td>
<td align="left">Abnormal weight gain record of any type; successive records with &gt; 90 day gap are considered independent episodes; note, weight measurements not used</td>
<td align="left"><span class="citation"><a href="#ref-Broder2016">7</a></span></td>
</tr>
<tr class="odd">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Abnormal%20weight%20loss.json">Abnormal weight loss</a></td>
<td align="left">Abnormal weight loss record of any type; successive records with &gt; 90 day gap are considered independent episodes; note, weight measurements not used</td>
<td align="left"><span class="citation"><a href="#ref-Williams2017">8</a></span></td>
</tr>
<tr class="even">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Acute%20myocardial%20infarction.json">Acute myocardial infarction</a></td>
<td align="left">Acute myocardial infarction condition record during an inpatient or ER visit; successive records with &gt; 180 day gap are considered independent episodes</td>
<td align="left"><span class="citation"><a href="#ref-Ammann2018">9</a></span> <span class="citation"><a href="#ref-Floyd2016a">10</a></span> <span class="citation"><a href="#ref-Rubbo2015">11</a></span> <span class="citation"><a href="#ref-Singh2018">12</a></span> <span class="citation"><a href="#ref-Wahl2010">13</a></span> <span class="citation"><a href="#ref-Normand1995">14</a></span></td>
</tr>
<tr class="odd">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Acute%20pancreatitis.json">Acute pancreatitis</a></td>
<td align="left">Acute pancreatitis condition record during an inpatient or ER visit; successive records with &gt;30 day gap are considered independent episodes</td>
<td align="left"><span class="citation"><a href="#ref-Dore2011">15</a></span> <span class="citation"><a href="#ref-Dore2013">16</a></span> <span class="citation"><a href="#ref-Yabe2015">17</a></span> <span class="citation"><a href="#ref-Chen2017">18</a></span></td>
</tr>
<tr class="even">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Acute%20renal%20failure.json">Acute renal failure</a></td>
<td align="left">A diagnosis of acute renal failure in an inpatient or ER setting; must be at least 30d between inpatient/ER visits to be considered separate episodes</td>
<td align="left"><span class="citation"><a href="#ref-Afzal2013">19</a></span> <span class="citation"><a href="#ref-Lenihan2013">20</a></span> <span class="citation"><a href="#ref-Winkelmayer2005">21</a></span> <span class="citation"><a href="#ref-Grams2014">22</a></span> <span class="citation"><a href="#ref-Arnold2018">23</a></span> <span class="citation"><a href="#ref-Sutherland2015">24</a></span> <span class="citation"><a href="#ref-Waikar2006">25</a></span> <span class="citation"><a href="#ref-Rhee2015">26</a></span></td>
</tr>
<tr class="odd">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/All-cause%20mortality.json">All-cause mortality</a></td>
<td align="left">Death record of any type</td>
<td align="left"><span class="citation"><a href="#ref-Singh2018">12</a></span> <span class="citation"><a href="#ref-Ooba2013">27</a></span> <span class="citation"><a href="#ref-Robinson2015">28</a></span></td>
</tr>
<tr class="even">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Anaphylactoid%20reaction.json">Anaphylactoid reaction</a></td>
<td align="left">Anaphylactoid reaction condition record during an inpatient or ER visit; successive records with &gt;7 day gap are considered independent episodes</td>
<td align="left"><span class="citation"><a href="#ref-Schneider2012a">29</a></span> <span class="citation"><a href="#ref-Walsh2013">30</a></span></td>
</tr>
<tr class="odd">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Anemia.json">Anemia</a></td>
<td align="left">The first condition record of anemia</td>
<td align="left"><span class="citation"><a href="#ref-Han2008">31</a></span> <span class="citation"><a href="#ref-Michalik2017">32</a></span> <span class="citation"><a href="#ref-Tuck2017">33</a></span></td>
</tr>
<tr class="even">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Angioedema.json">Angioedema</a></td>
<td align="left">Angioedema condition record during an inpatient or ER visit; successive records with &gt;7 day gap are considered independent episodes</td>
<td align="left"><span class="citation"><a href="#ref-Schneider2012a">29</a></span> <span class="citation"><a href="#ref-Cherepanov2015">34</a></span></td>
</tr>
<tr class="odd">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Anxiety.json">Anxiety</a></td>
<td align="left">The first condition record of anxiety, which is followed by another anxiety condition record or a drug used to treat anxiety</td>
<td align="left"><span class="citation"><a href="#ref-Bushnell2018">35</a></span> <span class="citation"><a href="#ref-John2016">36</a></span> <span class="citation"><a href="#ref-Marrie2016">37</a></span> <span class="citation"><a href="#ref-Castro2015">38</a></span></td>
</tr>
<tr class="even">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Bradycardia.json">Bradycardia</a></td>
<td align="left">The first condition record of bradycardia, which is followed by another bradycardia condition record</td>
<td align="left"><span class="citation"><a href="#ref-Shin2013">39</a></span> <span class="citation"><a href="#ref-Turgeon2015">40</a></span></td>
</tr>
<tr class="odd">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Cardiac%20arrhythmia.json">Cardiac arrhythmia</a></td>
<td align="left">The first condition record of cardiac arrhythmia, which is followed by another cardiac arrhythmia condition record, at least two drug records for a drug used to treat arrhythmias, or a procedure to treat arrhythmias</td>
<td align="left"><span class="citation"><a href="#ref-Hennessy2010">41</a></span> <span class="citation"><a href="#ref-Tamariz2012">42</a></span> <span class="citation"><a href="#ref-Gage2001">43</a></span> <span class="citation"><a href="#ref-Jensen2012">44</a></span> <span class="citation"><a href="#ref-Karnik2012">45</a></span> <span class="citation"><a href="#ref-NavarBoggan2015">46</a></span> <span class="citation"><a href="#ref-Yahi2015">47</a></span></td>
</tr>
<tr class="even">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Cardiovascular%20disease.json">Cardiovascular disease</a></td>
<td align="left">A condition record of ischemic stroke, hemorrhagic stroke, heart failure, acute myocardial infarction or sudden cardiac death during an inpatient or ER visit; successive records with &gt; 180 day gap are considered independent episodes</td>
<td align="left"><span class="citation"><a href="#ref-Ammann2018">9</a></span> <span class="citation"><a href="#ref-Floyd2016a">10</a></span> <span class="citation"><a href="#ref-Rubbo2015">11</a></span> <span class="citation"><a href="#ref-Singh2018">12</a></span> <span class="citation"><a href="#ref-Wahl2010">13</a></span> <span class="citation"><a href="#ref-Normand1995">14</a></span> <span class="citation"><a href="#ref-Hennessy2010">41</a></span></td>
</tr>
<tr class="odd">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Cardiovascular-related%20mortality.json">Cardiovascular-related mortality</a></td>
<td align="left">Death record with at least 1 cardiovascular-related condition record (myocardial infarction, ischemic stroke, intracranial hemorrhage, sudden cardiac death, hospitalization for heart failure) in 30 days prior to death</td>
<td align="left"><span class="citation"><a href="#ref-Singh2018">12</a></span></td>
</tr>
<tr class="even">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Chest%20pain%20or%20angina.json">Chest pain or angina</a></td>
<td align="left">The first condition record of chest pain or angina</td>
<td align="left"><span class="citation"><a href="#ref-Zaher2004">48</a></span></td>
</tr>
<tr class="odd">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Chronic%20kidney%20disease.json">Chronic kidney disease</a></td>
<td align="left">The first condition record of chronic kidney disease, which is followed by either another chronic kidney disease condition record or a dialysis procedure or observation</td>
<td align="left"><span class="citation"><a href="#ref-Winkelmayer2005">21</a></span> <span class="citation"><a href="#ref-Chase2010">49</a></span> <span class="citation"><a href="#ref-Fraccaro2016">50</a></span> <span class="citation"><a href="#ref-Frigaard2019">51</a></span> <span class="citation"><a href="#ref-Holzmann2016">52</a></span> <span class="citation"><a href="#ref-Luong2017">53</a></span> <span class="citation"><a href="#ref-Muntner2015">54</a></span> <span class="citation"><a href="#ref-Nadkarni2014">55</a></span> <span class="citation"><a href="#ref-Robertson2016">56</a></span></td>
</tr>
<tr class="even">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Cough.json">Cough</a></td>
<td align="left">Cough condition record of any type; successive records with &gt; 90 day gap are considered independent episodes</td>
<td align="left"><span class="citation"><a href="#ref-Chen2016">57</a></span> <span class="citation"><a href="#ref-Fathima2017">58</a></span> <span class="citation"><a href="#ref-Lazarus2001">59</a></span></td>
</tr>
<tr class="odd">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Decreased%20libido.json">Decreased libido</a></td>
<td align="left">The first condition record of decreased libido</td>
<td align="left"><span class="citation"><a href="#ref-Masterson2019">60</a></span></td>
</tr>
<tr class="even">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Dementia.json">Dementia</a></td>
<td align="left">The first condition record of dementia</td>
<td align="left"><span class="citation"><a href="#ref-Ostbye2008">61</a></span> <span class="citation"><a href="#ref-Sibbett2017">62</a></span> <span class="citation"><a href="#ref-Amra2017">63</a></span> <span class="citation"><a href="#ref-Jaakkimainen2016">64</a></span> <span class="citation"><a href="#ref-Kosteniuk2015">65</a></span> <span class="citation"><a href="#ref-McGuinness2019">66</a></span> <span class="citation"><a href="#ref-Williamson2014">67</a></span> <span class="citation"><a href="#ref-Lin2010">68</a></span></td>
</tr>
<tr class="odd">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Depression.json">Depression</a></td>
<td align="left">The first condition record of depression, which is followed by another depression condition record, at least two drugs used to treat depression without another indication, or two psychotherapy procedures</td>
<td align="left"><span class="citation"><a href="#ref-John2016">36</a></span> <span class="citation"><a href="#ref-Marrie2016">37</a></span> <span class="citation"><a href="#ref-Williamson2014">67</a></span> <span class="citation"><a href="#ref-Alaghehbandan2012">69</a></span> <span class="citation"><a href="#ref-Cepeda2018">70</a></span> <span class="citation"><a href="#ref-Davidson2018">71</a></span> <span class="citation"><a href="#ref-Doktorchik2019">72</a></span></td>
</tr>
<tr class="even">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Diarrhea.json">Diarrhea</a></td>
<td align="left">Diarrhea condition record of any type; successive records with &gt; 30 day gap are considered independent episodes</td>
<td align="left"><span class="citation"><a href="#ref-Buono2017">73</a></span> <span class="citation"><a href="#ref-Krishnarajah2016">74</a></span> <span class="citation"><a href="#ref-Panozzo2014">75</a></span></td>
</tr>
<tr class="odd">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/End%20stage%20renal%20disease.json">End stage renal disease</a></td>
<td align="left">End stage renal disease (ESRD) is defined by at least one diagnosis in any setting, followed by at least one additional diagnosis or a dialysis-related procedure within 90 days</td>
<td align="left"><span class="citation"><a href="#ref-Chen2017">18</a></span> <span class="citation"><a href="#ref-Muntner2015">54</a></span> <span class="citation"><a href="#ref-Shen2017">76</a></span></td>
</tr>
<tr class="even">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Fall.json">Fall</a></td>
<td align="left">Fall condition record of any type; successive records with &gt; 180 day gap are considered independent episodes</td>
<td align="left"><span class="citation"><a href="#ref-Homer2017">77</a></span> <span class="citation"><a href="#ref-Kim2018">78</a></span> <span class="citation"><a href="#ref-McCoy2017">79</a></span></td>
</tr>
<tr class="odd">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Gastrointestinal%20bleeding.json">Gastrointestinal bleeding</a></td>
<td align="left">Gastrointestinal hemorrhage condition record during an inpatient or ER visit; successive records with &gt; 30 day gap are considered independent episodes</td>
<td align="left"><span class="citation"><a href="#ref-Goldstein2003">4</a></span> <span class="citation"><a href="#ref-Wahl2010">13</a></span> <span class="citation"><a href="#ref-Curtis2011">80</a></span> <span class="citation"><a href="#ref-Lanza1995">81</a></span> <span class="citation"><a href="#ref-Lin2011">82</a></span> <span class="citation"><a href="#ref-Patorno2017">83</a></span> <span class="citation"><a href="#ref-Valkhoff2014">84</a></span></td>
</tr>
<tr class="even">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Gout.json">Gout</a></td>
<td align="left">The first condition record of gout</td>
<td align="left"><span class="citation"><a href="#ref-Cadzow2017">85</a></span> <span class="citation"><a href="#ref-Harrold2007">86</a></span> <span class="citation"><a href="#ref-MacFarlane2016">87</a></span> <span class="citation"><a href="#ref-Malik2009">88</a></span></td>
</tr>
<tr class="odd">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Headache.json">Headache</a></td>
<td align="left">Headache condition record of any type; successive records with &gt; 30 day gap are considered independent episodes</td>
<td align="left"><span class="citation"><a href="#ref-Hurwitz2016">89</a></span> <span class="citation"><a href="#ref-Rizzoli2016">90</a></span></td>
</tr>
<tr class="even">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Heart%20failure.json">Heart failure</a></td>
<td align="left">The first condition record of heart failure, which is followed by at least 1 heart failure condition record in the following year</td>
<td align="left"><span class="citation"><a href="#ref-Floyd2016a">10</a></span> <span class="citation"><a href="#ref-Floyd2016b">91</a></span> <span class="citation"><a href="#ref-Gini2016">92</a></span> <span class="citation"><a href="#ref-Kaspar2018">93</a></span> <span class="citation"><a href="#ref-Li2011">94</a></span> <span class="citation"><a href="#ref-Patel2018">95</a></span> <span class="citation"><a href="#ref-Saczynski2012">96</a></span> <span class="citation"><a href="#ref-Schultz2013">97</a></span> <span class="citation"><a href="#ref-Feder2018">98</a></span> <span class="citation"><a href="#ref-Rosenman2014">99</a></span> <span class="citation"><a href="#ref-Quach2010">100</a></span></td>
</tr>
<tr class="odd">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Hemorrhagic%20stroke.json">Hemorrhagic stroke</a></td>
<td align="left">Intracranial, cerebral or subarachnoid hemorrhage condition record during an inpatient or ER visit; successive records with &gt; 180 day gap are considered independent episodes</td>
<td align="left"><span class="citation"><a href="#ref-Andrade2012">101</a></span> <span class="citation"><a href="#ref-Park2016">102</a></span> <span class="citation"><a href="#ref-Gon2017">103</a></span> <span class="citation"><a href="#ref-Sung2016">104</a></span> <span class="citation"><a href="#ref-Tu2013">105</a></span></td>
</tr>
<tr class="even">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Hepatic%20failure.json">Hepatic failure</a></td>
<td align="left">The first condition record of hepatic failure, necrosis, or coma</td>
<td align="left"><span class="citation"><a href="#ref-Afzal2013">19</a></span> <span class="citation"><a href="#ref-Bui2014">106</a></span> <span class="citation"><a href="#ref-Cheetham2014">107</a></span> <span class="citation"><a href="#ref-Jinjuvadia2007">108</a></span> <span class="citation"><a href="#ref-Lo_Re2015">109</a></span> <span class="citation"><a href="#ref-Lo_Re2013">110</a></span> <span class="citation"><a href="#ref-Overby2013">111</a></span> <span class="citation"><a href="#ref-Udo2016">112</a></span> <span class="citation"><a href="#ref-Wing2016">113</a></span></td>
</tr>
<tr class="odd">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Hospitalization%20with%20heart%20failure.json">Hospitalization for heart failure</a></td>
<td align="left">Inpatient or ER visits with heart failure condition record; all qualifying inpatient visits occurring &gt; 7 days apart are considered independent episodes</td>
<td align="left"><span class="citation"><a href="#ref-Kaspar2018">93</a></span> <span class="citation"><a href="#ref-Feder2018">98</a></span> <span class="citation"><a href="#ref-Rosenman2014">99</a></span> <span class="citation"><a href="#ref-Ryan2018">114</a></span> <span class="citation"><a href="#ref-Voors2017">115</a></span></td>
</tr>
<tr class="even">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Hyperkalemia.json">Hyperkalemia</a></td>
<td align="left">Condition record for hyperkalemia or potassium measurements &gt; 5.6 mmol/L; successive records with &gt;90 day gap are considered independent episodes</td>
<td align="left"><span class="citation"><a href="#ref-Abbas2015">116</a></span> <span class="citation"><a href="#ref-Betts2018">117</a></span> <span class="citation"><a href="#ref-Fitch2017">118</a></span></td>
</tr>
<tr class="odd">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Hypokalemia.json">Hypokalemia</a></td>
<td align="left">Hypokalemia condition record of any type; successive records with &gt; 90 day gap are considered independent episodes</td>
<td align="left"><span class="citation"><a href="#ref-Krogager2017">119</a></span></td>
</tr>
<tr class="even">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Hypomagnesemia.json">Hypomagnesemia</a></td>
<td align="left">Hypomagnesemia condition record of any type; successive records with &gt; 90 day gap are considered independent episodes</td>
<td align="left"><span class="citation"><a href="#ref-Koulouridis2013">120</a></span> <span class="citation"><a href="#ref-Markovits2014">121</a></span></td>
</tr>
<tr class="odd">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Hyponatremia.json">Hyponatremia</a></td>
<td align="left">Hyponatremia condition record of any type; successive records with &gt; 90 day gap are considered independent episodes</td>
<td align="left"><span class="citation"><a href="#ref-Movig2003">122</a></span> <span class="citation"><a href="#ref-Shea2008">123</a></span></td>
</tr>
<tr class="even">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Hypotension.json">Hypotension</a></td>
<td align="left">Hypotension condition record of any type; successive records with &gt; 90 day gap are considered independent episodes</td>
<td align="left"><span class="citation"><a href="#ref-Chrischilles2001">124</a></span></td>
</tr>
<tr class="odd">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Impotence.json">Impotence</a></td>
<td align="left">The first condition record of impotence</td>
<td align="left"><span class="citation"><a href="#ref-Bekelman2011">125</a></span> <span class="citation"><a href="#ref-Frederick2014">126</a></span> <span class="citation"><a href="#ref-McVary2008">127</a></span> <span class="citation"><a href="#ref-Mulhall2016">128</a></span></td>
</tr>
<tr class="even">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Ischemic%20stroke.json">Ischemic stroke</a></td>
<td align="left">Ischemic stroke condition record during an inpatient or ER visit; successive records with &gt; 180 day gap are considered independent episodes</td>
<td align="left"><span class="citation"><a href="#ref-Floyd2016b">91</a></span> <span class="citation"><a href="#ref-Singh2018">12</a></span> <span class="citation"><a href="#ref-Wahl2010">13</a></span> <span class="citation"><a href="#ref-Andrade2012">101</a></span> <span class="citation"><a href="#ref-Yuan2017">129</a></span></td>
</tr>
<tr class="odd">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Malignant%20neoplasm.json">Malignant neoplasm</a></td>
<td align="left">First occurrence of malignant neoplasm, followed by at least one additional diagnosis of the same type (melanoma, bladder, brain, breast, colon and rectum, kidney, leukemia, liver, lung, lymphoma, multiple myeloma, ovary, pancreas, prostate, thyroid, uterus, myelodysplastic syndrome)</td>
<td align="left"><span class="citation"><a href="#ref-Czwikla2017">130</a></span> <span class="citation"><a href="#ref-Abraha2018a">131</a></span> <span class="citation"><a href="#ref-Abraha2018b">132</a></span> <span class="citation"><a href="#ref-Baldi2008">133</a></span> <span class="citation"><a href="#ref-Cea_Soriano2016">134</a></span> <span class="citation"><a href="#ref-Chawla2014">135</a></span> <span class="citation"><a href="#ref-Creighton2016">136</a></span> <span class="citation"><a href="#ref-Dregan2012">137</a></span> <span class="citation"><a href="#ref-Goldsbury2017">138</a></span> <span class="citation"><a href="#ref-Gupta2018">139</a></span> <span class="citation"><a href="#ref-Hassett2014">140</a></span> <span class="citation"><a href="#ref-Kim2013">141</a></span> <span class="citation"><a href="#ref-Nordstrom2012">142</a></span> <span class="citation"><a href="#ref-Penberthy2003">143</a></span> <span class="citation"><a href="#ref-Stavrou2012">144</a></span> <span class="citation"><a href="#ref-Goldberg2013">145</a></span></td>
</tr>
<tr class="even">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Measured%20renal%20dysfunction.json">Measured renal dysfunction</a></td>
<td align="left">The first creatinine measurement with value &gt; 3 mg/dL</td>
<td align="left"><span class="citation"><a href="#ref-Rhee2015">26</a></span></td>
</tr>
<tr class="odd">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Nausea.json">Nausea</a></td>
<td align="left">Nausea condition record of any type; successive records with &gt; 30 day gap are considered independent episodes</td>
<td align="left"><span class="citation"><a href="#ref-Goldstein2003">4</a></span> <span class="citation"><a href="#ref-Donga2017">146</a></span> <span class="citation"><a href="#ref-Marrett2016">147</a></span></td>
</tr>
<tr class="even">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Neutropenia%20or%20agranulocytosis.json">Neutropenia or agranulocytosis</a></td>
<td align="left">The first condition record of neutropenia or agranulocytosis</td>
<td align="left"><span class="citation"><a href="#ref-Kim2011">148</a></span> <span class="citation"><a href="#ref-Weycker2013">149</a></span></td>
</tr>
<tr class="odd">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Rash.json">Rash</a></td>
<td align="left">Rash condition record of any type; successive records with &gt; 90 day gap are considered independent episodes</td>
<td align="left"><span class="citation"><a href="#ref-Schneider2012b">150</a></span></td>
</tr>
<tr class="even">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Rhabdomyolysis.json">Rhabdomyolysis</a></td>
<td align="left">Rhabdomyolysis condition record or muscle disorder condition record with creatine measurement 5*ULN during an inpatient or ER visit; successive records with &gt;90 day gap are considered independent episodes</td>
<td align="left"><span class="citation"><a href="#ref-Andrade2005">151</a></span> <span class="citation"><a href="#ref-Chan2017">152</a></span></td>
</tr>
<tr class="odd">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Stroke.json">Stroke</a></td>
<td align="left">Stroke (ischemic or hemorrhagic) condition record during an inpatient or ER visit; successive records with &gt; 180 day gap are considered independent episodes</td>
<td align="left"><span class="citation"><a href="#ref-Floyd2016b">91</a></span> <span class="citation"><a href="#ref-Singh2018">12</a></span> <span class="citation"><a href="#ref-Wahl2010">13</a></span> <span class="citation"><a href="#ref-Gage2001">43</a></span> <span class="citation"><a href="#ref-Andrade2012">101</a></span> <span class="citation"><a href="#ref-Park2016">102</a></span> <span class="citation"><a href="#ref-Gon2017">103</a></span> <span class="citation"><a href="#ref-Sung2016">104</a></span> <span class="citation"><a href="#ref-Tu2013">105</a></span> <span class="citation"><a href="#ref-Yuan2017">129</a></span></td>
</tr>
<tr class="even">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Sudden%20cardiac%20death.json">Sudden cardiac death</a></td>
<td align="left">Sudden cardiac death condition record during an inpatient or ER visit; successive records with &gt; 180 day gap are considered independent episodes</td>
<td align="left"><span class="citation"><a href="#ref-Singh2018">12</a></span> <span class="citation"><a href="#ref-Hennessy2010">41</a></span></td>
</tr>
<tr class="odd">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Syncope.json">Syncope</a></td>
<td align="left">Syncope condition record of any type; successive records with &gt; 180 day gap are considered independent episodes</td>
<td align="left"><span class="citation"><a href="#ref-Chrischilles2001">124</a></span></td>
</tr>
<tr class="even">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Thrombocytopenia.json">Thrombocytopenia</a></td>
<td align="left">The first condition record of thrombocytopenia</td>
<td align="left"><span class="citation"><a href="#ref-Donga2017">146</a></span> <span class="citation"><a href="#ref-Wahl2010b">153</a></span> <span class="citation"><a href="#ref-Moulis2016">154</a></span></td>
</tr>
<tr class="odd">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Transient%20ischemic%20attack.json">Transient ischemic attack</a></td>
<td align="left">Transient ischemic attack condition record during an inpatient or ER visit; successive records with &gt; 30 day gap are considered independent episodes</td>
<td align="left"><span class="citation"><a href="#ref-Andrade2012">101</a></span> <span class="citation"><a href="#ref-Yuan2017">129</a></span></td>
</tr>
<tr class="even">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Type%202%20diabetes%20mellitus.json">Type 2 diabetes mellitus</a></td>
<td align="left">The first condition record of Type 2 Diabetes Mellitus, which is followed by another Type 2 Diabetes Mellitus condition record, at least 2 drugs used to treat Type 2 diabetes, or at least 2 HbA1c measurements with value &gt; 6.5%</td>
<td align="left"><span class="citation"><a href="#ref-Williamson2014">67</a></span> <span class="citation"><a href="#ref-Khokhar2016">155</a></span> <span class="citation"><a href="#ref-Leong2013">156</a></span> <span class="citation"><a href="#ref-Chen2010">157</a></span></td>
</tr>
<tr class="odd">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Hospitalization%20with%20preinfarction%20syndrome.json">Unstable angina</a></td>
<td align="left">Inpatient or ER visits with preinfarction syndrome condition record; all qualifying inpatient visits occurring &gt; 7 days apart are considered independent episodes</td>
<td align="left"><span class="citation"><a href="#ref-Zaher2004">48</a></span> <span class="citation"><a href="#ref-Saver2009">158</a></span> <span class="citation"><a href="#ref-Varas-Lorenzo2008">159</a></span></td>
</tr>
<tr class="even">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Vasculitis.json">Vasculitis</a></td>
<td align="left">The first condition record of vasculitis, which is followed by another vasculitis condition record or drug to treat vasculitis</td>
<td align="left"><span class="citation"><a href="#ref-Thorpe2018">160</a></span> <span class="citation"><a href="#ref-England2017">161</a></span></td>
</tr>
<tr class="odd">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Venous%20thromboembolic%20events%20.json">Venous thromboembolic events</a></td>
<td align="left">Venous thromboembolism condition record of any type; successive records with &gt; 180 day gap are considered independent episodes</td>
<td align="left"><span class="citation"><a href="#ref-Tamariz2012b">162</a></span> <span class="citation"><a href="#ref-Burwen2017">163</a></span> <span class="citation"><a href="#ref-Coleman2016">164</a></span> <span class="citation"><a href="#ref-Ammann2018b">165</a></span></td>
</tr>
<tr class="even">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Vertigo.json">Vertigo</a></td>
<td align="left">The first condition record of vertigo</td>
<td align="left"><span class="citation"><a href="#ref-Koo2016">166</a></span></td>
</tr>
<tr class="odd">
<td align="left"><a href="https://github.com/OHDSI/Legend/blob/master/inst/cohorts/Vomiting.json">Vomiting</a></td>
<td align="left">Vomiting condition record of any type; successive records with &gt; 30 day gap are considered independent episodes</td>
<td align="left"><span class="citation"><a href="#ref-Goldstein2003">4</a></span> <span class="citation"><a href="#ref-Donga2017">146</a></span> <span class="citation"><a href="#ref-Marrett2016">147</a></span></td>
</tr>
</tbody>
</table>

References
----------

1 Reboussin DM, Allen NB, Griswold ME, Guallar E, Hong Y, Lackland DT
*et al.* Systematic review for the 2017
ACC/AHA/AAPA/ABC/ACPM/AGS/APhA/ASH/ASPC/NMA/PCNA guideline for the
prevention, detection, evaluation, and management of high blood pressure
in adults: A report of the american college of Cardiology/American heart
association task force on clinical practice guidelines. *Hypertension*
2018; **71**: e116–e135. doi: 10.1161/HYP.0000000000000067. Epub 2017
Nov 13.

2 Whelton PK, Carey RM, Aronow WS, Casey DE Jr., Collins KJ, Dennison
Himmelfarb C *et al.* 2017
ACC/AHA/AAPA/ABC/ACPM/AGS/APhA/ASH/ASPC/NMA/PCNA guideline for the
prevention, detection, evaluation, and management of high blood pressure
in adults: A report of the american college of Cardiology/American heart
association task force on clinical practice guidelines. *Hypertension*
2018; **71**: e13–e115. doi: 10.1161/HYP.0000000000000065. Epub 2017 Nov
13.

3 Williams B, Mancia G, Spiering W, Agabiti Rosei E, Azizi M, Burnier M
*et al.* 2018 ESC/ESH guidelines for the management of arterial
hypertension. *Eur Heart J* 2018; **39**: 3021–3104. doi:
10.1093/eurheartj/ehy339.

4 Goldstein JL, Zhao SZ, Burke TA, Palmer R, Allmen H von, Henderson SC.
Incidence of outpatient physician claims for upper gastrointestinal
symptoms among new users of celecoxib, ibuprofen, and naproxen in an
insured population in the united states. *Am J Gastroenterol* 2003;
**98**: 2627–34. doi: 10.1111/j.1572–0241.2003.08722.x.

5 Rao G, Kirley K, Epner P, Zhang Y, Bauer V, Padman R *et al.*
Identifying, analyzing, and visualizing diagnostic paths for patients
with nonspecific abdominal pain. *Appl Clin Inform* 2018; **9**:
905–913. doi: 10.1055/s–0038–1676338.Epub 2018 Dec 19.

6 Saps M, Hudgens S, Mody R, Lasch K, Harikrishnan V, Baum C. Seasonal
patterns of abdominal pain consultations among adults and children. *J
Pediatr Gastroenterol Nutr* 2013; **56**: 290–6. doi:
10.1097/MPG.0b013e3182769796.

7 Broder MS, Chang E, Cherepanov D, Neary MP, Ludlam WH. Identification
of potential markers for cushing disease. *Endocr Pract* 2016; **22**:
567–74. doi: 10.4158/EP15914.OR. Epub 2016 Jan 20.

8 Williams BA. The clinical epidemiology of fatigue in newly diagnosed
heart failure. *BMC Cardiovasc Disord* 2017; **17**: 122. doi:
10.1186/s12872–017–0555–9.

9 Ammann EM, Schweizer ML, Robinson JG, Eschol JO, Kafa R, Girotra S *et
al.* Chart validation of inpatient ICD-9-CM administrative diagnosis
codes for acute myocardial infarction (AMI) among intravenous immune
globulin (IGIV) users in the sentinel distributed database.
*Pharmacoepidemiol Drug Saf* 2018; **27**: 398–404. doi:
10.1002/pds.4398. Epub 2018 Feb 15.

10 Floyd JS, Blondon M, Moore KP, Boyko EJ, Smith NL. Validation of
methods for assessing cardiovascular disease using electronic health
data in a cohort of veterans with diabetes. *Pharmacoepidemiol Drug Saf*
2016; **25**: 467–71. doi: 10.1002/pds.3921. Epub 2015 Nov 11.

11 Rubbo B, Fitzpatrick NK, Denaxas S, Daskalopoulou M, Yu N, Patel RS
*et al.* Use of electronic health records to ascertain, validate and
phenotype acute myocardial infarction: A systematic review and
recommendations. *Int J Cardiol* 2015; **187:705-11.**:
10.1016/j.ijcard.2015.03.075. Epub 2015 Mar 5.

12 Singh S, Fouayzi H, Anzuoni K, Goldman L, Min JY, Griffin M *et al.*
Diagnostic algorithms for cardiovascular death in administrative claims
databases: A systematic review. *Drug Saf* 2018; **23**: 018–0754.

13 Wahl PM, Rodgers K, Schneeweiss S, Gage BF, Butler J, Wilmer C *et
al.* Validation of claims-based diagnostic and procedure codes for
cardiovascular and gastrointestinal serious adverse events in a
commercially-insured population. *Pharmacoepidemiol Drug Saf* 2010;
**19**: 596–603. doi: 10.1002/pds.1924.

14 Normand SL, Morris CN, Fung KS, McNeil BJ, Epstein AM. Development
and validation of a claims based index for adjusting for risk of
mortality: The case of acute myocardial infarction. *J Clin Epidemiol*
1995; **48**: 229–43.

15 Dore DD, Chaudhry S, Hoffman C, Seeger JD. Stratum-specific positive
predictive values of claims for acute pancreatitis among commercial
health insurance plan enrollees with diabetes mellitus.
*Pharmacoepidemiol Drug Saf* 2011; **20**: 209–13. doi:
10.1002/pds.2077. Epub 2010 Dec 23.

16 Dore DD, Hussein M, Hoffman C, Pelletier EM, Smith DB, Seeger JD. A
pooled analysis of exenatide use and risk of acute pancreatitis. *Curr
Med Res Opin* 2013; **29**: 1577–86. doi: 10.1185/03007995.2013.838550.
Epub 2013 Sep 13.

17 Yabe D, Kuwata H, Kaneko M, Ito C, Nishikino R, Murorani K *et al.*
Use of the japanese health insurance claims database to assess the risk
of acute pancreatitis in patients with diabetes: Comparison of DPP-4
inhibitors with other oral antidiabetic drugs. *Diabetes Obes Metab*
2015; **17**: 430–4. doi: 10.1111/dom.12381. Epub 2014 Sep 17.

18 Chen HJ, Wang JJ, Tsay WI, Her SH, Lin CH, Chien CC. Epidemiology and
outcome of acute pancreatitis in end-stage renal disease dialysis
patients: A 10-year national cohort study. *Nephrol Dial Transplant*
2017; **32**: 1731–1736. doi: 10.1093/ndt/gfw400.

19 Afzal Z, Schuemie MJ, Blijderveen JC van, Sen EF, Sturkenboom MC,
Kors JA. Improving sensitivity of machine learning methods for automated
case identification from free-text electronic medical records. *BMC Med
Inform Decis Mak* 2013; **13:30.**: 10.1186/1472–6947–13–30.

20 Lenihan CR, Montez-Rath ME, Mora Mangano CT, Chertow GM, Winkelmayer
WC. Trends in acute kidney injury, associated use of dialysis, and
mortality after cardiac surgery, 1999 to 2008. *Ann Thorac Surg* 2013;
**95**: 20–8. doi: 10.1016/j.athoracsur.2012.05.131. Epub 2012 Dec 25.

21 Winkelmayer WC, Schneeweiss S, Mogun H, Patrick AR, Avorn J, Solomon
DH. Identification of individuals with CKD from medicare claims data: A
validation study. *Am J Kidney Dis* 2005; **46**: 225–32. doi:
10.1053/j.ajkd.2005.04.029.

22 Grams ME, Waikar SS, MacMahon B, Whelton S, Ballew SH, Coresh J.
Performance and limitations of administrative data in the identification
of AKI. *Clin J Am Soc Nephrol* 2014; **9**: 682–9. doi:
10.2215/CJN.07650713. Epub 2014 Jan 23.

23 Arnold J, Ng KP, Sims D, Gill P, Cockwell P, Ferro C. Incidence and
impact on outcomes of acute kidney injury after a stroke: A systematic
review and meta-analysis. *BMC Nephrol* 2018; **19**: 283. doi:
10.1186/s12882–018–1085–0.

24 Sutherland SM, Byrnes JJ, Kothari M, Longhurst CA, Dutta S, Garcia P
*et al.* AKI in hospitalized children: Comparing the pRIFLE, AKIN, and
KDIGO definitions. *Clin J Am Soc Nephrol* 2015; **10**: 554–61. doi:
10.2215/CJN.01900214. Epub 2015 Feb 3.

25 Waikar SS, Wald R, Chertow GM, Curhan GC, Winkelmayer WC, Liangos O
*et al.* Validity of international classification of diseases, ninth
revision, clinical modification codes for acute renal failure. *J Am Soc
Nephrol* 2006; **17**: 1688–94. doi: 10.1681/ASN.2006010073. Epub 2006
Apr 26.

26 Rhee C, Murphy MV, Li L, Platt R, Klompas M. Improving documentation
and coding for acute organ dysfunction biases estimates of changing
sepsis severity and burden: A retrospective study. *Crit Care* 2015;
**19:338.**: 10.1186/s13054–015–1048–9.

27 Ooba N, Setoguchi S, Ando T, Sato T, Yamaguchi T, Mochizuki M *et
al.* Claims-based definition of death in japanese claims database:
Validity and implications. *PLoS One* 2013; **8**: e66116. doi:
10.1371/journal.pone.0066116. Print 2013.

28 Robinson TE, Elley CR, Kenealy T, Drury PL. Development and
validation of a predictive risk model for all-cause mortality in type 2
diabetes. *Diabetes Res Clin Pract* 2015; **108**: 482–8. doi:
10.1016/j.diabres.2015.02.015. Epub 2015 Mar 16.

29 Schneider G, Kachroo S, Jones N, Crean S, Rotella P, Avetisyan R *et
al.* A systematic review of validated methods for identifying
anaphylaxis, including anaphylactic shock and angioneurotic edema, using
administrative and claims data. *Pharmacoepidemiol Drug Saf* 2012;
**21**: 240–7. doi: 10.1002/pds.2327.

30 Walsh KE, Cutrona SL, Foy S, Baker MA, Forrow S, Shoaibi A *et al.*
Validation of anaphylaxis in the food and drug administration’s
Mini-Sentinel. *Pharmacoepidemiol Drug Saf* 2013; **22**: 1205–13. doi:
10.1002/pds.3505. Epub 2013 Sep 5.

31 Han C, Zhao N, Rahman MU, Doyle MK, Bala MV. A case-control study of
anaemia in patients with rheumatoid arthritis treated with
disease-modifying antirheumatic drugs in an adult population in the US:
Prevalence and impact on healthcare utilisation. *J Med Econ* 2008;
**11**: 255–64. doi: 10.3111/13696990802066469.

32 Michalik DE, Taylor BW, Panepinto JA. Identification and validation
of a sickle cell disease cohort within electronic health records. *Acad
Pediatr* 2017; **17**: 283–287. doi: 10.1016/j.acap.2016.12.005. Epub
2016 Dec 13.

33 Tuck MG, Alemi F, Shortle JF, Avramovic S, Hesdorffer C. A
comprehensive index for predicting risk of anemia from patients’
diagnoses. *Big Data* 2017; **5**: 42–52. doi: 10.1089/big.2016.0073.

34 Cherepanov D, Raimundo K, Chang E, Eagan M, Zazzali JL, Solari PG *et
al.* Validation of an ICD-9-based claims algorithm for identifying
patients with chronic idiopathic/spontaneous urticaria. *Ann Allergy
Asthma Immunol* 2015; **114**: 393–8. doi: 10.1016/j.anai.2015.02.003.
Epub 2015 Mar 12.

35 Bushnell GA, Dusetzina SB, Compton SN, Gaynes BN, Brookhart MA,
Sturmer T. Psychotherapy claims surrounding pharmacotherapy initiation
in children and adolescents with anxiety disorders. *J Child Adolesc
Psychopharmacol* 2018; **19**.

36 John A, McGregor J, Fone D, Dunstan F, Cornish R, Lyons RA *et al.*
Case-finding for common mental disorders of anxiety and depression in
primary care: An external validation of routinely collected data. *BMC
Med Inform Decis Mak* 2016; **16:35.**: 10.1186/s12911–016–0274–7.

37 Marrie RA, Walker JR, Graff LA, Lix LM, Bolton JM, Nugent Z *et al.*
Performance of administrative case definitions for depression and
anxiety in inflammatory bowel disease. *J Psychosom Res* 2016;
**89:107-13.**: 10.1016/j.jpsychores.2016.08.014. Epub 2016 Sep 3.

38 Castro VM, Minnier J, Murphy SN, Kohane I, Churchill SE, Gainer V *et
al.* Validation of electronic health record phenotyping of bipolar
disorder cases and controls. *Am J Psychiatry* 2015; **172**: 363–72.
doi: 10.1176/appi.ajp.2014.14030423. Epub 2014 Dec 12.

39 Shin J, Gonzales M, Pletcher MJ. Risk of emergent bradycardia
associated with initiation of immediate- or slow-release metoprolol.
*Pharmacotherapy* 2013; **33**: 1353–61. doi: 10.1002/phar.1319. Epub
2013 Jun 27.

40 Turgeon RD, Fernandes KA, Juurlink D, Tu JV, Mamdani M. Ticagrelor
and bradycardia: A nested case-control study. *Pharmacoepidemiol Drug
Saf* 2015; **24**: 1281–5. doi: 10.1002/pds.3884. Epub 2015 Oct 7.

41 Hennessy S, Leonard CE, Freeman CP, Deo R, Newcomb C, Kimmel SE *et
al.* Validation of diagnostic codes for outpatient-originating sudden
cardiac death and ventricular arrhythmia in medicaid and medicare claims
data. *Pharmacoepidemiol Drug Saf* 2010; **19**: 555–62. doi:
10.1002/pds.1869.

42 Tamariz L, Harkins T, Nair V. A systematic review of validated
methods for identifying ventricular arrhythmias using administrative and
claims data. *Pharmacoepidemiol Drug Saf* 2012; **21**: 148–53. doi:
10.1002/pds.2340.

43 Gage BF, Waterman AD, Shannon W, Boechler M, Rich MW, Radford MJ.
Validation of clinical classification schemes for predicting stroke:
Results from the national registry of atrial fibrillation. *JAMA* 2001;
**285**: 2864–70.

44 Jensen PN, Johnson K, Floyd J, Heckbert SR, Carnahan R, Dublin S. A
systematic review of validated methods for identifying atrial
fibrillation using administrative data. *Pharmacoepidemiol Drug Saf*
2012; **21**: 141–7. doi: 10.1002/pds.2317.

45 Karnik S, Tan SL, Berg B, Glurich I, Zhang J, Vidaillet HJ *et al.*
Predicting atrial fibrillation and flutter using electronic health
records. *Conf Proc IEEE Eng Med Biol Soc* 2012; **2012:5562-5.**:
10.1109/EMBC.2012.6347254.

46 Navar-Boggan AM, Rymer JA, Piccini JP, Shatila W, Ring L, Stafford JA
*et al.* Accuracy and validation of an automated electronic algorithm to
identify patients with atrial fibrillation at risk for stroke. *Am Heart
J* 2015; **169**: 39–44.e2. doi: 10.1016/j.ahj.2014.09.014. Epub 2014
Oct 22.

47 Yahi A, Tatonetti NP. A knowledge-based, automated method for
phenotyping in the EHR using only clinical pathology reports. *AMIA Jt
Summits Transl Sci Proc* 2015; **2015**: 64–8. eCollection 2015.

48 Zaher C, Goldberg GA, Kadlubek P. Estimating angina prevalence in a
managed care population. *Am J Manag Care* 2004; **10**: S339–46.

49 Chase HS, Radhakrishnan J, Shirazian S, Rao MK, Vawdrey DK.
Under-documentation of chronic kidney disease in the electronic health
record in outpatients. *J Am Med Inform Assoc* 2010; **17**: 588–94.
doi: 10.1136/jamia.2009.001396.

50 Fraccaro P, Veer S van der, Brown B, Prosperi M, O’Donoghue D,
Collins GS *et al.* An external validation of models to predict the
onset of chronic kidney disease using population-based electronic health
records from salford, UK. *BMC Med* 2016; **14:104.**:
10.1186/s12916–016–0650–2.

51 Frigaard M, Rubinsky A, Lowell L, Malkina A, Karliner L, Kohn M *et
al.* Validating laboratory defined chronic kidney disease in the
electronic health record for patients in primary care. *BMC Nephrol*
2019; **20**: 3. doi: 10.1186/s12882–018–1156–2.

52 Holzmann MJ, Carlsson AC, Hammar N, Ivert T, Walldius G, Jungner I
*et al.* Chronic kidney disease and 10-year risk of cardiovascular
death. *Eur J Prev Cardiol* 2016; **23**: 1187–94. doi:
10.1177/2047487315614491. Epub 2015 Nov 5.

53 Luong DTA, Tran D, Pace WD, Dickinson M, Vassalotti J, Carroll J *et
al.* Extracting deep phenotypes for chronic kidney disease using
electronic health records. *EGEMS (Wash DC)* 2017; **5**: 9. doi:
10.5334/egems.226.

54 Muntner P, Gutierrez OM, Zhao H, Fox CS, Wright NC, Curtis JR *et
al.* Validation study of medicare claims to identify older US adults
with CKD using the reasons for geographic and racial differences in
stroke (REGARDS) study. *Am J Kidney Dis* 2015; **65**: 249–58. doi:
10.1053/j.ajkd.2014.07.012. Epub 2014 Sep 19.

55 Nadkarni GN, Gottesman O, Linneman JG, Chase H, Berg RL, Farouk S *et
al.* Development and validation of an electronic phenotyping algorithm
for chronic kidney disease. *AMIA Annu Symp Proc* 2014; **2014**:
907–16. eCollection 2014.

56 Robertson LM, Denadai L, Black C, Fluck N, Prescott G, Simpson W *et
al.* Is routine hospital episode data sufficient for identifying
individuals with chronic kidney disease? A comparison study with
laboratory data. *Health Informatics J* 2016; **22**: 383–96. doi:
10.1177/1460458214562286. Epub 2014 Dec 31.

57 Chen CC, Balderston McGuiness C, Krishnarajah G, Blanchette CM, Wang
Y, Sun K *et al.* Estimated incidence of pertussis in people aged &lt;50
years in the united states. *Hum Vaccin Immunother* 2016; **12**:
2536–2545. doi: 10.1080/21645515.2016.1186313. Epub 2016 May 31.

58 Fathima S, Simmonds KA, Drews SJ, Svenson LW, Kwong JC, Mahmud SM *et
al.* How well do ICD-9 physician claim diagnostic codes identify
confirmed pertussis cases in alberta, canada? A canadian immunization
research network (CIRN) study. *BMC Health Serv Res* 2017; **17**: 479.
doi: 10.1186/s12913–017–2321–1.

59 Lazarus R, Kleinman KP, Dashevsky I, DeMaria A, Platt R. Using
automated medical records for rapid identification of illness syndromes
(syndromic surveillance): The example of lower respiratory infection.
*BMC Public Health* 2001; **1**: 9. Epub 2001 Oct 22.

60 Masterson JM, Soodana-Prakash N, Patel AS, Kargi AY, Ramasamy R.
Elevated body mass index is associated with secondary hypogonadism among
men presenting to a tertiary academic medical center. *World J Mens
Health* 2019; **37**: 93–98. doi: 10.5534/wjmh.180047. Epub 2018 Oct 10.

61 Ostbye T, Taylor DH Jr., Clipp EC, Scoyoc LV, Plassman BL.
Identification of dementia: Agreement among national survey data,
medicare claims, and death certificates. *Health Serv Res* 2008; **43**:
313–26. doi: 10.1111/j.1475–6773.2007.00748.x.

62 Sibbett RA, Russ TC, Deary IJ, Starr JM. Dementia ascertainment using
existing data in UK longitudinal and cohort studies: A systematic review
of methodology. *BMC Psychiatry* 2017; **17**: 239. doi:
10.1186/s12888–017–1401–4.

63 Amra S, O’Horo JC, Singh TD, Wilson GA, Kashyap R, Petersen R *et
al.* Derivation and validation of the automated search algorithms to
identify cognitive impairment and dementia in electronic health records.
*J Crit Care* 2017; **37:202-205.**: 10.1016/j.jcrc.2016.09.026. Epub
2016 Oct 8.

64 Jaakkimainen RL, Bronskill SE, Tierney MC, Herrmann N, Green D, Young
J *et al.* Identification of Physician-Diagnosed alzheimer’s disease and
related dementias in Population-Based administrative data: A validation
study using family physicians’ electronic medical records. *J Alzheimers
Dis* 2016; **54**: 337–49. doi: 10.3233/JAD–160105.

65 Kosteniuk JG, Morgan DG, O’Connell ME, Kirk A, Crossley M, Teare GF
*et al.* Incidence and prevalence of dementia in linked administrative
health data in saskatchewan, canada: A retrospective cohort study. *BMC
Geriatr* 2015; **15:73.**: 10.1186/s12877–015–0075–3.

66 McGuinness LA, Warren-Gash C, Moorhouse LR, Thomas SL. The validity
of dementia diagnoses in routinely collected electronic health records
in the united kingdom: A systematic review. *Pharmacoepidemiol Drug Saf*
2019; **28**: 244–255. doi: 10.1002/pds.4669. Epub 2019 Jan 22.

67 Williamson T, Green ME, Birtwhistle R, Khan S, Garies S, Wong ST *et
al.* Validating the 8 CPCSSN case definitions for chronic disease
surveillance in a primary care database of electronic health records.
*Ann Fam Med* 2014; **12**: 367–72. doi: 10.1370/afm.1644.

68 Lin PJ, Kaufer DI, Maciejewski ML, Ganguly R, Paul JE, Biddle AK. An
examination of alzheimer’s disease case definitions using medicare
claims and survey data. *Alzheimers Dement* 2010; **6**: 334–41. doi:
10.1016/j.jalz.2009.09.001.

69 Alaghehbandan R, Macdonald D, Barrett B, Collins K, Chen Y. Using
administrative databases in the surveillance of depressive
disorders–case definitions. *Popul Health Manag* 2012; **15**: 372–80.
doi: 10.1089/pop.2011.0084. Epub 2012 Jul 12.

70 Cepeda MS, Reps J, Fife D, Blacketer C, Stang P, Ryan P. Finding
treatment-resistant depression in real-world data: How a data-driven
approach compares with expert-based heuristics. *Depress Anxiety* 2018;
**35**: 220–228. doi: 10.1002/da.22705. Epub 2017 Dec 15.

71 Davidson AJ, Xu S, Oronce CIA, Durfee MJ, McCormick EV, Steiner JF
*et al.* Monitoring depression rates in an urban community: Use of
electronic health records. *J Public Health Manag Pract* 2018; **24**:
E6–E14. doi: 10.1097/PHH.0000000000000751.

72 Doktorchik C, Patten S, Eastwood C, Peng M, Chen G, Beck CA *et al.*
Validation of a case definition for depression in administrative data
against primary chart data as a reference standard. *BMC Psychiatry*
2019; **19**: 9. doi: 10.1186/s12888–018–1990–6.

73 Buono JL, Mathur K, Averitt AJ, Andrae DA. Economic burden of
irritable bowel syndrome with diarrhea: Retrospective analysis of a U.S.
commercially insured population. *J Manag Care Spec Pharm* 2017; **23**:
453–460. doi: 10.18553/jmcp.2016.16138. Epub 2016 Nov 21.

74 Krishnarajah G, Duh MS, Korves C, Demissie K. Public health impact of
complete and incomplete rotavirus vaccination among commercially and
medicaid insured children in the united states. *PLoS One* 2016; **11**:
e0145977. doi: 10.1371/journal.pone.0145977. eCollection 2016.

75 Panozzo CA, Becker-Dreps S, Pate V, Weber DJ, Jonsson Funk M, Sturmer
T *et al.* Direct, indirect, total, and overall effectiveness of the
rotavirus vaccines for the prevention of gastroenteritis
hospitalizations in privately insured US children, 2007-2010. *Am J
Epidemiol* 2014; **179**: 895–909. doi: 10.1093/aje/kwu001. Epub 2014
Feb 26.

76 Shen AK, Kelman JA, Warnock R, Zhang W, Brereton S, McKean S *et al.*
Beneficiary characteristics and vaccinations in the end-stage renal
disease medicare beneficiary population, an analysis of claims data
2006-2015. *Vaccine* 2017; **35**: 7302–7308. doi:
10.1016/j.vaccine.2017.10.105. Epub 2017 Nov 10.

77 Homer ML, Palmer NP, Fox KP, Armstrong J, Mandl KD. Predicting falls
in people aged 65 years and older from insurance claims. *Am J Med*
2017; **130**: 744.e17–744.e23. doi: 10.1016/j.amjmed.2017.01.003. Epub
2017 Jan 20.

78 Kim DH, Schneeweiss S, Glynn RJ, Lipsitz LA, Rockwood K, Avorn J.
Measuring frailty in medicare data: Development and validation of a
Claims-Based frailty index. *J Gerontol A Biol Sci Med Sci* 2018;
**73**: 980–987. doi: 10.1093/gerona/glx229.

79 McCoy TH Jr., Castro VM, Cagan A, Roberson AM, Perlis RH. Validation
of a risk stratification tool for fall-related injury in a state-wide
cohort. *BMJ Open* 2017; **7**: e012189. doi:
10.1136/bmjopen–2016–012189.

80 Curtis JR, Chen SY, Werther W, John A, Johnson DA. Validation of
ICD-9-CM codes to identify gastrointestinal perforation events in
administrative claims data among hospitalized rheumatoid arthritis
patients. *Pharmacoepidemiol Drug Saf* 2011; **20**: 1150–8. doi:
10.1002/pds.2215. Epub 2011 Aug 27.

81 Lanza LL, Walker AM, Bortnichak EA, Dreyer NA. Peptic ulcer and
gastrointestinal hemorrhage associated with nonsteroidal
anti-inflammatory drug use in patients younger than 65 years. a large
health maintenance organization cohort study. *Arch Intern Med* 1995;
**155**: 1371–7.

82 Lin KJ, Garcia Rodriguez LA, Hernandez-Diaz S. Systematic review of
peptic ulcer disease incidence rates: Do studies without validation
provide reliable estimates? *Pharmacoepidemiol Drug Saf* 2011; **20**:
718–28. doi: 10.1002/pds.2153. Epub 2011 May 27.

83 Patorno E, Gagne JJ, Lu CY, Haynes K, Sterrett AT, Roy J *et al.* The
role of hemoglobin laboratory test results for the detection of upper
gastrointestinal bleeding outcomes resulting from the use of medications
in observational studies. *Drug Saf* 2017; **40**: 91–100. doi:
10.1007/s40264–016–0472–3.

84 Valkhoff VE, Coloma PM, Masclee GM, Gini R, Innocenti F, Lapi F *et
al.* Validation study in four health-care databases: Upper
gastrointestinal bleeding misclassification affects precision but not
magnitude of drug-related upper gastrointestinal bleeding risk. *J Clin
Epidemiol* 2014; **67**: 921–31. doi: 10.1016/j.jclinepi.2014.02.020.
Epub 2014 May 1.

85 Cadzow M, Merriman TR, Dalbeth N. Performance of gout definitions for
genetic epidemiological studies: Analysis of UK biobank. *Arthritis Res
Ther* 2017; **19**: 181. doi: 10.1186/s13075–017–1390–1.

86 Harrold LR, Saag KG, Yood RA, Mikuls TR, Andrade SE, Fouayzi H *et
al.* Validity of gout diagnoses in administrative data. *Arthritis
Rheum* 2007; **57**: 103–8. doi: 10.1002/art.22474.

87 MacFarlane LA, Liu CC, Solomon DH, Kim SC. Validation of claims-based
algorithms for gout flares. *Pharmacoepidemiol Drug Saf* 2016; **25**:
820–6. doi: 10.1002/pds.4044. Epub 2016 May 27.

88 Malik A, Dinnella JE, Kwoh CK, Schumacher HR. Poor validation of
medical record ICD-9 diagnoses of gout in a veterans affairs database.
*J Rheumatol* 2009; **36**: 1283–6. doi: 10.3899/jrheum.081195. Epub
2009 May 15.

89 Hurwitz EL, Vassilaki M, Li D, Schneider MJ, Stevans JM, Phillips RB
*et al.* Variations in patterns of utilization and charges for the care
of headache in north carolina, 2000-2009: A statewide claims’ data
analysis. *J Manipulative Physiol Ther* 2016; **39**: 229–39. doi:
10.1016/j.jmpt.2016.02.008.

90 Rizzoli P, Loder E, Joshi S. Validity of cluster headache diagnoses
in an electronic health record data repository. *Headache* 2016; **56**:
1132–6. doi: 10.1111/head.12850. Epub 2016 Jun 6.

91 Floyd JS, Wellman R, Fuller S, Bansal N, Psaty BM, Boer IH de *et
al.* Use of electronic health data to estimate heart failure events in a
Population-Based cohort with CKD. *Clin J Am Soc Nephrol* 2016; **11**:
1954–1961. doi: 10.2215/CJN.03900416. Epub 2016 Aug 9.

92 Gini R, Schuemie MJ, Mazzaglia G, Lapi F, Francesconi P, Pasqua A *et
al.* Automatic identification of type 2 diabetes, hypertension,
ischaemic heart disease, heart failure and their levels of severity from
italian general practitioners’ electronic medical records: A validation
study. *BMJ Open* 2016; **6**: e012413. doi:
10.1136/bmjopen–2016–012413.

93 Kaspar M, Fette G, Guder G, Seidlmayer L, Ertl M, Dietrich G *et al.*
Underestimated prevalence of heart failure in hospital inpatients: A
comparison of ICD codes and discharge letter information. *Clin Res
Cardiol* 2018; **107**: 778–787. doi: 10.1007/s00392–018–1245–z.Epub
2018 Apr 17.

94 Li Q, Glynn RJ, Dreyer NA, Liu J, Mogun H, Setoguchi S. Validity of
claims-based definitions of left ventricular systolic dysfunction in
medicare patients. *Pharmacoepidemiol Drug Saf* 2011; **20**: 700–8.
doi: 10.1002/pds.2146. Epub 2011 May 14.

95 Patel YR, Robbins JM, Kurgansky KE, Imran T, Orkaby AR, McLean RR *et
al.* Development and validation of a heart failure with preserved
ejection fraction cohort using electronic medical records. *BMC
Cardiovasc Disord* 2018; **18**: 128. doi: 10.1186/s12872–018–0866–5.

96 Saczynski JS, Andrade SE, Harrold LR, Tjia J, Cutrona SL, Dodd KS *et
al.* A systematic review of validated methods for identifying heart
failure using administrative data. *Pharmacoepidemiol Drug Saf* 2012;
**21**: 129–40. doi: 10.1002/pds.2313.

97 Schultz SE, Rothwell DM, Chen Z, Tu K. Identifying cases of
congestive heart failure from administrative data: A validation study
using primary care patient records. *Chronic Dis Inj Can* 2013; **33**:
160–6.

98 Feder SL, Redeker NS, Jeon S, Schulman-Green D, Womack JA, Tate JP
*et al.* Validation of the ICD-9 diagnostic code for palliative care in
patients hospitalized with heart failure within the veterans health
administration. *Am J Hosp Palliat Care* 2018; **35**: 959–965. doi:
10.1177/1049909117747519. Epub 2017 Dec 18.

99 Rosenman M, He J, Martin J, Nutakki K, Eckert G, Lane K *et al.*
Database queries for hospitalizations for acute congestive heart
failure: Flexible methods and validation based on set theory. *J Am Med
Inform Assoc* 2014; **21**: 345–52. doi:
10.1136/amiajnl–2013–001942.Epub 2013 Oct 10.

100 Quach S, Blais C, Quan H. Administrative data have high variation in
validity for recording heart failure. *Can J Cardiol* 2010; **26**:
306–12.

101 Andrade SE, Harrold LR, Tjia J, Cutrona SL, Saczynski JS, Dodd KS
*et al.* A systematic review of validated methods for identifying
cerebrovascular accident or transient ischemic attack using
administrative data. *Pharmacoepidemiol Drug Saf* 2012; **21**: 100–28.
doi: 10.1002/pds.2312.

102 Park TH, Choi JC. Validation of stroke and thrombolytic therapy in
korean national health insurance claim data. *J Clin Neurol* 2016;
**12**: 42–8. doi: 10.3988/jcn.2016.12.1.42. Epub 2015 Sep 11.

103 Gon Y, Kabata D, Yamamoto K, Shintani A, Todo K, Mochizuki H *et
al.* Validation of an algorithm that determines stroke diagnostic code
accuracy in a japanese hospital-based cancer registry using electronic
medical records. *BMC Med Inform Decis Mak* 2017; **17**: 157. doi:
10.1186/s12911–017–0554–x.

104 Sung SF, Hsieh CY, Lin HJ, Chen YW, Yang YH, Li CY. Validation of
algorithms to identify stroke risk factors in patients with acute
ischemic stroke, transient ischemic attack, or intracerebral hemorrhage
in an administrative claims database. *Int J Cardiol* 2016;
**215:277-82.**: 10.1016/j.ijcard.2016.04.069. Epub 2016 Apr 14.

105 Tu K, Wang M, Young J, Green D, Ivers NM, Butt D *et al.* Validity
of administrative data for identifying patients who have had a stroke or
transient ischemic attack using EMRALD as a reference standard. *Can J
Cardiol* 2013; **29**: 1388–94. doi: 10.1016/j.cjca.2013.07.676. Epub
2013 Sep 26.

106 Bui CL, Kaye JA, Castellsague J, Calingaert B, McQuay LJ,
Riera-Guardia N *et al.* Validation of acute liver injury cases in a
population-based cohort study of oral antimicrobial users. *Curr Drug
Saf* 2014; **9**: 23–8.

107 Cheetham TC, Lee J, Hunt CM, Niu F, Reisinger S, Murray R *et al.*
An automated causality assessment algorithm to detect drug-induced liver
injury in electronic medical record data. *Pharmacoepidemiol Drug Saf*
2014; **23**: 601–8. doi: 10.1002/pds.3531. Epub 2013 Oct 21.

108 Jinjuvadia K, Kwan W, Fontana RJ. Searching for a needle in a
haystack: Use of ICD-9-CM codes in drug-induced liver injury. *Am J
Gastroenterol* 2007; **102**: 2437–43. doi:
10.1111/j.1572–0241.2007.01456.x. Epub 2007 Jul 27.

109 Lo Re V 3rd, Carbonari DM, Forde KA, Goldberg D, Lewis JD, Haynes K
*et al.* Validity of diagnostic codes and laboratory tests of liver
dysfunction to identify acute liver failure events. *Pharmacoepidemiol
Drug Saf* 2015; **24**: 676–83. doi: 10.1002/pds.3774. Epub 2015 Apr 10.

110 Lo Re V 3rd, Haynes K, Goldberg D, Forde KA, Carbonari DM, Leidl KB
*et al.* Validity of diagnostic codes to identify cases of severe acute
liver injury in the US food and drug administration’s Mini-Sentinel
distributed database. *Pharmacoepidemiol Drug Saf* 2013; **22**: 861–72.
doi: 10.1002/pds.3470. Epub 2013 Jun 25.

111 Overby CL, Pathak J, Gottesman O, Haerian K, Perotte A, Murphy S *et
al.* A collaborative approach to developing an electronic health record
phenotyping algorithm for drug-induced liver injury. *J Am Med Inform
Assoc* 2013; **20**: e243–52. doi: 10.1136/amiajnl–2013–001930.Epub 2013
Jul 9.

112 Udo R, Maitland-van der Zee AH, Egberts TC, Breeijen JH den,
Leufkens HG, Solinge WW van *et al.* Validity of diagnostic codes and
laboratory measurements to identify patients with idiopathic acute liver
injury in a hospital database. *Pharmacoepidemiol Drug Saf* 2016;
**25**: 21–8. doi: 10.1002/pds.3824. Epub 2015 Jul 5.

113 Wing K, Bhaskaran K, Smeeth L, Staa TP van, Klungel OH, Reynolds RF
*et al.* Optimising case detection within UK electronic health records:
Use of multiple linked databases for detecting liver injury. *BMJ Open*
2016; **6**: e012102. doi: 10.1136/bmjopen–2016–012102.

114 Ryan PB, Buse JB, Schuemie MJ, DeFalco F, Yuan Z, Stang PE *et al.*
Comparative effectiveness of canagliflozin, SGLT2 inhibitors and
non-SGLT2 inhibitors on the risk of hospitalization for heart failure
and amputation in patients with type 2 diabetes mellitus: A real-world
meta-analysis of 4 observational databases (OBSERVE-4D). *Diabetes Obes
Metab* 2018; **20**: 2585–2597. doi: 10.1111/dom.13424. Epub 2018 Jun
25.

115 Voors AA, Ouwerkerk W, Zannad F, Veldhuisen DJ van, Samani NJ,
Ponikowski P *et al.* Development and validation of multivariable models
to predict mortality and hospitalization in patients with heart failure.
*Eur J Heart Fail* 2017; **19**: 627–634. doi: 10.1002/ejhf.785. Epub
2017 Mar 1.

116 Abbas S, Ihle P, Harder S, Schubert I. Risk of hyperkalemia and
combined use of spironolactone and long-term ACE inhibitor/angiotensin
receptor blocker therapy in heart failure using real-life data: A
population- and insurance-based cohort. *Pharmacoepidemiol Drug Saf*
2015; **24**: 406–13. doi: 10.1002/pds.3748. Epub 2015 Feb 12.

117 Betts KA, Woolley JM, Mu F, McDonald E, Tang W, Wu EQ. The
prevalence of hyperkalemia in the united states. *Curr Med Res Opin*
2018; **34**: 971–978. doi: 10.1080/03007995.2018.1433141. Epub 2018 Feb
21.

118 Fitch K, Woolley JM, Engel T, Blumen H. The clinical and economic
burden of hyperkalemia on medicare and commercial payers. *Am Health
Drug Benefits* 2017; **10**: 202–210.

119 Krogager ML, Torp-Pedersen C, Mortensen RN, Kober L, Gislason G,
Sogaard P *et al.* Short-term mortality risk of serum potassium levels
in hypertension: A retrospective analysis of nationwide registry data.
*Eur Heart J* 2017; **38**: 104–112. doi: 10.1093/eurheartj/ehw129.

120 Koulouridis I, Alfayez M, Tighiouart H, Madias NE, Kent DM, Paulus
JK *et al.* Out-of-hospital use of proton pump inhibitors and
hypomagnesemia at hospital admission: A nested case-control study. *Am J
Kidney Dis* 2013; **62**: 730–7. doi: 10.1053/j.ajkd.2013.02.373. Epub
2013 May 10.

121 Markovits N, Loebstein R, Halkin H, Bialik M, Landes-Westerman J,
Lomnicky J *et al.* The association of proton pump inhibitors and
hypomagnesemia in the community setting. *J Clin Pharmacol* 2014;
**54**: 889–95. doi: 10.1002/jcph.316. Epub 2014 May 6.

122 Movig KL, Leufkens HG, Lenderink AW, Egberts AC. Validity of
hospital discharge international classification of diseases (ICD) codes
for identifying patients with hyponatremia. *J Clin Epidemiol* 2003;
**56**: 530–5.

123 Shea AM, Curtis LH, Szczech LA, Schulman KA. Sensitivity of
international classification of diseases codes for hyponatremia among
commercially insured outpatients in the united states. *BMC Nephrol*
2008; **9:5.**: 10.1186/1471–2369–9–5.

124 Chrischilles E, Rubenstein L, Chao J, Kreder KJ, Gilden D, Shah H.
Initiation of nonselective alpha1-antagonist therapy and occurrence of
hypotension-related adverse events among men with benign prostatic
hyperplasia: A retrospective cohort study. *Clin Ther* 2001; **23**:
727–43.

125 Bekelman JE, Mitra N, Efstathiou J, Liao K, Sunderland R, Yeboa DN
*et al.* Outcomes after intensity-modulated versus conformal
radiotherapy in older men with nonmetastatic prostate cancer. *Int J
Radiat Oncol Biol Phys* 2011; **81**: e325–34. doi:
10.1016/j.ijrobp.2011.02.006. Epub 2011 Apr 16.

126 Frederick LR, Cakir OO, Arora H, Helfand BT, McVary KT.
Undertreatment of erectile dysfunction: Claims analysis of 6.2 million
patients. *J Sex Med* 2014; **11**: 2546–53. doi: 10.1111/jsm.12647.
Epub 2014 Jul 24.

127 McVary K, Foley KA, Long SR, Sander S, Curtice TG, Shah H.
Identifying patients with benign prostatic hyperplasia through a
diagnosis of, or treatment for, erectile dysfunction. *Curr Med Res
Opin* 2008; **24**: 775–84. doi: 10.1185/030079908X260916. Epub 2008 Jan
30.

128 Mulhall JP, Luo X, Zou KH, Stecher V, Galaznik A. Relationship
between age and erectile dysfunction diagnosis or treatment using
real-world observational data in the USA. *Int J Clin Pract* 2016;
**70**: 1012–1018. doi: 10.1111/ijcp.12908.

129 Yuan Z, Voss EA, DeFalco FJ, Pan G, Ryan PB, Yannicelli D *et al.*
Risk prediction for ischemic stroke and transient ischemic attack in
patients without atrial fibrillation: A retrospective cohort study. *J
Stroke Cerebrovasc Dis* 2017; **26**: 1721–1731. doi:
10.1016/j.jstrokecerebrovasdis.2017.03.036. Epub 2017 Apr 6.

130 Czwikla J, Jobski K, Schink T. The impact of the lookback period and
definition of confirmatory events on the identification of incident
cancer cases in administrative data. *BMC Med Res Methodol* 2017;
**17**: 122. doi: 10.1186/s12874–017–0407–4.

131 Abraha I, Montedori A, Serraino D, Orso M, Giovannini G, Scotti V
*et al.* Accuracy of administrative databases in detecting primary
breast cancer diagnoses: A systematic review. *BMJ Open* 2018; **8**:
e019264. doi: 10.1136/bmjopen–2017–019264.

132 Abraha I, Serraino D, Montedori A, Fusco M, Giovannini G, Casucci P
*et al.* Sensitivity and specificity of breast cancer ICD-9-CM codes in
three italian administrative healthcare databases: A diagnostic accuracy
study. *BMJ Open* 2018; **8**: e020627. doi:
10.1136/bmjopen–2017–020627.

133 Baldi I, Vicari P, Di Cuonzo D, Zanetti R, Pagano E, Rosato R *et
al.* A high positive predictive value algorithm using hospital
administrative data identified incident cancer cases. *J Clin Epidemiol*
2008; **61**: 373–9. doi: 10.1016/j.jclinepi.2007.05.017. Epub 2007 Oct
22.

134 Cea Soriano L, Soriano-Gabarro M, Garcia Rodriguez LA. Validity and
completeness of colorectal cancer diagnoses in a primary care database
in the united kingdom. *Pharmacoepidemiol Drug Saf* 2016; **25**:
385–91. doi: 10.1002/pds.3877. Epub 2015 Oct 5.

135 Chawla N, Yabroff KR, Mariotto A, McNeel TS, Schrag D, Warren JL.
Limited validity of diagnosis codes in medicare claims for identifying
cancer metastases and inferring stage. *Ann Epidemiol* 2014; **2014
Sep;24**: 666–672.

136 Creighton N, Walton R, Roder D, Aranda S, Currow D. Validation of
administrative hospital data for identifying incident pancreatic and
periampullary cancer cases: A population-based study using linked cancer
registry and administrative hospital data in new south wales, australia.
*BMJ Open* 2016; **6**: e011161. doi: 10.1136/bmjopen–2016–011161.

137 Dregan A, Moller H, Murray-Thomas T, Gulliford MC. Validity of
cancer diagnosis in a primary care database compared with linked cancer
registrations in england. population-based cohort study. *Cancer
Epidemiol* 2012; **36**: 425–9. doi: 10.1016/j.canep.2012.05.013. Epub
2012 Jun 21.

138 Goldsbury D, Weber M, Yap S, Banks E, O’Connell DL, Canfell K.
Identifying incident colorectal and lung cancer cases in health service
utilisation databases in australia: A validation study. *BMC Med Inform
Decis Mak* 2017; **17**: 23. doi: 10.1186/s12911–017–0417–5.

139 Gupta S, Nathan PC, Baxter NN, Lau C, Daly C, Pole JD. Validity of
administrative data in identifying cancer-related events in adolescents
and young adults: A population-based study using the IMPACT cohort. *Med
Care* 2018; **56**: e32–e38. doi: 10.1097/MLR.0000000000000777.

140 Hassett MJ, Ritzwoller DP, Taback N, Carroll N, Cronin AM, Ting GV
*et al.* Validating billing/encounter codes as indicators of lung,
colorectal, breast, and prostate cancer recurrence using 2 large
contemporary cohorts. *Med Care* 2014; **52**: e65–73. doi:
10.1097/MLR.0b013e318277eb6f.

141 Kim SC, Gillet VG, Feldman S, Lii H, Toh S, Brown JS *et al.*
Validation of claims-based algorithms for identification of high-grade
cervical dysplasia and cervical cancer. *Pharmacoepidemiol Drug Saf*
2013; **22**: 1239–44. doi: 10.1002/pds.3520. Epub 2013 Sep 12.

142 Nordstrom BL, Whyte JL, Stolar M, Mercaldi C, Kallich JD.
Identification of metastatic cancer in claims data. *Pharmacoepidemiol
Drug Saf* 2012; **21**: 21–8. doi: 10.1002/pds.3247.

143 Penberthy L, McClish D, Pugh A, Smith W, Manning C, Retchin S. Using
hospital discharge files to enhance cancer surveillance. *Am J
Epidemiol* 2003; **158**: 27–34.

144 Stavrou E, Pesa N, Pearson SA. Hospital discharge diagnostic and
procedure codes for upper gastro-intestinal cancer: How accurate are
they? *BMC Health Serv Res* 2012; **12:331.**: 10.1186/1472–6963–12–331.

145 Goldberg DS, Lewis JD, Halpern SD, Weiner MG, Lo Re V 3rd.
Validation of a coding algorithm to identify patients with
hepatocellular carcinoma in an administrative database.
*Pharmacoepidemiol Drug Saf* 2013; **22**: 103–7. doi: 10.1002/pds.3367.
Epub 2012 Nov 4.

146 Donga PZ, Bilir SP, Little G, Babinchak T, Munakata J. Comparative
treatment-related adverse event cost burden in immune thrombocytopenic
purpura. *J Med Econ* 2017; **20**: 1200–1206. doi:
10.1080/13696998.2017.1370425. Epub 2017 Sep 8.

147 Marrett E, Kwong WJ, Frech F, Qian C. Health care utilization and
costs associated with nausea and vomiting in patients receiving oral
Immediate-Release opioids for outpatient acute pain management. *Pain
Ther* 2016; **5**: 215–226. doi: 10.1007/s40122–016–0057–y.Epub 2016 Oct
4.

148 Kim SY, Solomon DH, Liu J, Chang CL, Daniel GW, Schneeweiss S.
Accuracy of identifying neutropenia diagnoses in outpatient claims data.
*Pharmacoepidemiol Drug Saf* 2011; **20**: 709–13. doi:
10.1002/pds.2157. Epub 2011 May 12.

149 Weycker D, Sofrygin O, Seefeld K, Deeter RG, Legg J, Edelsberg J.
Technical evaluation of methods for identifying chemotherapy-induced
febrile neutropenia in healthcare claims databases. *BMC Health Serv
Res* 2013; **13:60.**: 10.1186/1472–6963–13–60.

150 Schneider G, Kachroo S, Jones N, Crean S, Rotella P, Avetisyan R *et
al.* A systematic review of validated methods for identifying
hypersensitivity reactions other than anaphylaxis (fever, rash, and
lymphadenopathy), using administrative and claims data.
*Pharmacoepidemiol Drug Saf* 2012; **21**: 248–55. doi:
10.1002/pds.2333.

151 Andrade SE, Graham DJ, Staffa JA, Schech SD, Shatin D, La Grenade L
*et al.* Health plan administrative databases can efficiently identify
serious myopathy and rhabdomyolysis. *J Clin Epidemiol* 2005; **58**:
171–4. doi: 10.1016/j.jclinepi.2004.10.004.

152 Chan SL, Tham MY, Tan SH, Loke C, Foo B, Fan Y *et al.* Development
and validation of algorithms for the detection of statin myopathy
signals from electronic medical records. *Clin Pharmacol Ther* 2017;
**101**: 667–674. doi: 10.1002/cpt.526. Epub 2017 Jan 21.

153 Wahl PM, Terrell DR, George JN, Rodgers JK, Uhl L, Cataland S *et
al.* Validation of claims-based diagnostic codes for idiopathic
thrombotic thrombocytopenic purpura in a commercially-insured
population. *Thromb Haemost* 2010; **103**: 1203–9. doi:
10.1160/TH09–08–0595.Epub 2010 Mar 29.

154 Moulis G, Germain J, Adoue D, Beyne-Rauzy O, Derumeaux H, Sailler L
*et al.* Validation of immune thrombocytopenia diagnosis code in the
french hospital electronic database. *Eur J Intern Med* 2016;
**32:e21-2.**: 10.1016/j.ejim.2016.02.021. Epub 2016 Mar 21.

155 Khokhar B, Jette N, Metcalfe A, Cunningham CT, Quan H, Kaplan GG *et
al.* Systematic review of validated case definitions for diabetes in
ICD-9-coded and ICD-10-coded data in adult populations. *BMJ Open* 2016;
**6**: e009952. doi: 10.1136/bmjopen–2015–009952.

156 Leong A, Dasgupta K, Bernatsky S, Lacaille D, Avina-Zubieta A, Rahme
E. Systematic review and meta-analysis of validation studies on a
diabetes case definition from health administrative records. *PLoS One*
2013; **8**: e75256. doi: 10.1371/journal.pone.0075256. eCollection
2013.

157 Chen G, Khan N, Walker R, Quan H. Validating ICD coding algorithms
for diabetes mellitus from administrative data. *Diabetes Res Clin
Pract* 2010; **89**: 189–95. doi: 10.1016/j.diabres.2010.03.007. Epub
2010 Apr 2.

158 Saver BG, Dobie SA, Green PK, Wang CY, Baldwin LM. No pain, but no
gain? The disappearance of angina hospitalizations, 1992-1999. *Med
Care* 2009; **47**: 1106–10. doi: 10.1097/MLR.0b013e31819e1f53.

159 Varas-Lorenzo C, Castellsague J, Stang MR, Tomas L, Aguado J,
Perez-Gutthann S. Positive predictive value of ICD-9 codes 410 and 411
in the identification of cases of acute coronary syndromes in the
saskatchewan hospital automated database. *Pharmacoepidemiol Drug Saf*
2008; **17**: 842–52. doi: 10.1002/pds.1619.

160 Thorpe CT, Thorpe JM, Jiang T, Atkinson D, Kang Y, Schleiden LJ *et
al.* Healthcare utilization and expenditures for united states medicare
beneficiaries with systemic vasculitis. *Semin Arthritis Rheum* 2018;
**47**: 507–519. doi: 10.1016/j.semarthrit.2017.08.005. Epub 2017 Aug
10.

161 England BR, Mikuls TR, Xie F, Yang S, Chen L, Curtis JR. Herpes
zoster as a risk factor for incident giant cell arteritis. *Arthritis
Rheumatol* 2017; **69**: 2351–2358. doi: 10.1002/art.40236. Epub 2017
Nov 9.

162 Tamariz L, Harkins T, Nair V. A systematic review of validated
methods for identifying venous thromboembolism using administrative and
claims data. *Pharmacoepidemiol Drug Saf* 2012; **21**: 154–62. doi:
10.1002/pds.2341.

163 Burwen DR, Wu C, Cirillo D, Rossouw JE, Margolis KL, Limacher M *et
al.* Venous thromboembolism incidence, recurrence, and mortality based
on women’s health initiative data and medicare claims. *Thromb Res*
2017; **150:78-85.**: 10.1016/j.thromres.2016.11.015. Epub 2016 Nov 15.

164 Coleman CI, Peacock WF, Fermann GJ, Crivera C, Weeda ER, Hull M *et
al.* External validation of a multivariable claims-based rule for
predicting in-hospital mortality and 30-day post-pulmonary embolism
complications. *BMC Health Serv Res* 2016; **16**: 610. doi:
10.1186/s12913–016–1855–y.

165 Ammann EM, Cuker A, Carnahan RM, Perepu US, Winiecki SK, Schweizer
ML *et al.* Chart validation of inpatient international classification
of diseases, ninth revision, clinical modification (ICD-9-CM)
administrative diagnosis codes for venous thromboembolism (VTE) among
intravenous immune globulin (IGIV) users in the sentinel distributed
database. *Medicine* 2018; **97**: e9960. doi:
10.1097/MD.0000000000009960.

166 Koo M, Chen JC, Hwang JH. Risk of peripheral artery occlusive
disease in patients with vertigo, tinnitus, or sudden deafness: A
secondary Case-Control analysis of a nationwide, Population-Based health
claims database. *PLoS One* 2016; **11**: e0162629. doi:
10.1371/journal.pone.0162629. eCollection 2016.
