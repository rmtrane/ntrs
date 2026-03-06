# Demo Data

A completely made up demo data set meant to mimic the structure of the
NACC data. While IDs look like valid NACC IDs, these are simply the
characters "NACC" with six random digits. Similarly, birth and visit
dates are randomly created, and so are all scores with the only
constraint that -4's are believable. (For example, if OTRAILA is -4, so
is OTRAILB.)

## Usage

``` r
demo_data
```

## Format

### `demo_data`

The data set contains the columns listed below. Each column name is
accompanied with its short description from the `rdd` object. For more
info on any of the variables, see `rdd[[col]]` (replace `col` with
column name).

- NACCID:

  Subject ID number

- NACCAGE:

  Subject's age at visit

- SEX:

  Subject's sex

- EDUC:

  Years of education

- BIRTHYR:

  Subject's year of birth

- BIRTHMO:

  Subject's month of birth

- VISITYR:

  Form date - year

- VISITMO:

  Form date - month

- VISITDAY:

  Form date - day

- RACE:

  Race

- CDRGLOB:

  Global CDR

- MOCATOTS:

  MoCA Total Raw Score - uncorrected

- MOCBTOTS:

  MoCA-Blind Total Raw Score � uncorrected

- TRAILA:

  Trail Making Test Part A - Total number of seconds to complete

- TRAILARR:

  Part A - Number of commission errors

- TRAILALI:

  Part A - Number of correct lines

- OTRAILA:

  Oral Trail Making Test Part A � Total number of seconds to complete

- OTRLARR:

  Oral Trail Making Test Part A � Number of commission errors

- DIGFORCT:

  Number Span Test: Forward - Number of correct trials

- DIGFORSL:

  Number Span Test: Forward - Longest span forward

- DIGBACCT:

  Number Span Test: Backward - Number of correct trials

- DIGBACLS:

  Number Span Test: Backward - Longest span backward

- WAIS:

  WAIS-R Digit Symbol

- MINTTOTS:

  Multilingual Naming Test (MINT) - Total score

- ANIMALS:

  Animals - Total number of animals named in 60 seconds

- VEG:

  Vegetables - Total number of vegetables named in 60 seconds

- UDSVERTN:

  Total number of correct F-words and L-words

- UDSVERFC:

  Number of correct F-words generated in 1 minute

- UDSVERLC:

  Number of correct L-words generated in 1 minute

- UDSBENTC:

  Total Score for copy of Benson figure

- UDSBENTD:

  Total score for 10- to 15-minute delayed drawing of Benson figure

- CRAFTVRS:

  Craft Story 21 Recall (Immediate) - Total story units recalled,
  verbatim scoring

- CRAFTURS:

  Craft Story 21 Recall (Immediate) - Total story units recalled,
  paraphrase scoring

- CRAFTDVR:

  Craft Story 21 Recall (Delayed) - Total story units recalled, verbatim
  scoring

- CRAFTDRE:

  Craft Story 21 Recall (Delayed) - Total story units recalled,
  paraphrase scoring

- REY1REC:

  Rey Auditory Verbal Learning (Immediate) Trial 1 Total recall

- REY2REC:

  Rey Auditory Verbal Learning (Immediate) Trial 2 Total recall

- REY3REC:

  Rey Auditory Verbal Learning (Immediate) Trial 3 Total recall

- REY4REC:

  Rey Auditory Verbal Learning (Immediate) Trial 4 Total recall

- REY5REC:

  Rey Auditory Verbal Learning (Immediate) Trial 5 Total recall

- REY6REC:

  Rey Auditory Verbal Learning (Immediate) Trial 6 Total recall

- REYDREC:

  Rey Auditory Verbal Learning (Delayed) - Total Recall

- REYTCOR:

  Rey Auditory Verbal Learning (Delayed) Recognition � Total correct

- TRAILB:

  Trail Making Test Part B - Total number of seconds to complete

- TRAILBLI:

  Part B - Number of correct lines

- TRAILBRR:

  Part B - Number of commission errors

- MOCACLOC:

  MoCA: Visuospatial/executive - Clock contour

- MOCACLOH:

  MoCA: Visuospatial/executive - Clock hands

- MOCACLON:

  MoCA: Visuospatial/executive - Clock numbers

- OTRAILB:

  Oral Trail Making Test Part B � Total number of seconds to complete

- OTRLBRR:

  Oral Trail Making Test Part B � Number of commission errors

- OTRLBLI:

  Oral Trail Making Test Part B � Number of correct lines

- NACCGDS:

  Total GDS Score

- CDRSUM:

  Standard CDR sum of boxes

- UDSBENRS:

  Benson Complex Figure Recall - Recognized original stimulus among four
  options

- BILLS:

  In the past four weeks, did the subject have any difficulty or need
  help with: Writing checks, paying bills, or balancing a checkbook

- TAXES:

  In the past four weeks, did the subject have any difficulty or need
  help with: Assembling tax records, business affairs, or other papers

- SHOPPING:

  In the past four weeks, did the subject have any difficulty or need
  help with: Shopping alone for clothes, household necessities, or
  groceries

- GAMES:

  In the past four weeks, did the subject have any difficulty or need
  help with: Playing a game of skill such as bridge or chess, working on
  a hobby

- STOVE:

  In the past four weeks, did the subject have any difficulty or need
  help with: Heating water, making a cup of coffee, turning off the
  stove

- MEALPREP:

  In the past four weeks, did the subject have any difficulty or need
  help with: Preparing a balanced meal

- EVENTS:

  In the past four weeks, did the subject have any difficulty or need
  help with: Keeping track of current events

- PAYATTN:

  In the past four weeks, did the subject have any difficulty or need
  help with: Paying attention to and understanding a TV program, book,
  or magazine

- REMDATES:

  In the past four weeks, did the subject have any difficulty or need
  help with: Remembering appointments, family occasions, holidays,
  medications

- TRAVEL:

  In the past four weeks, did the subject have any difficulty or need
  help with: Traveling out of the neighborhood, driving, or arranging to
  take public transportation

- REYFPOS:

  Rey Auditory Verbal Learning (Delayed) Recognition � Total false
  positive

- NACCUDSD:

  Cognitive status at UDS visit

- NACCMMSE:

  Total MMSE score (using D-L-R-O-W)

- BOSTON:

  Boston Naming Test (30) - Total score

- LOGIMEM:

  Total number of story units recalled from this current test
  administration

- MEMUNITS:

  Logical Memory IIA - Delayed - Total number of story units recalled

- MEMTIME:

  Logical Memory IIA - Delayed - Time elapsed since Logical Memory IA -
  Immediate

- DIGIF:

  Digit span forward trials correct

- DIGIFLEN:

  Digit span forward length

- DIGIB:

  Digit span backward trials correct

- DIGIBLEN:

  Digit span backward length

- OTHCOG:

  Presumptive etiologic diagnosis - Other neurological, genetic, or
  infectious condition

- OTHCOGX:

  Presumptive etiologic diagnosis of the cognitive disorder - Other
  neurological, genetic, or infectious condition (specify)

- OTHPSY:

  Presumptive etiologic diagnosis - Other psychiatric disease

- OTHPSYX:

  Presumptive etiologic diagnosis of the cognitive disorder - Other
  psychiatric disease (specify)

- COGOTH:

  Presumptive etiologic diagnosis of the cognitive disorder - Other 1
  (specify)

- COGOTHIF:

  Primary, contributing, or non-contributing cause of cognitive
  impairment - Other 1 (specify)

- COGOTHX:

  Other presumptive etiologic diagnosis of the cognitive disorder 1,
  specify

- COGOTH2:

  Presumptive etiologic diagnosis of the cognitive disorder - Other 2
  (specify)

- COGOTH2F:

  Primary, contributing, or non-contributing cause of cognitive
  impairment - Other 2 (specify)

- COGOTH2X:

  Other presumptive etiologic diagnosis of the cognitive disorder 2,
  specify

- COGOTH3:

  Presumptive etiologic diagnosis of the cognitive disorder - Other 3
  (specify)

- COGOTH3X:

  Other presumptive etiologic diagnosis of the cognitive disorder 3,
  specify

- ALCDEM:

  Presumptive etiologic diagnosis of the cognitive disorder - Cognitive
  impairment due to alcohol abuse

- ALCDEMIF:

  Primary, contributing, or non-contributing cause of cognitive
  impairment - Alcohol abuse

- ANXIET:

  Presumptive etiologic diagnosis - Anxiety

- ANXIETIF:

  Primary, contributing, or non-contributing cause of cognitive
  impairment - Anxiety

- BIPOLDX:

  Presumptive etiologic diagnosis - Bipolar disorder

- BIPOLDIF:

  Primary, contributing, or non-contributing cause of cognitive
  impairment - bipolar disorder

- BRNINJ:

  Presumptive etiologic diagnosis - Traumatic brain injury (TBI)

- BRNINJIF:

  Primary, contributing, or non-contributing cause of cognitive
  impairment - Traumatic brain injury (TBI)

- CORT:

  Presumptive etiologic diagnosis - Corticobasal degeneration (CBD)

- CORTIF:

  Primary, contributing, or non-contributing cause of cognitive
  impairment - Corticobasal degeneration (CBD)

- CVD:

  Presumptive etiologic diagnosis - Vascular brain injury (VBI)

- CVDIF:

  Primary, contributing, or non-contributing cause of cognitive
  impairment - vascular brain injury

- DELIR:

  Presumptive etiologic diagnosis - Delirium

- DELIRIF:

  Primary, contributing, or non-contributing cause of cognitive
  impairment - Delirium

- DEMUN:

  Presumptive etiologic diagnosis of the cognitive disorder -
  Undetermined etiology

- DEMUNIF:

  Primary, contributing, or non-contributing cause of cognitive
  impairment - Undetermined etiology

- DEP:

  Presumptive etiologic diagnosis - Depression

- DEPIF:

  Primary, contributing, or non-contributing cause of cognitive
  impairment - Depression

- DOWNS:

  Presumptive etiologic diagnosis of the cognitive disorder - Down
  syndrome

- DOWNSIF:

  Primary, contributing, or non-contributing cause of cognitive
  impairment - Down syndrome

- DYSILL:

  Presumptive etiologic diagnosis of the cognitive disorder - Cognitive
  impairment due to systemic disease/medical illness

- DYSILLIF:

  Primary, contributing, or non-contributing cause of cognitive
  impairment - systemic disease/medical illness

- EPILEP:

  Presumptive etiologic diagnosis - Epilepsy

- EPILEPIF:

  Primary, contributing, or non-contributing cause of cognitive
  impairment - Epilepsy

- ESSTREM:

  Presumptive etiologic diagnosis - Essential tremor

- ESSTREIF:

  Primary, contributing, or non-contributing cause of cognitive
  impairment - Essential tremor

- FTLDMO:

  Presumptive etiologic diagnosis - FTLD with motor neuron disease (MND)

- FTLDMOIF:

  Primary, contributing, or non-contributing cause of cognitive
  impairment - FTLD with motor neuron disease (MND)

- FTLDNOS:

  Presumptive etiologic diagnosis of the cognitive disorder - FTLD not
  otherwise specified (NOS)

- FTLDNOIF:

  Primary, contributing, or non-contributing cause of cognitive
  impairment - FTLD not otherwise specified (NOS)

- HIV:

  Presumptive etiologic diagnosis - Human immunodeficiency virus (HIV)

- HIVIF:

  Primary, contributing, or non-contributing cause of cognitive
  impairment - HIV

- HUNT:

  Presumptive etiologic diagnosis of the cognitive disorder -
  Huntington's disease

- HUNTIF:

  Primary, contributing, or non-contributing cause of cognitive
  impairment - Huntington's disease

- HYCEPH:

  Presumptive etiologic diagnosis of the cognitive disorder -
  Normal-pressure hydrocephalus (NPH)

- HYCEPHIF:

  Primary, contributing, or non-contributing cause of cognitive
  impairment - Normal-pressure hydrocephalus (NPH)

- IMPSUB:

  Presumptive etiologic diagnosis of the cognitive disorder - Cognitive
  impairment due to other substance abuse

- IMPSUBIF:

  Primary, contributing, or non-contributing cause of cognitive
  impairment - Other substance abuse

- MEDS:

  Presumptive etiologic diagnosis of the cognitive disorder - Cognitive
  impairment due to medications

- MEDSIF:

  Primary, contributing, or non-contributing cause of cognitive
  impairment - medications

- MSA:

  Presumptive etiologic diagnosis - Multiple system atrophy (MSA)

- MSAIF:

  Primary, contributing, or non-contributing cause of cognitive
  impairment - Multiple system atrophy (MSA)

- NACCALZD:

  Presumptive etiologic diagnosis of the cognitive disorder -
  Alzheimer's disease

- NACCALZP:

  Primary, contributing, or non-contributing cause of observed cognitive
  impairment - Alzheimer's disease (AD)

- NACCLBDE:

  Presumptive etiologic diagnosis - Lewy body disease

- NACCLBDP:

  Primary, contributing, or non-contributing cause of cognitive
  impairment - Lewy body disease (LBD)

- NEOP:

  Presumptive etiologic diagnosis - CNS neoplasm

- NEOPIF:

  Primary, contributing, or non-contributing cause of cognitive
  impairment - CNS neoplasm

- OTHCOGIF:

  Primary, contributing, or non-contributing cause of cognitive
  impairment - Other neurological, genetic, or infectious condition

- OTHPSYIF:

  Primary, contributing, or non-contributing cause of cognitive
  impairment - Other psychiatric disease

- POSSAD:

  Presumptive etiologic diagnosis of the cognitive disorder - Possible
  Alzheimer's disease

- POSSADIF:

  Primary, contributing, or non-contributing cause of cognitive
  impairment - Possible Alzheimer's disease

- PPAPH:

  Presumptive etiologic diagnosis of the cognitive disorder - Primary
  progressive aphasia (PPA)

- PPAPHIF:

  Primary, contributing, or non-contributing cause of cognitive
  impairment - primary progressive aphasia (PPA)

- PRION:

  Presumptive etiologic diagnosis of the cognitive disorder - Prion
  disease (CJD, other)

- PRIONIF:

  Primary, contributing, or non-contributing cause of cognitive
  impairment - Prion disease (CJD, other)

- PROBAD:

  Presumptive etiologic diagnosis of the cognitive disorder - Probable
  Alzheimer's disease

- PROBADIF:

  Primary, contributing, or non-contributing cause of cognitive
  impairment - Probable Alzheimer's disease

- PSP:

  Presumptive etiologic diagnosis - primary supranuclear palsy (PSP)

- PSPIF:

  Primary, contributing, or non-contributing cause of cognitive
  impairment - Primary supranuclear palsy (PSP)

- PTSDDX:

  Presumptive etiologic diagnosis - Post-traumatic stress disorder
  (PTSD)

- PTSDDXIF:

  Primary, contributing, or non-contributing cause of cognitive
  impairment - PTSD

- SCHIZOP:

  Presumptive etiologic diagnosis - Schizophrenia or other psychosis

- SCHIZOIF:

  Primary, contributing, or non-contributing cause of cognitive
  impairment - Schizophrenia or other psychosis

- STROKE:

  Presumptive etiologic diagnosis - Stroke

- STROKIF:

  Primary, contributing, or non-contributing cause of cognitive
  impairment - stroke

- VASC:

  Presumptive etiologic diagnosis of the cognitive disorder - Probable
  vascular dementia (NINDS/AIREN criteria)

- VASCIF:

  Primary, contributing, or non-contributing cause of cognitive
  impairment - Probable vascular dementia (NINDS/AIREN criteria)

- VASCPS:

  Presumptive etiologic diagnosis of the cognitive disorder - Possible
  vascular dementia (NINDS/AIREN criteria)

- VASCPSIF:

  Primary, contributing, or non-contributing cause of cognitive
  impairment - possible vascular dementia (NINDS/AIREN criteria)
