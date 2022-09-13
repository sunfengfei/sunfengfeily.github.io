source("Code/setup.R")

#Import and process outlet data
source("Code/Import/importTop500.R")
source("Code/Process/processTop500.R")

#Import and process Comscore data
source("Code/Import/importComscore.R")
source("Code/Process/processComscore.R")

# Create Figure 2
source("Code/Analyze/Media/analyzeNewsAnnotation.R")

# Create Figure 3, Table A.7a, Table A.8a, Table A.8b, Table A.9a
source("Code/Analyze/comscoreSegregation.R")

# Create Figure 4, Table A.7b, Table A.9b
source("Code/Analyze/Media/analyzeControlSlant.R")

# Create Figure 5a, Figure 5b
source("Code/Analyze/comscoreDemonstrate.R")

# Create Figure 6, Figure A.2, Figure A.5, Figure A.7, Table A.10
source("Code/Analyze/Media/analyzeMediaBehavior.R")

# Create Figure 7, Figure A.8, Table A.11, Table A.23
source("Code/Analyze/Media/analyzeMediaBehaviorSlant.R")

# Create Figure 8a, Figure 8b, Figure A.9a, Figure A.9b
source("Code/Analyze/Media/analyzeMediaPersistence.R")

# Create Figure 9, Figure A.11, Figure A.20, Table A.14a, Table A.14b, Table A.15a, Table A.15b, Table A.18a, Table A.18b, Table A.24a, Table A.24b
source("Code/Analyze/Surveys/analyzePrimaryOutcomes.R")

# Create Figure 10, Figure A.21, Table A.28
source("Code/Analyze/Media/decomposeExposure.R")

# Create Figure A.3
source("Code/Analyze/Surveys/analyzeOutcomesUserPage.R")

# Create Figure A.4
source("Code/Analyze/Surveys/analyzeOtherOutcomes.R")

# Create Figure A.6
source("Code/Analyze/Media/analyzeFB.R")

# Create Figure A.10, Table A.12a
source("Code/Analyze/Surveys/analyzePersuasion.R")

# Create Figure A.12, Table A.12b, Table A.13, Table A.17, Table A.19
source("Code/Analyze/Surveys/analyzePolarization.R")

# Create Figure A.14, Figure A.15, Figure A.16, Figure A.17, Table A.22a, Table A.22b, Table A.22c
source("Code/Analyze/Media/analyzePostContent.R")

# Create Figure A.18, Figure A.19
source("Code/Analyze/Media/analyzeMediaBehaviorHetero.R")

# Create Table 2, Table A.2, Table A.3, Table A.4, Table A.5, Table A.6
source("Code/Analyze/balanceTables.R")

# Create Table 3, Table A.1
source("Code/Analyze/analyzeCompliance.R")

# Create Table A.16a, Table A.16b
source("Code/Analyze/analyzePolarizationWithMedia.R")

# Create Table A.20a, Table A.20b, Table A.22a, Table A.22b, Table A.22c
source("Code/Analyze/Surveys/analyzeSurveyPurpose.R")

# Create Table A.25
source("Code/Analyze/analyzeHeteroForest.R")

# Create Table A.26
source("Code/Analyze/Surveys/analyzeKnowledge.R")

# Create Table A.27
source("Code/Analyze/Media/analyzeFBContent.R")

