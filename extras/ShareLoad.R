# Copyright 2018 Observational Health Data Sciences and Informatics
#
# This file is part of Legend
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Use this script to share a run between two machines. Can only do this once all data is fetched and
# positive control synthesis is complete.

# Use S3 ---------------------------------------------------------------------------------
analysisFolder <- "r:/legend/ccae/Hypertension"
nParts <- 3
prefix <- "mschuemi/CcaeHypertension/"

# Split summary file
exposureSummary <- read.csv(file.path(analysisFolder, "pairedExposureSummaryFilteredBySize.csv"), stringsAsFactors = FALSE)
write.csv(exposureSummary, file.path(analysisFolder, "pairedExposureSummaryFilteredBySize_Full.csv"), row.names = FALSE)
set.seed(123)
exposureSummary$bin <- sample.int(nParts, nrow(exposureSummary), replace = TRUE)
summaryParts <- split(exposureSummary, exposureSummary$bin)
for (i in 1:nParts) {
    write.csv(summaryParts[[i]], file.path(analysisFolder, sprintf("pairedExposureSummaryFilteredBySize_part%s.csv", summaryParts[[i]]$bin[1])), row.names = FALSE)
}

# Upload required files to S3
remoteParts <- summaryParts
remoteParts[[1]] <- NULL # local part does not need upload
remoteParts <- do.call("rbind", remoteParts)

tempFolder <- tempdir()
oldWd <- setwd(file.path(analysisFolder, "cmOutput"))

alreadyUploaded <- aws.s3::get_bucket_df(bucket =  Sys.getenv("ajitsS3Bucket"),
                                         key = Sys.getenv("ajitsS3Key"),
                                         secret = Sys.getenv("ajitsS3Secret"),
                                         prefix = prefix,
                                         max = Inf)
alreadyUploaded <- alreadyUploaded$Key

# Exposure summaries
for (i in 2:nParts) {
    file <- sprintf("pairedExposureSummaryFilteredBySize_part%s.csv", summaryParts[[i]]$bin[1])
    fileLocal <- file.path(analysisFolder, file)
    fileRemote <- paste0(prefix, file)
    fileSize <- file.info(fileLocal)$size
    aws.s3::put_object(file = fileLocal,
                       object = fileRemote,
                       bucket =  Sys.getenv("ajitsS3Bucket"),
                       headers = list(`x-amz-server-side-encryption` = "AES256"),
                       key = Sys.getenv("ajitsS3Key"),
                       secret = Sys.getenv("ajitsS3Secret"),
                       region = "us-east-1",
                       check_region = FALSE)
}

# signalInjectionSummary.csv
file <- "signalInjectionSummary.csv"
fileLocal <- file.path(analysisFolder, file)
fileRemote <- paste0(prefix, file)
fileSize <- file.info(fileLocal)$size
aws.s3::put_object(file = fileLocal,
                   object = fileRemote,
                   bucket =  Sys.getenv("ajitsS3Bucket"),
                   headers = list(`x-amz-server-side-encryption` = "AES256"),
                   key = Sys.getenv("ajitsS3Key"),
                   secret = Sys.getenv("ajitsS3Secret"),
                   region = "us-east-1",
                   check_region = FALSE)

# CmData objects
uploadCmData <- function(i) {
    folder <- sprintf("CmData_l1_t%s_c%s", remoteParts$targetId[i],remoteParts$comparatorId[i])
    tarFile <- paste0(folder, ".tar")
    tarFileRemote <- paste0(prefix, tarFile)
    if (!(tarFileRemote %in% alreadyUploaded)) {
        tarFileLocal <- file.path(tempFolder, tarFile)
        tar(tarFileLocal, folder)
        aws.s3::put_object(file = tarFileLocal,
                           object = tarFileRemote,
                           bucket =  Sys.getenv("ajitsS3Bucket"),
                           headers = list(`x-amz-server-side-encryption` = "AES256"),
                           key = Sys.getenv("ajitsS3Key"),
                           secret = Sys.getenv("ajitsS3Secret"),
                           region = "us-east-1",
                           check_region = FALSE)
        unlink(tarFileLocal)
    }
    return(NULL)
}

plyr::l_ply(1:nrow(remoteParts), uploadCmData, .progress = "text")

# PS files
uploadPsFile <- function(i) {
    file <- sprintf("Ps_l1_p1_t%s_c%s.rds", remoteParts$targetId[i],remoteParts$comparatorId[i])
    fileRemote <- paste0(prefix, file)
    if (!(fileRemote %in% alreadyUploaded)) {
        fileLocal <- file
        aws.s3::put_object(file = fileLocal,
                           object = fileRemote,
                           bucket =  Sys.getenv("ajitsS3Bucket"),
                           headers = list(`x-amz-server-side-encryption` = "AES256"),
                           key = Sys.getenv("ajitsS3Key"),
                           secret = Sys.getenv("ajitsS3Secret"),
                           region = "us-east-1",
                           check_region = FALSE)
    }
    return(NULL)
}

plyr::l_ply(1:nrow(remoteParts), uploadPsFile, .progress = "text")

# Download ------------------------------------------------------------------------------
analysisFolder <- "d:/legend/ccae/Hypertension"
prefix <- "mschuemi/CcaeHypertension/"
part <- 2


tempFolder <- tempdir()
if (!file.exists(analysisFolder)) {
    dir.create(analysisFolder, recursive = TRUE)
}

# Exposure summary
file <- sprintf("pairedExposureSummaryFilteredBySize_part%s.csv", part)
fileLocal <- file.path(analysisFolder, file)
fileRemote <- paste0(prefix, file)
aws.s3::save_object(file = fileLocal,
                    object = fileRemote,
                    bucket = Sys.getenv("ajitsS3Bucket"),
                    key = Sys.getenv("ajitsS3Key"),
                    secret = Sys.getenv("ajitsS3Secret"),
                    region = "us-east-1",
                    check_region = FALSE)
exposureSummary <- read.csv(fileLocal)

# signalInjectionSummary.csv
file <- "signalInjectionSummary.csv"
fileLocal <- file.path(analysisFolder, file)
fileRemote <- paste0(prefix, file)
aws.s3::save_object(file = fileLocal,
                    object = fileRemote,
                    bucket = Sys.getenv("ajitsS3Bucket"),
                    key = Sys.getenv("ajitsS3Key"),
                    secret = Sys.getenv("ajitsS3Secret"),
                    region = "us-east-1",
                    check_region = FALSE)

# CmData objects
cmOutputFolder <- file.path(analysisFolder, "cmOutput")
if (!file.exists(cmOutputFolder)) {
    dir.create(cmOutputFolder)

}
downloadCmData <- function(i) {
    folder <- sprintf("CmData_l1_t%s_c%s", exposureSummary$targetId[i], exposureSummary$comparatorId[i])
    folderLocal <- file.path(cmOutputFolder, folder)
    tarFileLocal <- file.path(tempFolder, paste0(folder, ".tar"))
    tarFileRemote <- paste0(prefix, paste0(folder, ".tar"))
    if (!file.exists(folderLocal)) {
        aws.s3::save_object(file = tarFileLocal,
                            object = tarFileRemote,
                            bucket = Sys.getenv("ajitsS3Bucket"),
                            key = Sys.getenv("ajitsS3Key"),
                            secret = Sys.getenv("ajitsS3Secret"),
                            region = "us-east-1",
                            check_region = FALSE)
        untar(tarFileLocal, exdir = cmOutputFolder)
        unlink(tarFileLocal)
    }
    return(NULL)
}

plyr::l_ply(1:nrow(exposureSummary), downloadCmData, .progress = "text")

downloadPsFile <- function(i) {
    file <- sprintf("Ps_l1_p1_t%s_c%s.rds", exposureSummary$targetId[i], exposureSummary$comparatorId[i])
    fileLocal <- file.path(cmOutputFolder, file)
    fileRemote <- paste0(prefix, file)
    if (!file.exists(fileLocal)) {
        aws.s3::save_object(file = fileLocal,
                            object = fileRemote,
                            bucket = Sys.getenv("ajitsS3Bucket"),
                            key = Sys.getenv("ajitsS3Key"),
                            secret = Sys.getenv("ajitsS3Secret"),
                            region = "us-east-1",
                            check_region = FALSE)
    }
    return(NULL)
}

plyr::l_ply(1:nrow(exposureSummary), downloadPsFile, .progress = "text")




# Other option: over shared folders------------------------------------------------------


remoteFolder <- "\\\\AWSASJNVA3014/Legend/optum/Hypertension"
localFolder <- "d:/legend/optum/Hypertension"
fraction <- 0.2

if (!file.exists(localFolder)) {
    dir.create(localFolder, recursive = TRUE)
}

# Split exposure summary file --------------------------------------------------------------
exposureSummary <- read.csv(file.path(remoteFolder, "pairedExposureSummaryFilteredBySize.csv"), stringsAsFactors = FALSE)
nrow(exposureSummary)
# Save a copy
write.csv(exposureSummary, file.path(remoteFolder, "pairedExposureSummaryFilteredBySize_Full.csv"), row.names = FALSE)
set.seed(123)
idx <- sample.int(nrow(exposureSummary), round(fraction * nrow(exposureSummary)))
remoteExposureSummary <- exposureSummary[-idx, ]
nrow(remoteExposureSummary)
localExposureSummary <- exposureSummary[idx, ]
nrow(localExposureSummary)
write.csv(remoteExposureSummary, file.path(remoteFolder, "pairedExposureSummaryFilteredBySize_part1.csv"), row.names = FALSE)
write.csv(localExposureSummary, file.path(remoteFolder, "pairedExposureSummaryFilteredBySize_part2.csv"), row.names = FALSE)
write.csv(localExposureSummary, file.path(localFolder, "pairedExposureSummaryFilteredBySize.csv"), row.names = FALSE)
# DANGER:
# write.csv(remoteExposureSummary, file.path(remoteFolder, "pairedExposureSummaryFilteredBySize.csv"), row.names = FALSE)

# Copy files from remote to local ------------------------------------------------------------
remoteCmOutput <- file.path(remoteFolder, "cmOutput")
localCmOutput <- file.path(localFolder, "cmOutput")
localExposureSummary <- read.csv(file.path(localFolder, "pairedExposureSummaryFilteredBySize.csv"))
if (!file.exists(localCmOutput)) {
    dir.create(localCmOutput, recursive = TRUE)
}

copyFilesForTc <- function(i) {
    pattern <- paste0("_t", localExposureSummary$targetId[i], "_c", localExposureSummary$comparatorId[i], "([^0-9]|$)")
    files <- list.files(remoteCmOutput, pattern, include.dirs = TRUE)
    files <- files[!file.exists(files)]
    file.copy(file.path(remoteCmOutput, files), localCmOutput, recursive = TRUE)
    return(NULL)
}

plyr::l_ply(1:nrow(localExposureSummary), copyFilesForTc, .progress = "text")
file.copy(file.path(remoteFolder, "signalInjectionSummary.csv"), file.path(localFolder, "signalInjectionSummary.csv"))

# Todo: add covariate balance data


# Copy finished files back from local to remote -----------------------------------------------


