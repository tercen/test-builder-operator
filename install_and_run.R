library(tercen)
library(tercenApi)


# ============================================================
# STEP 1 - Create the step (possibly with workflow and so on)
# ============================================================
client <- tercen::TercenClient$new()
userId <- "test"
baseRestUri <- client$operatorService$baseRestUri
userSession <- client$userService$connect(userId, "test")

# 1: Create Project
proj <- tercenApi::Project$new()
proj$name <- "r_project"
proj$acl <- userSession$user$acl
proj$acl$owner <- userId
proj$isPublic <- TRUE
proj$isDeleted <- FALSE
client$projectService$create(proj)
# OK up to here...

# Next steps
# Add document
# Create a step for document
# Create a step for operator
# Install/read an operator
# Run the operator
# Collect info about it...


# ??: Add the input file to it

inputProjectDocument <- tercenApi::ProjectDocument$new()

inputProjectDocument$acl <- userSession$user$acl
inputProjectDocument$acl$owner <- userId
inputProjectDocument$projectId <- proj$id
client$projectDocumentService$create(inputProjectDocument)

