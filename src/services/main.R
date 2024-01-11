

# main.R
library(plumber)



# Load service1 from the groupAServices directory
source('services/fileServices/readDataEntryExampleService.R')

pr <- plumber::plumber()

# Mount service 1
pr$mount(
    '/api/read-data-entry-example-service', 
    as.router(readDataEntryExampleService_instance)
)

# Add other services as needed...

# Start the Plumber API
pr$run(port = 8000)
