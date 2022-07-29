# Operator Execution using `tercenApi`

The `run_operator.R` handles the creation of the necessary objects to install and run an operator using the [tercenApi](https://github.com/tercen/tercenApi), as well as collecting the estimated memory consumption.


#### Memory Estimation

It is not trivial to calculate the exact allocated memory to the operator over the course of execution, thus two methods are presented.

In both cases, memory is polled periodically (in the background) during each execution of the operator. From the resulting time series, memory usage is defined as the maximum minus the minimum allocated memory over this time period.

##### Using `free`

The `track_memory.R` calls the `free` command every 50ms during th execution of the task and reads the `available` column.

##### Using `docker stats`

From within the RStudior docker, it is not possible to use `docker stats` on the Tercen docker. To circumvent that, there is the possibility to share a volume between host and RSTudio docker where a memory tracking file will be saved. Before sourcing `run_operator.R`, start `mem_tracker.sh` in the host. Allocated memory for the `tercen`, `couchdb` and `tercen docker` are added and written to the file every 500ms. 