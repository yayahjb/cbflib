message("input: ${input}")
message("output: ${output}")
execute_process(
	COMMAND @HDF5_BINARY_DIR@/h5dump ${input}
	COMMAND @CMAKE_RUNTIME_OUTPUT_DIRECTORY@/cbf_tail -n 1
	OUTPUT_FILE ${output}
	RESULT_VARIABLE result
	ERROR_VARIABLE error
)
if (result)
	message("${error}")
	message(FATAL_ERROR "failed: ${result}")
endif ()
