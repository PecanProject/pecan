#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mpi/mpi.h>

/* Actual processing of the file */
void process(int rank, int size, char *filename) {
	FILE *fp;
	char command[1024];
	char line[1024];
	char execute[2048];
	int  lineno;

	/* open configuration file */
	fp=fopen(filename, "r");
	if (fp == NULL) {
		if (rank == 0) {
			printf("File '%s' not found.\n", filename);
		}
		return;
	}

	/* first line is command to execute */
	fgets(command, 1024, fp);
	command[strlen(command) - 1] = '\0';

	/* read configuration file and execute */
	printf("Executing %s of every %dth line starting with line %d of %s\n", command, size, rank+1, filename);
	lineno = 0;
	while(fgets(line, 1024, fp)) {
		if (lineno % size == rank) {
			line[strlen(line) - 1] = '\0';
			if (chdir(line) == 0) {
				sprintf(execute, "%s 2>stderr.txt >stdout.txt", command);
				printf("[%d] cwd=%s exec=%s\n", rank, line, execute);
				int ret = system(execute);
				if (ret != 0) {
					printf("[%d] returned %d as exit status.", rank, ret);
				}
			} else {
				printf("[%d] could not change directory to %s.", rank, line);
			}
		}
		lineno++;
	}

	/* all done */
	fclose(fp);
}

/* Main function */
int main (int argc, char** argv) {
	int rank, size;

	/* starts MPI */
	MPI_Init (&argc, &argv);

	/* get current process id */
	MPI_Comm_rank (MPI_COMM_WORLD, &rank);

	/* get number of processes */
	MPI_Comm_size (MPI_COMM_WORLD, &size);

	/* check right number of arguments */
	if (argc == 2) {
		process(rank, size, argv[1]);
	} else if (rank == 0) {
		printf("Usage %s configfile\n", argv[0]);
	}

	/* All done */
	MPI_Finalize();
	return 0;
}

