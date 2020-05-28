#  CMPU-365, Fall 2010
#  Makefile
# ------------------------------------------------
#  Makefile for CS 365 assignment using Otter.
#  Should by run by students from inside their own assignment directory.

# turn on silent mode
.SILENT:

# macro definitions

.SUFFIXES:	.in .out

HOME = /home/cs365/
OTTER = /home/cs365/bin/otter

OTTER_KB_SOURCES = kb.in 
QUERY_SOURCES = query1.in query2.in query3.in query4.in query5.in 
SOURCES = $(OTTER_KB_SOURCES) $(QUERY_SOURCES)
OUTPUTS = $(QUERY_SOURCES:.in=.out) 
OTHER_FILES = Makefile 

## How to create an .out file from an .in file:

.in.out:
	echo 'Running $*...'
	-cat $(OTTER_KB_SOURCES) $*.in | $(OTTER) >$*.out
	echo '...done running $*!'

default:	$(OUTPUTS)


# EOF - Makefile for CS 365.
