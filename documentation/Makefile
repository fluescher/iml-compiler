RM_CMD = rm -f
LATEX_CMD = latex
PDF_LATEX_CMD = pdflatex 
BIBTEX_CMD = bibtex
MKDIR_CMD = mkdir -p

BUILD_DIR = ./build/
OUT_DIR = ./out/

MAIN_FILE = report
LOG_FILE = ${BUILD_DIR}creation.log

all: clean pdf

prepare:
	${MKDIR_CMD} ${BUILD_DIR}
	${MKDIR_CMD} ${OUT_DIR}
	cp template/* ${BUILD_DIR}

latex: prepare
	${PDF_LATEX_CMD} -output-directory=${BUILD_DIR} ${MAIN_FILE}.tex

pdf: bibtex
	${PDF_LATEX_CMD} -output-directory=${BUILD_DIR} ${MAIN_FILE}.tex
	${PDF_LATEX_CMD} -output-directory=${BUILD_DIR} ${MAIN_FILE}.tex
	mv ${BUILD_DIR}${MAIN_FILE}.pdf ${OUT_DIR}

bibtex: latex
	cp *bib ${BUILD_DIR}
	cd ${BUILD_DIR}; ${BIBTEX_CMD} ${MAIN_FILE}.aux; pwd > pwd.txt

clean:
	${RM_CMD} -r ${BUILD_DIR}
	
show: all
	evince ${OUT_DIR}${MAIN_FILE}.pdf &
	
count:
	wc content/* -m
