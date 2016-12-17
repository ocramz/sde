.DEFAULT_GOAL := help

help:
	@echo "Use \`make <target>\` where <target> is one of"
	@echo "  help     to display this help message"
	@echo "  build    to build the library and binaries"
	@echo "  plot     to render the plot as 'plot.png'"

build:
	stack build

plot: 
	# stack exec sde-plot -- series 1800 1.5 1 -o series.png
	# stack exec sde-plot -- hist 1800 1.5 1 -o hist.png
	stack exec sde-plot -- series 500 0.0042 0.991 0.104 1.885 -o series.png
	stack exec sde-plot -- hist 500 0.0042 0.991 0.104 1.885 -o hist.png

all:
	make build
	make -B plot
