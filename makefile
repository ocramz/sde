.DEFAULT_GOAL := help

help:
	@echo "Use \`make <target>\` where <target> is one of"
	@echo "  help     to display this help message"
	@echo "  build    to build the library and binaries"
	@echo "  plot     to render the plot as 'plot.png'"

build:
	stack build

plot: 
	# stack exec sde-plot -- -o series.png series 1000
	# stack exec sde-plot -- -o hist.png hist 1000
	stack exec sde-plot -- series 1000 -o series.png
	stack exec sde-plot -- hist 1000 -o hist.png

all:
	make build
	make -B plot
