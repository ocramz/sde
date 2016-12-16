.DEFAULT_GOAL := help

help:
	@echo "Use \`make <target>\` where <target> is one of"
	@echo "  help     to display this help message"
	@echo "  build    to build the library and binaries"
	@echo "  plot     to render the plot as 'plot.png'"

build:
	stack build

plot: plot.png
	stack exec sde-plot -- -o plot.png 200 0.002


all:
	make build
	make -B plot
