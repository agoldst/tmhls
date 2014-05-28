essay:
	$(MAKE) -C essay 

preprint:
	$(MAKE) -C essay preprint.pdf

figs := criticism.pdf formalism-waves.pdf numbers.pdf power.pdf recent.pdf t080.pdf theory.pdf
submit_figs :=  $(addprefix essay/figure/,$(figs))
pre_figs := $(addprefix essay/figure-pre/,$(figs))

figure: knit_figs $(submit_figs) $(pre_figs)
	
knit_figs: figures.Rmd
	R -e 'library(knitr); knit("$<")'

$(submit_figs): essay/figure/%.pdf: figure/%.pdf
	mkdir -p essay/figure
	cp -f $< $@

$(pre_figs): essay/figure-pre/%.pdf: figure/%-pre.pdf
	mkdir -p essay/figure-pre
	cp -f $< $@

.PHONY: essay figure knit_figs preprint

.DEFAULT_GOAL := essay
