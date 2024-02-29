# Produce output/ content from input/ and lout/

LAYOUT = $(shell find ../lout -type f -name '*')

LECTURES = lecture1.html lecture2.html lecture3.html lecture4.html lecture5.html lecture6.html lecture7.html lecture8.html lecture9.html lecture13.html lecture15.html

LABS = $(SIMPLE_LABS) $(PRETTY_LABS)
SIMPLE_LABS = labs.html lab3.html lab3-agda.html
PRETTY_LABS = lab1.html lab2.html exam2020.html exam2020mocked.html

SIMPLE_HTML = index.html inf.html exams.html lec.html $(SIMPLE_LABS)
PRETTY_HTML = $(PRETTY_LABS) $(LECTURES)

HTML = $(SIMPLE_HTML) $(PRETTY_HTML)

.PHONY: html all
html all: $(HTML)

.PHONY: information labs lectures
information: inf.html
labs: $(LABS)
lectures: lec.html $(LECTURES)

# Template for simple conversion from md to html

$(SIMPLE_HTML) : %.html : ../input/%.md $(LAYOUT)
	 generate-md --input $< --output ./ --layout ../lout/

# Template for pretty conversion from md to html (using pretty.sh)

$(PRETTY_HTML) : %.html : ../input/%.md $(LAYOUT)
	 ./pretty.sh ../input/$*.md ../input/$*P.md
	 generate-md --input ../input/$*P.md --output ./ --layout ../lout/
	 mv $*P.html $*.html
	 rm ../input/$*P.md

.PHONY: clean
clean:
	rm -f $(HTML)
	rm -f -r assets/
