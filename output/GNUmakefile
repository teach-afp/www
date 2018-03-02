LAYOUT = $(shell find ../lout -type f -name '*')
URL_COURSE = remote11.chalmers.se:/chalmers/groups/edu2009/www/www.cse.chalmers.se/year/2018/course/TDA342_Advanced_Functional_Programming
INFORMATION = inf.html
LABS = labs.html lab1.html lab2.html lab3.html
LECTURES = lec.html lecture1.html lecture2.html lecture3.html lecture4.html lecture5.html lecture6.html lecture8.html lecture9.html lecture10.html lecture13.html lecture15.html
HTML = index.html $(INFORMATION) $(LABS) $(LECTURES) exams.html

.PHONY: html all upload upload_html information labs lectures clean

html all: $(HTML)

upload: html
	 rsync -rv assets $(HTML) $(URL_COURSE)

upload_html upload_lectures: html
	 rsync -rv $(HTML) $(URL_COURSE)

index.html: ../input/index.md $(LAYOUT)
	 generate-md --input ../input/index.md --output ./ --layout ../lout/

# Information of the course

information: $(INFORMATION)

inf.html: ../input/inf.md $(LAYOUT)
	 generate-md --input ../input/inf.md --output ./ --layout ../lout/

# Exams
exams.html: ../input/exams.md $(LAYOUT)
	 generate-md --input ../input/exams.md --output ./ --layout ../lout/

# Labs
labs: $(LABS)

labs.html: ../input/labs.md $(LAYOUT)
	 generate-md --input ../input/labs.md --output ./ --layout ../lout/

lab1.html: ../input/lab1.md $(LAYOUT)
	 ./pretty.sh ../input/lab1.md ../input/lab1P.md
	 generate-md --input ../input/lab1P.md --output ./ --layout ../lout/
	 mv lab1P.html lab1.html
	 rm ../input/lab1P.md


lab2.html: ../input/lab2.md $(LAYOUT)
	 ./pretty.sh ../input/lab2.md ../input/lab2P.md
	 generate-md --input ../input/lab2P.md --output ./ --layout ../lout/
	 mv lab2P.html lab2.html
	 rm ../input/lab2P.md

lab3.html: ../input/lab3.md $(LAYOUT)
	 generate-md --input ../input/lab3.md --output ./ --layout ../lout/


# Lectures
lectures: $(LECTURES)

lec.html: ../input/lec.md $(LAYOUT)
	 generate-md --input ../input/lec.md --output ./ --layout ../lout/

lecture1.html: ../input/lecture1.md $(LAYOUT)
	 ./pretty.sh ../input/lecture1.md ../input/lecture1P.md
	 generate-md --input ../input/lecture1P.md --output ./ --layout ../lout/
	 mv lecture1P.html lecture1.html
	 rm ../input/lecture1P.md

lecture2.html: ../input/lecture2.md $(LAYOUT)
	 ./pretty.sh ../input/lecture2.md ../input/lecture2P.md
	 generate-md --input ../input/lecture2P.md --output ./ --layout ../lout/
	 mv lecture2P.html lecture2.html
	 rm ../input/lecture2P.md


lecture3.html: ../input/lecture3.md $(LAYOUT)
	 ./pretty.sh ../input/lecture3.md ../input/lecture3P.md
	 generate-md --input ../input/lecture3P.md --output ./ --layout ../lout/
	 mv lecture3P.html lecture3.html
	 rm ../input/lecture3P.md

lecture4.html: ../input/lecture4.md $(LAYOUT)
	 ./pretty.sh ../input/lecture4.md ../input/lecture4P.md
	 generate-md --input ../input/lecture4P.md --output ./ --layout ../lout/
	 mv lecture4P.html lecture4.html
	 rm ../input/lecture4P.md

lecture5.html: ../input/lecture5.md $(LAYOUT)
	 ./pretty.sh ../input/lecture5.md ../input/lecture5P.md
	 generate-md --input ../input/lecture5P.md --output ./ --layout ../lout/
	 mv lecture5P.html lecture5.html
	 rm ../input/lecture5P.md

lecture6.html: ../input/lecture6.md ./make_menu.sh ./pretty.sh $(LAYOUT)
	 ./pretty.sh ../input/lecture6.md ../input/lecture6P.md
	 generate-md --input ../input/lecture6P.md --output ./ --layout ../lout/
	 mv lecture6P.html lecture6.html
	 rm ../input/lecture6P.md

lecture8.html: ../input/lecture8.md ./make_menu.sh ./pretty.sh $(LAYOUT)
	 ./pretty.sh ../input/lecture8.md ../input/lecture8P.md
	 generate-md --input ../input/lecture8P.md --output ./ --layout ../lout/
	 mv lecture8P.html lecture8.html
	 rm ../input/lecture8P.md

lecture9.html: ../input/lecture9.md ./make_menu.sh ./pretty.sh $(LAYOUT)
	 ./pretty.sh ../input/lecture9.md ../input/lecture9P.md
	 generate-md --input ../input/lecture9P.md --output ./ --layout ../lout/
	 mv lecture9P.html lecture9.html
	 rm ../input/lecture9P.md

lecture10.html: ../input/lecture10.md ./make_menu.sh ./pretty.sh $(LAYOUT)
	 ./pretty.sh ../input/lecture10.md ../input/lecture10P.md
	 generate-md --input ../input/lecture10P.md --output ./ --layout ../lout/
	 mv lecture10P.html lecture10.html
	 rm ../input/lecture10P.md

lecture13.html: ../input/lecture13.md ./make_menu.sh ./pretty.sh $(LAYOUT)
	 ./pretty.sh ../input/lecture13.md ../input/lecture13P.md
	 generate-md --input ../input/lecture13P.md --output ./ --layout ../lout/
	 mv lecture13P.html lecture13.html
	 rm ../input/lecture13P.md

lecture15.html: ../input/lecture15.md ./make_menu.sh ./pretty.sh $(LAYOUT)
	 ./pretty.sh ../input/lecture15.md ../input/lecture15P.md
	 generate-md --input ../input/lecture15P.md --output ./ --layout ../lout/
	 mv lecture15P.html lecture15.html
	 rm ../input/lecture15P.md


clean:
	rm -f $(HTML)
	rm -f -r assets/
