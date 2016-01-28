USERR = russo
LAYOUT = $(shell find ../lout -type f -name '*')
URL_COURSE = $(USERR)@remote11.chalmers.se:/chalmers/groups/edu2009/www/www.cse.chalmers.se/year/2016/course/TDA342_Advanced_Functional_Programming

all: index.html information labs lectures exams.html



upload:
	 #rsync -rv -f"+ */" -f"- *" ./assets $(URL_COURSE)
	 rsync -rv ./ $(URL_COURSE)

upload_lectures:
	 rsync -rv ./*.html $(URL_COURSE)

index.html: ../input/index.md $(LAYOUT)
	 generate-md --input ../input/index.md --output ./ --layout ../lout/

# Information of the course

information: inf.html

inf.html: ../input/inf.md $(LAYOUT)
	 generate-md --input ../input/inf.md --output ./ --layout ../lout/

ex1.html: ../input/ex1.md $(LAYOUT)
	 generate-md --input ../input/ex1.md --output ./ --layout ../lout/

# Exams
exams.html: ../input/exams.md $(LAYOUT)
	 generate-md --input ../input/exams.md --output ./ --layout ../lout/

# Labs
labs: labs.html lab1.html lab2.html lab3.html lab4.html

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

lab4.html: ../input/lab4.md $(LAYOUT)
	 generate-md --input ../input/lab4.md --output ./ --layout ../lout/


# Lectures
lectures: lec.html lecture1.html lecture2.html lecture3.html lecture4.html lecture5.html lecture6.html lecture7.html lecture8.html lecture9.html lecture10.html

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

lecture7.html: ../input/lecture7.md ./make_menu.sh ./pretty.sh $(LAYOUT)
	 ./pretty.sh ../input/lecture7.md ../input/lecture7P.md
	 generate-md --input ../input/lecture7P.md --output ./ --layout ../lout/
	 mv lecture7P.html lecture7.html
	 rm ../input/lecture7P.md

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

clean:
	rm -f *.html
	rm -f -r ./assets/
