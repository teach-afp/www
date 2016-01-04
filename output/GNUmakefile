USERR = russo
LAYOUT = $(shell find ../lout -type f -name '*')
URL_COURSE = $(USERR)@remote12.chalmers.se:/chalmers/groups/edu2009/www/www.cse.chalmers.se/year/2015/course/TDA383_LP1/

all: index.html information exercises labs lectures exams.html



upload:
	 rsync -rv -f"+ */" -f"- *" ./assets $(URL_COURSE)
	 rsync -rv --rsh=ssh ./assets/css ./assets/fonts ./assets/img ./assets/js \
	       $(URL_COURSE)/assets
	 rsync -rv ./assets/erl/*.erl  $(URL_COURSE)/assets/erl
	 rsync -rv ./assets/java/*.java  $(URL_COURSE)/assets/java
	 rsync -rv ./assets/gl/*  $(URL_COURSE)/assets/gl
	 rsync -rv ./assets/exams/*  $(URL_COURSE)/assets/exams
	 rsync -rv ./*.html $(URL_COURSE)

upload_lectures:
	 # @read -p "Enter User Name:" module; \
	 # module_dir=./modules/$$module; \
	 # mkdir -p $$module_dir/build \
	 # rsync -avp ./*.html $(URL_COURSE)
	 # echo "Username: ";
	 read -p "Username: " user; \
	 rsync -rvt ./*.html $$user@remote11.chalmers.se:/chalmers/groups/edu2009/www/www.cse.chalmers.se/year/2015/course/TDA383_LP1/


index.html: ../input/index.md $(LAYOUT)
	 generate-md --input ../input/index.md --output ./ --layout ../lout/

# Information of the course

information: course_inf.html lit_inf.html time_inf.html comp_inf.html lang_inf.html

course_inf.html: ../input/course_inf.md $(LAYOUT)
	 generate-md --input ../input/course_inf.md --output ./ --layout ../lout/

lit_inf.html: ../input/lit_inf.md $(LAYOUT)
	 generate-md --input ../input/lit_inf.md --output ./ --layout ../lout/

time_inf.html: ../input/time_inf.md $(LAYOUT)
	 generate-md --input ../input/time_inf.md --output ./ --layout ../lout/

comp_inf.html: ../input/comp_inf.md $(LAYOUT)
	 generate-md --input ../input/comp_inf.md --output ./ --layout ../lout/

lang_inf.html: ../input/lang_inf.md $(LAYOUT)
	 generate-md --input ../input/lang_inf.md --output ./ --layout ../lout/

# Exercises
exercises: exercises.html ex1.html

exercises.html: ../input/exercises.md $(LAYOUT)
	 generate-md --input ../input/exercises.md --output ./ --layout ../lout/

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
	 generate-md --input ../input/lab1.md --output ./ --layout ../lout/

lab2.html: ../input/lab2.md $(LAYOUT)
	 generate-md --input ../input/lab2.md --output ./ --layout ../lout/

lab3.html: ../input/lab3.md $(LAYOUT)
	 generate-md --input ../input/lab3.md --output ./ --layout ../lout/

lab4.html: ../input/lab4.md $(LAYOUT)
	 generate-md --input ../input/lab4.md --output ./ --layout ../lout/


# Lectures
lectures: lec.html lecture1.html lecture2.html lecture3.html lecture4.html lecture5.html lecture6.html lecture7.html lecture8.html lecture9.html lecture10.html

lec.html: ../input/lec.md $(LAYOUT)
	 generate-md --input ../input/lec.md --output ./ --layout ../lout/

lecture1.html: ../input/lecture1.md $(LAYOUT)
	 generate-md --input ../input/lecture1.md --output ./ --layout ../lout/

lecture2.html: ../input/lecture2.md $(LAYOUT)
	 generate-md --input ../input/lecture2.md --output ./ --layout ../lout/

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
