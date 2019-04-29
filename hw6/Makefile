#NAME: Kevin Chuang
#EMAIL: kevinchuang2@gmail.com
#ID: 704769121
clean:
	rm -rf *.tar.gz *.jar
dist: 
	tar -czf hw6.tar.gz everyNth.kt Makefile
check:
	kotlinc everyNth.kt -include-runtime -d everyNth.jar
	java -jar everyNth.jar
	rm everyNth.jar


	
	