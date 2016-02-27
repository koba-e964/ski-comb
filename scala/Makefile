.PHONY: clean
clean:
	rm -f *.class
Main:
	fsc *.scala
%.out: %.txt Main
	java Main $< $@


