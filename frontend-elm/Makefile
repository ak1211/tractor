SRCS = $(shell find . -path ./elm-stuff -prune -o -type f -name "*.elm" -print)

OBJECT=public/main.js

TARGET=public/app.js

ELM=elm make

$(TARGET): $(OBJECT)
		cat $< "src/index.js" > $@

$(OBJECT): $(SRCS)
		$(ELM) src/Main.elm --output=$@ --optimize

.PHONY: clean
clean:
	rm -f $(OBJECT)
	rm -f $(TARGET)

