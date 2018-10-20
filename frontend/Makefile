SRCS = $(shell find . -path ./elm-stuff -prune -o -type f -name "*.elm" -print)

TARGET=public/main.js

ELM=elm make

$(TARGET): $(SRCS)
		$(ELM) src/Main.elm --output=$@

.PHONY: clean
clean:
	rm -f $(TARGET)

