SRCS = $(shell find . -path ./elm-stuff -prune -o -type f -name "*.elm" -print)

TARGET=public/main.js

ELM=elm-make --yes

$(TARGET): $(SRCS)
		$(ELM) Main.elm --warn --output=$@

.PHONY: clean
clean:
	rm -f $(TARGET)

