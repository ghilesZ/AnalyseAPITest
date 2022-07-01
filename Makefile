BIN_NAME=main.exe
TARGET=_build/default/src/$(BIN_NAME)

all:
	@dune build
	@cp $(TARGET) .

clean:
	@dune clean
	@rm -f $(BIN_NAME)
