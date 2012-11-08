.SUFFIXES : .ml
OCAMLC	= ocamlc
TARGET = hello fact10

all: $(TARGET)

.ml:
	$(OCAMLC) $< -o $@

clean:
	- rm -f *.cmo *.cmi $(TARGET)
