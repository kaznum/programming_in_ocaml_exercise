TARGET = ilogic
LABLCMA = labltk.cma
STRCMA = str.cma
INCLUDE = -I +labltk
OBJS = mySupport.cmo board.cmo input.cmo gui.cmo

all: $(DEPEND) $(TARGET)

ilogic : $(OBJS)
	ocamlc $(INCLUDE) -o $(TARGET) $(LABLCMA) $(STRCMA) $(OBJS)

%.cmi : %.mli
	ocamlc -c $(INCLUDE) $<

%.cmo : %.ml
	ocamlc -c $(INCLUDE) $<

clean::
	-rm -rf *.cmi *.cmo $(TARGET) *.cmx *~

depend:: $(DEPEND)
	ocamldep *.mli *.ml > .depend

-include .depend

