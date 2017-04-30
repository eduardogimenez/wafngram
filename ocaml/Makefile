#
# Pure OCaml, package from Opam, two directories
#

# - The -I flag introduces sub-directories 
# - -use-ocamlfind is required to find packages (from Opam)
# - _tags file introduces packages, bin_annot flag for tool chain
# - using *.mll and *.mly are handled automatically

# - we are using menhir, the modern replacement for OCamlYacc

PCKGS = -pkg str -pkg netstring

OCB_FLAGS   = -use-ocamlfind $(PCKGS) -use-menhir -I src -I lib

OCB = 		ocamlbuild $(OCB_FLAGS)

all: 		native byte # profile debug

clean:
			rm -f counter.native split.native abstract.native counter.byte split.byte abstract.byte
			$(OCB) -clean

native:  	sanity
			$(OCB) abstract.native
			$(OCB) split.native
			$(OCB) counter.native

byte: 		sanity
			$(OCB) abstract.byte
			$(OCB) split.byte
			$(OCB) counter.native

profile: 	sanity
			$(OCB) -tag profile main.native

debug: 		sanity
			$(OCB) -tag debug main.byte

sanity:
			# check that menhir is installed, use "opam install menhir"
			which menhir

.PHONY: 	all clean byte native profile debug sanity test
