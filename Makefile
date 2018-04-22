NAME		=	koak

SRC			=	token.ml \
				rule.ml \
				debug.ml \
				lexer.ml \
				ast.ml \
				grammar.ml \
				operator.ml \
				function_type.ml \
				misc_type.ml \
				infer_type.ml \
				arg_type.ml \
				check_type.ml \
				asm.ml \
				main.ml \

FLAGS		=	-w Aelz

LIBS		=	-linkpkg -package llvm,llvm.analysis,llvm.bitwriter,llvm.target,llvm_X86,llvm.executionengine str.cma -thread
CAMLC		=	ocamlfind ocamlc

all:
	$(CAMLC) $(FLAGS) -o $(NAME) $(LIBS) $(SRC)

.mli.cmi:
	$(CAMLC) -c $<

clean:
	find . -name "*.cm[iox]" -delete
	find . -name "*.bc" -delete
	find . -name "*.s" -delete
	rm -rf a.out

fclean: clean
	rm -f $(NAME)

re: fclean all
