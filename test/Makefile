.PHONY: all clean

fslex=mono lib/fslex.exe
fsyacc=mono lib/fsyacc.exe
powerpack=lib/FSharp.PowerPack.dll
fsharpc=fsharpc --nologo

all: Siris.exe

Siris.exe: SirisTest.fsx Lexer.dll Parser.dll AbSyn.dll SymTab.dll Interpreter.dll $(powerpack)
	$(fsharpc) SirisTest.fsx -r Lexer.dll -r Parser.dll -r AbSyn.dll -r Interpreter.dll -r $(powerpack)

Lexer.dll: Lexer.fs Parser.dll $(powerpack)
	$(fsharpc) -a Lexer.fs -r Parser.dll -r $(powerpack)

Lexer.fs: ../src/Lexer.fsl
	$(fslex)  ../src/Lexer.fsl -o ./Lexer.fs

Parser.dll: Parser.fs AbSyn.dll $(powerpack)
	$(fsharpc) -a Parser.fs -r AbSyn.dll -r $(powerpack)

Parser.fs: ../src/Parser.fsp
	$(fsyacc) -v --module Parser ../src/Parser.fsp -o ./Parser.fs

AbSyn.dll: ../src/AbSyn.fs
	$(fsharpc) -a ../src/AbSyn.fs

SymTab.dll: ../src/SymTab.fs
	$(fsharpc) -a ../src/SymTab.fs -o SymTab.dll


Interpreter.dll: ../src/Interpreter.fs AbSyn.dll SymTab.dll
	$(fsharpc) -a ../src/Interpreter.fs -r AbSyn.dll -r SymTab.dll -o Interpreter.dll


clean:
	rm -f *.dll Makefile~ SirisTest.exe Lexer.fs Parser.fs Parser.fsyacc.output Parser.fsi
