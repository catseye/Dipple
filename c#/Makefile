CSC?=gmcs

all: test.exe pair.exe ref.exe

test.exe: test.cs
	$(CSC) test.cs -out:test.exe

# I would have thought /r:System.Numeric.dll would work with Mono, but no.
bigint.exe: bigint.cs
	$(CSC) bigint.cs -out:bigint.exe

pair.exe: pair.cs
	$(CSC) pair.cs -out:pair.exe

ref.exe: ref.cs
	$(CSC) ref.cs -out:ref.exe

clean:
	rm -f *.exe
