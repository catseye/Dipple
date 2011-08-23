#define X "Arrrgh!"

#define A(x) x
#define B(x) x

#define C(x) A(B(x))

C(X)
