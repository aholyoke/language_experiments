class C;

class A {
public:
	virtual int x(C&);
};

class B : public A {
public:
	virtual int x(C&);
};

class C {
public:
	virtual int x(A&);
	virtual int x(B&);
	int y(A&);
	int y(B&);
};

class D : public C {
public:
	virtual int x(A&);
	virtual int x(B&);
	int y(A&);
	int y(B&);
};
