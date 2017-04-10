#include <iostream>
#include "dd.h"

using namespace std;

int A::x(C& c) {
	return c.x(*this);
}

int B::x(C& c) {
	return c.x(*this);
}

int C::x(A&) {
	return 1;
}

int C::x(B&) {
	return 2;
}

int C::y(A&) {
	return 1;
}

int C::y(B&) {
	return 2;
}

int D::x(A&) {
	return 3;
}

int D::x(B&) {
	return 4;
}

int D::y(A&) {
	return 3;
}

int D::y(B&) {
	return 4;
}


void call_static(C* c, A* a) {
	// y does the same thing as x but is not virtual
	cout << c->y(*a) << endl;
}

void call_overload(C* c, A* a) {
	// Since x is virtual, the runtime type of c is used to determine which method to call
	// However, the A& version is always called (Method overloading is decided statically)
	cout << c->x(*a) << endl;
}

void call_visitor(C* c, A* a) {
	// Calls A::x(C& c) or B::x(C& c) depending on the runtime type of c
	// The methods dispatch the intended method based on the runtime type of a 
	cout << a->x(*c) << endl;
}

int main(int argc, char const *argv[]) {
	// The static types of the callee and argument are used to decide which method to call
	// The runtime types of the callee and the argument have no effect
	// We always call C::y(A&)
	call_static(new C(), new A()); // 1
	call_static(new C(), new B()); // 1
	call_static(new D(), new A()); // 1
	call_static(new D(), new B()); // 1

	// The runtime type of the callee is used to decide which method to call
	// But the runtime type of the argument has no effect
	call_overload(new C(), new A()); // 1
	call_overload(new C(), new B()); // 1
	call_overload(new D(), new A()); // 3
	call_overload(new D(), new B()); // 3

	// Double-dispatch is simulated - A method is called based on the runtime type of
	// both the callee and the argument
	call_visitor(new C(), new A()); // 1
	call_visitor(new C(), new B()); // 2
	call_visitor(new D(), new A()); // 3
	call_visitor(new D(), new B()); // 4
}
