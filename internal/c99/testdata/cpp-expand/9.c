#define TEST42 0x0502
#define TESTBASE 1
#define TEST3 (TEST42 >= 0x0502 || !defined (TESTBASE))

#if TEST3 && defined (TEST4)
#endif

int TEST = TEST3;

int main (void) {
	return 0;
}
