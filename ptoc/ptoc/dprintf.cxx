#include "dprintf.h"

#include <stdarg.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>

#include <string>

static std::string dprintf(const char *fmt, va_list args) {
	char tmp[65536];
	const size_t len = vsprintf(tmp, fmt, args);
	assert(len < sizeof(tmp));
	return tmp;
}

char *dprintf(const char *fmt, ...) {
	va_list args;

	va_start(args, fmt);
	const auto tmp = dprintf(fmt, args);
	va_end(args);
	return strdup(tmp.c_str());
}
