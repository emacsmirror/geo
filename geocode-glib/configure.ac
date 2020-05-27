#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT(geocode_emacs, 1.0, none@dummy)
AM_INIT_AUTOMAKE
AC_OUTPUT(Makefile src/Makefile)

AM_PATH_LISPDIR

AC_CONFIG_SRCDIR([src/geocode-emacs.c])
AC_CONFIG_HEADERS([src/config.h])
AC_CONFIG_MACRO_DIRS([/usr/share/aclocal])

# Checks for programs.
AC_PROG_CC

# Checks for libraries.

PKG_CHECK_MODULES(GEOCODE, [geocode-glib-1.0])

CFLAGS="$GEOCODE_CFLAGS $CFLAGS"
LDFLAGS="$GEOCODE_LIBS $LDFLAGS"
# Checks for header files.
AC_PATH_X
AC_FUNC_ALLOCA
AC_CHECK_HEADERS([float.h limits.h malloc.h stddef.h stdint.h stdlib.h string.h strings.h sys/time.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_CHECK_HEADER_STDBOOL
AC_TYPE_UID_T
AC_C_INLINE
AC_TYPE_INT16_T
AC_TYPE_INT32_T
AC_TYPE_INT64_T
AC_TYPE_INT8_T
AC_TYPE_MODE_T
AC_FUNC_OBSTACK
AC_TYPE_OFF_T
AC_TYPE_PID_T
AC_C_RESTRICT
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T
AC_CHECK_DECLS([sys_siglist])
AC_STRUCT_TIMEZONE
AC_TYPE_UINT16_T
AC_TYPE_UINT32_T
AC_TYPE_UINT64_T
AC_TYPE_UINT8_T
AC_CHECK_TYPES([ptrdiff_t])

# Checks for library functions.
AC_FUNC_FSEEKO
AC_FUNC_MALLOC
AC_FUNC_MKTIME
AC_FUNC_REALLOC
AC_FUNC_STRCOLL
AC_FUNC_STRERROR_R
AC_FUNC_STRTOD
AC_CHECK_FUNCS([atexit bzero clock_gettime getdelim gettimeofday localtime_r mblen memchr memmove mempcpy memset putenv realpath rpmatch select setenv stpcpy strcasecmp strchr strcspn strdup strerror strncasecmp strndup strpbrk strrchr strspn strstr strtol strtoul strtoull strverscmp tzset])

AC_OUTPUT
