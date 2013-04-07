dnl -*- Autoconf -*-

AC_INIT(guildhall, 0.1.0)
AC_CONFIG_SRCDIR(scripts/update.scm)
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([color-tests -Wall -Wno-portability no-dist-gzip dist-bzip2])
AM_SILENT_RULES([yes])

GUILE_PROGS
AC_SUBST(GUILE)

if $GUILE_TOOLS | grep -q compile; then
   # everything cool
   true
else
   AC_MSG_ERROR([Guile 2.0 required.])
fi

GUILE_EFFECTIVE_VERSION=`$GUILE -c '(display (effective-version))'`
AC_SUBST(GUILE_EFFECTIVE_VERSION)

GUILE_VERSION=`$GUILE -c '(display (version))'`
AC_SUBST(GUILE_VERSION)

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([guildhall/build-info.scm])
AC_CONFIG_FILES([env], [chmod +x env])

AC_OUTPUT
