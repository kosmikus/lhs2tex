dnl The AC_CHECK_TEXMF macro is copied from somewhere (and slightly
dnl modified); alas, I cannot remember where I found it.
dnl
AC_DEFUN(AC_CHECK_TEXMF, [
AC_ARG_WITH(texmf,
  [  --with-texmf=<path>     path to an existing texmf tree ($TEXMFLOCAL)],
  [case "$withval" in
    yes|no) texmf= ;;
    *) texmf="$withval" ;;
  esac], , [=DIR])
AC_PATH_PROG(KPSEWHICH, kpsewhich)
if test -z "$texmf"; then
  AC_MSG_CHECKING([for a texmf tree])
  # If user did not specify something, try it ourselves
  AC_CACHE_VAL(ac_cv_texmf_tree, [
  if test -x "$KPSEWHICH"; then
    ac_cv_texmf_tree=`$KPSEWHICH --expand-var='$TEXMFLOCAL'`
    if test -z "$ac_cv_texmf_tree"; then
      ac_cv_texmf_tree=`$KPSEWHICH --expand-var='$TEXMFMAIN'`
    fi
  fi
  if test -z "$ac_cv_texmf_tree"; then
    # try some common paths
    for i in /usr/share/texmf /usr/local/share/texmf; do
      if test -d $$i; then
        ac_cv_texmf_tree=$$i
        break
      fi
    done
  fi
  ])
  AC_MSG_RESULT([$ac_cv_texmf_tree])
  texmf="$ac_cv_texmf_tree"
else
  ac_cv_texmf_tree="$texmf"
fi
if test -n "$texmf"; then
  AC_DEFINE_UNQUOTED(TEXMFTOP, "$texmf",
    [Define to the full path of an existing texmf tree])
  AC_MSG_CHECKING([for texmf.cnf])
  if test -x "$KPSEWHICH"; then
    texmfcnf="`$KPSEWHICH --format=web2c texmf.cnf`"
  fi
  if test -z "$texmfcnf"; then
    texmfcnf="$texmf/web2c/texmf.cnf"
  fi
  if test -f "$texmfcnf"; then
    AC_MSG_RESULT(yes)
    AC_DEFINE_UNQUOTED(TEXMF_IS_WEB2C, 1,
      [Define if you are using a web2c-based TeX distribution])
    texmf_is_web2c=yes
  else
    AC_MSG_RESULT(no)
    texmf_is_web2c=no
  fi
fi
AC_SUBST(texmf)
])

dnl This one is for checking the presence and version of
dnl polytable.sty ...
AC_DEFUN(AC_CHECK_POLYTABLE,[
AC_MSG_CHECKING([for the polytable package])
if test -x "$KPSEWHICH"; then
  POLYTABLE="`$KPSEWHICH polytable.sty`"
fi
if test -f "$POLYTABLE"; then
  AC_MSG_RESULT($POLYTABLE)
  AC_MSG_CHECKING([for version of polytable])
  POLYTABLE_VERSION=`$GREP " v.* .polytable. package" $POLYTABLE | $SED -e "s/^.*v\(.*\) .polytable. package.*$/\1/"`
  AC_MSG_RESULT($POLYTABLE_VERSION)
else
  AC_MSG_RESULT(no)
fi
])

dnl The following macro is from the Glasgow fptools:
dnl
dnl FPTOOLS_PROG_CHECK_VERSION(VERSIONSTR1, TEST, VERSIONSTR2,
dnl                            ACTION-IF-TRUE [, ACTION-IF-FALSE])
dnl
dnl compare versions field-wise (separator is '.')
dnl TEST is one of {-lt,-le,-eq,-ge,-gt}
dnl
dnl quite shell-independant and SUSv2 compliant code
dnl
dnl NOTE: the loop could be unrolled within autoconf, but the
dnl       macro code would be a) longer and b) harder to debug... ;)
dnl
AC_DEFUN(FPTOOLS_PROG_CHECK_VERSION,
[if ( IFS=".";
      a="[$1]";  b="[$3]";
      while test -n "$a$b"
      do
              set -- [$]a;  h1="[$]1";  shift 2>/dev/null;  a="[$]*"
              set -- [$]b;  h2="[$]1";  shift 2>/dev/null;  b="[$]*"
              test -n "[$]h1" || h1=0;  test -n "[$]h2" || h2=0
              test [$]{h1} -eq [$]{h2} || break
      done
      test [$]{h1} [$2] [$]{h2}
    )
then ifelse([$4],,[:],[
  $4])
ifelse([$5],,,
[else
  $5])
fi
])])dnl
