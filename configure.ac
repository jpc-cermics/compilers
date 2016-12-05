dnl Process this file with autoconf to produce a configure script.
## Copyright Enpc

AC_REVISION($Revision$)dnl
AC_INIT([compilers],[1.0])
DATE=`date +%Y-%m-%d`
SCIDIR=`pwd`
AC_CONFIG_AUX_DIR(config)
AC_CONFIG_MACRO_DIR(config)
# AC_CONFIG_HEADER(src/include/nsp/config.h)
# AH_BOTTOM([#include "nsp/package_undef.h"])
AM_INIT_AUTOMAKE

dnl Get system type
dnl use "config.guess" and "config.sub" from directory "config"
AC_CANONICAL_HOST

# default prefix
ac_default_prefix=/usr/local

#################################
## all the --with-* argument help
#################################

AC_ARG_ENABLE(wall,
    [  --disable-wall          do not use Wall compilation option (for gcc g77) ])

AC_ARG_ENABLE(debug,
    [  --disable-debug         do not use g compilation option (for gcc g77) ])

AC_ARG_ENABLE(optimize,
    [  --disable-optimize      do not optimize ])

AC_ARG_ENABLE(pg,
    [  --enable-pg             use -pg compilation option])

AC_ARG_WITH(nsp-pvm,
    [  --with-nsp-pvm          prefer local pvm version to a preinstalled one ])

AC_ARG_WITH(tcltk,
    [  --with-tcltk            add tcl-tk detection ])

AC_ARG_WITH(fink,
    [  --with-fink             use /sw for searching packages (default=no) ])

AC_ARG_WITH(macports,
    [  --with-macports         use /opt/local for searching (default=no) ])

AC_ARG_WITH(brew,
    [  --with-brew             use /usr/local for searching (default=no) ])

AC_ARG_WITH(openmp,
    [  --with-openmp           use gcc openmp flags (default=no) ])

AC_ARG_WITH(addedf2c,
    [  --with-addedf2c         build a local f2c (src/f2c)])

AC_ARG_WITH(maxpluslib,
    [  --with-maxpluslib       add maxplus lib ])

AC_ARG_WITH(amoscephes,
    [  --with-amoscephes       add amoscephes lib ])

AC_ARG_WITH(gi,
    [  --with-gi               add  gobject-introspection lib (default=no) ])

AC_ARG_WITH(gtk3,
    [  --with-gtk3             prefer gtk3 to gtk2 ])

###############################
## get the version
###############################

NSP_VERSION=${PACKAGE_NAME}-${PACKAGE_VERSION}
NSP_HOST_TYPE=$host

########################
## removing and cleaning
########################

RM='rm -f'
FILES_TO_CLEAN='*.CKP *.ln *.BAK *.bak core errs ,* *~ *.a .emacs_* tags TAGS make.log MakeOut .*~ *.trace so_locations *.orig *.cos *.obj'


#############################################
## Compilers and options according to machine
#############################################

# scilab script file in script directory: default is "scilab.g" for UNIX
#   for gcwin32 use "scilab-gcwin32.g"
NSP_SCRIPT=nsp.g
# Fortran program in src/sun: default is "fgetarg.f"
#   for HPUX use "fgetarg-hpux.f"
FGETARG=fgetarg
# For old Hp computer (9.x), used in "src/sun/Makefile.in"
#   for choosing the good file between link_std.c and link_SYSV.c
HPPA_OLD=
# Scilab main file: default is "mainsci.o" for UNIX
MAIN='mainsci.o'
# use cairo driver
PERICAIRO=
#
GUILIB=gtksci
# specific for gnome/gtk version
GZVT=
# a directory providing the xdr support
# used only for win32 target
#
LIBXDR_LOCAL=
LIBXDR=
LIBXDRA=
# objects files in src/menus: default is XOBJS for UNIX
#   for Win32 use WOBJS
MENUSOBJS=XOBJS
# use other malloc (in "src/sun" directory) for some 64 bits machines
MALLOC=
# USE_CDEC directives for Dec Alpha
FC_USE_CDEC=DEC
# want to build Scilab f2c too
if test "$with_addedf2c" = yes; then
  F2C_SUBDIR=f2c/src
else
  F2C_SUBDIR=
fi
# on windows target generating scripts for
# printing is useless
BLPR_SCRIPTS='$(BPROGRAMS)'
# on windows target when generating script
# cygdrive/xxx must be replaced by xxx:/
# for mingw32
SED_CYGDRIVE=
# subset of examples which are supposed to work
EXAMPLES_TESTS='$(SUBDIRS1) $(SUBDIRS2)'
# special stuff for cygwin in impdir
IMPPREF=S
IMPENV=
# strip executable (not to be done on darwin)
STRIP=strip
# src/system : use unix or win subdirectory
SYST_FILES=unix

dnl Checks for programs.

#LT_PROG_RC
AC_PROG_INSTALL
AC_PROG_LN_S
AC_CHECK_TOOL(RANLIB, ranlib, :)
AC_CHECK_TOOL(AR, ar, :)

NSP_ISS=
TARGET_MACHINE=
CROSS=no
CROSS_VERSION=
CROSS_POPULATE=
CROSS_COPIER=crosscopy.sh

if test "$with_gtk3" == yes; then
  CROSS_COPIER=crosscopy-gtk3.sh
fi

case "$host" in
  x86_64-w64-mingw32 )
    # cross compilation target 64 bits
    AC_DEFINE(TARGET_W64,[],[Defined if target is 64bits windows])
    NSP_ISS=w64
    TARGET_MACHINE=x64
    CROSS_VERSION=`x86_64-w64-mingw32-gcc -dumpversion`
    CROSS=yes
    CROSS_POPULATE=cross-populate
    ;;
  i686-pc-mingw32 )
    # cross compilation target 32 bits (old name)
    NSP_ISS=w32
    TARGET_MACHINE=ix86
    CROSS_VERSION=`i686-w64-mingw32-gcc -dumpversion`
    CROSS=yes
    CROSS_POPULATE=cross-populate
    ;;
  i686-w64-mingw32 )
    # cross compilation target 32 bits
    NSP_ISS=w32
    TARGET_MACHINE=ix86
    CROSS_VERSION=`i686-w64-mingw32-gcc -dumpversion`
    CROSS=yes
    CROSS_POPULATE=cross-populate
    ;;
esac

# to be improved

case "$CROSS_VERSION" in
       5.3.* )
         CROSS_VERSION="5.3-win32"
	 ;;	 
esac

# visual linker needed to produce lib files 
# from dlls 

SHAREDWIN32=shared-win32
LIBEXE=

if test "$CROSS" == yes; then
   AC_CHECK_TOOL(LIBEXE,lib.exe,no) 
   if test "$LIBEXE" != no; then 
      SHAREDWIN32=shared-win32-and-lib
   fi 
fi

#########################
## ocaml 
#########################

builtin(include,config/ocaml.m4)

CAML_OUTPUTS=
TOOLBOX_COMPILERS=

# if toolboxes/compilers is present then we need ocaml to compile the toolbox 

   AC_PROG_OCAML
   AC_PROG_OCAMLYACC
   AC_PROG_OCAMLLEX
   AC_PROG_HTMLC
   AC_PROG_OCAMLPRINTC

   if test "$OCAMLC" = "no"; then
     AC_MSG_WARN([Ocaml compiler is not found, toolboxes/compilers will not be compiled])
   else 
     TOOLBOX_COMPILERS=compilers
     CAML_BYT_COMP_FLAGS="-strict-sequence -warn-error Ae -annot -g"
     CAML_BIN_COMP_FLAGS="-strict-sequence -w Ae -unsafe -noassert -inline 10000"
     AC_SUBST(CAML_BYT_COMP_FLAGS)
     AC_SUBST(CAML_BIN_COMP_FLAGS)
   fi			
   # toolboxes/compilers will copy executables in bin/
   COMPILERS_SRC_INSTALL_DIR=../..
   AC_SUBST(COMPILERS_SRC_INSTALL_DIR)

   CAML_OUTPUTS="toolboxes/compilers/Makefile.incl"

#########################
## substituting variables
#########################

AC_SUBST(SHAREDWIN32)
AC_SUBST(LIBEXE)
AC_SUBST(SHREXT_NAME)
AC_SUBST(RM)
AC_SUBST(AR)
AC_SUBST(DATE)
AC_SUBST(AR_FLAGS)
AC_SUBST(RC)
AC_SUBST(RC_FLAGS)
AC_SUBST(FILES_TO_CLEAN)
AC_SUBST(CC)
AC_SUBST(CC_OPTIONS)
AC_SUBST(CXX_OPTIONS)
AC_SUBST(CC_LDFLAGS)
AC_SUBST(CC_PICFLAGS)
AC_SUBST(FC)
AC_SUBST(FC_OPTIONS)
AC_SUBST(FC_OPTIONS1)
AC_SUBST(CC_OPTIONS1)
AC_SUBST(FC_OPTIONS2)
AC_SUBST(FC_LDFLAGS)
AC_SUBST(FC_PICFLAGS)
AC_SUBST(LD)
AC_SUBST(LD_LDFLAGS)
AC_SUBST(NSP_SCRIPT)
AC_SUBST(FGETARG)
AC_SUBST(HPPA_OLD)
AC_SUBST(DLDLIB)
AC_SUBST(DLD_SUBDIR)
AC_SUBST(XMULIB)
AC_SUBST(GTKCFLAGS)
AC_SUBST(GTKLIBS)
AC_SUBST(GLU_LIBS)
AC_SUBST(TERMCAPLIB)
AC_SUBST(MAIN)
AC_SUBST(MAKEFILE_TARGET)
AC_SUBST(PERICAIRO)
AC_SUBST(GUILIB)
AC_SUBST(GZVT)
AC_SUBST(LIBXDR_LOCAL)
AC_SUBST(LIBXDR)
AC_SUBST(LIBXDRA)
AC_SUBST(WINXFLAGS)
AC_SUBST(MENUSOBJS)
AC_SUBST(MALLOC)
AC_SUBST(PVMARCH)
AC_SUBST(PVMLIB)
AC_SUBST(PVMNSPDIR)
AC_SUBST(PVMGLIB)
AC_SUBST(PVMTARGET)
AC_SUBST(PVMROOT)
AC_SUBST(PVMROOTR)
AC_SUBST(PVMINST)
AC_SUBST(PVMINCLUDE)
AC_SUBST(PVMHOSTERFILES)
AC_SUBST(PVMHOSTER)
# tcltk
AC_SUBST(TCLTK_INC_PATH)
AC_SUBST(TCLTK_LIBS)
AC_SUBST(TCLTK_NSPDIR)
#
AC_SUBST(NOINLINE)
AC_SUBST(WHICH_BLAS)
AC_SUBST(ATLAS_DIR)
AC_SUBST(IN_ATLAS)
AC_SUBST(IN_FFTPACK)
AC_SUBST(FFTW3_CFLAGS)
AC_SUBST(FFTW3_LIBS)
AC_SUBST(GMP_CFLAGS)
AC_SUBST(GMP_LIBS)
AC_SUBST(SNDFILE_CFLAGS)
AC_SUBST(SNDFILE_LIBS)
AC_SUBST(SNDFILE_LIB)
AC_SUBST(PORTAUDIO_CFLAGS)
AC_SUBST(PORTAUDIO_LIBS)
AC_SUBST(PORTAUDIO_LIB)
AC_SUBST(GRAPHVIZ_CFLAGS)
AC_SUBST(GRAPHVIZ_LIBS)
AC_SUBST(GRAPHVIZ_LIB)
#
AC_SUBST(ASOUND_CFLAGS)
AC_SUBST(ASOUND_LIBS)
AC_SUBST(SHELL)
AC_SUBST(DLLTOOL)
AC_SUBST(LD)
AC_SUBST(LIBM)
AC_SUBST(LIBTOOL)
AC_SUBST(LIBTOOL_DEPS)
AC_SUBST(LN_S)
AC_SUBST(NM)
AC_SUBST(OBJDUMP)
AC_SUBST(PACKAGE)
AC_SUBST(RANLIB)
AC_SUBST(STATIC)
AC_SUBST(NSP_VERSION)
AC_SUBST(NSP_HOST_TYPE)
AC_SUBST(CPPFLAGS)
AC_SUBST(LDFLAGS)
AC_SUBST(LIBS)
AC_SUBST(FLIBS)
AC_SUBST(FLIBS_SH)
#AC_SUBST(CC)
AC_SUBST(CFLAGS)
AC_SUBST(FC_USE_CDEC)
AC_SUBST(FC_SUBDIR)
AC_SUBST(BLPR_SCRIPTS)
AC_SUBST(SED_CYGDRIVE)
AC_SUBST(F2C_SUBDIR)
AC_SUBST(EXAMPLES_TESTS)
AC_SUBST(IMPPREF)
AC_SUBST(IMPENV)
AC_SUBST(ZZLEDT)
AC_SUBST(STRIP)
AC_SUBST(PERIGLEXT)
AC_SUBST(HELP_OBJS)
AC_SUBST(WEBKIT_GEN)
AC_SUBST(GTKPLUGBUG)
AC_SUBST(MPLUSLIB)
AC_SUBST(MPLUSLIBDIR)
AC_SUBST(ENABLE_DEBUG)
AC_SUBST(UMFPACK_LIBS)
AC_SUBST(CHOLMOD_LIBS)
AC_SUBST(SPQR_LIBS)
AC_SUBST(BLAS_LIBS)
AC_SUBST(LAPACK_LIBS)
AC_SUBST(BLAS_LOCAL)
AC_SUBST(LAPACK_LOCAL)
AC_SUBST(GTK_PKGS_CFLAGS)
AC_SUBST(UFSPARSE_LIB)
AC_SUBST(SYST_FILES)
AC_SUBST(RC_FILE)
AC_SUBST(DUMPEXTS)
AC_SUBST(ZLIB_LIBS)
AC_SUBST(GTKSOURCEVIEW_OBJS)
AC_SUBST(AMOS_CEPHES_LIB)
AC_SUBST(STDCPP_LIBS)
AC_SUBST(MONGODB_C_FILE)
AC_SUBST(VTE_FILE)
AC_SUBST(VTE_PKG)

# for summary file

AC_SUBST(HAVE_FFTW3)
AC_SUBST(WITH_GL)
AC_SUBST(HAVE_SNDFILE)
AC_SUBST(HAVE_GI)
AC_SUBST(HAVE_PORTAUDIO)
AC_SUBST(HAVE_MONGODB_C)
AC_SUBST(HAVE_GRAPHVIZ)
AC_SUBST(HAVE_ASOUND)
AC_SUBST(HAVE_GLPK)
AC_SUBST(HAVE_CLP)
AC_SUBST(HAVE_CHOLMOD)
AC_SUBST(HAVE_UMFPACK)
AC_SUBST(HAVE_SPQR)
AC_SUBST(have_gtksourceview)
AC_SUBST(MAN_VIZUALIZATION)
AC_SUBST(GTK_PKGS_CFLAGS)
AC_SUBST(LAPACK_LIBS) AC_SUBST(BLAS_LIBS)
AC_SUBST(CC)
AC_SUBST(CC_OPTIONS)
AC_SUBST(FC)
AC_SUBST(FC_OPTIONS)
AC_SUBST(NSP_ISS)
AC_SUBST(SCICOS_GEN_LIB)
AC_SUBST(GTKDIR)
AC_SUBST(GRAPHICS_LIB_OLD)
AC_SUBST(GI_FILE)
AC_SUBST(GTK_OSX)

AC_SUBST(CROSS_VERSION)
AC_SUBST(CROSS_POPULATE)
AC_SUBST(TARGET_MACHINE)
AC_SUBST(CROSS_COPIER)
  
AC_OUTPUT(Makefile.incl)

