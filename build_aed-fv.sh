#!/bin/sh

echo "don't use this, it's old - use the one one directory up"
exit 0

export EXTERNAL_LIBS=shared
export SINGLE=false
export PRECISION=1
export DEBUG=false
export HAVEPLUS=false

while [ $# -gt 0 ] ; do
  case $1 in
    --debug)
      export DEBUG=true
      ;;
    --fence)
      export FENCE=true
      ;;
    --single)
      export SINGLE=true
      ;;
    --have-plus|--haveplus)
      export HAVEPLUS=true
      ;;
    --static)
      export EXTERNAL_LIBS=static
      ;;
    --no-ben)
      export NO_BEN=true
      ;;
    --no-demo)
      export NO_DEMO=true
      ;;
    *)
      ;;
  esac
  shift
done

# The default is MacPorts : uncomment the next line if you use "homebrew"
#export HOMEBREW=true

export CURDIR=`pwd`

export DAEDWATDIR=${CURDIR}/../libaed-water
export DAEDBENDIR=${CURDIR}/../libaed-benthic
export DAEDDMODIR=${CURDIR}/../libaed-demo

if [ "$HAVEPLUS" = "true" ] ; then
  export DAEDRIPDIR=${CURDIR}/../libaed-riparian
  if [ ! -d ${DAEDRIPDIR} ] ; then
    export NO_RIPARIAN=true
  fi
  export DAEDLGTDIR=${CURDIR}/../libaed-light
  if [ ! -d ${DAEDLGTDIR} ] ; then
    export NO_LGT=true
  fi
  export DAEDDEVDIR=${CURDIR}/../libaed-dev
  if [ ! -d ${DAEDDEVDIR} ] ; then
    export NO_DEV=true
  fi
else
  export NO_RIPARIAN=true
  export NO_LGT=true
  export NO_DEV=true
fi

if [ ! -d ${DAEDDMODIR} ] ; then
  export NO_DEMO=true
fi
if [ ! -d ${DAEDBENDIR} ] ; then
  export NO_BEN=true
fi

if [ "$FC" = "" ] ; then
  export FC=ifort
fi

if [ "$FC" = "ifort" ] ; then
  if [ -d /opt/intel/bin ] ; then
    #. /opt/intel/bin/compilervars.sh intel64
    . /opt/intel/oneapi/setvars.sh
  fi
  which ifort >& /dev/null
  if [ $? != 0 ] ; then
    echo ifort compiler requested, but not found
    exit 1
  fi
fi

export F77=$FC
export F90=$FC
export F95=$FC

export PARAMS="AEDWATDIR=${DAEDWATDIR}"
cd ${DAEDWATDIR}
echo making in ${DAEDWATDIR}
make || exit 1
if [ "${NO_BEN}" != "true" ] ; then
   cd ${DAEDBENDIR}
   echo making in ${DAEDBENDIR}
   make || exit 1
   export PARAMS="${PARAMS} AEDBENDIR=${DAEDBENDIR}"
fi
if [ "${NO_DEMO}" != "true" ] ; then
   cd ${DAEDDMODIR}
   echo making in ${DAEDDMODIR}
   make || exit 1
   export PARAMS="${PARAMS} AEDDMODIR=${DAEDDMODIR}"
fi
if [ "$HAVEPLUS" = "true" ] ; then
   cd ${DAEDRIPDIR}
   echo making in ${DAEDRIPDIR}
   make || exit 1
   export PARAMS="${PARAMS} AEDRIPDIR=${DAEDRIPDIR}"
   cd ${DAEDLGTDIR}
   echo making in ${DAEDLGTDIR}
   make || exit 1
   export PARAMS="${PARAMS} AEDLGTDIR=${DAEDLGTDIR}"
   cd ${DAEDDEVDIR}
   echo making in ${DAEDDEVDIR}
   make || exit 1
   export PARAMS="${PARAMS} AEDDEVDIR=${DAEDDEVDIR}"
fi
cd ${CURDIR}
#make distclean
echo make ${PARAMS}
make ${PARAMS} || exit 1

cd "${CURDIR}/.."

# =====================================================================
# Package building bit

# ***************************** Linux *********************************
if [ "$OSTYPE" = "Linux" ] ; then
  if [ $(lsb_release -is) = Ubuntu ] ; then
    BINPATH=binaries/ubuntu/$(lsb_release -rs)
    if [ ! -d "${BINPATH}" ] ; then
      mkdir -p "${BINPATH}"/
    fi
    cd ${CURDIR}
    if [ -x glm+ ] ; then
       /bin/cp debian/control-with+ debian/control
    else
       /bin/cp debian/control-no+ debian/control
    fi
    VERSDEB=`head -1 debian/changelog | cut -f2 -d\( | cut -f1 -d-`
    echo debian version $VERSDEB
    if [ "$VERSION" != "$VERSDEB" ] ; then
      echo updating debian version
      dch --newversion ${VERSION}-0 "new version ${VERSION}"
    fi
    VERSRUL=`grep 'version=' debian/rules | cut -f2 -d=`
    if [ "$VERSION" != "$VERSRUL" ] ; then
      sed -i "s/version=$VERSRUL/version=$VERSION/" debian/rules
    fi

    fakeroot ${MAKE} -f debian/rules binary || exit 1

    cd ..

    mv glm*.deb ${BINPATH}/
  else
    BINPATH="binaries/$(lsb_release -is)/$(lsb_release -rs)"
    echo "No package build for $(lsb_release -is)"
  fi
fi


exit 0
