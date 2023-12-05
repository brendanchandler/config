# 12/04/02 APS
##################################################################
#
#         .profile file
#
#         Read in when you log in.   Not read in for subsequent
#         shells.  For setting up terminal and global environment
#         characteristics.
#
##################################################################

OSVER=`uname -s `
case ${OSVER} in
        # Add SunOS 4.1.X Path and Environmental Variables here.
        Linux)
            lpath="${HOME}:${HOME}/bin"   # prepend to system path
            PATH="${lpath}:${PATH}"
            ;;

        # Add Solaris Path and Environmental Variables here.
        SunOS)
            lpath="${HOME}:${HOME}/bin"   # prepend to system path
            PATH="${lpath}:${PATH}"
            ;;
esac
export PATH

if [ -f "${HOME}/.env" ] ; then
   ENV="${HOME}/.env"
   export ENV
fi

#         general terminal characteristics

#stty -crterase
#stty -tabs
#stty crt
#stty erase '^?'
#stty werase '^?'
#stty kill '^['
#stty new

#         environment variables

#setenv EXINIT 'set sh=/bin/csh sw=4 ai report=2'
#setenv MORE '-c'
PRINTER=lp
export PRINTER
# MEDM PRINTER
PSPRINTER=lp
export PSPRINTER
source ~/.bashrc
. "$HOME/.cargo/env"
