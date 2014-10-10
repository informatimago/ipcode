#!/bin/bash
#*****************************************************************************
#FILE:               lisp.sh
#LANGUAGE:           sh
#SYSTEM:             POSIX
#USER-INTERFACE:     NONE
#DESCRIPTION
#    
#    This script launches clisp.
#    
#AUTHORS
#    <PJB> Pascal Bourguignon <pjb@informatimago.com>
#MODIFICATIONS
#    2014-10-10 <PJB> Added translation of msg_cannot_find_clisp.
#    2006-04-19 <PJB> Added this header.
#BUGS
#LEGAL
#    GPL
#    
#    Copyright Pascal Bourguignon 2006 - 2014
#    
#    This program is free software; you can redistribute it and/or
#    modify it under the terms of the GNU General Public License
#    as published by the Free Software Foundation; either version
#    2 of the License, or (at your option) any later version.
#    
#    This program is distributed in the hope that it will be
#    useful, but WITHOUT ANY WARRANTY; without even the implied
#    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#    PURPOSE.  See the GNU General Public License for more details.
#    
#    You should have received a copy of the GNU General Public
#    License along with this program; if not, write to the Free
#    Software Foundation, Inc., 59 Temple Place, Suite 330,
#    Boston, MA 02111-1307 USA
#*****************************************************************************

cd "$(dirname $0)"
DIR="$(pwd -P)"

lang="$LANG"
if [ -z "$lang" ] ; then lang="$LC_ALL" ; fi
if [ -z "$lang" ] ; then lang="$LC_MESSAGES" ; fi
if [ -z "$lang" ] ; then lang="en_EN" ; fi
lang="${lang/_*}"
case "$lang" in
en) lang=english ;;
de) lang=german  ;;
fr) lang=french  ;;
es) lang=spanish ;;
du) lang=dutch   ;;
ru) lang=russian ;;
da) lang=danish  ;;
*)  lang=english ;;
esac

msg_cannot_find_clisp () {
    case "$lang" in
    german)  printf "ERROR: Ich kann nicht finden clisp!\n" ;;
    french)  printf "ERREUR: Je ne trouve pas clisp !\n" ;;
    spanish) printf "ERROR: ¡No puedo encontrar clisp!\n" ;;
    dutch)   printf "FOUT: Ik kan het niet vinden clisp!\n" ;;
    russian) printf "ОШИБКА: Не могу найти CLISP!\n" ;;
    danish)  printf "FEJL: Jeg kan ikke finde clisp!\n" ;;
    *)       printf "ERROR: Cannot find clisp!\n" ;;
    esac
}

if [ ! -x "$CLISP" ] ; then
    if [ -x /usr/local/bin/clisp ] ; then
        CLISP=/usr/local/bin/clisp
    else
        CLISP=/usr/bin/clisp
    fi
fi


if [ ! -x "$CLISP" ] ; then
    msg_cannot_find_clisp
    exit 1
fi

case $(uname) in
Darwin) ef=UTF-8 ; et=UTF-8 ; em=UTF-8 ; ep=UTF-8      ; ef=UTF-8 ;;
# Note: this is discutable, but where is the file system names encoding specified?
*)      ef=UTF-8 ; et=UTF-8 ; em=UTF-8 ; ep=ISO-8859-1 ; ef=UTF-8 ;;
esac

# -K full   is obsolete.
exec "$CLISP" -ansi -q -m 32M -I \
    -Efile     "$ef" \
    -Eterminal "$et" \
    -Emisc     "$em" \
    -Epathname "$ep" \
    -Eforeign  "$ef" \
    -L "$lang" \
    -norc \
    -i "ipl-init.lisp" \
    -x '(in-package :ipl-user)' \
    -repl
