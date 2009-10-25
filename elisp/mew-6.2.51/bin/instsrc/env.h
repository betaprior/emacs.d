/*
 * Copyright (C) 1997-2005 Mew developing team.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
 * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
/*
 * IM/Mew installer  on  Meadow/NTEmacs/XEmacs
 *
 *    Environments.
 */

#ifndef __ENV_H__
#define __ENV_H__


/**** definitions ****/

#define EMACS_NONE          127	/* No Emacs */
#define EMACS_MEADOW          0	/* Meadow */
#define EMACS_EMACS           1 /* NTEmacs */
#define EMACS_XEMACS          2 /* XEmacs */


#define PERL_SUBKEY           "SOFTWARE\\Mew\\Perl"
#define PERL_SUBKEY_BIN       "bin"
#define PERL_SUBKEY_LIB       "lib"


/**** variables ****/
extern char  szHomePath[MAX_PATH];
extern char  szConfPath[MAX_PATH];
extern char  szConfFile[MAX_PATH];

extern char  bNoEmacsRegistry;
extern char  bEmacsType;
extern char  szEmacs[MAX_PATH];
extern char  szEmacsPath[MAX_PATH];
extern char  szEmacsLoadPath[MAX_PATH];
extern char  szInfoPath[MAX_PATH];
extern BOOL  fSiteLisp;
extern char  szMeadowVersion[16];

extern char  szPerl[MAX_PATH];
extern char  szPerlDir[MAX_PATH];
extern char  szPerlBin[MAX_PATH];
extern char  szPerlLib[MAX_PATH];


/**** functions ****/
BOOL ReadConfig( LPCSTR, LPSTR, DWORD );
BOOL PrintEnvironments( VOID );
BOOL PrintEmacsEnvironments( VOID );
BOOL PrintPerlEnvironments( VOID );
BOOL ReadEnvironments( VOID );
BOOL ReadEmacsEnvironments( LPCSTR );
BOOL ReadPerlEnvironments( LPCSTR );


#endif
