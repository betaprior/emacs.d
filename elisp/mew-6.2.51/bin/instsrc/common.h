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
 * Mew installer  on  Meadow/NTEmacs/XEmacs
 *
 *    Common Header.
 */

#ifndef __COMMON_H__
#define __COMMON_H__



/**** definitions ****/

#define LANGID_JAPANESE (MAKELANGID(LANG_JAPANESE,SUBLANG_DEFAULT))

#define BUFLEN   4096		/* ok? */

#define s_printf _snprintf

/**** variables ****/
extern BOOL  fOutputDebug;
extern BOOL  fOutputLog;
extern BOOL  fOutputLogTime;
extern char  szLogFileName[MAX_PATH];
extern DWORD dwOutputLogSizeMax;
extern char  szCrCode[4];
extern BOOL  fOutputQuiet;

extern OSVERSIONINFO VersionInfo;
extern LANGID        langId;


/**** functions ****/
/* logging */
BOOL SetupLog( LPCSTR, BOOL, BOOL, BOOL,   BOOL, DWORD, BOOL );
BOOL OutputLog( LPCSTR );
BOOL OutputDebugLog( LPCSTR );

/* misc */
BOOL SetEOLCode( LPCSTR );
BOOL ReadLines( LPSTR, DWORD, BOOL );
BOOL GetFileNameFromFullPath( LPCSTR, LPSTR, DWORD );
BOOL GetDirectoryNameFromFullPath( LPCSTR, LPSTR, DWORD );
BOOL GetModulePath( LPSTR, DWORD );
BOOL RevConvertPathSeparator( LPSTR );
BOOL ConvertPathSeparator( LPSTR );
BOOL CheckFile( LPCSTR );
BOOL CheckDirectory( LPCSTR );
VOID ReplaceString( LPSTR, LPCSTR, LPSTR );
int  BilMessageBox( HWND, LPCTSTR, LPCTSTR, LPCTSTR, UINT );
BOOL GetEnvironments( VOID );

#define BilErrorMessageBox( lpEnText, lpJpText ) \
BilMessageBox( NULL, lpEnText, lpJpText, "Error", \
	       MB_OK|MB_SETFOREGROUND|MB_ICONERROR );
#define BilWarnMessageBox( lpEnText, lpJpText ) \
BilMessageBox( NULL, lpEnText, lpJpText, "Information", \
	       MB_OK|MB_SETFOREGROUND|MB_ICONWARNING );
#define BilInfoMessageBox( lpEnText, lpJpText ) \
BilMessageBox( NULL, lpEnText, lpJpText, "Information", \
	       MB_OK|MB_SETFOREGROUND|MB_ICONINFORMATION );



#endif
