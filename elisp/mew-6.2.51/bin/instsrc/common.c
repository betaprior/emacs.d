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
 *    Common routines.
 */

#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <mbstring.h>
#include <conio.h>

#include "common.h"


BOOL  fOutputDebug=FALSE;
BOOL  fOutputLog=FALSE;
BOOL  fOutputLogTime=FALSE;
char  szLogFileName[MAX_PATH]="";
DWORD dwOutputLogSizeMax=0;
BOOL  fOutputQuiet=FALSE;

LANGID        langId=0;
OSVERSIONINFO VersionInfo;

char  szCrCode[4]="\r\n";


BOOL SetEOLCode( LPCSTR lpszFileName )
{
  FILE *fp;
  char szBuf[1024];
  char *pt;

  if ( (fp=fopen( lpszFileName, "rb" )) == NULL ) return ( FALSE );
  fread( szBuf, 1, sizeof(szBuf), fp );
  fclose(fp);
  pt = strchr( szBuf, '\n' );
  if ( *(pt-1) == '\r' )
    strcpy(szCrCode,"\r\n");
  else
    strcpy(szCrCode,"\n");

  return ( TRUE );
}


BOOL GetEnvironments( VOID )
{
  langId = GetUserDefaultLangID();

  VersionInfo.dwOSVersionInfoSize = sizeof(VersionInfo);
  GetVersionEx(&VersionInfo);

  return ( TRUE );
}


int BilMessageBox( HWND    hWnd,
		   LPCTSTR lpEnText,
		   LPCTSTR lpJpText,
		   LPCTSTR lpCaption,
		   UINT    uiType )
{
  if ( fOutputQuiet ) return 0;
  if ( langId == LANGID_JAPANESE ){
    return ( MessageBox( hWnd, lpJpText, lpCaption, uiType ) );
  } else {
    return ( MessageBox( hWnd, lpEnText, lpCaption, uiType ) );
  }
}


BOOL ReadLines( LPSTR lpLine,
		DWORD dwLength,
		BOOL  fPrintMode )
{
  DWORD  i=0;
  char   c,st[2];
  BOOL   fRet=FALSE;
  HANDLE hStdin,hStdout;
  DWORD  dwMode;
  DWORD  dwLen;
  CONSOLE_SCREEN_BUFFER_INFO csbi;

  /* error check */
  hStdin  = GetStdHandle(STD_INPUT_HANDLE);
  hStdout = GetStdHandle(STD_OUTPUT_HANDLE);
  GetConsoleMode( hStdin, &dwMode );
  dwMode &= ~ENABLE_LINE_INPUT;
  dwMode &= ~ENABLE_ECHO_INPUT;
  dwMode &= ENABLE_PROCESSED_INPUT;
  SetConsoleMode( hStdin, dwMode );

  st[1] = 0;
  while ( i < dwLength ){
    ReadFile( hStdin, &c, 1, &dwLen, NULL );
    if ( (c==0x0d) || (c==0x0a) ){ /* return */
      *lpLine = 0x00;
      fRet = TRUE;
      break;
    }
    if ( c == 0x03 ){		/* \C-c */
      fRet = FALSE;
      break;
    }
    if ( c == 0x08 ){		/* \C-h */
      if ( i > 0 ){
	GetConsoleScreenBufferInfo( hStdout, &csbi );
	csbi.dwCursorPosition.X--;
	SetConsoleCursorPosition( hStdout, csbi.dwCursorPosition );
	WriteFile( hStdout, " ", 1, &dwLen, NULL );
	GetConsoleScreenBufferInfo( hStdout, &csbi );
	csbi.dwCursorPosition.X--;
	SetConsoleCursorPosition( hStdout, csbi.dwCursorPosition );
	i--;
	lpLine--;
      }
      continue;
    }
    if ( fPrintMode ){		/* show input character */
      if ( c >= 0x20 ){
	st[0] = c;
	WriteFile( hStdout, &c, 1, &dwLen, NULL );
      }
    }
    *lpLine++ = c;
    i++;
  } /* while ( i < dwLength ){ */

  WriteFile( hStdout, "\n", 1, &dwLen, NULL );

  return ( fRet );
}


BOOL GetFileNameFromFullPath( LPCSTR szFullPath,
			      LPSTR  lpBuf,
			      DWORD  dwLength )
{
  char  szDrive[_MAX_DRIVE],szDir[_MAX_DIR];
  char  szFileName[_MAX_FNAME],szExt[_MAX_EXT];

  if ( strlen(szFullPath) > MAX_PATH ){
    SetLastError(ERROR_INVALID_PARAMETER);
    return ( FALSE );
  }

  _splitpath( szFullPath, szDrive, szDir, szFileName, szExt );

  if ( strlen(szDrive)+strlen(szDir)+1 > dwLength ){
    SetLastError(ERROR_INVALID_PARAMETER);
    return ( FALSE );
  }

  strcpy(lpBuf,szFileName);
  strcat(lpBuf,szExt);

  return ( TRUE );
}


BOOL GetDirectoryNameFromFullPath( LPCSTR szFullPath,
				   LPSTR  lpBuf,
				   DWORD  dwLength )
{
  char  szDrive[_MAX_DRIVE],szDir[_MAX_DIR],szFileName[_MAX_FNAME],szExt[_MAX_EXT];

  if ( strlen(szFullPath) > MAX_PATH ){
    SetLastError(ERROR_INVALID_PARAMETER);
    return ( FALSE );
  }

  _splitpath( szFullPath, szDrive, szDir, szFileName, szExt );

  if ( strlen(szFileName)+strlen(szExt)+1 > dwLength ){
    SetLastError(ERROR_INVALID_PARAMETER);
    return ( FALSE );
  }

  strcpy(lpBuf,szDrive);
  strcat(lpBuf,szDir);
  lpBuf[strlen(lpBuf)-1] = '\0'; // to delete last '\'.  xxx

  return ( TRUE );
}


BOOL GetModulePath( LPSTR szModulePath,
		    DWORD dwLength )
{
  HMODULE         hModule;
  char            szModuleFileName[MAX_PATH];
  char            szBuf[MAX_PATH];

  hModule = GetModuleHandle(NULL);
  GetModuleFileName(hModule,szModuleFileName,sizeof(szModuleFileName));
  GetDirectoryNameFromFullPath(szModuleFileName,szBuf,sizeof(szBuf));
  strcat(szBuf,"\\");
  if ( (DWORD)(strlen(szBuf)+1) > dwLength ){
    SetLastError(ERROR_INVALID_PARAMETER);
    return ( FALSE );
  }
  strcpy(szModulePath,szBuf);

  return ( TRUE );
}


BOOL SetupLog( LPCSTR lpszFileName,
	       BOOL   fCreate,
	       BOOL   fLog,
	       BOOL   fLogTime,
	       BOOL   fDebug,
	       DWORD  dwLogSizeMax,
	       BOOL   fQuiet )
{
  HANDLE hFile;

  // xxx add error check.
  strcpy(szLogFileName,lpszFileName);
  fOutputLog         = fLog;
  fOutputLogTime     = fLogTime;
  fOutputDebug       = fDebug;
  dwOutputLogSizeMax = dwLogSizeMax;
  fOutputQuiet       = fQuiet;

  if ( fCreate ){
      hFile =  CreateFile(szLogFileName,
			  GENERIC_WRITE,
			  0,
			  NULL,
			  CREATE_ALWAYS,
			  0,
			  NULL);
      if ( hFile != INVALID_HANDLE_VALUE )
	  CloseHandle( hFile );
  }

  return ( TRUE );
}


BOOL OutputLog( LPCSTR lpszString )
{
  BOOL       fRet=TRUE;
  HANDLE     hFile;
  DWORD      dwLen;
  char       szLogFile[MAX_PATH];
  LPSTR      lpBuf,lpPRBuf;
  SYSTEMTIME st;
#if 0
  HANDLE     hStdout;
#endif

  /* allocate buffer */
  lpPRBuf = (LPSTR)malloc(strlen(lpszString)+1);
  if ( lpPRBuf == NULL ){
    SetLastError(ERROR_OUTOFMEMORY);
    return ( FALSE );
  }
  lpBuf   = (LPSTR)malloc(strlen(lpszString)+128);
  if ( lpBuf == NULL ){
    free(lpPRBuf);
    SetLastError(ERROR_OUTOFMEMORY);
    return ( FALSE );
  }
  strcpy(lpPRBuf,lpszString);

#if 0
  /* write to console */
  hStdout = GetStdHandle( STD_OUTPUT_HANDLE );
  if ( hStdout ){
    WriteFile( hStdout, lpPRBuf, strlen(lpPRBuf), &dwLen, NULL );
    WriteFile( hStdout, "\n", 1, &dwLen, NULL );
  }
#endif

  if ( ! fOutputLog ) goto Exit;

  /* write to file */
  if ( fOutputLogTime ) GetLocalTime( &st );
  GetModulePath(szLogFile,sizeof(szLogFile));
  if ( (strlen(szLogFile)+strlen(szLogFileName)+1) > MAX_PATH ){
    fRet = FALSE;
    goto Exit;
  }
  strcat(szLogFile,szLogFileName);
  hFile =  CreateFile(szLogFile,
		      GENERIC_WRITE,
		      FILE_SHARE_READ,
		      NULL,
		      OPEN_ALWAYS,
		      0,
		      NULL);
  if ( hFile != INVALID_HANDLE_VALUE){
    SetFilePointer(hFile,0,NULL,FILE_END);
    if ( fOutputLogTime ){
      sprintf(lpBuf,"%04d/%02d/%02d %02d:%02d:%02d | %s\r\n",
	      st.wYear,st.wMonth,st.wDay,st.wHour,st.wMinute,st.wSecond,
	      lpPRBuf);
    } else {
      sprintf(lpBuf,"%s\r\n",lpPRBuf);
    }
    WriteFile(hFile,lpBuf,strlen(lpBuf),&dwLen,NULL);
    CloseHandle(hFile);
  }
 Exit:
  free(lpPRBuf);
  free(lpBuf);

  return ( fRet );
}


BOOL OutputDebugLog( LPCSTR szString )
{
  if ( fOutputDebug ){
    return ( OutputLog(szString) );
  } else {
    return ( TRUE );
  }
}


/* convert '/' -> '\' */
BOOL RevConvertPathSeparator( LPSTR szPathName )
{
  char *pt;

  while ( 1 ){
    pt = strrchr( szPathName, '/' );
    if ( pt == NULL ) break;
    if ( pt == szPathName ){
      *pt = '\\';
      break;
    }
    if ( _ismbblead(*(pt-1)) == 0 ) *pt = '\\'; /* replace */
  }
  return ( TRUE );
}


/* convert '\' -> '/' */
BOOL ConvertPathSeparator( LPSTR szPathName )
{
  char *pt;

  while ( 1 ){
    pt = strrchr( szPathName, '\\' );
    if ( pt == NULL ) break;
    if ( pt == szPathName ){
      *pt = '/';
      break;
    }
    if ( _ismbblead(*(pt-1)) == 0 ) *pt = '/'; /* replace */
  }
  return ( TRUE );
}


BOOL CheckFile( LPCSTR szFileName )
{
  HANDLE   hFile;

  hFile = CreateFile( szFileName,
		      GENERIC_READ,
		      FILE_SHARE_READ,
		      NULL,
		      OPEN_EXISTING,
		      0,
		      NULL );
  if ( hFile == INVALID_HANDLE_VALUE ){
    return ( FALSE );
  } else {
    CloseHandle( hFile );
    return ( TRUE );
  }
}


BOOL CheckDirectory( LPCSTR szDirName )
{
  HANDLE          hFind;
  WIN32_FIND_DATA wfd;

  hFind = FindFirstFile(szDirName,&wfd);
  if ( hFind == INVALID_HANDLE_VALUE ){
    return ( FALSE );
  } else {
    FindClose( hFind );
    if ( wfd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY )
      return ( TRUE );
    else
      return ( FALSE );
  }
}


VOID ReplaceString( LPSTR  lpSrc,
		    LPCSTR lpSrcPat,
		    LPSTR  lpDstPat )
{
  char  szBuf[BUFLEN];
  LPSTR pt;

  if ( pt = strstr( lpSrc, lpSrcPat ) ){
    *pt = (char)0;
    strcpy(szBuf,lpSrc);
    strcat(szBuf,lpDstPat);
    pt += strlen(lpSrcPat);
    strcat(szBuf,pt);
    strcpy(lpSrc,szBuf);
  } /* if ( pt = strstr( lpSrc, lpSrcPat ) ){ */
}

