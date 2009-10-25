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
  *    Environments.
  */

#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <process.h>
#include <mbstring.h>

#include "common.h"
#include "env.h"

char   szHomePath[MAX_PATH];
char   szConfPath[MAX_PATH];
char   szConfFile[MAX_PATH];

char   bNoEmacsRegistry=FALSE;
char   bEmacsType=EMACS_NONE;
char   szEmacs[MAX_PATH];
char   szEmacsPath[MAX_PATH];
char   szEmacsLoadPath[MAX_PATH];
char   szInfoPath[MAX_PATH];
BOOL   fSiteLisp=FALSE;
char   szMeadowVersion[16];

char   szPerl[MAX_PATH];
char   szPerlDir[MAX_PATH];
char   szPerlBin[MAX_PATH];
char   szPerlLib[MAX_PATH];


/*
 * MUST call after calling ReadEmacsEnvironments
 */

BOOL ReadConfig( LPCSTR szKey, LPSTR szValue, DWORD dwSize )
{
    FILE *fp;
    char *pt,*pt2;
    char szBuf[1024];

    szValue[0] = 0;

    if ( (fp = fopen(szConfFile,"r")) == NULL )
	return ( FALSE );

    while ( fgets( szBuf, sizeof(szBuf), fp) ){
	if ( ! strncmp(szBuf,szKey,strlen(szKey)) ){
	    pt = strchr(szBuf,'=');
	    if ( pt ){
		pt++;
		pt2 = strchr(pt,' ');
		if ( pt2 ) *pt2 = 0;
		pt2 = strchr(pt,'\t');
		if ( pt2 ) *pt2 = 0;
	    } else {
		return ( FALSE );
	    }      
	    break;
	}
    }

    if ( ferror(fp) || feof(fp) ){
	fclose(fp);
	return ( FALSE );
    }
    fclose(fp);
  
    if ( pt )
	if ( strlen(pt) < dwSize ){
	    strcpy(szValue,pt);
	    return ( TRUE );
	}
    return ( FALSE );
}


BOOL ReadEnvironments( VOID )
{
    BOOL     ret=TRUE;
    char     szPath[MAX_PATH];

    /* Initialize */
    szHomePath[0] = 0;
    szConfPath[0] = 0;
    szConfFile[0] = 0;

    /* directories */
    if ( GetEnvironmentVariable("HOME",szHomePath,sizeof(szHomePath)) == 0 ){
	BilErrorMessageBox( "Environment variable \"HOME\" is not set.",
			    "環境変数 HOME が設定されていません" );
	szConfPath[0] = 0;
	szConfFile[0] = 0;
	ret = FALSE;
    } else {
	sprintf(szConfPath,"%s\\.im",szHomePath);
	sprintf(szConfFile,"%s\\Config",szConfPath);
    }
  
    if ( GetModulePath( szPath, sizeof(szPath) ) )
	if ( _mbschr( szPath, ' ' ) != NULL ){
	    BilErrorMessageBox( "Extract archive on path that do not have space(' ') char.",
				"スペースを含まないパス上にアーカイブを展開して下さい" );
	    ret = FALSE;
	}
  
    return ( ret );
}


BOOL ReadEmacsEnvironments( LPCSTR lpszIniFile )
{
    DWORD    ret;
    BOOL     rete=TRUE;
    HKEY     hKey;
    DWORD    dwBufLen;
    char     *pt;
    char     szBuf[256];
    char     szPBuf[256];
    char     emacs_dir[MAX_PATH];
    char     szMeadowKey[MAX_PATH];

    /* Initialize */
    szEmacs[0]         = 0;
    szEmacsPath[0]     = 0;
    szEmacsLoadPath[0] = 0;
    szInfoPath[0]      = 0;

    /* detect Emacs type */
    if ( bEmacsType == EMACS_NONE ){
	GetPrivateProfileString("Make","EMACS","Meadow",
				szBuf,sizeof(szBuf),
				lpszIniFile);
	if ( ! stricmp(szBuf,"Emacs") )
	    bEmacsType = EMACS_EMACS;
	else if ( ! stricmp(szBuf,"XEmacs") )
	    bEmacsType = EMACS_XEMACS;
	else
	    bEmacsType = EMACS_MEADOW;
    }

    /* no registry entries are found. */
    if (bNoEmacsRegistry) {
	    switch (bEmacsType) {
	    case EMACS_EMACS:
		    /* NTEmacs */
		    GetEnvironmentVariable("emacs_dir",
					   szEmacsPath, sizeof(szEmacsPath));
		    RevConvertPathSeparator( szEmacsPath );
		    if (szEmacsPath[strlen(szEmacsPath)-1] != '\\')
			    strcat(szEmacsPath, "\\");
		    strcat(szEmacsPath, "bin");
		    sprintf(szEmacs,"%s\\emacs.exe", szEmacsPath);
		    sprintf(szEmacsLoadPath, "%s\\..\\site-lisp\\mew",
			    szEmacsPath);
		    sprintf(szInfoPath, "%s\\..\\info", szEmacsPath);
		    break;
	    case EMACS_MEADOW:
	    default:
		    /* Meadow */
		    BilErrorMessageBox( "environment variable (getenv) mode is not supported for Meadow.",
					"Meadowでは環境変数(getenv)モードはサポートされていません");

		    rete = FALSE;
		    break;
	    }
	    return (rete);
    }

    switch ( bEmacsType  ){
    case EMACS_EMACS:
	/* NTEmacs */
	ret = RegOpenKeyEx(HKEY_LOCAL_MACHINE,
			   "SOFTWARE\\GNU\\Emacs",
			   0,
			   KEY_EXECUTE,
			   &hKey);
	if ( ret != ERROR_SUCCESS ){
	    BilErrorMessageBox( "Emacs is not installed!\nIf you want to install with another Emacs,\nchange EMACS key in INI file.",
				"Emacs がインストールされていません\n別の Emacs にインストールするなら、\nINI ファイル中の EMACS キーを変更して下さい");
	    rete = FALSE;
	}
	break;
    case EMACS_MEADOW:
    default:
	/* Meadow */
	if ( szMeadowVersion[0] == 0 ){
	    GetPrivateProfileString("Make","MEADOWVERSION","1.00",
				    szMeadowVersion,sizeof(szMeadowVersion),
				    lpszIniFile);
	}
	if ( ! strcmp( szMeadowVersion, "1.00") ||
	     ! strcmp( szMeadowVersion, "1.01") ){
	    strcpy( szMeadowKey, "SOFTWARE\\GNU\\Meadow\\Environment" );
	} else {
	    strcpy( szMeadowKey, "SOFTWARE\\GNU\\Meadow\\" );
	    strcat( szMeadowKey, szMeadowVersion );
	    strcat( szMeadowKey, "\\Environment");
	}
	ret = RegOpenKeyEx(HKEY_LOCAL_MACHINE,
			   szMeadowKey,
			   0,
			   KEY_EXECUTE,
			   &hKey);
	if ( ret != ERROR_SUCCESS ){
	    BilErrorMessageBox( "Meadow is not installed!\nSpecified version is correct?\nIf you want to install with another Emacs,\nchange EMACS key in INI file.",
				"Meadow がインストールされていません\nMeadow のバージョン指定が正しいか確認して下さい\n別の Emacs にインストールするなら、\nINI ファイル中の EMACS キーを変更して下さい");
	    rete = FALSE;
	}
	break;
    }

    if ( rete ){
	dwBufLen = sizeof(szEmacsPath);
	ret = RegQueryValueEx(hKey,
			      "EMACSPATH",
			      NULL,
			      NULL,
			      szEmacsPath,
			      &dwBufLen);
	if ( ret != ERROR_SUCCESS ){
	    OutputLog("Error: cannot query EMACSPATH");
	    rete = FALSE;
	} else {
	    if ( bEmacsType == EMACS_EMACS ){	/* NTEmacs */
		dwBufLen = sizeof(emacs_dir);
		ret = RegQueryValueEx(hKey,
				      "emacs_dir",
				      NULL,
				      NULL,
				      emacs_dir,
				      &dwBufLen);
		if ( ret != ERROR_SUCCESS ){
		    OutputLog("Error: cannot query emacs_dir");
		    rete = FALSE;
		} else {
		    ReplaceString( szEmacsPath, "%emacs_dir%", emacs_dir );
		    RevConvertPathSeparator( szEmacsPath );
		    sprintf(szEmacs,"%s\\emacs.exe",szEmacsPath);
		}
	    } else if ( VersionInfo.dwPlatformId == VER_PLATFORM_WIN32_NT ){ /* NT */
		switch ( bEmacsType ){
		case EMACS_MEADOW:
		default:
		    if (szMeadowVersion[0] == '1' && szMeadowVersion[2] < '9')
			sprintf(szEmacs,"%s\\MeadowNT.exe",szEmacsPath);
		    else
			sprintf(szEmacs,"%s\\Meadow.exe",szEmacsPath);
		    break;
		}
	    } else {			/* 95 */
		switch ( bEmacsType ){
		case EMACS_MEADOW:
		default:
		    if (szMeadowVersion[0] == '1' && szMeadowVersion[2] < '9')
			sprintf(szEmacs,"%s\\Meadow95.exe",szEmacsPath);
		    else
			sprintf(szEmacs,"%s\\Meadow.exe",szEmacsPath);
		    break;
		}
	    }
	}
	dwBufLen = sizeof(szEmacsLoadPath);
	ret = RegQueryValueEx(hKey,
			      "EMACSLOADPATH",
			      NULL,
			      NULL,
			      szEmacsLoadPath,
			      &dwBufLen);
	if ( ret != ERROR_SUCCESS ){
	    OutputLog("Error: cannot query EMACSLOADPATH");
	    rete = FALSE;
	} else {
	    /* if site-lisp directory exists, use site-lisp/mew directory */
	    if ( bEmacsType == EMACS_MEADOW ){
		strcpy(szEmacsLoadPath,szEmacsPath);
		strcat(szEmacsLoadPath,"\\..\\site-lisp");
		/* check whether ${bin}\site-lisp is exist(New) or not(Old). */
		if ( CheckDirectory(szEmacsLoadPath) ){
		    /* New Meadow layout. */
		    strcpy(szEmacsLoadPath,szEmacsPath);
		    strcat(szEmacsLoadPath,"\\..\\site-lisp\\mew");
		} else {
		    /* Old Meadow layout. */
		    strcpy(szEmacsLoadPath,szEmacsPath);
		    strcat(szEmacsLoadPath,"\\..\\..\\site-lisp\\mew");
		}
		fSiteLisp = TRUE;
	    } else if ( bEmacsType == EMACS_EMACS ){
		strcpy(szEmacsLoadPath,szEmacsPath);
		ReplaceString( szEmacsLoadPath, "%emacs_dir%", emacs_dir );
		RevConvertPathSeparator( szEmacsLoadPath );
		strcat(szEmacsLoadPath,"\\..\\site-lisp\\mew");
		fSiteLisp = TRUE;
	    } else {
		strcpy(szBuf,szEmacsLoadPath);
		if ( pt = strrchr(szBuf,'\\') ){
		    *pt = (char)0;
		    strcat(szBuf,"\\site-lisp");
		    if ( CheckDirectory( szBuf ) ){
			strcat(szBuf,"\\mew");
			fSiteLisp = TRUE;
			if ( ! CheckDirectory( szBuf ) ){
			    sprintf(szPBuf,"creating [%s] ...",szBuf);
			    if ( ! CreateDirectory( szBuf, NULL ) ){
				strcat(szPBuf,"fail.");
				OutputDebugLog(szPBuf);
				return ( FALSE );
			    } else {
				strcat(szPBuf,"ok.");
				OutputDebugLog(szPBuf);
				strcpy(szEmacsLoadPath,szBuf);
			    }
			} else {
			    strcpy(szEmacsLoadPath,szBuf);
			}
		    } /* if ( CheckDirectory( szBuf ) ){ */
		} /* if ( pt = strrchr(szBuf,'\\') ){ */
	    } /* if ( bEmacsType == EMACS_MEADOW */
	} /* if ( ret != ERROR_SUCCESS */
    
	if ( bEmacsType == EMACS_EMACS ){
	    sprintf( szInfoPath, "%s\\info", emacs_dir );
	} else {
	    dwBufLen = sizeof(szInfoPath);
	    ret = RegQueryValueEx(hKey,
				  "INFOPATH",
				  NULL,
				  NULL,
				  szInfoPath,
				  &dwBufLen);
	    if ( ret != ERROR_SUCCESS ){
		OutputLog("Error: cannot query INFOPATH");
		rete = FALSE;
	    }
	    RegCloseKey(hKey);
	}
    } /* if ( rete ){ */

    return ( rete );
}

BOOL ReadPerlEnvironments( LPCSTR lpszIniFile )
{
    DWORD    ret;
    BOOL     retp=TRUE;
    char     *pt;
    LPSTR    lpFilePart;
    char     szEPBuf[256];
    char     szJPBuf[256];
    char     szPerlTemp[MAX_PATH];
    char     szFoundPerl[MAX_PATH];
#if 0
    char     szPP[MAX_PATH];
    char     szInstConf[MAX_PATH];
#endif

    /* Initialize */
    szPerl[0]    = 0;
    szPerlDir[0] = 0;
    szPerlBin[0] = 0;
    szPerlLib[0] = 0;

    /* perl existent check */
    ret = SearchPath( NULL,
		      "Perl.exe",
		      NULL,
		      sizeof(szFoundPerl),
		      szFoundPerl,
		      &lpFilePart );
    if ( ret == 0 ){
	sprintf(szEPBuf,"Perl.exe is not found in PATH.");
	sprintf(szJPBuf,"Perl.exe が実行パス上に見付かりません");
	BilErrorMessageBox( szEPBuf, szJPBuf );
	return ( FALSE );
    } /* if ( ret == 0 ){ */
#if 0
    /* query perl's path */
    printf("%s\n",szFoundPerl);
    _spawnl( _P_WAIT, szFoundPerl, szFoundPerl, "env.pl", NULL );
    pt = getenv( "TEMP" );
    if ( pt != NULL ){
	sprintf( szInstConf, "%s\\iminst.tmp", pt );
	GetPrivateProfileString( "Config", "PerlPath", "NONE",
				 szPP, sizeof(szPP), szInstConf );
	if ( strcmp( szPP, "NONE" ) )
	    strcpy( szFoundPerl, szPP );
	if ( ! fOutputDebug )
	    remove( szInstConf );
    } else {
	OutputDebugLog("environment variable TEMP is not found...");
    }

    strcpy(szPerl,szFoundPerl);	/* X:/Perl/5.0050x/bin/MSWin32-x86-object/perl.exe */
#endif
    /* X:/Perl/bin/perl.exe */
    strcpy(szPerlBin,szPerl);
    pt = strrchr( szPerlBin, '\\' );
    *pt = '\0';			/* X:/Perl/5.0050x/bin/MSWin32-x86-object */
    /* X:/Perl/bin */
    strcpy(szPerlDir,szPerlBin);

    sprintf(szPerlTemp,"%s\\..\\lib",szPerlDir);
    /* checking Perl type */
    if ( ! CheckDirectory( szPerlTemp ) ){
	/* ActivePerl < build 503 */
	pt = strrchr( szPerlDir, '\\' );
	if ( pt ) *pt = '\0';	/* X:/Perl/5.0050x/bin */
	pt = strrchr( szPerlDir, '\\' );
	if ( pt ) *pt = '\0';	/* X:/Perl/5.0050x */
	pt = strrchr( szPerlDir, '\\' );
	if ( pt ) *pt = '\0';	/* X:/Perl */
	sprintf(szPerlLib,"%s\\site\\lib",szPerlDir); /* X:/Perl/site/lib */
	if ( ! CheckDirectory( szPerlLib ) ){
	    if ( ! CreateDirectory( szPerlLib, NULL ) ){
		sprintf( szEPBuf, "Error: directory [%s] creation failure.", szPerlLib );
		OutputLog(szEPBuf);
		retp = FALSE;
	    } else {
		sprintf( szEPBuf, "[%s] is created.", szPerlLib );
		OutputDebugLog(szEPBuf);
	    }	/* if ( ! CreateDirectory( szPerlLib, NULL ) ){ */
	} /* if ( ! CheckDirectory( szPerlLib ) ){ */
    } else {
	sprintf(szPerlTemp,"%s\\..\\site",szPerlDir);
	if ( CheckDirectory( szPerlTemp ) ){
	    /* AcrivePerl >= build 503 */
	    pt = strrchr( szPerlDir, '\\' );
	    if ( pt ) *pt = '\0';	/* X:/Perl */
	    sprintf(szPerlLib,"%s\\site\\lib",szPerlDir); /* X:/Perl/site/lib */
	} else {
	    /* PL5404W0.ZIP */
	    strcpy( szPerl, szFoundPerl );
	    strcpy( szPerlBin, szFoundPerl );
	    pt = strrchr( szPerlBin, '\\' );
	    *pt = (char)0;
	    strcpy( szPerlDir, szPerlBin );
	    sprintf( szPerlLib, "%s\\..\\lib", szPerlBin );
	}
    } /* if ( retp ){ */

    return ( retp );
}



BOOL PrintEnvironments( VOID )
{
    char szBuf[256];
    char szPBuf[256];
    BOOL fFlag=TRUE;

    strcpy(szPBuf,"Operating System = [Windows ");
    switch ( VersionInfo.dwPlatformId ){
    case VER_PLATFORM_WIN32s:
	strcat(szPBuf,"3.1(Win32s)]\nWin32s is not supported.");
	fFlag = FALSE;
    case VER_PLATFORM_WIN32_WINDOWS:
	strcat(szPBuf,"95");
	break;
    case VER_PLATFORM_WIN32_NT:
	strcat(szPBuf,"NT");
	break;
    default:
	strcat(szPBuf,"unknown] : this platform id is not supported.");
	fFlag = FALSE;
    } /* switch ( VersionInfo.dwPlatformId ){ */
    if ( fFlag ){
	sprintf(szBuf," Version %ld.%ld ( Build ",
		VersionInfo.dwMajorVersion,VersionInfo.dwMinorVersion);
	strcat(szPBuf,szBuf);
	switch ( VersionInfo.dwPlatformId ){
	case VER_PLATFORM_WIN32_NT:
	    sprintf(szBuf,"%ld",VersionInfo.dwBuildNumber);
	    strcat(szPBuf,szBuf);
	    sprintf(szBuf," / %s",VersionInfo.szCSDVersion);
	    break;
	default:
	    sprintf(szBuf,"%d",LOWORD(VersionInfo.dwBuildNumber));
	    break;
	}
	strcat(szPBuf,szBuf);
	strcat(szPBuf," )]");
    } /* if ( fFlag ){ */
    OutputLog(szPBuf);

    OutputLog("");

    if ( fOutputDebug ){
	sprintf(szPBuf,"HomeDir=[%s]",szHomePath);
	OutputLog(szPBuf);
	sprintf(szPBuf,"ConfDir=[%s]",szConfPath);
	OutputLog(szPBuf);
	sprintf(szPBuf,"ConfFile=[%s]",szConfFile);
	OutputLog(szPBuf);
    } /* if ( fOutputDebug */

    OutputLog("");

    return ( TRUE );
}

BOOL PrintEmacsEnvironments( VOID )
{
    BOOL fFlag=TRUE;
    char szPBuf[256];

    strcpy(szPBuf,"Emacs is [");
    switch ( bEmacsType ){
    case EMACS_MEADOW:
	strcat(szPBuf,"Meadow");
	break;
    case EMACS_EMACS:
	strcat(szPBuf,"Emacs");
	break;
    case EMACS_XEMACS:
	strcat(szPBuf,"XEmacs");
	break;
    default:
	strcat(szPBuf,"unknown");
	break;
    } /* switch ( bEmacsType ){ */
    strcat(szPBuf,"]");
    OutputLog(szPBuf);
    
    sprintf(szPBuf,"Emacs=[%s]",szEmacs);
    OutputLog(szPBuf);
    sprintf(szPBuf,"EmacsPath=[%s]",szEmacsPath);
    OutputLog(szPBuf);
    sprintf(szPBuf,"EmacsLoadPath=[%s]",szEmacsLoadPath);
    OutputLog(szPBuf);
    sprintf(szPBuf,"InfoPath=[%s]",szInfoPath);
    OutputLog(szPBuf);

    OutputLog("");
  
    return ( TRUE );
}

BOOL PrintPerlEnvironments( VOID )
{
    char szPBuf[256];

    sprintf(szPBuf,"Perl=[%s]",szPerl);
    OutputLog(szPBuf);
    sprintf(szPBuf,"PerlDir=[%s]",szPerlDir);
    OutputLog(szPBuf);
    sprintf(szPBuf,"PerlBin=[%s]",szPerlBin);
    OutputLog(szPBuf);
    sprintf(szPBuf,"PerlLib=[%s]",szPerlLib);
    OutputLog(szPBuf);

    OutputLog("");

    return ( TRUE );
}
