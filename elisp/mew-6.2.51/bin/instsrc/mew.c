/*
 * Copyright (C) 1997-2006 Mew developing team.
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
 * Mew installer for Win32
 *
 *    Main routine.
 */

#include <windows.h>
#include <windowsx.h>
#include <stdio.h>
#include <stdlib.h>
#include <process.h>

#include "env.h"
#include "common.h"

#include "resource.h"


#define  COPYRIGHT_YEARS   "1997-2006"
#define  MEWINST_VERSION   "1.3.0"
#define  MEWINST_DEFAULT   "mew.ini"
#define  LOGFILE_NAME      "mew.log"

#define  MAX_EMACS         32

HINSTANCE hInst;
HANDLE    hConsole;

char  szCurrentPath[MAX_PATH];
char  szIniFile[MAX_PATH];

char  szBinDir[MAX_PATH];
char  szElispDir[MAX_PATH];
char  szObjs[BUFLEN];
char  szSrcs[BUFLEN];
char  szBins[BUFLEN];
char  szContribs[BUFLEN];
char  szTempFile[MAX_PATH];
char  szInfos[BUFLEN];

BOOL  fFull = FALSE;

char  szUserFullName[256];
char  szMailAddress[256];
char  szPopserver[256];
char  szPopUser[256];
BOOL  fApopAuth = TRUE;
char  szSmtpserver[256];
char  szUserName[256];
char  szDomainName[256];
char  szConfFileName[MAX_PATH];

int   iEmacsenNum;
char  szEmacsenList[MAX_EMACS][32];

char szMeadowSettings[4][128] = {
  "(setq load-path		; Meadow",
  "      (cons (expand-file-name (concat exec-directory \"../../site-lisp/mew\"))",
  "	       load-path))",
  "END"};
char szEmacsSettings[4][128] = {
  "(setq load-path		; NTEmacs",
  "      (cons (expand-file-name (concat exec-directory \"../site-lisp/mew\"))",
  "	       load-path))",
  "END"};

char  szSetupSettings[][128] = {
  "(autoload 'mew \"mew\" nil t)",
  "(autoload 'mew-send \"mew\" nil t)",
  "(autoload 'mew-user-agent-compose \"mew\" nil t)",
  "",
  ";; If using XEmacs/Emacs 21",
  ";(setq mew-icon-directory \"icon directory\")",
  "",
  ";; Optional setup (Read Mail menu for Emacs 21):",
  ";(if (boundp 'read-mail-command)",
  ";    (setq read-mail-command 'mew))",
  "",
  ";; Optional setup (e.g. C-xm for sending a message):",
  "(if (boundp 'mail-user-agent)",
  "    (setq mail-user-agent 'mew-user-agent))",
  "(if (fboundp 'define-mail-user-agent)",
  "    (define-mail-user-agent",
  "      'mew-user-agent",
  "      'mew-user-agent-compose",
  "      'mew-draft-send-letter",
  "      'mew-draft-kill",
  "      'mew-send-hook))",
  "",
  ";; backup message",
  ";(setq mew-fcc \"+backup\")",
  "",
  ";; do not show demo",
  ";(setq mew-demo nil)",
  "",
  ";; do not get mail when startup (offline mode)",
  ";(setq mew-auto-get nil)",
  "",
  ";; ignore limitation of message size",
  ";(setq mew-pop-size 0)",
  "",
  ";; print message",
  ";(setq mew-w32-prog-print     \"notepad.exe\")",
  ";(setq mew-w32-prog-print-arg \"/p\")",
  ";(setq mew-w32-cs-print       'shift_jis-dos)",
  ";(define-process-argument-editing \"/notepad\\.exe$\"",
  ";  (lambda (x)",
  ";    (general-process-argument-editing-function x nil t)))",
  "",
  ";; pick with japanese (Perl and mg is required).",
  ";(setq mew-prog-grep \"mg\")",
  ";(setq mew-prog-grep-opts '(\"-j\" \"jis\" \"-l\" \"-e\" \"-x\" \"&mime\"))",
  "",
  ";; mouse click and start web browser",
  ";(require 'mew-browse)",
  ";(define-key mew-message-mode-map [mouse-2] 'browse-url-at-mouse)",
  "",
  ";; enable password caching",
  ";(setq mew-use-cached-passwd t)",
  "",
  ";; show thread separator",
  ";(setq mew-use-thread-separator t)",
  "",
  ";; biff",
  ";(setq mew-use-biff t)",
  ";(setq mew-use-biff-bell t)",
  "",
  ";; unread",
  ";(setq mew-use-unread-mark nil)",
  "",
  ";; SMTP/POP3 over SSH with PuTTY",
  ";(setq mew-ssh-prog \"plink\")",
  "END"};

/******************************************************************
 *                     read from Makefile                         *
 ******************************************************************/
int GetLine(FILE *fp, char *buf,size_t len)
{
  size_t blen;

  if ( fgets( buf, len, fp) ){
    blen = strlen(buf);
    if ( buf[blen-1] < 0x20 ) buf[blen-1] = 0;
    if ( buf[blen-2] < 0x20 ) buf[blen-2] = 0;
    return ( TRUE );
  } else {
    return ( FALSE );
  }
}

void GetLines(FILE *fp, char *buf, size_t len, char *dstbuf)
{
  char   *pt;

  if ( (pt=strchr(buf,'\\')) == NULL ) return;
  *pt = 0;
  pt = strchr( buf, '=' );
  pt += 2;
  strcpy( dstbuf, pt);
  while ( GetLine(fp,buf,len) ){
    strcat( dstbuf, buf );
    if ( (pt=strchr(dstbuf,'\\')) == NULL ) return;
    *pt = 0;
  }
}


void GetLinesFromMakefile( void )
{
  FILE   *fp;
  char   buf[256];
  char   *pt;

  if ((fp=fopen("Makefile.in", "r")) == NULL){
      perror("Makefile.in");
      return;
  }
  if (!GetLine(fp, buf, sizeof(buf))) return;
  while (1){
    if ((pt=strchr(buf, '=')) != NULL){
      if (!strcmp(szObjs, "Default") &&
	  strstr(buf, "OBJS")){
	  GetLines(fp, buf, sizeof(buf), szObjs);
	  continue;
      }
      if (!strcmp(szSrcs, "Default") &&
	  strstr(buf, "SRCS")){
	  GetLines(fp, buf, sizeof(buf), szSrcs);
	  continue;
      }
      if (!strcmp(szTempFile, "Default") &&
	  strstr(buf, "TEMPFILE")){
	  pt += 2;
	  strcpy(szTempFile, pt);
      }
    }
    if (!GetLine(fp, buf, sizeof(buf))) return;
  }
  fclose(fp);
}



/******************************************************************
 *                     read Ini file                              *
 ******************************************************************/
BOOL ReadIniFile()
{
  BOOL     fError=FALSE;
  char     szBuf[256];
  char     szPBuf[4096];

  sprintf(szPBuf,"INI file = [%s]",szIniFile);
  OutputDebugLog(szPBuf);

  if ( ! ReadEmacsEnvironments(szIniFile) ) return ( FALSE );
  PrintEmacsEnvironments();

  GetPrivateProfileString("Make","Full","no",
			  szBuf,sizeof(szBuf),
			  szIniFile);
  if ( ! strcmp(szBuf,"yes") ) fFull = TRUE;

  GetPrivateProfileString("Make","BINDIR","Default",
			  szBinDir,sizeof(szBinDir),
			  szIniFile);
  if ( ! strcmp(szBinDir,"Default") )
    strcpy(szBinDir,szEmacsPath);
  sprintf(szPBuf,"BINDIR=[%s]",szBinDir);
  OutputDebugLog(szPBuf);
  
  GetPrivateProfileString("Make","ELISPDIR","Default",
			  szElispDir,sizeof(szElispDir),
			  szIniFile);
  if ( ! strcmp(szElispDir,"Default") )
    strcpy(szElispDir,szEmacsLoadPath);
  sprintf(szPBuf,"ELISPDIR=[%s]",szElispDir);
  OutputDebugLog(szPBuf);
  GetPrivateProfileString("Make","INFOPATH","Default",
			  szBuf,sizeof(szBuf),
			  szIniFile);
  if ( strcmp(szBuf,"Default") )
    strcpy(szInfoPath,szBuf);
  sprintf(szPBuf,"INFOPATH=[%s]",szInfoPath);
  OutputDebugLog(szPBuf);

  GetPrivateProfileString("Make","OBJS","Default",
			  szObjs,sizeof(szObjs),
			  szIniFile);
  GetPrivateProfileString("Make","SRCS","Default",
			  szSrcs,sizeof(szSrcs),
			  szIniFile);
  GetPrivateProfileString("Make","BINS","Default",
			  szBins,sizeof(szBins),
			  szIniFile);
  GetPrivateProfileString("Make","TEMPFILE","Default",
			  szTempFile,sizeof(szTempFile),
			  szIniFile);
  GetPrivateProfileString("Make","INFOS","Default",
			  szInfos,sizeof(szInfos),
			  szIniFile);
  GetPrivateProfileString("Make","CONTRIBS","Default",
			  szContribs,sizeof(szContribs),
			  szIniFile);

  GetLinesFromMakefile();

  if ( ! strcmp(szObjs,"Default") ){
    sprintf(szPBuf,"Error: OBJS is not found in [%s]",MEWINST_DEFAULT);
    OutputLog(szPBuf);
    fError = TRUE;
  }
  if ( ! strcmp(szSrcs,"Default") ){
    sprintf(szPBuf,"Error: SRCS is not found in [%s]",MEWINST_DEFAULT);
    OutputLog(szPBuf);
    fError = TRUE;
  }
  if ( ! strcmp(szTempFile,"Default") ){
    sprintf(szPBuf,"Error: TEMPFILE is not found in [%s]",MEWINST_DEFAULT);
    OutputLog(szPBuf);
    fError = TRUE;
  }
  if ( ! strcmp(szInfos,"Default") ){
    sprintf(szPBuf,"Error: INFOS is not found in [%s]",MEWINST_DEFAULT);
    OutputLog(szPBuf);
    fError = TRUE;
  }

  if ( fError ) return ( FALSE );

  sprintf(szPBuf,"Objs=[%s]",szObjs);
  OutputDebugLog(szPBuf);
  sprintf(szPBuf,"Srcs=[%s]",szSrcs);
  OutputDebugLog(szPBuf);
  sprintf(szPBuf,"Bins=[%s]",szBins);
  OutputDebugLog(szPBuf);
  sprintf(szPBuf,"TempFile=[%s]",szTempFile);
  OutputDebugLog(szPBuf);
  sprintf(szPBuf,"Infos=[%s]",szInfos);
  OutputDebugLog(szPBuf);
  sprintf(szPBuf,"Contribs=[%s]",szContribs);
  OutputDebugLog(szPBuf);

  return ( TRUE );
}


/******************************************************************
 *                     byte compile sources                       *
 ******************************************************************/
BOOL CompileSources( VOID )
{
  char szBuf[1024];
  char *token;
  char seps[] = " \t";
  char szTargetBuf[BUFLEN];
  FILE *fp;
  int ret;

  /* make temp.el */
  memcpy(szTargetBuf,szObjs,BUFLEN);
  fp = fopen(szTempFile,"w+");
  if ( fp == NULL ) return ( FALSE );
  fprintf(fp,"(setq load-path (cons \".\" load-path))\n(defun mew-compile () (mapcar (function (lambda (x) (byte-compile-file x))) (list ");
  token = strtok( szTargetBuf, seps );
  while ( token ){
    fprintf(fp,"\"");
    ReplaceString( token, ".elc", ".el");
    fprintf(fp,token);
    fprintf(fp,"\" ");
    token = strtok( NULL, seps );
  }
  fprintf(fp,")))\n");
  fclose(fp);

  /* compile sources */
  OutputDebugLog("Compiling sources...");
  sprintf(szBuf,
	  "%s -batch -q -no-site-file -l ./%s -f mew-compile",
	  szEmacs,szTempFile);
  OutputDebugLog(szBuf);
  ret = system(szBuf);
  if (ret != 0){
    sprintf(szBuf, "system() failed.(%d)\n", ret);
    OutputDebugLog(szBuf);
  }

  return ( TRUE );
}


/******************************************************************
 *                     instaill info files                        *
 ******************************************************************/
BOOL InstallInfo( VOID )
{
  char seps[] = " \t";
  char *token;
  BOOL fError;
  char szBuf[MAX_PATH],szBuf2[MAX_PATH];
  FILE *fp;
  char szPBuf[256];

  if ( szInfoPath[0] == 0 ){
    OutputLog("info directory is not exist!");
    return ( FALSE );
  }
  if ( ! CheckDirectory( szInfoPath ) ){
    sprintf(szPBuf,"[%s] is not exist!",szInfoPath);
    OutputLog(szPBuf);
    return ( FALSE );
  }

  fError = FALSE;

  token = strtok( szInfos, seps );
  while ( token ){
    sprintf(szBuf,"%s\\%s",szInfoPath,token);
    sprintf(szBuf2,"info\\%s",token);
    sprintf(szPBuf,"Copying [%s] to [%s] ...",szBuf2,szBuf);
    if ( ! CopyFile(szBuf2,szBuf,FALSE) ){
      strcat(szPBuf,"fail.");
      OutputLog(szPBuf);
      return ( FALSE );
    } else {
      strcat(szPBuf,"ok.");
      OutputDebugLog(szPBuf);
    }
    token = strtok( NULL, seps );
  }

  /* add Mew entry to "dir" */
  sprintf(szBuf,"%s\\dir",szInfoPath);

  fp = fopen(szBuf,"r");
  if ( fp == NULL ){
    sprintf(szPBuf,"cannot open(r) [%s].",szBuf);
    OutputLog(szPBuf);
    return ( FALSE );
  }
  fError = FALSE;
  while ( fgets(szBuf2,sizeof(szBuf2),fp) != NULL ){
    if ( memcmp(szBuf2,"* Mew:",6) == 0 ){
      fError = TRUE;
      break;
    }
  }
  if ( ! fError ){
    SetEOLCode( szBuf );
    fp = fopen(szBuf,"a+b");
    if ( fp == NULL ){
      sprintf(szPBuf,"cannot open(a+b) [%s].",szBuf);
      OutputLog(szPBuf);
      return ( FALSE );
    }
    fprintf(fp,"* Mew: (mew.info).      Messaging in the Emacs World (in English)%s", szCrCode );
    fprintf(fp,"* Mewj: (mew.jis.info). Messaging in the Emacs World (in Japanese)%s", szCrCode );
    fclose(fp);
  }
  return ( TRUE );
}


/******************************************************************
 *                     instaill image files                       *
 ******************************************************************/
BOOL InstallImage( VOID )
{
    char szExecuteCmd[MAX_PATH];
    char szPBuf[256];

    if ( ! CheckDirectory( szElispDir ) )
	return ( FALSE );

    /* copy image files to site-lisp/mew/etc */
    sprintf( szExecuteCmd, "xcopy /s /y /i etc %s\\etc", szElispDir );

    sprintf( szPBuf, "copying [etc] to [%s]...", szElispDir );
    OutputLog( szPBuf );

    /* XXX: no error checks. */
    system( szExecuteCmd );

    return ( TRUE );
}


/******************************************************************
 *                     installing Mew...                          *
 ******************************************************************/
VOID OutputErrorMessageLog( DWORD dwError )
{
	LPVOID lpMsgBuf;

	if (!FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER |
			   FORMAT_MESSAGE_FROM_SYSTEM |
			   FORMAT_MESSAGE_IGNORE_INSERTS,
			   NULL,
			   dwError,
			   MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
			   (LPTSTR)&lpMsgBuf,
			   0,
			   NULL )){
		/* formatting message is failed. */
		return;
	}

	OutputLog( lpMsgBuf );

	LocalFree( lpMsgBuf );
}

BOOL InstallMew( VOID )
{
  /* szSrcs/szObjs are destroyed */
  char *token;
  char seps[] = " \t";
  char szBuf[256],szBuf2[256],szSrcFile[MAX_PATH];
  BOOL fError = FALSE;
  char szPBuf[256], szPBuf2[32];
  DWORD dwError;

  /* Make directories(if not exist) */
  if ( ! CheckDirectory( szElispDir ) ){
    sprintf(szPBuf,"[%s] is not exist. creating...",szElispDir);
    if ( ! CreateDirectory( szElispDir, NULL ) ){
      dwError = GetLastError();
      sprintf(szPBuf2, "fail.(0x%08x)", dwError);
      strcat(szPBuf, szPBuf2);
      fError = TRUE;
    } else {
      strcat(szPBuf,"ok.");
    }
    OutputLog(szPBuf);
    if (fError)
      OutputErrorMessageLog(dwError);
  }
  if ( ! CheckDirectory( szBinDir ) ){
    sprintf(szPBuf,"[%s] is not exist. creating...",szBinDir);
    if ( ! CreateDirectory( szBinDir, NULL ) ){
      dwError = GetLastError();
      sprintf(szPBuf2, "fail.(0x%08x)", dwError);
      strcat(szPBuf, szPBuf2);
      fError = TRUE;
    } else {
      strcat(szPBuf,"ok.");
    }
    OutputLog(szPBuf);
    if (fError)
      OutputErrorMessageLog(dwError);
  }
  if ( fError ) return ( FALSE );

  /* Sources */
  token = strtok( szSrcs, seps );
  while ( token ){
    sprintf(szBuf,"%s\\%s",szElispDir,token);
    sprintf(szPBuf,"Copying [%s] to [%s] ...",token,szBuf);
    if ( ! CopyFile(token,szBuf,FALSE) ){
      dwError = GetLastError();
      sprintf(szPBuf2, "fail.(0x%08x)", dwError);
      strcat(szPBuf, szPBuf2);
      OutputLog(szPBuf);
      OutputErrorMessageLog(dwError);
      return ( FALSE );
    } else {
      strcat(szPBuf,"ok.");
      OutputDebugLog(szPBuf);
    }
    token = strtok( NULL, seps );
  }

  /* Objs */
  token = strtok( szObjs, seps );
  while ( token ){
    sprintf(szBuf,"%s\\%s",szElispDir,token);
    sprintf(szPBuf,"Copying [%s] to [%s] ...",token,szBuf);
    if ( ! CopyFile(token,szBuf,FALSE) ){
      dwError = GetLastError();
      sprintf(szPBuf2, "fail.(0x%08x)", dwError);
      strcat(szPBuf, szPBuf2);
      OutputLog(szPBuf);
      OutputErrorMessageLog(dwError);
      return ( FALSE );
    } else {
      strcat(szPBuf,"ok.");
      OutputDebugLog(szPBuf);
    }
    token = strtok( NULL, seps );
  }

  /* Contribs */
  if ( fFull && strcmp( szContribs, "Default" ) ){
    token = strtok( szContribs, seps );
    while ( token ){
      sprintf(szBuf,"%s\\%s",szElispDir,token);
      sprintf(szBuf2,"contrib\\%s",token);
      sprintf(szPBuf,"Copying [%s] to [%s] ...",szBuf2,szBuf);
      if ( ! CopyFile(szBuf2,szBuf,FALSE) ){
	dwError = GetLastError();
	sprintf(szPBuf2, "fail.(0x%08x)", dwError);
	strcat(szPBuf, szPBuf2);
	OutputLog(szPBuf);
	OutputErrorMessageLog(dwError);
	return ( FALSE );
      } else {
	strcat(szPBuf,"ok.");
	OutputDebugLog(szPBuf);
      }
      if ( langId == LANGID_JAPANESE ){
	sprintf(szPBuf,">>非公式パッケージ [%s] がインストールされました",szBuf2);
      } else {
	sprintf(szPBuf,">>UNOFFICIAL package [%s] is installed.",szBuf2);
      }
      OutputLog(szPBuf);
      token = strtok( NULL, seps );
    }
  }

  /* Bins */
  token = strtok( szBins, seps );
  while ( token ){
    sprintf(szBuf,"%s\\%s",szBinDir,token);
    sprintf(szSrcFile,"bin\\%s",token);
    sprintf(szPBuf,"Copying [%s] to [%s] ...",szSrcFile,szBuf);
    if ( ! CopyFile(szSrcFile,szBuf,FALSE) ){
      dwError = GetLastError();
      sprintf(szPBuf2, "fail.(0x%08x)", dwError);
      strcat(szPBuf, szPBuf2);
      OutputLog(szPBuf);
      OutputErrorMessageLog(dwError);
      return ( FALSE );
    } else {
      strcat(szPBuf,"ok.");
      OutputDebugLog(szPBuf);
    }
    token = strtok( NULL, seps );
  }

  return ( TRUE );
}


/******************************************************************
 *                        Setup                                   *
 ******************************************************************/
BOOL CALLBACK SetupMewDlgProc( HWND   hDlg,
			       UINT   uiMsg,
			       WPARAM wParam,
			       LPARAM lParam )
{
  BOOL fRet = TRUE;
  char *pt;
  char szFileName[MAX_PATH];
  OPENFILENAME of;

  switch ( uiMsg ){
  case WM_INITDIALOG:
    CheckRadioButton( hDlg, IDC_POP_APOP, IDC_POP_APOP, IDC_POP_APOP );
    ImmAssociateContext(GetDlgItem(hDlg, IDC_USERNAME), (HIMC)NULL);
    ImmAssociateContext(GetDlgItem(hDlg, IDC_MAILADDRESS), (HIMC)NULL);
    ImmAssociateContext(GetDlgItem(hDlg, IDC_SMTPSERVER), (HIMC)NULL);
    ImmAssociateContext(GetDlgItem(hDlg, IDC_POPSERVER), (HIMC)NULL);
    ImmAssociateContext(GetDlgItem(hDlg, IDC_POPUSER), (HIMC)NULL);
    SetDlgItemText(hDlg, IDC_CONFIGFILE, (LPCTSTR)lParam);
    ShowWindow( hDlg, SW_SHOW );
    UpdateWindow( hDlg );
    break;

  case WM_COMMAND:
    switch ( GET_WM_COMMAND_ID( wParam, lParam ) ){
    case IDC_FILESELECT:
	memset(szFileName, 0, sizeof(szFileName));
	memset(&of, 0, sizeof(of));
	of.lStructSize = sizeof(of);
	of.hwndOwner   = hDlg;
	of.lpstrFilter = "All Files(*.*)\0*.*\0";
	of.lpstrFile   = szFileName;
	of.nMaxFile    = MAX_PATH;
	of.Flags       = OFN_FILEMUSTEXIST|OFN_LONGNAMES|OFN_PATHMUSTEXIST;
	of.lpstrTitle  = "Select .emacs or .mew file.";
	if (GetOpenFileName(&of))
	    SetDlgItemText(hDlg, IDC_CONFIGFILE, szFileName);
	break;
    case IDOK:
      GetDlgItemText( hDlg, IDC_USERNAME, szUserFullName, sizeof(szUserFullName) );
      if ( ! strcmp( szUserFullName, "" ) ){
	BilErrorMessageBox( "User Name is empty.",
			    "User Name の欄が空白です" );
	break;
      }
      GetDlgItemText( hDlg, IDC_MAILADDRESS, szMailAddress, sizeof(szMailAddress) );
      if ( ! strcmp( szMailAddress, "" ) ){
	BilErrorMessageBox( "Mail Address is empty.",
			    "Mail Address の欄が空白です" );
	break;
      }
      pt = strchr( szMailAddress, '@' );
      if ( pt != NULL ){
	*pt = '\0';
	if ( *(pt+1) != '\0' ){
	  strcpy(szUserName,szMailAddress);
	  strcpy(szDomainName, pt+1 );
	  *pt = '@';
	} /* if ( *(pt+1) != '\0' ){ */
      } else {
	BilErrorMessageBox( "Invalid mail address.",
			    "メールアドレスが不正です" );
	SetDlgItemText( hDlg, IDC_MAILADDRESS, "" );
	fRet = FALSE;
	break;
      } /* if ( pt != NULL ){ */
      GetDlgItemText( hDlg, IDC_SMTPSERVER, szSmtpserver, sizeof(szSmtpserver) );
      if ( ! strcmp( szSmtpserver, "" ) ){
	BilErrorMessageBox( "SMTP Server is empty.",
			    "SMTP Server の欄が空白です" );
	break;
      }
      GetDlgItemText( hDlg, IDC_POPSERVER, szPopserver, sizeof(szPopserver) );
      if ( ! strcmp( szPopserver, "" ) ){
	BilErrorMessageBox( "POP3 Server is empty.",
			    "POP3 Server の欄が空白です" );
	break;
      }
      GetDlgItemText( hDlg, IDC_POPUSER, szPopUser, sizeof(szPopUser) );
      if ( ! strcmp( szPopUser, "" ) ){
	BilErrorMessageBox( "POP3 Username is empty.",
			    "POP3 Username の欄が空白です" );
	break;
      }
      if (IsDlgButtonChecked( hDlg, IDC_POP_APOP ) != BST_CHECKED){
	  fApopAuth = FALSE;
      }
      GetDlgItemText(hDlg, IDC_CONFIGFILE,
		     szConfFileName, sizeof(szConfFileName));
      EndDialog( hDlg, 0 );
      break;
    case IDCANCEL:
      fRet = FALSE;
      EndDialog( hDlg, -1 );
      break;
    }
  default:
    fRet = FALSE;
    break;
  } /* switch ( uiMsg ){ */
  
  return ( fRet );
}


/* setup for IM ... Window Version ? */
BOOL SetupMew( VOID )
{
  FILE *fp;
  int  i;
  char szBuf[MAX_PATH];
  char szPBuf[256];
#if 0
  char *pt;
#endif

  /* check Mew installation */
  sprintf(szBuf,"%s\\mew.el",szElispDir);
  if ( ! CheckFile( szBuf ) ){
    BilErrorMessageBox( "Mew is not installed.",
			"Mew がインストールれていません。" );
    return ( FALSE );
  }

  sprintf(szBuf,"%s\\.emacs",szHomePath);

  if ( DialogBoxParam( hInst,
		       MAKEINTRESOURCE(IDD_SETUP),
		       NULL,
		       SetupMewDlgProc,
		       (LPARAM)szBuf ) != 0 )
      return ( FALSE );

  SetEOLCode( szConfFileName );

  /* add configuration to ~/.emacs file */

  fp = fopen(szConfFileName,"a+b");
  if ( fp == NULL ){
    sprintf(szPBuf,"cannot open(a+b) [%s].",szConfFileName);
    OutputLog(szPBuf);
    return ( FALSE );
  }

  fprintf( fp, "%s%s", szCrCode, szCrCode );
  fprintf( fp, ";;; Mew Easy Settings (generated automatically)%s", szCrCode );

#if 0
  i=0;
  while ( 1 ){
    switch ( bEmacsType ){
    case EMACS_EMACS:
      pt = szEmacsSettings[i];
      break;
    case EMACS_MEADOW:
    default:
      pt = szMeadowSettings[i];
      break;
    }
    if ( !strcmp( pt, "END" ) ) break;
    fprintf( fp, "%s%s",(LPCSTR)pt, szCrCode );
    i++;
  }
  fprintf( fp, "%s", szCrCode );
#endif
  fprintf( fp, ";; configuration%s", szCrCode );
  fprintf( fp, "(setq mew-config-alist%s", szCrCode );
  fprintf( fp, "    '((\"default\"%s", szCrCode );
  fprintf( fp, "       (\"name\"        . \"%s\")%s", szUserFullName, szCrCode );
  fprintf( fp, "       (\"user\"        . \"%s\")%s", szUserName, szCrCode );
  fprintf( fp, "       (\"mail-domain\" . \"%s\")%s", szDomainName, szCrCode );
  fprintf( fp, "       (\"smtp-server\" . \"%s\")%s", szSmtpserver,  szCrCode );
  fprintf( fp, "       (\"pop-server\"  . \"%s\")%s", szPopserver, szCrCode );
  if (!fApopAuth)
      fprintf( fp, "       (\"pop-auth\"    . pass)%s", szCrCode );
  fprintf( fp, "       (\"pop-user\"    . \"%s\"))%s", szPopUser, szCrCode );
  fprintf( fp, "      ))%s", szCrCode );
  fprintf( fp, "%s", szCrCode );

  i=0;
  while ( strcmp( szSetupSettings[i], "END" ) ){
    fprintf( fp, "%s%s",(LPCSTR)szSetupSettings[i], szCrCode );
    i++;
  }
  fclose(fp);

  return ( TRUE );
}


/******************************************************************
 *                      Selecting Emacsen                         *
 ******************************************************************/
BOOL CALLBACK SelectEmacsenDlgProc( HWND   hDlg,
				    UINT   uiMsg,
				    WPARAM wParam,
				    LPARAM lParam )
{
  BOOL fRet = TRUE;
  int  nItem;
  int  i;
  HWND hwndList;

  switch ( uiMsg ){
  case WM_INITDIALOG:
    hwndList = GetDlgItem( hDlg, IDC_SELIST );
    for ( i=0; i<iEmacsenNum; i++ ){
      SendMessage( hwndList, LB_ADDSTRING, 0, (LPARAM)szEmacsenList[i] );
      SendMessage( hwndList, LB_SETITEMDATA, i, i );
    }
    SendMessage( hwndList, LB_SETCURSEL, 0, 0 );
    if ( langId == LANGID_JAPANESE )
      SetDlgItemText( hDlg, IDC_SESTATIC, "Emacs の種類を選択して下さい" );
    ShowWindow( hDlg, SW_SHOW );
    UpdateWindow( hDlg );
    break;
    
  case WM_COMMAND:
    switch ( GET_WM_COMMAND_ID( wParam, lParam ) ){
    case IDOK:
      hwndList = GetDlgItem( hDlg, IDC_SELIST );
      nItem = SendMessage( hwndList, LB_GETCURSEL, 0, 0 );
      EndDialog( hDlg, nItem );
      break;
    case IDCANCEL:
      EndDialog( hDlg, -1);
      fRet = FALSE;
      break;
    }
  default:
    fRet = FALSE;
    break;
  } /* switch ( uiMsg ){ */
  return ( fRet );
}


BOOL SelectEmacsen( VOID )
{
  int      nIndex;
  LONG     rret;
  HKEY     hKey,hKey2;
  DWORD    i;
  char     szKeyBuf[260];
  char     szValueBuf[1024];
  DWORD    dwKeyBuf,dwValueBuf;
  FILETIME ft;
  char     *pszEDir;

  /* gathering Emacs infomation... */
  iEmacsenNum=0;
  memset( szEmacsenList, sizeof(szEmacsenList), 0 );

  /* NTEmacs/Meadow */
  pszEDir = getenv("emacs_dir");
  if (pszEDir) {
	  do {
		  /* check NTEmacs */
		  strcpy(szValueBuf, pszEDir);
		  RevConvertPathSeparator(szValueBuf);
		  if (szValueBuf[strlen(szValueBuf)-1] != '\\')
			  strcat(szValueBuf, "\\");
		  strcat(szValueBuf, "bin\\emacs.exe");
		  if (CheckFile(szValueBuf)) {
			  strcpy(szEmacsenList[iEmacsenNum], "NTEmacs (getenv)" );
			  iEmacsenNum++;
		  }
		  /* check Meadow */
		  strcpy(szValueBuf, pszEDir);
		  RevConvertPathSeparator(szValueBuf);
		  if (szValueBuf[strlen(szValueBuf)-1] != '\\')
			  strcat(szValueBuf, "\\");
		  strcat(szValueBuf, "bin\\Meadow.exe");
		  if (CheckFile(szValueBuf)) {
			  strcpy(szEmacsenList[iEmacsenNum], "Meadow (getenv)" );
			  iEmacsenNum++;
		  }
	  } while (0);
  }

  /* NTEmacs */
  rret = RegOpenKey( HKEY_LOCAL_MACHINE,
		     "SOFTWARE\\GNU\\Emacs",
		     &hKey );
  if ( rret == ERROR_SUCCESS ){
    strcpy( szEmacsenList[iEmacsenNum], "NTEmacs" );
    dwValueBuf = sizeof(szValueBuf);
    if ( RegQueryValueEx( hKey,
			  "EMACSPATH",
			  NULL,
			  NULL,
			  szValueBuf,
			  &dwValueBuf ) == ERROR_SUCCESS ){
      dwKeyBuf = sizeof(szKeyBuf);
      if ( RegQueryValueEx( hKey,
			    "emacs_dir",
			    NULL,
			    NULL,
			    szKeyBuf,
			    &dwKeyBuf ) == ERROR_SUCCESS ){
	ReplaceString( szValueBuf, "%emacs_dir%", szKeyBuf );
	RevConvertPathSeparator( szValueBuf );
	strcat( szValueBuf, "\\emacs.exe" );
	if ( CheckFile( szValueBuf ) )
	  iEmacsenNum++;
      }
    } /* if ( RegQueryValueEx( */
    RegCloseKey( hKey );
  }
  /* Meadow */
  i=0;
  rret = RegOpenKey( HKEY_LOCAL_MACHINE,
		     "SOFTWARE\\GNU\\Meadow",
		     &hKey );
  if ( rret == ERROR_SUCCESS ){
    dwKeyBuf = sizeof(szKeyBuf);
    dwValueBuf = sizeof(szValueBuf);
    rret = RegEnumKeyEx( hKey,
			 i++,
			 szKeyBuf,
			 &dwKeyBuf,
			 NULL,
			 szValueBuf,
			 &dwValueBuf,
			 &ft );
    if ( rret == ERROR_SUCCESS ){
      do {
	if ( !strcmp( szKeyBuf, "Environment" ) ){
	  /* 1.00/1.01 */
	  strcpy( szValueBuf, "SOFTWARE\\GNU\\Meadow\\Environment" );
	  strcpy( szEmacsenList[iEmacsenNum], "Meadow 1.00/1.01" );
	} else {
	  /* 1.04a1 or later */
	  sprintf( szValueBuf, "SOFTWARE\\GNU\\Meadow\\%s\\Environment",
		   szKeyBuf);
	  sprintf( szEmacsenList[iEmacsenNum], "Meadow %s", szKeyBuf );
	}
	/* "Meadow.exe" exists? */
	if ( RegOpenKey( HKEY_LOCAL_MACHINE,
			 szValueBuf,
			 &hKey2 ) == ERROR_SUCCESS ){
	  dwKeyBuf = sizeof(szKeyBuf);
	  if ( RegQueryValueEx( hKey2,
				"EMACSPATH",
				NULL,
				NULL,
				szKeyBuf,
				&dwKeyBuf ) == ERROR_SUCCESS ){
	    sprintf( szValueBuf, "%s\\Meadow.exe", szKeyBuf );
	    if ( CheckFile( szValueBuf ) )
	      iEmacsenNum++;
	  } /* if ( RegQueryValueEx( */
	  RegCloseKey( hKey2 );
	} /* if ( RegOpenKey( */
	dwKeyBuf = sizeof(szKeyBuf);
	dwValueBuf = sizeof(szValueBuf);
	if ( iEmacsenNum >= MAX_EMACS ) break;
      } while ( RegEnumKeyEx( hKey,
			      i++,
			      szKeyBuf,
			      &dwKeyBuf,
			      NULL,
			      szValueBuf,
			      &dwValueBuf,
			      &ft ) == ERROR_SUCCESS );
    }
    RegCloseKey( hKey );
  }

  /* IF NOT FOUND ANY EMACSEN */
  if ( iEmacsenNum == 0 ) return ( FALSE );

  if ( iEmacsenNum > 1 ){
    /* select dialog */
    nIndex = DialogBoxParam( hInst,
			     MAKEINTRESOURCE(IDD_SEDIALOG),
			     0,
			     SelectEmacsenDlgProc,
			     0 );
    if ( nIndex < 0 ) return ( FALSE );
  } else {
    /* only one Emacs has found */
    nIndex = 0;
  }
  if (strstr(szEmacsenList[nIndex], "getenv"))
    bNoEmacsRegistry = TRUE;
  if ( ! strncmp( szEmacsenList[nIndex], "NTEmacs", 7 ) ){
    bEmacsType = EMACS_EMACS;
  } else if ( ! strncmp( szEmacsenList[nIndex], "Meadow", 6 ) ){
    bEmacsType = EMACS_MEADOW;
    if ( ! strncmp( szEmacsenList[nIndex], "Meadow 1.00", 11 ) ){
      strcpy( szMeadowVersion, "1.00" );
    } else {
      strcpy( szMeadowVersion, szEmacsenList[nIndex]+7 );
    }
  } else {
    bEmacsType = EMACS_NONE;
  }
  
  /* refrect setting */

  return ( TRUE );
}


VOID ShowUsage( VOID )
{
  char buf[65536];

  sprintf(buf,
	  "Mew installer for Win32  Version %s\n"
	  "          Copyright (C) %s  Shuichi Kitaguchi\n"
	  "Usage : mew.exe [options]\n"
	  "Option:\n"
	  "        -h         show this message.\n"
	  "        -cd/-nc  installation only (not compile)\n"
	  "        -ni         compilation only (not install)\n",
	  MEWINST_VERSION,COPYRIGHT_YEARS
	  );
  MessageBox( NULL, buf, "Usage", MB_OK|MB_SETFOREGROUND|MB_ICONINFORMATION);
}


BOOL CALLBACK TitleDlgProc(HWND   hDlg,
			   UINT   uiMsg,
			   WPARAM wParam,
			   LPARAM lParam)
{
    switch (uiMsg){
    case WM_INITDIALOG:
	break;
    case WM_COMMAND:
	switch (GET_WM_COMMAND_ID(wParam, lParam)){
	case IDOK:
	    EndDialog(hDlg, 1);
	    return TRUE;
	    break;
	case IDCANCEL:
	    EndDialog(hDlg, 0);
	    return TRUE;
	    break;
	}
    default:
	return FALSE;
    }
    return FALSE;
}



/******************************************************************
 *                     main                                       *
 ******************************************************************/
int WINAPI WinMain( HINSTANCE hInstance,
		    HINSTANCE hPrevInstance,
		    LPSTR     lpCmdLine,
		    int       nCmdShow )
{
  BOOL fLogTime = FALSE;
  BOOL fCompile = TRUE;
  BOOL fInstall = TRUE;
  BOOL fSetup   = FALSE;
  BOOL fQuiet   = FALSE;
  char szPBuf[256];
  char szEPBuf[256];
  char szJPBuf[256];
  char seps[] = " \t";
  char *token;
  char     *pt;
  HMODULE  hModule;

  hInst = hInstance;
  memset( szMeadowVersion, sizeof(szMeadowVersion), 0 );

  token = strtok( lpCmdLine, seps );
  while ( token ){
    if ( ! strcmp(token,"-cd") ||
	 ! strcmp(token,"-nc") )
      fCompile = FALSE;		/* install only (not compile) */
    if ( ! strcmp(token,"-ni") )
      fInstall = FALSE;		/* compile only (not install) */
    if ( ! strcmp(token,"-s") )
      fSetup = TRUE;		/* setup mode */
    if ( ! strcmp(token,"-q") )
      fQuiet = TRUE;		/* quiet mode */
    if ( ! strcmp(token,"-h") ){
      ShowUsage();
      return 0;
    }      
    token = strtok( NULL, seps );
  } /* while ( token ){ */

  /* Setup log module
   * After this, OutputLog function could be used.
   */
  SetupLog( LOGFILE_NAME, TRUE, TRUE, fLogTime, TRUE, 0, fQuiet );

  /* Startup Log Message */
  sprintf(szPBuf,"Mew installer for Win32  Version %s",MEWINST_VERSION);
  OutputLog(szPBuf);
  sprintf(szPBuf,"          Copyright (C) %s  Shuichi Kitaguchi",
	  COPYRIGHT_YEARS);
  OutputLog(szPBuf);
  OutputLog("");

  /* Startup window */
  if (!fQuiet && !DialogBoxParam(hInst,
				 MAKEINTRESOURCE(IDD_TITLE),
				 0, TitleDlgProc, 0)){
      OutputLog("Installation is canceled or title window is failed to create.");
      return -1;
  }
  
  /* Ini file name */
  hModule = GetModuleHandle(NULL);
  GetModuleFileName(hModule,szCurrentPath,sizeof(szCurrentPath));
  if ( (pt = strrchr(szCurrentPath,'\\')) )
    *pt = (char)0;
  sprintf(szIniFile,"%s\\%s",szCurrentPath,MEWINST_DEFAULT);


#if 0  
  /* parent process's console? xxx */
  AllocConsole();
  hConsole = CreateConsoleScreenBuffer( GENERIC_READ|GENERIC_WRITE,
					0,
					NULL,
					CONSOLE_TEXTMODE_BUFFER,
					NULL );
  if ( hConsole == INVALID_HANDLE_VALUE ) goto Error;
  if ( ! SetStdHandle( STD_OUTPUT_HANDLE, hConsole ) ){
    OutputLog("SetStdhandle fails.");
    goto Error;
  }
  if ( ! SetConsoleActiveScreenBuffer( hConsole ) ){
    OutputLog("SetConsoleActiveScreenBuffer fails.");
    goto Error;
  }
#endif

  if ( ! GetEnvironments() ) goto Error;
  if ( ! ReadEnvironments() ) goto Error;
  PrintEnvironments();

  if ( ! CheckFile(MEWINST_DEFAULT) ){
    sprintf(szEPBuf,"[%s] is not found!",MEWINST_DEFAULT);
    sprintf(szJPBuf,"[%s] が見付かりません!",MEWINST_DEFAULT);
    BilErrorMessageBox( szEPBuf, szJPBuf );
    goto Error;
  }
  /*
  if ( ! CheckFile("mew.dot.emacs") ){
    BilErrorMessageBox( "Archive Extraction Error!\nCheck your archiver program(support long filename?).",
			"アーカイブの解凍に失敗しています\n解凍プログラムがロングファイルネームをサポートしているか、確認して下さい");
    goto Error;
  }
  */
  /* selecting Emacs type */
  if ( CheckFile( szIniFile ) ){
    GetPrivateProfileString("Make","EMACS","Default",
			    szPBuf,sizeof(szPBuf),
			    szIniFile);
    if ( !strcmp( szPBuf, "Default" ) && !fQuiet ){
      if ( ! SelectEmacsen() ){
	BilErrorMessageBox( "Any Emacsen cannot found!\nHas Emacs been installed correctly?",
			    "Emacs が見付かりませんでした\nEmacs を正しくインストールしましたか?" );
	goto Error;
      }
    }
  } /* if ( CheckFile( szIniFile ) ) */
  
  if ( langId == LANGID_JAPANESE ){
    OutputLog(">>INI ファイルを読み込んでいます...");
  } else {
    OutputLog(">>Read INI File...");
  }
  if ( ! ReadIniFile() ) goto Error;

  if ( langId == LANGID_JAPANESE )
    strcpy(szPBuf,">>>Emacs は [");
  else
    strcpy(szPBuf,">>>Emacs is [");
  switch ( bEmacsType ){
  case EMACS_MEADOW:
    strcat(szPBuf,"Meadow ");
    strcat(szPBuf,szMeadowVersion);
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
  }
  if ( langId == LANGID_JAPANESE )
    strcat(szPBuf,"] です");
  else
    strcat(szPBuf,"]");
  OutputLog(szPBuf);

#if 0				/* XXX: */
  if ( fSetup ){
    if ( langId == LANGID_JAPANESE ){
      OutputLog(">>設定を生成しています...");
    } else {
      OutputLog(">>Creating configuration...");
    }
    if ( ! SetupMew() ) goto Error;
    BilInfoMessageBox( "Add configuration to your ~/.emacs file.\nPlease check it.",
		       "~/.emacs ファイルに設定を追加しました\n確認して下さい");
    return ( 0 );
  }
#endif
  
  if ( fCompile ){
    if ( langId == LANGID_JAPANESE ){
      OutputLog(">>ソースをコンパイルしています...");
    } else {
      OutputLog(">>Compiling sources...");
    }
    if ( ! CompileSources() ) goto Error;
    
    if ( langId == LANGID_JAPANESE ){
      OutputLog(">>実行ファイルを作成しています...");
    } else {
      OutputLog(">>Making Executables...");
    }
  } /* if ( fCompile ){ */
  
  if ( fInstall ){
    if ( langId == LANGID_JAPANESE ){
      OutputLog(">>el/elc/実行 ファイルをインストールしています...");
    } else {
      OutputLog(">>Installing el/elc/executable Files...");
    }
    if ( ! InstallMew() ) goto Error;
    
    if ( langId == LANGID_JAPANESE ){
      OutputLog(">>画像ファイルをインストールしています...");
    } else {
      OutputLog(">>Installing Image files...");
    }
    if ( ! InstallImage() ) goto Error;

    if ( langId == LANGID_JAPANESE ){
      OutputLog(">>Info ファイルをインストールしています...");
    } else {
      OutputLog(">>Installing Info files...");
    }
    if ( ! InstallInfo() ) goto Error;
  } /* if ( fInstall ){ */

#if 0				/* XXX: */
  if ( fInstall && !fQuiet ){
      if ( BilMessageBox( NULL,
			  "Do you want to setup your ~/.emacs?",
			  "~/.emacs の設定を行ないますか?",
			  "Question", MB_YESNO ) == IDYES ){
	  if ( ! SetupMew() ) goto Error;
	  BilInfoMessageBox( "Add configuration to your ~/.emacs file.\nPlease check it.",
			     "~/.emacs ファイルに設定を追加しました\n確認して下さい");
      }
  }
#endif

  if ( fInstall )
    BilInfoMessageBox( "Mew installation complete",
		       "Mew のインストールが終了しました" );

  if ( langId == LANGID_JAPANESE ){
    OutputLog(">>Mew のインストールが終了しました");
  } else {
    OutputLog(">>Mew installation complete.");
  }

  return ( 0 );

 Error:
  BilErrorMessageBox( "Mew installation is NOT complete!\nCheck mew.log file",
		      "Mew のインストールが正常に終了しませんでした。mew.logファイルをチェックしてください" );

  return ( -1 );
}
