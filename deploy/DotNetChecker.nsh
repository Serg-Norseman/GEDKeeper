!include LogicLib.nsh
!define LOCAL_DOTNET_INSTALLER_LOCATION $TEMP\dotnet.exe

!macro CheckNetFramework FrameworkVersion ; This just takes the version, if the installer restarts in the middle it does.
	!insertmacro CheckNetFrameworkWithRestartOptions ${FrameworkVersion} false $R0
!macroend

!macro CheckNetFrameworkDelayRestart FrameworkVersion ReturnDotNetInstalled ; This just takes the version, prevents a restart, and returns if an install happened to the caller can then handle a reboot later.
	!insertmacro CheckNetFrameworkWithRestartOptions ${FrameworkVersion} true ${ReturnDotNetInstalled}
!macroend

;https://dotnet.microsoft.com/en-us/download/dotnet-framework/net481
;https://learn.microsoft.com/en-us/dotnet/framework/migration-guide/versions-and-dependencies

;https://go.microsoft.com/fwlink/?linkid=2203306  DEV download
;https://go.microsoft.com/fwlink/?LinkId=2203304 WEb Download

!macro GetFrameworkUrlAndReadableVersions FrameworkVersion FrameworkUrl FrameworkReadableVersion
	!ifndef DOTNET481_URL
		!define DOTNET481_URL        "https://go.microsoft.com/fwlink/?LinkId=2203306"
		!define DOTNET48_URL        "https://go.microsoft.com/fwlink/?LinkId=2088631"
		!define DOTNET472_URL       "https://go.microsoft.com/fwlink/?LinkId=863265"
		!define DOTNET471_URL       "https://go.microsoft.com/fwlink/?LinkId=852104"
		!define DOTNET47_URL        "https://go.microsoft.com/fwlink/?LinkId=825302"
		!define DOTNET462_URL       "https://go.microsoft.com/fwlink/?LinkId=780600"
		!define DOTNET461_URL       "https://go.microsoft.com/fwlink/?LinkId=671743"
		!define DOTNET46_URL        "https://go.microsoft.com/fwlink/?LinkId=528232"
		!define DOTNET452_URL       "https://go.microsoft.com/fwlink/?LinkId=397708"
		!define DOTNET451_URL       "https://go.microsoft.com/fwlink/?LinkId=322116"
		!define DOTNET45_URL        "https://go.microsoft.com/fwlink/?LinkId=225702"
		!define DOTNET40Full_URL    "https://www.microsoft.com/downloads/info.aspx?na=41&srcfamilyid=0a391abd-25c1-4fc0-919f-b21f31ab88b7&srcdisplaylang=en&u=http%3a%2f%2fdownload.microsoft.com%2fdownload%2f9%2f5%2fA%2f95A9616B-7A37-4AF6-BC36-D6EA96C8DAAE%2fdotNetFx40_Full_x86_x64.exe"
		!define DOTNET40Client_URL  "https://www.microsoft.com/downloads/info.aspx?na=41&srcfamilyid=e5ad0459-cbcc-4b4f-97b6-fb17111cf544&srcdisplaylang=en&u=http%3a%2f%2fdownload.microsoft.com%2fdownload%2f5%2f6%2f2%2f562A10F9-C9F4-4313-A044-9C94E0A8FAC8%2fdotNetFx40_Client_x86_x64.exe"
		!define DOTNET35_URL        "https://download.microsoft.com/download/2/0/e/20e90413-712f-438c-988e-fdaa79a8ac3d/dotnetfx35.exe"
		!define DOTNET30_URL        "https://download.microsoft.com/download/2/0/e/20e90413-712f-438c-988e-fdaa79a8ac3d/dotnetfx35.exe"
		!define DOTNET20_URL        "https://www.microsoft.com/downloads/info.aspx?na=41&srcfamilyid=0856eacb-4362-4b0d-8edd-aab15c5e04f5&srcdisplaylang=en&u=http%3a%2f%2fdownload.microsoft.com%2fdownload%2f5%2f6%2f7%2f567758a3-759e-473e-bf8f-52154438565a%2fdotnetfx.exe"
		!define DOTNET11_URL        "https://www.microsoft.com/downloads/info.aspx?na=41&srcfamilyid=262d25e3-f589-4842-8157-034d1e7cf3a3&srcdisplaylang=en&u=http%3a%2f%2fdownload.microsoft.com%2fdownload%2fa%2fa%2fc%2faac39226-8825-44ce-90e3-bf8203e74006%2fdotnetfx.exe"
		!define DOTNET10_URL        "https://www.microsoft.com/downloads/info.aspx?na=41&srcfamilyid=262d25e3-f589-4842-8157-034d1e7cf3a3&srcdisplaylang=en&u=http%3a%2f%2fdownload.microsoft.com%2fdownload%2fa%2fa%2fc%2faac39226-8825-44ce-90e3-bf8203e74006%2fdotnetfx.exe"
	!endif

	${If} ${FrameworkVersion} == "481"
		StrCpy ${FrameworkUrl} ${DOTNET481_URL}
		StrCpy ${FrameworkReadableVersion} "4.8.1"
	${ElseIf} ${FrameworkVersion} == "48"
		StrCpy ${FrameworkUrl} ${DOTNET48_URL}
		StrCpy ${FrameworkReadableVersion} "4.8"		
	${ElseIf} ${FrameworkVersion} == "472"
		StrCpy ${FrameworkUrl} ${DOTNET472_URL}
		StrCpy ${FrameworkReadableVersion} "4.7.2"
	${ElseIf} ${FrameworkVersion} == "471"
		StrCpy ${FrameworkUrl} ${DOTNET471_URL}
		StrCpy ${FrameworkReadableVersion} "4.7.1"
	${ElseIf} ${FrameworkVersion} == "47"
		StrCpy ${FrameworkUrl} ${DOTNET47_URL}
		StrCpy ${FrameworkReadableVersion} "4.7"
	${ElseIf} ${FrameworkVersion} == "462"
		StrCpy ${FrameworkUrl} ${DOTNET462_URL}
		StrCpy ${FrameworkReadableVersion} "4.6.2"
	${ElseIf} ${FrameworkVersion} == "461"
		StrCpy ${FrameworkUrl} ${DOTNET461_URL}
		StrCpy ${FrameworkReadableVersion} "4.6.1"
	${ElseIf} ${FrameworkVersion} == "46"
		StrCpy ${FrameworkUrl} ${DOTNET46_URL}
		StrCpy ${FrameworkReadableVersion} "4.6"
	${ElseIf} ${FrameworkVersion} == "452"
		StrCpy ${FrameworkUrl} ${DOTNET452_URL}
		StrCpy ${FrameworkReadableVersion} "4.52"
	${ElseIf} ${FrameworkVersion} == "451"
		StrCpy ${FrameworkUrl} ${DOTNET451_URL}
		StrCpy ${FrameworkReadableVersion} "4.51"
	${ElseIf} ${FrameworkVersion} == "45"
		StrCpy ${FrameworkUrl} ${DOTNET45_URL}
		StrCpy ${FrameworkReadableVersion} "4.5"
	${ElseIf} ${FrameworkVersion} == "40Full"
		StrCpy ${FrameworkUrl} ${DOTNET40Full_URL}
		StrCpy ${FrameworkReadableVersion} "4.0 Full"
	${ElseIf} ${FrameworkVersion} == "40Client"
		StrCpy ${FrameworkUrl} ${DOTNET40Client_URL}
		StrCpy ${FrameworkReadableVersion} "4.0 Client"
	${ElseIf} ${FrameworkVersion} == "35"
		StrCpy ${FrameworkUrl} ${DOTNET35_URL}
		StrCpy ${FrameworkReadableVersion} "3.5"
	${ElseIf} ${FrameworkVersion} == "30"
		StrCpy ${FrameworkUrl} ${DOTNET30_URL}
		StrCpy ${FrameworkReadableVersion} "3.0"
	${ElseIf} ${FrameworkVersion} == "20"
		StrCpy ${FrameworkUrl} ${DOTNET20_URL}
		StrCpy ${FrameworkReadableVersion} "2.0"
	${ElseIf} ${FrameworkVersion} == "11"
		StrCpy ${FrameworkUrl} ${DOTNET11_URL}
		StrCpy ${FrameworkReadableVersion} "1.1"
	${ElseIf} ${FrameworkVersion} == "10"
		StrCpy ${FrameworkUrl} ${DOTNET10_URL}
		StrCpy ${FrameworkReadableVersion} "1.0"
	${EndIf}
!macroend

Var DidDotNetInstall
Var dotNetUrl
Var dotNetReadableVersion

!macro CheckNetFrameworkWithRestartOptions FrameworkVersion DelayRestart ReturnDidDotNetInstall
	!insertmacro GetFrameworkUrlAndReadableVersions ${FrameworkVersion} $dotNetUrl $dotNetReadableVersion

	DetailPrint "Checking .NET Framework version..."

	Push $0
	Push $1
	Push $2
	Push $3
	Push $4
	Push $5
	Push $6
	Push $7

	StrCpy $DidDotNetInstall "false"
	DotNetChecker::IsDotNet${FrameworkVersion}Installed
	Pop $0

	${If} $0 == "false"
	${OrIf} $0 == "f"  ; if script is compiled in ANSI mode then we get only an "f"  https://github.com/ReVolly/NsisDotNetChecker/issues/4
		DetailPrint ".NET Framework $dotNetReadableVersion not found, download is required for program to run."
		Goto NoDotNET${FrameworkVersion}${DelayRestart}
	${Else}
		DetailPrint ".NET Framework $dotNetReadableVersion found, no need to install."
		Goto NewDotNET${FrameworkVersion}${DelayRestart}
	${EndIf}

NoDotNET${FrameworkVersion}${DelayRestart}:
	MessageBox MB_YESNOCANCEL|MB_ICONEXCLAMATION \
	".NET Framework not installed. Required version: $dotNetReadableVersion.$\nDownload .NET Framework $dotNetReadableVersion from microsoft.com?" \
	/SD IDYES IDYES DownloadDotNET${FrameworkVersion}${DelayRestart} IDNO NewDotNET${FrameworkVersion}${DelayRestart}
	goto GiveUpDotNET${FrameworkVersion}${DelayRestart} ;IDCANCEL

DownloadDotNET${FrameworkVersion}${DelayRestart}:
	; Load the stack with URL and Readable Version number
    Push $dotNetUrl
	Push $dotNetReadableVersion
	Call DownloadDotNet
	Pop $0
	${If} $0 == "halt"
		Goto GiveUpDotNET${FrameworkVersion}${DelayRestart}
	${ElseIf} $0 == "success"
		Push ${DelayRestart}
		Call InstallDotNet
		Pop $DidDotNetInstall
		${If} $DidDotNetInstall == "true"
			Call CleanupDotNetInstaller
		${EndIf}
	${EndIf}
	Goto NewDotNET${FrameworkVersion}${DelayRestart}

GiveUpDotNET${FrameworkVersion}${DelayRestart}:
	Abort "Installation canceled by user."

NewDotNET${FrameworkVersion}${DelayRestart}:
	DetailPrint "Proceeding with remainder of installation."
	Pop $7
	Pop $6
	Pop $5
	Pop $4
	Pop $3
	Pop $2
	Pop $1
	Pop $0

	StrCpy ${ReturnDidDotNetInstall} $DidDotNetInstall
!macroend

; Expects the url followed by the readable version number to be pushed onto stack
; Returns on the stack:
; 	"success" 	== Downloaded file ready to install
; 	"failed" 	== Download failed, but use would like to continue
; 	"halt" 		== Download failed, and user doesn't want to continue.
Function "DownloadDotNet"
	Pop $1
	Pop $0

	DetailPrint "Beginning download of .NET Framework $1."
	NSISDL::download $0 "${LOCAL_DOTNET_INSTALLER_LOCATION}"
	DetailPrint "Completed download."

	Pop $0
	${If} $0 == "cancel"
		Push false
		MessageBox MB_YESNO|MB_ICONEXCLAMATION \
		"Download canceled.  Continue Installation?" \
		IDYES Continue IDNO Halt
	${ElseIf} $0 != "success"
		Push false
		MessageBox MB_YESNO|MB_ICONEXCLAMATION \
		"Download failed:$\n$0$\n$\nContinue Installation?" \
		IDYES Continue IDNO Halt
	${Else}
		Push "success"
		Goto End
	${EndIf}

Continue:
	Push "failed"
	Goto End

Halt:
	Push "halt"

End:
FunctionEnd

; Expects if we are delaying the restart
; Returns on the stack:
; 	"true" 	== Installer ran
; 	"false" == Currently no path where this happens.
Function "InstallDotNet"
	Pop $0
	DetailPrint "Pausing installation while downloaded .NET Framework installer runs."
	${If} $0 == "true"
		StrCpy $1 "/norestart"
	${Else}
		StrCpy $1 ""
	${EndIf}

	ExecWait '${LOCAL_DOTNET_INSTALLER_LOCATION} /q $1 /c:"install /q"'
	Push "true"
FunctionEnd

; Expects if we are delaying the restart
; Returns on the stack:
; 	"true" 	== Installer ran
; 	"false" == Currently no path where this happens.
Function "CleanupDotNetInstaller"
	DetailPrint "Completed .NET Framework install/update. Removing .NET Framework installer."
	Delete "${LOCAL_DOTNET_INSTALLER_LOCATION}"
	DetailPrint ".NET Framework installer removed."
FunctionEnd
