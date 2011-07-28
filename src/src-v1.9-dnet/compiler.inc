{ **************************************************************************** }
{                                                                              }
{ Delphi/Kylix/FreePascal compiler directives                                  }
{ Brainstorm Defines Include Unit                                              }
{                                                                              }
{ Copyright (c) 1997-2009 Serg V. Zhdanovskih                                  }
{                                                                              }
{ **************************************************************************** }

{====================== Conditional defines defined here ======================}

//   DELPHI       - Delphi compiler.
//   DELPHI_NET   - dotNET Delphi compiler.
//   KYLIX        - Kylix compiler.
//   BP           - Borland Pascal
//   FREEPASCAL   - Free Pascal compiler.
//   TMTPASCAL    - TMT Pascal compiler.
//   VPASCAL      - Virtual Pascal.
//   OS_UNIX      - Unix type operating system.
//     OS_BSD     - BSD type operating system.
//     OS_LINUX   - Linux operating system.
//     OS_FREEBSD - FreeBSD operating system.
//   OS_MSDOS     - MS DOS operating system.
//   OS_MSWIN     - MS Windows type operating system.
//   OS_MSWIN_CE  - MS Windows Mobile type operating system.
//     WIN16      - Win16 operating system.
//     WIN32      - Win32 operating system.
//     WIN64      - Win64 operating system.
//   OS_OS2       - OS/2 operating system.
//   CPU_x86_32   - CPU 32 bit.
//   CPU_x86_64   - CPU 64 bit.

{ Turbo Pascal 1.0 }
{$IFDEF VER10}
{$ENDIF}

{ Turbo Pascal 2.0 }
{$IFDEF VER20}
{$ENDIF}

{ Turbo Pascal 3.0 }
{$IFDEF VER30}
{$ENDIF}

{ Turbo Pascal 4.0 }
{$IFDEF VER40}
{$ENDIF}

{ Turbo Pascal 5.0 }
{$IFDEF VER50}
{$ENDIF}

{ Turbo Pascal 6.0 }
{$IFDEF VER60}
{$ENDIF}

{ Borland Pascal 7.0 }
{$IFDEF VER70}
  {$DEFINE BP}
  {$DEFINE BP7}

  { defines for BP7 DOS real mode }
  {$IFDEF MSDOS}
    {$DEFINE OS_MSDOS}
    {$DEFINE REALMODE}
  {$ENDIF}

  { defines for BP7 DOS protected mode }
  {$IFDEF DPMI}
    {$DEFINE OS_MSDOS}
    {$DEFINE PROTECTEDMODE}
  {$ENDIF}

  { defines for BP7 Windows }
  {$IFDEF WINDOWS}
    {$DEFINE OS_MSWIN}
    {$DEFINE WIN16}
  {$ENDIF}
{$ENDIF}


{ Delphi 1 }
{$IFDEF VER80}
  {$DEFINE DELPHI}
  {$DEFINE WIN16}

  {$DEFINE DELPHI1}
  {$DEFINE DELPHI1UP}
{$ENDIF}


{ Delphi 2 }
{$IFDEF VER90}
  {$DEFINE DELPHI}
  {$DEFINE WIN32}
  {$DEFINE CPU_x86_32}

  {$DEFINE DELPHI2}
  {$DEFINE DELPHI1UP}
  {$DEFINE DELPHI2UP}
{$ENDIF}


{ Delphi 3 }
{$IFDEF VER100}
  {$DEFINE DELPHI}
  {$DEFINE WIN32}
  {$DEFINE CPU_x86_32}

  {$DEFINE DELPHI3}
  {$DEFINE DELPHI1UP}
  {$DEFINE DELPHI2UP}
  {$DEFINE DELPHI3UP}
{$ENDIF}


{ Delphi 4 }
{$IFDEF VER120}
  {$DEFINE DELPHI}
  {$DEFINE WIN32}
  {$DEFINE CPU_x86_32}

  {$DEFINE DELPHI4}
  {$DEFINE DELPHI1UP}
  {$DEFINE DELPHI2UP}
  {$DEFINE DELPHI3UP}
  {$DEFINE DELPHI4UP}
{$ENDIF}


{ Delphi 5 }
{$IFDEF VER130}
  {$DEFINE DELPHI}
  {$DEFINE WIN32}
  {$DEFINE CPU_x86_32}

  {$DEFINE DELPHI5}
  {$DEFINE DELPHI1UP}
  {$DEFINE DELPHI2UP}
  {$DEFINE DELPHI3UP}
  {$DEFINE DELPHI4UP}
  {$DEFINE DELPHI5UP}
{$ENDIF}


{ Delphi 6 }
{$IFDEF VER140}
  {$IFDEF LINUX}
    {$DEFINE KYLIX}
  {$ELSE}
    {$DEFINE DELPHI}
  {$ENDIF}

  {$DEFINE DELPHI6}
  {$DEFINE DELPHI1UP}
  {$DEFINE DELPHI2UP}
  {$DEFINE DELPHI3UP}
  {$DEFINE DELPHI4UP}
  {$DEFINE DELPHI5UP}
  {$DEFINE DELPHI6UP}
{$ENDIF}


{ Delphi 7 }
{$IFDEF VER150}
  {$IFDEF LINUX}
    {$DEFINE KYLIX}
  {$ELSE}
    {$DEFINE DELPHI}
  {$ENDIF}

  {$DEFINE DELPHI7}
  {$DEFINE DELPHI1UP}
  {$DEFINE DELPHI2UP}
  {$DEFINE DELPHI3UP}
  {$DEFINE DELPHI4UP}
  {$DEFINE DELPHI5UP}
  {$DEFINE DELPHI6UP}
  {$DEFINE DELPHI7UP}
{$ENDIF}


{ Delphi 8 }
{$IFDEF VER160}
  {$IFDEF LINUX}
    {$DEFINE KYLIX}
  {$ELSE}
    {$DEFINE DELPHI}
  {$ENDIF}

  {$DEFINE DELPHI8}
  {$DEFINE DELPHI1UP}
  {$DEFINE DELPHI2UP}
  {$DEFINE DELPHI3UP}
  {$DEFINE DELPHI4UP}
  {$DEFINE DELPHI5UP}
  {$DEFINE DELPHI6UP}
  {$DEFINE DELPHI7UP}
  {$DEFINE DELPHI8UP}

  {$DEFINE BDS}
  {$DEFINE BDS2}

  {$DEFINE BDS1UP}
  {$DEFINE BDS2UP}

  {$IFDEF CLR}
    {$DEFINE DELPHI_NET}
    {$DEFINE WIN32}
    {$DEFINE MSWINDOWS}
  {$ENDIF}
{$ENDIF}


{ Delphi 9 (2005) }
{$IFDEF VER170}
  {$IFDEF LINUX}
    {$DEFINE KYLIX}
  {$ELSE}
    {$DEFINE DELPHI}
  {$ENDIF}

  {$DEFINE DELPHI9}
  {$DEFINE DELPHI1UP}
  {$DEFINE DELPHI2UP}
  {$DEFINE DELPHI3UP}
  {$DEFINE DELPHI4UP}
  {$DEFINE DELPHI5UP}
  {$DEFINE DELPHI6UP}
  {$DEFINE DELPHI7UP}
  {$DEFINE DELPHI8UP}
  {$DEFINE DELPHI9UP}

  {$DEFINE BDS}
  {$DEFINE BDS3}

  {$DEFINE BDS1UP}
  {$DEFINE BDS2UP}
  {$DEFINE BDS3UP}

  {$IFDEF CLR}
    {$DEFINE DELPHI_NET}
    {$DEFINE WIN32}
    {$DEFINE MSWINDOWS}
  {$ENDIF}
{$ENDIF}


{ Delphi 10 (2006) }
{$IFDEF VER180}
  {$IFDEF LINUX}
    {$DEFINE KYLIX}
  {$ELSE}
    {$DEFINE DELPHI}
  {$ENDIF}

  {$DEFINE DELPHI10}
  {$DEFINE DELPHI1UP}
  {$DEFINE DELPHI2UP}
  {$DEFINE DELPHI3UP}
  {$DEFINE DELPHI4UP}
  {$DEFINE DELPHI5UP}
  {$DEFINE DELPHI6UP}
  {$DEFINE DELPHI7UP}
  {$DEFINE DELPHI8UP}
  {$DEFINE DELPHI9UP}
  {$DEFINE DELPHI10UP}

  {$DEFINE BDS}
  {$DEFINE BDS4}

  {$DEFINE BDS1UP}
  {$DEFINE BDS2UP}
  {$DEFINE BDS3UP}
  {$DEFINE BDS4UP}

  {$IFDEF CLR}
    {$DEFINE DELPHI_NET}
    {$DEFINE WIN32}
    {$DEFINE MSWINDOWS}
  {$ENDIF}

  {$IFDEF CF}
    {$DEFINE DELPHI_NET_CF}
  {$ENDIF}
{$ENDIF}

{ Delphi 11 }
{$IFDEF VER190}
  {$IFDEF LINUX}
    {$DEFINE KYLIX}
  {$ELSE}
    {$DEFINE DELPHI}
  {$ENDIF}

  {$DEFINE DELPHI11}
  {$DEFINE DELPHI1UP}
  {$DEFINE DELPHI2UP}
  {$DEFINE DELPHI3UP}
  {$DEFINE DELPHI4UP}
  {$DEFINE DELPHI5UP}
  {$DEFINE DELPHI6UP}
  {$DEFINE DELPHI7UP}
  {$DEFINE DELPHI8UP}
  {$DEFINE DELPHI9UP}
  {$DEFINE DELPHI10UP}
  {$DEFINE DELPHI11UP}

  {$DEFINE BDS}
  {$DEFINE BDS3}
  {$DEFINE BDS3UP}
{$ENDIF}


{ Kylix }
{$IFDEF LINUX}
  {$IFDEF CONDITIONALEXPRESSIONS}
    {$IF RTLVersion = 14.0}
      {$DEFINE KYLIX}
      {$DEFINE KYLIX1}
      {$DEFINE KYLIX1UP}
    {$IFEND}
    {$IF RTLVersion = 14.2}
      {$DEFINE KYLIX}
      {$DEFINE KYLIX2}
      {$DEFINE KYLIX1UP}
      {$DEFINE KYLIX2UP}
    {$IFEND}
    {$IF RTLVersion = 14.5}
      {$DEFINE KYLIX}
      {$DEFINE KYLIX3}
      {$DEFINE KYLIX1UP}
      {$DEFINE KYLIX2UP}
      {$DEFINE KYLIX3UP}
    {$IFEND}
  {$ENDIF}
{$ENDIF}


{ Virtual Pascal 2.x }
{$IFDEF VIRTUALPASCAL}
   {$DEFINE DELPHI}    { Use Delphi Syntax }
   {$DEFINE VP2}
{$ENDIF}


{ Free Pascal }
{$IFDEF FPC}
  {$DEFINE FREEPASCAL}

  {$MODE DELPHI}
  {$PACKRECORDS 4}
  {.$SMARTLINK ON}

  {$H+}
  {$R-} {- Range-Checking }

  {$IFDEF CPU32}
    {$DEFINE CPU_x86_32}
  {$ENDIF}

  {$IFDEF CPU64}
    {$DEFINE CPU_x86_64}
  {$ENDIF}

  {$IFDEF WINDOWS}
    {$DEFINE OS_MSWIN}
  {$ENDIF}

  {$IFDEF WINCE}
    {$DEFINE OS_MSWIN_CE}
  {$ENDIF}
{$ENDIF}


{ TMT Pascal }
{$IFDEF __TMT__}
  {$DEFINE TMTPASCAL}

  {$IFDEF MSDOS}
    {$DEFINE OS_MSDOS}
  {$ENDIF}
  {$IFDEF __DOS__}
    {$DEFINE OS_MSDOS}
  {$ENDIF}

  {$IFDEF __VER3__}
   {$DEFINE TMT3}
  {$ENDIF}

  {$IFDEF __VER4__}
   {$DEFINE TMT4}
  {$ENDIF}

  {$IFDEF __OS2__}
    {$DEFINE OS_OS2}
  {$ENDIF}

  {$IFDEF __WIN32__}
    {$DEFINE OS_MSWIN}
    {$DEFINE WIN32}
  {$ENDIF}
{$ENDIF}


{$IFDEF WIN32}
  {$IFNDEF MSWINDOWS}
    {$DEFINE MSWINDOWS}
  {$ENDIF}
{$ENDIF}

{ Operating system type }
{$IFDEF MSWINDOWS}
  {$DEFINE OS_MSWIN}

  {$IFDEF WIN32} { Alert }
  {$ENDIF}
{$ENDIF}
{$IFDEF LINUX}
  {$DEFINE UNIX}
  {$DEFINE OS_LINUX}
{$ENDIF}
{$IFDEF FREEBSD}
  {$DEFINE BSD}
  {$DEFINE OS_FREEBSD}
{$ENDIF}
{$IFDEF BSD}
  {$DEFINE UNIX}
  {$DEFINE OS_BSD}
{$ENDIF}
{$IFDEF UNIX}
  {$DEFINE OS_UNIX}
{$ENDIF}

{========================= Delphi compiler directives =========================}

{$IFDEF DELPHI}
  {$B-} {- Complete Boolean Evaluation }
  {$C+} {- Assert directives }
  {$H+} {- Use long strings by default }
  {$I+} {- Input/output checking }
  {$J-} {- Writeable structured consts }
  {$M+} {- Runtime type information }
  {$O+} {- Optimization}
  {$P+} {- Open string params }
  {$Q-} {- Overflow check}
  {$R-} {- Range-Checking }
  {$T-} {- Typed @ operator }
  {$V-} {- Var-String Checking }
  {$W+} {- Stack frames}
  {$X+} {- Extended syntax }
  {.$Y+/-} {- Symbol declaration }
{$ENDIF}

{$IFDEF DELPHI6UP}
  {$DEFINE SUPPORTS_DEPRECATED_WARNINGS}
  {$DEFINE SUPPORTS_PLATFORM_WARNINGS}
{$ENDIF}

{$IFDEF DELPHI7UP}
  {$DEFINE SUPPORTS_UNSAFE_WARNINGS}
{$ENDIF}

{$IFDEF SUPPORTS_DEPRECATED_WARNINGS}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN UNIT_DEPRECATED OFF}
{$ENDIF}

{$IFDEF SUPPORTS_PLATFORM_WARNINGS}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
{$ENDIF}

{$IFDEF SUPPORTS_UNSAFE_WARNINGS}
  {$WARN UNSAFE_CAST OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
{$ENDIF}

{$IFDEF KYLIX}
  {$J-} {- Writeable structured consts }
{$ENDIF}

{==============================================================================}

{$IFDEF FREEPASCAL}
  {$DEFINE NOVCL}
{$ENDIF}

{.$DEFINE PROFILE_NOVCL}

