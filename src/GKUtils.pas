unit GKUtils;

interface

// Установить русский язык
procedure SetRU;
// Установить английский язык
procedure SetEN;

function ProgramIsRegistered(): Boolean;
procedure RegisterProgram(Registering: Boolean);
function ExtIsRegistered(fExt, fName: string): Boolean;
procedure RegisterExt(fExt, fName, fDesc: string; iIndex: Integer; Registering: Boolean);

implementation

uses
  SysUtils, Windows, Registry, ShellAPI, ComObj, ShlObj, ActiveX;

// Установить русский язык
procedure SetRU;
var
  {$IFNDEF OS_MSWIN_CE}
  Layout: array [0..KL_NAMELENGTH] of Char;
  {$ELSE}
  ws: PWideChar;
  {$ENDIF}
begin
  {$IFNDEF OS_MSWIN_CE}
  LoadKeyboardLayout(StrCopy(Layout, '00000419'), KLF_ACTIVATE);
  {$ELSE}
  ws := PCharToPWideChar('00000419');
  LoadKeyboardLayout(ws, KLF_ACTIVATE);
  FreeMem(ws);
  {$ENDIF}
end;

// Установить английский язык
procedure SetEN;
var
  {$IFNDEF OS_MSWIN_CE}
  Layout: array [0..KL_NAMELENGTH] of Char;
  {$ELSE}
  ws: PWideChar;
  {$ENDIF}
begin
  {$IFNDEF OS_MSWIN_CE}
  LoadKeyboardLayout(StrCopy(Layout, '00000409'), KLF_ACTIVATE);
  {$ELSE}
  ws := PCharToPWideChar('00000409');
  LoadKeyboardLayout(ws, KLF_ACTIVATE);
  FreeMem(ws);
  {$ENDIF}
end;

function ProgramIsRegistered(): Boolean;
var
  reg: TRegistry;
begin
  Result := False;

  try
    reg := TRegistry.Create;
    try
      reg.RootKey := HKEY_LOCAL_MACHINE;
      reg.OpenKey('Software\Microsoft\Windows\CurrentVersion\App Paths', True);

      Result := reg.KeyExists(ExtractFileName(ParamStr(0)));
    finally
      reg.Free;
    end;
  except
  end;
end;

procedure RegisterProgram(Registering: Boolean);
var
  reg: TRegistry;
begin
  try
    reg := TRegistry.Create;
    try
      reg.RootKey := HKEY_LOCAL_MACHINE;
      reg.OpenKey('Software\Microsoft\Windows\CurrentVersion\App Paths', True);

      if Registering then begin
        reg.OpenKey(ExtractFileName(ParamStr(0)), True);
        reg.WriteString('', ParamStr(0));
        reg.WriteString('Path', ExtractFilePath(ParamStr(0)));
      end else begin
        reg.DeleteKey(ExtractFileName(ParamStr(0)));
      end;
    finally
      reg.Free;
    end;
  except
  end;
end;

function ExtIsRegistered(fExt, fName: string): Boolean;
var
  reg: TRegistry;
begin
  Result := False;

  try
    reg := TRegistry.Create;
    try
      reg.RootKey := HKEY_CLASSES_ROOT;
      Result := reg.KeyExists(fExt) and reg.KeyExists(fName);
    finally
      reg.Free;
    end;
  except
  end;
end;

procedure RegisterExt(fExt, fName, fDesc: string; iIndex: Integer; Registering: Boolean);
var
  reg: TRegistry;
  ef: Longword;
begin
  ef := 0;
  try
    reg := TRegistry.Create;
    try
      reg.RootKey := HKEY_CLASSES_ROOT;
      if Registering then begin
        reg.OpenKey(fExt, True);
        reg.WriteString('', fName);
        reg.CloseKey;

        reg.OpenKey(fName, True);
        reg.WriteString('', fDesc);
        reg.WriteBinaryData('EditFlags', ef, 4);

        reg.OpenKey('DefaultIcon', True);
        reg.WriteString('', ParamStr(0)+','+IntToStr(iIndex));
        reg.CloseKey;

        reg.OpenKey(fName, True);
        reg.OpenKey('ShellNew', True);
        reg.WriteString('NullFile', '');
        reg.CloseKey;

        reg.OpenKey(fName, True);
        reg.OpenKey('shell', True);
        reg.WriteString('', 'open');
        reg.OpenKey('open', True);
        reg.WriteString('', '&Открыть');
        reg.OpenKey('command', True);
        reg.WriteString('', ParamStr(0) + ' "%1"');
        reg.CloseKey;
      end else begin
        reg.DeleteKey(fExt);
        reg.DeleteKey(fName);
      end;
    finally
      reg.Free;
    end;
  except
  end;
end;

end.
