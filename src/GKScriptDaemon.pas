unit GKScriptDaemon; {prepare:fin; trans:fin}

{$I GEDKeeper.inc}

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  ToolWin, GKBase, GKLangs;

type
  TfmScriptDaemon = class(TForm, ILocalization)
    ToolBar1: TToolBar;
    btnLoadScript: TToolButton;
    ToolButton2: TToolButton;
    btnRun: TToolButton;
    mmDebugOutput: TMemo;
    Splitter1: TSplitter;
    mmScriptText: TMemo;
    OpenDialog1: TOpenDialog;
    btnSaveScript: TToolButton;
    SaveDialog1: TSaveDialog;
    btnNewScript: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnNewScriptClick(Sender: TObject);
    procedure btnLoadScriptClick(Sender: TObject);
    procedure btnSaveScriptClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure mmScriptTextChange(Sender: TObject);
  private
    FFileName: string;
    FModified: Boolean;

    function CheckModified(): Boolean;
    function GetBase(): TfmBase;
    procedure SetFileName(const Value: string);
    procedure SetModified(const Value: Boolean);
    procedure SetTitle();
  public
    property Base: TfmBase read GetBase;
    property FileName: string read FFileName write SetFileName;
    property Modified: Boolean read FModified write SetModified;

    procedure SetLang();
  end;

var
  fmScriptDaemon: TfmScriptDaemon;

implementation

{$R *.dfm}

uses
  {$IFDEF DELPHI_NET} System.IO, {$ENDIF}
  GKUtils, GKEngineAPI, GKMain;

procedure TfmScriptDaemon.FormCreate(Sender: TObject);
begin
  btnNewScriptClick(nil);
  SetLang();
end;

procedure TfmScriptDaemon.FormDestroy(Sender: TObject);
begin
  //
end;

procedure TfmScriptDaemon.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if not(CheckModified())
  then Action := caNone
  else begin
    Action := caFree;
  end;
end;

function TfmScriptDaemon.GetBase(): TfmBase;
begin
  Result := TfmBase(Owner);
end;

procedure TfmScriptDaemon.mmScriptTextChange(Sender: TObject);
begin
  Modified := True;
end;

procedure TfmScriptDaemon.SetFileName(const Value: string);
begin
  FFileName := Value;
  SetTitle();
end;

procedure TfmScriptDaemon.SetLang();
begin

end;

procedure TfmScriptDaemon.SetModified(const Value: Boolean);
begin
  FModified := Value;
  SetTitle();
end;

procedure TfmScriptDaemon.SetTitle();
begin
  Caption := ExtractFileName(FFileName);

  if FModified
  then Caption := '* ' + Caption;
end;

procedure TfmScriptDaemon.btnNewScriptClick(Sender: TObject);
begin
  if CheckModified() then begin
    mmScriptText.Lines.Clear;
    FileName := 'unknown.lua';
    Modified := False;
  end;
end;

procedure TfmScriptDaemon.btnLoadScriptClick(Sender: TObject);
begin
  if CheckModified() and (OpenDialog1.Execute) then begin
    mmScriptText.Lines.LoadFromFile(OpenDialog1.FileName);
    FileName := OpenDialog1.FileName;
    Modified := False;
  end;
end;

procedure TfmScriptDaemon.btnSaveScriptClick(Sender: TObject);
begin
  SaveDialog1.FileName := FileName;
  if SaveDialog1.Execute then begin
    mmScriptText.Lines.SaveToFile(SaveDialog1.FileName);
    FileName := SaveDialog1.FileName;
    Modified := False;
  end;
end;

procedure TfmScriptDaemon.btnRunClick(Sender: TObject);
begin
  mmDebugOutput.Clear;
  try
    lua_run(mmScriptText.Lines.Text, Base, fmScriptDaemon.mmDebugOutput.Lines);
  except
    on E: Exception do LogWrite('GKScriptDaemon.Run(): ' + E.Message);
  end;
end;

function TfmScriptDaemon.CheckModified(): Boolean;
begin
  Result := True;

  if Modified then begin
    case MessageDlg(LSList[LSID_FileSaveQuery], mtWarning, [mbYes, mbNo, mbCancel], 0) of
      mrYes: btnSaveScriptClick(nil);
      mrNo: {dummy};
      mrCancel: Result := False;
    end;
  end;
end;

end.
