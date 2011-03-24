unit GKMediaView; {prepare:fin}

{$I GEDKeeper.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  ComCtrls, ToolWin, ExtCtrls, GedCom551, GKBase;

type
  TfmMediaView = class(TForm)
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FFileRef: TGEDCOMFileReferenceWithTitle;
    FExtern: Boolean;

    function GetBase: TfmBase;
    procedure SetFileRef(const Value: TGEDCOMFileReferenceWithTitle);
  public
    property Base: TfmBase read GetBase;
    property Extern: Boolean read FExtern;
    property FileRef: TGEDCOMFileReferenceWithTitle read FFileRef write SetFileRef;
  end;

implementation

uses
  GKUtils, GKEngine
  {$IFNDEF DELPHI_NET}, GraphicEx, SHDocVw{$ENDIF};

{$R *.dfm}

procedure TfmMediaView.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then Close;
end;

function TfmMediaView.GetBase(): TfmBase;
begin
  Result := TfmBase(Owner);
end;

procedure TfmMediaView.SetFileRef(const Value: TGEDCOMFileReferenceWithTitle);
var
  target_fn: string;
  fs: TStream;
  {$IFNDEF DELPHI_NET}
  wb: TWebBrowser;
  {$ENDIF}
begin
  FFileRef := Value;
  fs := nil;

  FExtern := False;

  case FFileRef.MultimediaFormat of
    mfNone, mfOLE, mfUnknown: ;

    mfBMP, mfGIF, mfJPG, mfPCX, mfTIF, mfTGA, mfPNG: begin
      Base.MediaLoad(FFileRef.StringValue, target_fn);

      with TImage.Create(Self) do begin
        Parent := Self;
        Align := alClient;
        Center := True;
        Proportional := True;
        Picture.LoadFromFile(target_fn);
        if (Picture.Width > Width) or (Picture.Height > Height)
        then Stretch := True;
      end;
    end;

    mfWAV, mfAVI, mfMPG: begin
      FExtern := True;
      Base.MediaLoad(FFileRef.StringValue, target_fn);

      LoadExtFile(target_fn);
    end;

    mfTXT: begin
      Base.MediaLoad(FFileRef.StringValue, fs);

      with TMemo.Create(Self) do begin
        Parent := Self;
        Align := alClient;
        ReadOnly := True;
        ScrollBars := ssBoth;
        Lines.LoadFromStream(fs);
      end;
    end;

    mfRTF: begin
      Base.MediaLoad(FFileRef.StringValue, fs);

      with TRichEdit.Create(Self) do begin
        Parent := Self;
        Align := alClient;
        ReadOnly := True;
        ScrollBars := ssBoth;
        Lines.LoadFromStream(fs);
      end;
    end;

    mfHTM: begin
      {$IFNDEF DELPHI_NET}
      Base.MediaLoad(FFileRef.StringValue, target_fn);

      wb := TWebBrowser.Create(Self);
      TWinControl(wb).Parent := Self;
      wb.Align := alClient;
      wb.Navigate(target_fn);
      {$ENDIF}
    end;
  end;

  if Assigned(fs) then fs.Free;
end;

end.
