unit GKMediaView; {trans:fin}

{$I GEDKeeper2.inc}

interface

uses
  System.Drawing, System.ComponentModel, System.Windows.Forms, System.Text,
  System.IO,
  VCLStub, GedCom551, GKBase, GKUtils, GKEngine, GKLangs;

type
  TfmMediaView = class(System.Windows.Forms.Form)
  strict private
    FBase: TfmBase;
    FFileRef: TGEDCOMFileReferenceWithTitle;
    FExtern: Boolean;

    procedure SetFileRef(const Value: TGEDCOMFileReferenceWithTitle);
    procedure TfmMediaView_KeyDown(sender: System.Object; e: System.Windows.Forms.KeyEventArgs);
    procedure InitializeComponent;
  public
    constructor Create(aBase: TfmBase);

    property Base: TfmBase read FBase;
    property Extern: Boolean read FExtern;
    property FileRef: TGEDCOMFileReferenceWithTitle read FFileRef write SetFileRef;
  end;

implementation

procedure TfmMediaView.InitializeComponent;
begin
  //
  // TfmMediaView
  //
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 14);
  Self.ClientSize := System.Drawing.Size.Create(792, 573);
  Self.Font := System.Drawing.Font.Create('Tahoma', 8.25, System.Drawing.FontStyle.Regular,
      System.Drawing.GraphicsUnit.Point, (Byte(204)));
  Self.FormBorderStyle := System.Windows.Forms.FormBorderStyle.FixedDialog;
  Self.KeyPreview := True;
  Self.Name := 'TfmMediaView';
  Self.ShowInTaskbar := False;
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'Просмотр';
  Include(Self.KeyDown, Self.TfmMediaView_KeyDown);
end;

constructor TfmMediaView.Create(aBase: TfmBase);
begin
  inherited Create;
  InitializeComponent;

  FBase := aBase;
end;

procedure TfmMediaView.TfmMediaView_KeyDown(sender: System.Object; e: System.Windows.Forms.KeyEventArgs);
begin
  if (e.KeyCode = Keys.Escape) then Close;
end;

procedure TfmMediaView.SetFileRef(const Value: TGEDCOMFileReferenceWithTitle);
var
  target_fn: string;
  fs: TStream;
  ctl: System.Windows.Forms.Control;
  strd: System.IO.StreamReader;
  {$IFNDEF DELPHI_NET}
  wb: TWebBrowser;
  {$ENDIF}
begin
  FFileRef := Value;
  fs := nil;

  FExtern := False;
  Text := FFileRef.Title;

  case FFileRef.MultimediaFormat of
    mfNone, mfOLE, mfUnknown: ;

    mfBMP, mfGIF, mfJPG, mfPCX, mfTIF, mfTGA, mfPNG: begin
      Base.Engine.MediaLoad(FFileRef.StringValue, target_fn);

      ctl := PictureBox.Create();
      ctl.Dock := DockStyle.Fill;
      PictureBox(ctl).Image.FromFile(target_fn);
      Controls.Add(ctl);
    end;

    mfWAV, mfAVI, mfMPG: begin
      FExtern := True;
      Base.Engine.MediaLoad(FFileRef.StringValue, target_fn);
      TGKUtils.LoadExtFile(target_fn);
    end;

    mfTXT: begin
      Base.Engine.MediaLoad(FFileRef.StringValue, fs);
      strd := StreamReader.Create(TStreamToCLRStream.GetStream(fs));

      ctl := TextBox.Create();
      ctl.Dock := DockStyle.Fill;
      TextBox(ctl).ReadOnly := True;
      TextBox(ctl).ScrollBars := ScrollBars.Both;
      TextBox(ctl).Text := strd.ReadToEnd.ToString();
      Controls.Add(ctl);
    end;

    mfRTF: begin
      Base.Engine.MediaLoad(FFileRef.StringValue, fs);
      strd := StreamReader.Create(TStreamToCLRStream.GetStream(fs));

      ctl := RichTextBox.Create();
      ctl.Dock := DockStyle.Fill;
      RichTextBox(ctl).ReadOnly := True;
      RichTextBox(ctl).Text := strd.ReadToEnd.ToString();
      Controls.Add(ctl);
    end;

    mfHTM: begin
      Base.Engine.MediaLoad(FFileRef.StringValue, target_fn);

      {$IFNDEF DELPHI_NET}
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
