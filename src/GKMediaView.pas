unit GKMediaView;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GedCom551, StdCtrls, ComCtrls, ExtCtrls, GraphicEx, MPlayer,
  ToolWin, OleCtrls, WMPLib_TLB, ActiveX, GKBase, SHDocVw;

type
  TfmMediaView = class(TForm)
    PageControl: TPageControl;
    SheetText: TTabSheet;
    mText: TMemo;
    SheetRTF: TTabSheet;
    RichEdit: TRichEdit;
    SheetImage: TTabSheet;
    Image1: TImage;
    SheetPlayer: TTabSheet;
    WindowsMediaPlayer1: TWindowsMediaPlayer;
    SheetHTML: TTabSheet;
    WebBrowser1: TWebBrowser;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SheetPlayerResize(Sender: TObject);
  private
    FFileRef: TGEDCOMFileReferenceWithTitle;

    function GetBase: TfmBase;
    procedure SetFileRef(const Value: TGEDCOMFileReferenceWithTitle);
  public
    property Base: TfmBase read GetBase;
    property FileRef: TGEDCOMFileReferenceWithTitle read FFileRef write SetFileRef;
  end;

var
  fmMediaView: TfmMediaView;

implementation

uses GKCommon;

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
begin
  FFileRef := Value;
  fs := nil;

  case FFileRef.MultimediaFormat of
    mfNone, mfOLE, mfUnknown: ;

    mfBMP, mfGIF, mfJPG, mfPCX, mfTIF, mfTGA, mfPNG: begin
      PageControl.ActivePage := SheetImage;

      Base.MediaLoad(FFileRef.StringValue, target_fn);
      Image1.Picture.LoadFromFile(target_fn);
    end;

    mfWAV, mfAVI, mfMPG: begin
      PageControl.ActivePage := SheetPlayer;

      Base.MediaLoad(FFileRef.StringValue, target_fn);
      WindowsMediaPlayer1.URL := (WideString(target_fn));
    end;

    mfTXT: begin
      PageControl.ActivePage := SheetText;

      Base.MediaLoad(FFileRef.StringValue, fs);
      mText.Lines.LoadFromStream(fs);
    end;

    mfRTF: begin
      PageControl.ActivePage := SheetRTF;

      Base.MediaLoad(FFileRef.StringValue, fs);
      RichEdit.Lines.LoadFromStream(fs);
    end;

    mfHTM: begin
      PageControl.ActivePage := SheetHTML;

      Base.MediaLoad(FFileRef.StringValue, target_fn);
      WebBrowser1.Navigate(target_fn);
    end;
  end;

  if Assigned(fs) then fs.Free;
end;

procedure ResizeOleControl(Ctrl: TOleControl; Left, Top, Width, Height: Integer);
const
  IID_IOleInPlaceObject: TGUID = '{00000113-0000-0000-C000-000000000046}';
var
  obj: IOleInPlaceObject;
  rect: Windows.TRECT;
begin
   if SUCCEEDED(Ctrl.DefaultDispatch.QueryInterface(IID_IOleInPlaceObject, obj))
   then begin
      Rect.left := Left;
      Rect.top := Top;
      Rect.right := Rect.left + Width;
      Rect.bottom := Rect.top + Height;

      obj.SetObjectRects(rect, rect);
      obj._Release();
   end;
end;

procedure TfmMediaView.SheetPlayerResize(Sender: TObject);
begin
  //WindowsMediaPlayer1.DoObjectVerb(OLEIVERB_SHOW);
  //ResizeOleControl(WindowsMediaPlayer1, 0, 0, SheetPlayer.Width, SheetPlayer.Height);
end;

end.
