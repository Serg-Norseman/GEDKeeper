unit HTMemo;

{$I compiler.inc}

(*
Copyright (C) 1997 Paul Toth (TothPaul@Mygale.org)
  http://www.mygale.org/~tothpaul

An HyperText (readonly) Memo for Delphi
  from TJumpMemo by Alexander Kuznetsov (sanhome@hotmail.com)

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
*)

{
 It work something like <HTML TAGS>

 ~~  = ~
 ~u~ = Underline
 ~b~ = Bold
 ~i~ = Italic
 ~s~ = Strike Out
 ~+x~ = Increment Font Size by x
 ~-x~ = Decrement Font Size by x
 ~0~ = Reset font to default
 ~r~ = (horizontal) Rule like <hr>

 ~ColorIdent~ = Set Font color to ColorIdent (clBlue, clYellow...)

 ~@LinkName~ = name a link (like html tag <a name=LinkName> )

 ~^LinkName:Caption~ = use FontLink to draw Caption
                       (like html tag <a href="#linkname>Caption</a>)

 Note :

  There is no ~/tag~ because "ubis" are "flip-flop" tags :
   "normal text ~i~ Italic ~bu~ Italic,Bold,UnderLine ~ib~ UnderLine ~u~ normal"

  If you specify a colorIdent, it should be the last tag :
   ~ub+5clBlue~ : UnderLine, Bold, size+5, Color Blue.
   ~clBlueub+5~ : color "clBlueub+5" !!!

  The BackGround is not a tag because this component is not a new HTML file format
   but a way to create Delphi application that look like Browser interface.
   You have to specify inside your app, with BackGround you want to put in.
   This component support also only Internal Links, You have to use OnLink event to
   build a multi-page application.
}

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, ExtCtrls;

type
  TLink = class(TObject)
  public
    Name: string;
    Rect: TRect;

    constructor Create(Aname: string; x, y, w, h: Integer);
    function GotMouse(x, y: Integer): Boolean;
  end;

  TLinkEvent = procedure(Sender: TObject; LinkName: string) of object;

  THeights = array of Integer;

  TRuleStyle = (rsLowered, rsRaised);

  THTMemo = class(TCustomControl)
  private
    FColor: TColor;
    FLines: TStringList;
    FHeights: THeights;
    FHeightCount: Word;
    FTopPos: Integer;
    FLeftPos: Integer;
    FRange: TPoint;
    FPageHeight: Integer;
    FPageWidth: Integer;
    FFont: TFont;
    FLinks: TList;
    FLinkColor: TColor;
    FLink: Integer;
    FAcceptFontChange: boolean;
    FResize: boolean;
    FBorderWidth: Integer;
    FBackground: TPicture;
    FRuleStyle: TRuleStyle;
    FUpColor, FDwnColor: TColor;
    FActiveLink: Integer;
    EOnLink: TLinkEvent;
    FBorderStyle: TBorderStyle;
    FWheelAccumulator: Integer;

    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure FontChanged(Sender: TObject);
    procedure LinesChanged(Sender: TObject);
    procedure SetBackground(Value: TPicture);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetBorderWidth(Value: Integer);
    procedure SetColor(Value: TColor);
    procedure SetFont(Value: TFont);
    procedure SetLeftPos(Value: Integer);
    procedure SetLines(Value: TStringList);
    procedure SetLinkColor(Value: TColor);
    procedure SetTopPos(Value: Integer);
    procedure SetRuleStyle(Value: TRuleStyle);
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure CMMouseWheel(var Message: TCMMouseWheel); message CM_MOUSEWHEEL;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint); dynamic;
  protected
    procedure ArrangeText();
    procedure ClearLinks();
    procedure CreateParams(var Params: TCreateParams); override;
    procedure FontStyle(AStyle: TFontStyles);
    procedure GotoLink(Link: Word);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded(); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint(); override;
    procedure Prepare();
    procedure ScrollRange();
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property LeftPos: Integer read FLeftPos write SetLeftPos;
    property TopPos: Integer read FTopPos write SetTopPos;
  published
    property Align;
    property Background: TPicture read FBackground write SetBackground;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth;
    property Color: TColor read FColor write SetColor default clBtnFace;
    property Ctl3D;
    property Enabled;
    property Font: TFont read FFont write SetFont;
    property Lines: TStringList read FLines write SetLines;
    property LinkColor: TColor read FLinkColor write SetLinkColor;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnLink: TLinkEvent read EOnLink write EOnLink;
    property RuleStyle: TRuleStyle read FRuleStyle write SetRuleStyle;
    property TabStop;
    property TabOrder;
    property Tag;
    property Visible;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Additional', [THTMemo]);
end;

{$R crhand32.res}

const
  crHand = 1;

{ TLink }

constructor TLink.Create(AName: string; x, y, w, h: Integer);
begin
  inherited Create;
  Name := AName;
  Rect.Left := x;
  Rect.Top := y;
  Rect.Right := x + w;
  Rect.Bottom := y + h;
end;

function TLink.GotMouse(x, y: Integer): Boolean;
begin
  Result := (x >= Rect.Left) and (x <= Rect.Right)
        and (y >= Rect.Top) and (y <= Rect.Bottom);
end;

{ THTMemo }

constructor THTMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csCaptureMouse, csClickEvents, csDoubleClicks];
  TabStop := True;

  FBorderStyle := bsSingle;
  FHeightcount := 0;
  FBackGround := TPicture.Create;
  FAcceptFontChange := False;
  FLines := TStringList.Create;
  FLines.OnChange := LinesChanged;

  SetLength(FHeights, 0);

  FTopPos := 0;
  FLeftPos := 0;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FFont.OnChange := FontChanged;
  FColor := clBtnFace;
  FLinks := TList.Create;
  FLinkColor := clBlue;
  FFont.OnChange := FontChanged;
  FLink := -1;
  FUpColor := clGray;
  FDwnColor := clWhite;
  FResize := False;
  FActiveLink := -1;
end;

procedure THTMemo.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or WS_HSCROLL or WS_VSCROLL;

  with Params do begin
    Style := Style or BorderStyles[FBorderStyle];
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;

procedure THTMemo.SetBorderStyle(Value: TBorderStyle);
begin
  if (Value <> FBorderStyle) then begin
    FBorderStyle := Value;
    RecreateWnd();
  end;
end;

procedure THTMemo.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls and (FBorderStyle = bsSingle) then RecreateWnd;
  inherited;
end;

procedure THTMemo.Loaded();
begin
  inherited Loaded;
  ArrangeText();
  TopPos := 0;
  FAcceptFontChange := True;
end;

destructor THTMemo.Destroy;
begin
  FBackground.Free;
  ClearLinks();
  FLinks.Free;
  FFont.Free;

  {$IFNDEF DELPHI_NET}
  Finalize(FHeights);
  {$ELSE}
  FHeights := nil;
  {$ENDIF}

  FLines.Free;
  inherited Destroy;
end;

procedure THTMemo.SetColor(Value: TColor);
begin
  if FColor <> Value then begin
    FColor := Value;
    Paint;
  end;
end;

procedure THTMemo.SetLines(Value: TStringList);
begin
  FLines.Assign(Value);
end;

procedure THTMemo.LinesChanged(Sender: TObject);
begin
  FActiveLink := -1;
  if (FHeightCount <> FLines.Count) then begin
    FHeightCount := FLines.Count;
    SetLength(FHeights, FHeightCount);
  end;
  ArrangeText();
  TopPos := 0;
  Paint();
end;

procedure THTMemo.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
  ArrangeText();
  Paint();
end;

procedure THTMemo.SetLinkColor(Value: TColor);
begin
  if (FLinkColor <> Value) then begin
    FLinkColor := Value;
    Paint();
  end;
end;

procedure THTMemo.SetTopPos(Value: Integer);
var
  {$IFDEF DELPHI_NET}dummy,{$ENDIF} R: TRect;
begin
  if (Value < 0)
  then Value := 0
  else
    if (Value > FRange.y)
    then Value := FRange.y;

  if (FTopPos <> Value) then begin
    {$IFNDEF DELPHI_NET}
    ScrollWindowEx(Handle, 0, FTopPos - Value, nil, nil, 0, @R, 0);
    InvalidateRect(Handle, @R, False);
    {$ELSE}
    dummy.Empty();
    ScrollWindowEx(Handle, 0, FTopPos - Value, dummy, dummy, 0, R, 0);
    InvalidateRect(Handle, R, False);
    {$ENDIF}

    FTopPos := Value;
    SetScrollPos(Handle, sb_Vert, FTopPos, True);
  end;
end;

procedure THTMemo.SetLeftPos(Value: Integer);
var
  {$IFDEF DELPHI_NET}dummy,{$ENDIF} R: TRect;
begin
  if (Value < 0)
  then Value := 0
  else
    if (Value > FRange.x)
    then Value := FRange.x;

  if (FLeftPos <> Value) then begin
    {$IFNDEF DELPHI_NET}
    ScrollWindowEx(Handle, FLeftPos-Value, 0, nil, nil, 0, @R, 0);
    InvalidateRect(Handle, @R, False);
    {$ELSE}
    dummy.Empty();
    ScrollWindowEx(Handle, FLeftPos-Value, 0, dummy, dummy, 0, R, 0);
    InvalidateRect(Handle, R, False);
    {$ENDIF}

    FLeftPos := Value;
    SetScrollPos(Handle, sb_Horz, FLeftPos, True);
  end;
end;

procedure THTMemo.SetBorderWidth(Value: Integer);
begin
  if (FBorderWidth <> Value) then begin
    FBorderWidth := Value;
    ArrangeText();
    Paint();
  end;
end;

procedure THTMemo.SetBackGround(Value: TPicture);
begin
  FBackGround.Assign(Value);
  Paint();
end;

procedure THTMemo.SetRuleStyle(Value: TRuleStyle);
begin
  if (FRuleStyle <> Value) then begin
    FRuleStyle := Value;
    if (FRuleStyle = rsRaised) then begin
      FUpColor := clWhite;
      FDwnColor := clGray;
    end else begin
      FUpColor := clGray;
      FDwnColor := clWhite;
    end;
    Paint();
  end;
end;

procedure THTMemo.FontChanged(Sender: TObject);
begin
  if FAcceptFontChange then begin
    ArrangeText();
    Invalidate();
  end;
end;

procedure THTMemo.FontStyle(AStyle: TFontStyles);
begin
  with Canvas.Font do
    if (Style * AStyle = [])
    then Style := Style + AStyle
    else Style := Style - AStyle;
end;

procedure THTMemo.ScrollRange();
begin
  if (FPageWidth < ClientWidth) then begin
    FRange.x := 0;
    LeftPos := 0;
  end else
    FRange.x := FPageWidth - ClientWidth;

  SetScrollRange(Handle, sb_Horz, 0, FRange.x, False);

  FRange.y := FPageHeight;
  SetScrollRange(Handle, sb_Vert, 0, FRange.y, False);
end;

procedure THTMemo.ArrangeText();
begin
  FResize := True;
  Paint();
  FResize := False;
  Paint();

  ScrollRange();
end;

procedure THTMemo.Prepare();
begin
end;

procedure THTMemo.Paint();
var
  Line, x, y, i, hMax, xMax: Integer;
  s, ss, sn: string;

  procedure OutText();
  var
    h: Integer;
  begin
    if (y >= -hMax) and (ss <> '') then begin
      if not FResize then Canvas.TextOut(x, y, ss);

      Inc(x, Canvas.TextWidth(ss));

      if FResize then begin
        if (x > xMax) then xMax := x;
        h := Canvas.TextHeight(ss);
        if (h > hMax) then hMax := h;
      end;
    end;
  end;

  function FontSize(): Integer;
  begin
    Result := 0;
    while (s[i+1] in ['0'..'9']) do begin
      Inc(i);
      Result := 10 * Result + Ord(s[i]) - Ord('0');
    end;
  end;

  function FontColor(): Longint;
  begin
    ss := s[i];
    while (s[i+1] <> '~') do begin
      Inc(i);
      ss := ss + s[i];
    end;
    IdentToColor(ss, Result);
  end;

var
  SaveColor: TColor;
  R3d: TRect;
  BrushStyle: TBrushStyle;
begin
  if (csLoading in ComponentState) then Exit;

  FAcceptFontChange := False;
  ClearLinks();

  // focus debug code
  {if Focused
  then Font.Color := clBlack
  else Font.Color := clGray;}

  with Canvas do begin
    if not FResize then begin
      { BackGround }
      Brush.Style := bsSolid;
      Brush.Color := FColor;
      FillRect(ClientRect);

      if (FBackground.Graphic <> nil) and (not FBackground.Graphic.Empty) then begin
        if (FBackground.Width <> 0) and (FBackground.Height <> 0) then begin
          y := -FTopPos;
          while (y + FBackground.Height < 0) do Inc(y, FBackground.Height);
          while (y < ClientHeight) do begin
            x := -FLeftPos;
            while (x + FBackground.Width < 0) do Inc(x, FBackground.Width);
            while (x < ClientWidth) do begin
              Draw(x, y, FBackground.Graphic);
              Inc(x, FBackground.Width);
            end;
            Inc(y, FBackground.Height);
          end;
        end;
        Brush.Style := bsClear;
      end;
    end;

    Font.Assign(FFont); { Default Font }

    if FResize then begin
      y := 0;
      xMax := 0;
    end else y := FBorderWidth - FTopPos;  { start Position }

    { For Each line from the starting line to the last line }
    for Line := 0 to FLines.Count - 1 do begin
      if FResize then begin
        x := 0;
        hMax := TextHeight('A');
      end else begin
        hMax := FHeights[Line]; { Line Height }
        x := FBorderWidth - FLeftPos; { Left position }
      end;

      s := FLines[Line]; { get the text }

      i := 1;
      ss := '';
      while (i <= Length(s)) do begin
        if (s[i] = '~') then begin
          if s[i+1] = '~' then ss := ss + '~';

          OutText();
          Inc(i);

          while (s[i] <> '~') do begin
            case UpCase(s[i]) of
              { reset }
              '0' : Font.Assign(FFont);

              { attributs }
              'U' : FontStyle([fsUnderline]);
              'B' : FontStyle([fsBold]);
              'I' : FontStyle([fsItalic]);
              'S' : FontStyle([fsStrikeOut]);

              { Font Size }
              '+' : Font.Size := Font.Size + FontSize;
              '-' : Font.Size := Font.Size - FontSize;

              { Color }
              'C' : Font.Color := FontColor;

              { Link Name:Caption }
              '^' : begin
                sn := '';
                while (s[i+1] <> ':') do begin { get Link Name }
                  Inc(i);
                  sn := sn + s[i];
                end;
                Inc(i);
                ss := '';
                while (s[i+1] <> '~') do begin { get Caption }
                  Inc(i);
                  ss := ss + s[i];
                end;
                SaveColor := Font.Color;
                Font.Color := FLinkColor;
                FontStyle([fsUnderline]);

                if not FResize then
                  FLinks.Add(TLink.Create(sn, x, y, TextWidth(ss), hMax)); { Create TLink }

                if (FActiveLink = FLinks.Count - 1) then begin
                  BrushStyle := Brush.Style;
                  Brush.Color := Font.Color;
                  Font.Color := Color;
                  OutText();
                  Brush.Color := Color;
                  Brush.Style := BrushStyle;
                end else
                  OutText();

                Font.Color := SaveColor;
                FontStyle([fsUnderline]);
              end;

              { Horizontal Rule }
              'R': if not FResize then begin
                R3d.Left := x - FLeftPos;
                R3d.Top := y + hMax div 2 - 1;
                R3d.Bottom := R3d.Top + 2;
                R3d.Right := R3d.Left + FPageWidth - x - FBorderWidth;
                Frame3D(Canvas, R3d, FUpColor, FDwnColor, 1);
              end;

              { skip others }
              else while s[i+1]<>'~' do inc(i);
            end;

            Inc(i);
          end;

          ss := '';
        end else ss := ss + s[i];

        Inc(i);
      end;

      OutText();
      Inc(y, hMax);
      if FResize then FHeights[Line] := hMax;
    end;
  end;

  if FResize then begin
    FPageWidth  := xMax + 2 * FBorderWidth;
    FPageHeight := y;
  end;

  FAcceptFontChange := True;
end;

procedure THTMemo.ClearLinks();
var
  i: Integer;
begin
  for i := 0 to FLinks.Count - 1 do TLink(FLinks[i]).Free;
  FLinks.Clear;
end;

procedure THTMemo.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  inherited MouseMove(Shift, X, Y);

  for i := 0 to FLinks.Count - 1 do
    if TLink(FLinks[i]).GotMouse(x, y) then begin
      FLink := i;
      Cursor := crHand;
      Exit;
    end;

  if FLink >= 0 then begin
    Cursor := crDefault;
    FLink := -1;
  end;
end;

procedure THTMemo.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if not Focused then SetFocus;

  if (FLink >= 0) then GotoLink(FLink);
end;

procedure THTMemo.GotoLink(Link: Word);
var
  l: string;
  i, h: Integer;
begin
  l := '~@' + TLink(FLinks[Link]).Name + '~';
  h := FBorderWidth;
  for i := 0 to FLines.Count - 1 do begin
    if Pos(l, FLines[i]) > 0 then begin
      TopPos := h;
      Exit;
    end;
    Inc(h, FHeights[i]);
  end;

  if Assigned(EOnLink)
  then EOnLink(Self, TLink(FLinks[Link]).Name)
end;

procedure THTMemo.WMVScroll(var Msg: TWMVScroll);
begin
  case Msg.ScrollCode of
    sb_LineUp        : TopPos := TopPos - (ClientHeight div 20);
    sb_LineDown      : TopPos := TopPos + (ClientHeight div 20);
    sb_PageUp        : TopPos := TopPos - (ClientHeight div 2);
    sb_PageDown      : TopPos := TopPos + (ClientHeight div 2);
    sb_Top           : TopPos := 0;
    sb_Bottom        : TopPos := FPageHeight;
    sb_ThumbTrack,
    sb_ThumbPosition : TopPos := Msg.Pos;
  end;
end;

procedure THTMemo.WMHScroll(var Msg:TWMHScroll);
begin
  case msg.ScrollCode of
    sb_LineUp        : LeftPos := LeftPos - (Clientwidth div 20);
    sb_LineDown      : LeftPos := LeftPos + (Clientwidth div 20);
    sb_PageUp        : LeftPos := LeftPos - (ClientWidth div 2);
    sb_PageDown      : LeftPos := LeftPos + (ClientWidth div 2);
    sb_Top           : LeftPos := 0;
    sb_Bottom        : LeftPos := FPageWidth;
    sb_ThumbTrack,
    sb_ThumbPosition : LeftPos := Msg.Pos;
  end;
end;

procedure THTMemo.WMSize(var Msg: TWMSize);
begin
  ScrollRange();
end;

procedure THTMemo.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  inherited;
  Msg.Result := Msg.Result or DLGC_WANTARROWS or DLGC_WANTTAB or DLGC_WANTCHARS;
end;

procedure THTMemo.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key <> vk_tab) and (Key <> vk_return) and (Key <> vk_shift) then
    if FActiveLink >= 0 then begin
      {$IFNDEF DELPHI_NET}
      InvalidateRect(Handle, @TLink(FLinks[FActiveLink]).Rect, False);
      {$ELSE}
      InvalidateRect(Handle, TLink(FLinks[FActiveLink]).Rect, False);
      {$ENDIF}

      FActiveLink := -1;
    end;

  case Key of
    VK_TAB: begin
      if FLinks.Count = 0 then Exit;
      if FActiveLink >= FLinks.Count then Exit;
      if FActiveLink = -1 then begin
        FActiveLink := FLinks.Count-1;
        while (FActiveLink > 0) and (TLink(FLinks[FActiveLink-1]).Rect.Top > 0) do
          Dec(FActiveLink);
      end else begin
        {$IFNDEF DELPHI_NET}
        InvalidateRect(Handle, @TLink(FLinks[FActiveLink]).Rect, False);
        {$ELSE}
        InvalidateRect(Handle, TLink(FLinks[FActiveLink]).Rect, False);
        {$ENDIF}

        if (ssShift in Shift) then begin
          Dec(FActiveLink);
          if (FActiveLink < 0) then FActiveLink := FLinks.Count - 1;
        end else begin
          Inc(FActiveLink);
          if (FActiveLink = FLinks.Count) then FActiveLink := 0;
        end;
      end;

      {$IFNDEF DELPHI_NET}
      InvalidateRect(Handle, @TLink(FLinks[FActiveLink]).Rect, False);
      {$ELSE}
      InvalidateRect(Handle, TLink(FLinks[FActiveLink]).Rect, False);
      {$ENDIF}

      if (TLink(FLinks[FActiveLink]).Rect.Top<0)
      or (TLink(FLinks[FActiveLink]).Rect.Bottom>ClientHeight)
      then TopPos := TopPos+TLink(FLinks[FActiveLink]).Rect.Top;
    end;

    VK_RETURN:
      if FActiveLink >= 0 then GotoLink(FActiveLink);

    VK_UP: TopPos := TopPos - (ClientHeight div 20);

    VK_DOWN: TopPos := TopPos + (ClientHeight div 20);

    VK_RIGHT: LeftPos := LeftPos + (Clientwidth div 20);

    VK_LEFT: LeftPos := LeftPos - (Clientwidth div 20);

    VK_PRIOR: TopPos := TopPos - (ClientHeight div 2);

    VK_NEXT:
      TopPos := TopPos + (ClientHeight div 2);

    VK_HOME:
      if (ssCtrl in Shift) then TopPos := 0 else LeftPos := 0;

    VK_END:
      if (ssCtrl in Shift)
      then TopPos := FPageHeight - ClientHeight + 2 * FBorderWidth
      else LeftPos := FPageWidth - ClientWidth;
    else inherited KeyDown(Key, Shift);
  end;
end;

procedure THTMemo.CMMouseWheel(var Message: TCMMouseWheel);
begin
  with Message do
    MouseWheel(ShiftState, WheelDelta, SmallPointToPoint(Pos));
end;

procedure THTMemo.MouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint);
var
  ScrollNotify: Integer;
  hasShift, hasCtrl: Boolean;
begin
  ScrollNotify := -1;
  hasShift := (ssShift in Shift);
  hasCtrl := (ssCtrl in Shift);

  if hasCtrl then begin
    if WheelDelta > 0 then ScrollNotify := SB_LINELEFT;
    if WheelDelta < 0 then ScrollNotify := SB_LINERIGHT;
    if (ScrollNotify <> -1) then begin
      Perform(WM_HSCROLL, ScrollNotify, 0);
      Perform(WM_HSCROLL, ScrollNotify, 0);
    end;
    Exit;
  end;

  if hasShift then begin
    //DrawVisible;
    //HideCaret := False;
    //if WheelDelta > 0 then CaretPos.x := CaretPos.x - 1;
    //if WheelDelta < 0 then CaretPos.x := CaretPos.x + 1;
    //if CaretPos.x < 0 then CaretPos.x := 0;
    //if CaretPos.x > pred(FLines.Count) then CaretPos.x := pred(FLines.Count);
    //DrawCaret(CaretPos.x, CaretPos.y, HideCaret);
    Exit;
  end;

  if not hasShift and not hasCtrl then begin
    if WheelDelta > 0 then ScrollNotify := SB_LINEUP;
    if WheelDelta < 0 then ScrollNotify := SB_LINEDOWN;
    if (ScrollNotify <> -1) then Perform(WM_VSCROLL, ScrollNotify, 0);
  end;

  Inc(FWheelAccumulator, WheelDelta);

  while (Abs(FWheelAccumulator) >= WHEEL_DELTA) do begin
    FWheelAccumulator := Abs(FWheelAccumulator) - WHEEL_DELTA;
    if (FWheelAccumulator < 0) then begin
      if (FWheelAccumulator <> 0) then FWheelAccumulator := -FWheelAccumulator;
    end;
  end;
end;

begin
  {$IFNDEF DELPHI_NET}
  Screen.Cursors[crHand] := LoadCursor(hInstance, 'CRHAND');
  {$ENDIF}
end.
