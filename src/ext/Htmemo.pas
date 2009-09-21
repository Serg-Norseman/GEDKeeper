unit Htmemo;

{ An HyperText (readonly) Memo  for Delphi 1 & 2 *)
{ from TJumpMemo by Alexander Kuznetsov (sanhome@hotmail.com) }

(*
Copyright (C) 1997 Paul Toth (TothPaul@Mygale.org)
 http://www.mygale.org/~tothpaul

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

 ~ColorIdent~ = Set Font color to ColorIdent (clBlue, clYellow...)

 ~0~ = Reset font to default
 ~p~ = Use FontFix

 ~r~ = (horizontal) Rule like <hr>

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

 How to implement :

  <b>BoldText</b> : ~b~BoldText~b~

  <h1>Header</h1> : ~+5~Header~-5~

  <pre>text</pre> : ~p~text~0~

  <font size=+5>       :  ~+5~Sample:
   Sample :               ~p~this is a sample
   <pre>                  ~0+5~end of sample
    this is a sample
   </pre>
   end of sample
  </font>


 THTMemo take care of End of Lines !!!

  this text                    : this text is on a single line
  is on a single line

  this text is<br>on two lines : this text is
                                 on two lines

 }

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, ExtCtrls;

type
  TLink = class
   Name:string;
   Rect:TRect;
   constructor Create(Aname:string; x,y,w,h:integer);
   function GotMouse(x,y:Integer):boolean;
  end;

  TLinkEvent=procedure(Sender:TObject;LinkName:string) of object;

  THeights=array[0..65520 div sizeOf(integer)] of integer;

  TRuleStyle=(rsLowered, rsRaised);

  THTMemo = class(TCustomControl)
  private
    FColor:TColor;
    FLines:TStringList;
    FHeights:^THeights;
    FHeightCount:word;
    FTopPos:integer;
    FLeftPos:Integer;
    FRange:TPoint;
    FPageHeight:integer;
    FPageWidth:Integer;
    FFont:TFont;
    FFontFix:TFont;
    FLinks:TList;
    FLinkColor:TColor;
    FLink:integer;
    FAcceptFontChange:boolean;
    FResize:boolean;
    FBorderWidth:integer;
    FBackGround:TPicture;
    FRuleStyle:TRuleStyle;
    FUpColor,FDwnColor:TColor;
    FActiveLink:integer;
    EOnLink:TLinkEvent;

    procedure SetColor(Value:TColor);
    procedure SetLines(Value:TStringList);
    procedure SetFont(Value:TFont);
    procedure SetFontFix(Value:TFont);
    procedure SetLinkColor(Value:TColor);
    procedure SetTopPos(Value:Integer);
    procedure SetLeftPos(Value:Integer);
    procedure SetBorderWidth(Value:integer);
    procedure SetBackGround(Value:TPicture);
    procedure SetRuleStyle(Value:TRuleStyle);
    procedure WMVScroll(Var Msg:TWMVScroll); message wm_vscroll;
    procedure WMHScroll(Var Msg:TWMHScroll); message wm_hscroll;
    procedure WMSize(Var Msg:TWMSize); message wm_size;
    procedure WMGetDlgCode(Var Msg:TWMGetDlgCode); message wm_getdlgcode;
    procedure FontChanged(Sender:TObject);
    procedure LinesChanged(Sender:TObject);
  protected
    procedure ArrangeText;
    procedure ClearLinks;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure FontStyle(AStyle:TFontStyles);
    procedure GotoLink(Link:word);
    procedure KeyDown(Var Key:word; Shift:TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure ScrollRange;
  public
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;
    Procedure Loaded; override;
    Property TopPos:Integer read FTopPos write SetTopPos;
    Property LeftPos:Integer read FLeftPos write SetLeftPos;
  published
    Property Color:TColor read FColor write SetColor default clBtnFace;
    Property Lines:TStringList read FLines write SetLines;
    Property Font:TFont read FFont write SetFont;
    Property FontFix:TFont read FFontFix write SetFontFix;
    Property LinkColor:TColor read FLinkColor write SetLinkColor;
    Property OnLink:TLinkEvent read EOnLink write EOnLink;
    Property BorderWidth:integer read FBorderWidth write SetBorderWidth;
    Property BackGround:TPicture read FBackGround write SetBackGround;
    Property RuleStyle:TRuleStyle read FRuleStyle write SetRuleStyle;
    Property Align;
    Property OnKeyDown;
    Property OnKeyUp;
    Property OnKeyPress;
    Property TabStop;
    Property TabOrder;
    Property Tag;
    Property Visible;
    Property Enabled;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Additional', [THTMemo]);
end;

{$IFDEF VER80}
 {$R CRHAND}
{$ELSE}
 {$R CRHAND32}
{$ENDIF}

Const
 crHand=1;

{ TLink }
Constructor TLink.Create(AName:string; x,y,w,h:integer);
 begin
  Name:=AName;
  Rect.Left:=x;
  Rect.Top:=y;
  Rect.Right:=x+w;
  Rect.Bottom:=y+h;
 end;

Function TLink.GotMouse(x,y:integer):boolean;
 begin
  Result:=(x>=Rect.Left)and(x<=Rect.Right)
       and(y>=Rect.Top)and(y<=Rect.Bottom);
 end;

{ Hyper-Text Memo }
Constructor THTMemo.Create(AOwner:TComponent);
 begin
  inherited Create(AOwner);
  FHeightcount:=0;
  FBackGround:=TPicture.Create;
  FAcceptFontChange:=False;
  FLines:=TStringList.Create;
   FLines.OnChange:=LinesChanged;
  FTopPos:=0;
  FLeftPos:=0;
  FFont:=TFont.Create;
  FFont.OnChange:=FontChanged;
  FFontFix:=TFont.Create;
  FFontFix.Name:='Courier New';
  FFont.OnChange:=FontChanged;
  FColor:=clBtnFace;
  FLinks:=TList.Create;
  FLinkColor:=clBlue;
  FFont.OnChange:=FontChanged;
  FLink:=-1;
  FUpColor:=clGray;
  FDwnColor:=clWhite;
  FResize:=False;
  FActiveLink:=-1;
 end;

procedure THTMemo.CreateParams(var Params: TCreateParams);
 begin
  inherited CreateParams(Params);
  Params.Style:=Params.Style or WS_HSCROLL or WS_VSCROLL;
 end;

Procedure THTMemo.Loaded;
 begin
  inherited Loaded;
  ArrangeText;
  TopPos:=0;
  FAcceptFontChange:=True;
 end;

Destructor THTMemo.Destroy;
 begin
  FBackGround.Free;
  ClearLinks;
  FLinks.Free;
  FFont.Free;
  FFontFix.Free;
  FLines.Free;
  inherited Destroy;
 end;

Procedure THTMemo.SetColor(Value:TColor);
 begin
  if FColor<>Value then begin
   FColor:=value;
   Paint;
  end;
 end;

Procedure THTMemo.SetLines(Value:TStringList);
 begin
  FLines.Assign(Value);
 end;

procedure THTMemo.LinesChanged;
 begin
  FActiveLink:=-1;
  if FHeightCount<>FLines.Count then begin
   if FHeightCount>0 then FreeMem(FHeights,FHeightCount*SizeOf(Integer));
   FHeightCount:=FLines.Count;
   if FHeightCount>0 then GetMem(FHeights,FHeightCount*SizeOf(Integer));
  end;
  ArrangeText;
  TopPos:=0;
  Paint;
 end;

Procedure THTMemo.SetFont(Value:TFont);
 begin
  FFont.Assign(Value);
  ArrangeText;
  Paint;
 end;

Procedure THTMemo.SetFontFix(Value:TFont);
 begin
  FFontFix.Assign(Value);
  FFontFix.Pitch:=fpFixed;
  ArrangeText;
  Paint;
 end;

Procedure THTMemo.SetLinkColor(Value:TColor);
 begin
  if FLinkColor<>Value then begin
   FLinkColor:=Value;
   Paint;
  end;
 end;

Procedure THTMemo.SetTopPos(Value:integer);
 var
  R:TRect;
 begin
  if Value<0 then Value:=0 else if Value>FRange.y then Value:=FRange.y;
  if (FTopPos<>Value) then begin
   { ScrollWindowEx(Handle,dx,dy,What,Clip,UpdateHandle,UpdateRect,Flags) }
   ScrollWindowEx(Handle,0,FTopPos-Value,nil,nil,0,@R,0);
   InvalidateRect(Handle,@R,False);
   FTopPos:=Value;
   SetScrollPos(Handle,sb_Vert,FTopPos,True);
  end;
 end;

Procedure THTMemo.SetLeftPos(Value:Integer);
 var
  R:TRect;
 begin
  if Value<0 then Value:=0 else if Value>FRange.x then Value:=FRange.x;
  if (FLeftPos<>Value) then begin
   { ScrollWindowEx(Handle,dx,dy,What,Clip,UpdateHandle,UpdateRect,Flags) }
   ScrollWindowEx(Handle,FLeftPos-Value,0,nil,nil,0,@R,0);
   InvalidateRect(Handle,@R,False);
   FLeftPos:=Value;
   SetScrollPos(Handle,sb_Horz,FLeftPos,True);
  end;
 end;

Procedure THTMemo.SetBorderWidth(Value:integer);
 begin
  if FBorderWidth<>Value then begin
   FBorderWidth:=Value;
   ArrangeText;
   Paint;
  end;
 end;

Procedure THTMemo.SetBackGround(Value:TPicture);
 begin
  FBackGround.Assign(Value);
  Paint;
 end;

procedure THTMemo.SetRuleStyle(Value:TRuleStyle);
 begin
  if FRuleStyle<>Value then begin
   FRuleStyle:=Value;
   if FRuleStyle=rsRaised then begin
    FUpColor:=clWhite;
    FDwnColor:=clGray;
   end else begin
    FUpColor:=clGray;
    FDwnColor:=clWhite;
   end;
   Paint;
  end;
 end;

Procedure THTMemo.FontChanged(Sender:TObject);
 begin
  if FAcceptFontChange then begin
   ArrangeText;
   Invalidate;
  end;
 end;

Procedure THTMemo.FontStyle(AStyle:TFontStyles);
 begin
  with Canvas.Font do
   if Style * AStyle = [] then
    Style:=Style+AStyle
   else
    Style:=Style-AStyle;
 end;

Procedure THTMemo.ArrangeText;
 begin
  FResize:=True;  Paint;
  FResize:=False; Paint;
  ScrollRange;
 end;

Procedure THTMemo.ScrollRange;
 begin
  if FPageWidth<ClientWidth then begin
   FRange.x:=0;
   LeftPos:=0;
  end else
   FRange.x:=FPageWidth-ClientWidth;
  SetScrollRange(Handle,sb_Horz,0,FRange.x,False);

  FRange.y:=FPageHeight;
  SetScrollRange(Handle,sb_Vert,0,FRange.y,False);
 end;

procedure THTMemo.Paint;
var
  Line, x, y, i, size, hMax, xMax: Integer;
  s, ss, sn: string;

  procedure OutText;
  var
    h:integer;
  begin
    if (y>=-hMax)and(ss<>'') then begin
     if not FResize then Canvas.TextOut(x,y,ss);
     inc(x,Canvas.TextWidth(ss));
     if FResize then begin
      if x>xMax then xMax:=x;
      h:=Canvas.TextHeight(ss);
      if h>hMax then hMax:=h;
     end;
    end;
  end;

  function FontSize: Integer;
  begin
    Result:=0;
    while (s[i+1] in ['0'..'9']) do begin
     inc(i);
     Result:=10*Result+Ord(s[i])-Ord('0');
    end;
  end;

  function FontColor:LongInt;
  begin
    ss:=s[i];
    while s[i+1]<>'~' do begin
     inc(i);
     ss:=ss+s[i];
    end;
    IdentToColor(ss,Result);
  end;

var
  SaveColor: TColor;
  R3d: TRect;
  BrushStyle: TBrushStyle;
begin
  if (csLoading in componentState) then exit;

  FAcceptFontChange:=False;
  ClearLinks;

  with Canvas do begin
    { BackGround }
    if not FResize then begin
      with Brush do begin
        Style:=bsSolid;
        Color:=FColor;
      end;

      FillRect(ClientRect);

      if (FBackGround.Graphic<>nil)and(not FBackGround.Graphic.Empty) then begin
        if (FBackGround.Width<>0)and(FBackGround.Height<>0) then begin
          y:=-FTopPos;
          while (y+FBackGround.Height<0) do inc(y,FBackGround.Height);
          while (y<ClientHeight) do begin
            x:=-FLeftPos; while (x+FBackGround.Width<0) do inc(x,FBackGround.Width);
            while (x<ClientWidth) do begin
              Draw(x,y,FBackGround.Graphic);
              inc(x,FBackGround.Width);
            end;
            inc(y,FBackGround.Height);
          end;
        end;
        Brush.style:=bsClear;
      end;
    end;

    Font.Assign(FFont); { Default Font }

    if FResize then begin
      y:=0;
      xMax:=0;
    end
    else
      y:=FBorderWidth-FTopPos;  { start Position }

    { For Each line from the starting line to the last line }
    for Line:=0 to FLines.Count-1 do begin
      if FResize then begin
        x:=0;
        hMax:=TextHeight('A');
      end else begin
        hMax:=FHeights^[Line]; { Line Height }
        x:=FBorderWidth-FLeftPos; { Left position }
      end;

      s:=FLines[Line]; { get the text }

      i := 1;
      ss := '';
      while (i <= Length(s)) do begin
        if (s[i] = '~') then begin
          if s[i+1] = '~' then ss := ss + '~';

          OutText;
          Inc(i);

          while s[i]<>'~' do begin
            case upcase(s[i]) of
              { reset }
              '0' : Font.Assign(FFont);
              { attributs }
              'U' : FontStyle([fsUnderline]);
              'B' : FontStyle([fsBold]);
              'I' : FontStyle([fsItalic]);
              'S' : FontStyle([fsStrikeOut]);
              { Fixed Font }
              'P' : begin Size:=Font.size; Font.Assign(FFontFix); Font.Size:=Size; end;
              { Font Size }
              '+' : Font.Size:=Font.Size+FontSize;
              '-' : Font.Size:=Font.Size-FontSize;
              { Color }
              'C' : Font.Color:=FontColor;
              { Link Name:Caption }
              '^' : begin
                sn:='';
                while s[i+1]<>':' do begin { get Link Name }
                  inc(i);
                  sn:=sn+s[i];
                end;
                inc(i);
                ss:='';
                while s[i+1]<>'~' do begin { get Caption }
                  inc(i); ss:=ss+s[i];
                end;
                SaveColor:=Font.Color;
                Font.Color:=FLinkColor;
                FontStyle([fsUnderLine]);
                if not FResize then
                  FLinks.Add(TLink.Create(sn,x,y,TextWidth(ss),hMax)); { Create TLink }

                if FActiveLink=FLinks.Count-1 then begin
                  BrushStyle:=Brush.Style;
                  Brush.Color:=Font.Color;
                  Font.Color:=Color;
                  OutText;
                  Brush.Color:=Color;
                  Brush.Style:=BrushStyle;
                end else
                  OutText;

                Font.Color:=SaveColor;
                FontStyle([fsUnderLine]);
              end;
              { Horizontal Rule }
              'R' : if not FResize then begin
                R3d.Left:=x-FLeftPos;
                R3d.Top:=y+hMax div 2-1;
                R3d.Bottom:=R3d.Top+2;
                R3d.Right:=R3d.Left+FPageWidth-x-FBorderWidth;
                Frame3D(Canvas,R3d,FUpColor,FDwnColor,1);
              end;
              { skip others }
              else while s[i+1]<>'~' do inc(i);
            end;

            inc(i);
          end;
          ss := '';
        end else ss := ss + s[i];

        Inc(i);
      end;

      OutText();
      Inc(y, hMax);
      if FResize then FHeights^[Line] := hMax;
    end;
  end;

  if FResize then begin
   FPageWidth :=xMax+2*FBorderWidth;
   FPageHeight:=y;
  end;

  FAcceptFontChange:=True;
end;

Procedure THTMemo.ClearLinks;
 var
  i:integer;
 begin
  For i:=0 to FLinks.Count-1 do TLink(FLinks[i]).Free;
  FLinks.Clear;
 end;

procedure THTMemo.MouseMove(Shift: TShiftState; X, Y: Integer);
 var
  i:integer;
 begin
  For i:=0 to FLinks.Count-1 do
   if TLink(FLinks[i]).GotMouse(x,y) then begin
    FLink:=i;
    Cursor:=crHand;
    exit;
   end;
  if FLink>=0 then begin
   Cursor:=crDefault;
   FLink:=-1;
  end;
 end;

procedure THTMemo.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
 begin
  if FLink>=0 then GotoLink(FLink);
 end;

procedure THTMemo.GotoLink(Link:word);
 var
  l:string;
  i:integer;
  h:integer;
 begin
  l:='~@'+TLink(FLinks[Link]).Name+'~';
  h:=FBorderWidth;
  for i:=0 to FLines.Count-1 do begin
   if pos(l,FLines[i])>0 then begin
    TopPos:=h;
    exit;
   end;
   inc(h,FHeights^[i]);
  end;
  if Assigned(EOnLink) then Try EOnLink(self,TLink(FLinks[Link]).Name) except end;
 end;

procedure THTMemo.WMVScroll(Var Msg:TWMVScroll);
 begin
   case msg.ScrollCode of
    sb_LineUp        : TopPos:=TopPos-(ClientHeight div 20);
    sb_LineDown      : TopPos:=TopPos+(ClientHeight div 20);
    sb_PageUp        : TopPos:=TopPos-(ClientHeight div 2);
    sb_PageDown      : TopPos:=TopPos+(ClientHeight div 2);
    sb_Top           : TopPos:=0;
    sb_Bottom        : TopPos:=FPageHeight;
    sb_ThumbTrack,
    sb_ThumbPosition : TopPos:=msg.Pos;
   end;
 end;

procedure THTMemo.WMHScroll(Var Msg:TWMHScroll);
 begin
   case msg.ScrollCode of
    sb_LineUp        : LeftPos:=LeftPos-(Clientwidth div 20);
    sb_LineDown      : LeftPos:=LeftPos+(Clientwidth div 20);
    sb_PageUp        : LeftPos:=LeftPos-(ClientWidth div 2);
    sb_PageDown      : LeftPos:=LeftPos+(ClientWidth div 2);
    sb_Top           : LeftPos:=0;
    sb_Bottom        : LeftPos:=FPageWidth;
    sb_ThumbTrack,
    sb_ThumbPosition : LeftPos:=Msg.Pos;
   end;
 end;

procedure THTMemo.WMSize(Var Msg:TWMSize);
 begin
  ScrollRange;
 end;

procedure THTMemo.WMGetDlgCode(Var Msg:TWMGetDlgCode);
 begin
  inherited;
  Msg.Result:=Msg.Result or dlgc_WantArrows or dlgc_WantTab;
 end;

procedure THTMemo.KeyDown(var Key: Word; Shift: TShiftState);
 begin
  if (key<>vk_tab)and(key<>vk_return)and(key<>vk_shift) then
   if FActiveLink>=0 then begin
    InvalidateRect(Handle,@TLink(FLinks[FActiveLink]).Rect,False);
    FActiveLink:=-1;
   end;
  case Key of
   vk_tab  : begin
              if FLinks.Count=0 then exit;
              if FActiveLink>=FLinks.count then exit;
              if FActiveLink=-1 then begin
               FActiveLink:=FLinks.Count-1;
               While (FActiveLink>0)and(TLink(FLinks[FActiveLink-1]).Rect.Top>0) do
                dec(FActiveLink);
              end else begin
               InvalidateRect(Handle,@TLink(FLinks[FActiveLink]).Rect,False);
               if ssShift in Shift then begin
                dec(FActiveLink);
                if FActiveLink<0 then FActiveLink:=FLinks.Count-1;
               end else begin
                inc(FActiveLink);
                if FActiveLink=FLinks.count then FActiveLink:=0;
               end;
              end;
              InvalidateRect(Handle,@TLink(FLinks[FActiveLink]).Rect,False);
              if (TLink(FLinks[FActiveLink]).Rect.Top<0)
               or(TLink(FLinks[FActiveLink]).Rect.Bottom>ClientHeight)
               then
               TopPos:=TopPos+TLink(FLinks[FActiveLink]).Rect.Top;
             end;
   vk_return: if FActiveLink>=0 then GotoLink(FActiveLink);
   vk_up   : TopPos:=TopPos-(ClientHeight div 20);
   vk_down : TopPos:=TopPos+(ClientHeight div 20);
   vk_right: LeftPos:=LeftPos+(Clientwidth div 20);
   vk_left : LeftPos:=LeftPos-(Clientwidth div 20);
   vk_prior: TopPos:=TopPos-(ClientHeight div 2);
   vk_next : TopPos:=TopPos+(ClientHeight div 2);
   vk_home : if ssCtrl in Shift then TopPos:=0 else LeftPos:=0;
   vk_end  : if ssCtrl in Shift then
              TopPos:=FPageHeight-ClientHeight+2*FBorderWidth
             else
              LeftPos:=FPageWidth-ClientWidth;
   else inherited Keydown(Key,Shift);
  end;
 end;

begin
 Screen.Cursors[crHand]:=LoadCursor(hInstance,'CRHAND');
end.
