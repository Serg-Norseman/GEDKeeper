unit XLSFile;

interface

uses
  SysUtils, Classes;

const
  CBOF      = $0009;
  BIT_BIFF5 = $0800;
  BIT_BIFF4 = $0400;
  BIT_BIFF3 = $0200;
  BOF_BIFF5 = CBOF or BIT_BIFF5;
  BOF_BIFF4 = CBOF or BIT_BIFF4;
  BOF_BIFF3 = CBOF or BIT_BIFF3;

  BIFF_EOF = $000a;

  DOCTYPE_XLS = $0010;
  DOCTYPE_XLC = $0020;
  DOCTYPE_XLM = $0040;
  DOCTYPE_XLW = $0100;

  DIMENSIONS = $0000;
  DIMENSIONS_BIFF4 = DIMENSIONS or BIT_BIFF3;
  DIMENSIONS_BIFF3 = DIMENSIONS or BIT_BIFF3;

type
  EReadError = class(Exception);
  EopCodeError = class(Exception);
  EOverUnderError = class(Exception);

  TAtributCell = (
    acHiddenFormula, acLocked, acShaded, acBottomBorder, acTopBorder,
    acRightBorder, acLeftBorder, acLeft, acCenter, acRight, acFill);

  TSetOfAtribut = set of TAtributCell;

  TMyWriter = class
  public
    Stream: TStream;

    procedure WriteSingleStr(s: String);
    procedure WriteStr(s: String);
    procedure WriteByte(b: Byte);
    procedure WriteDouble(d: Double);
    procedure WriteInt(i: Integer);
    procedure WriteWord(w: Word);
  end;

  TData = class
  public
    opCode: Word;
    procedure Write(W: TMyWriter); virtual; abstract;
  end;

  TDispatcher = class
  private
    StrList: TStringList;
    Writer: TMyWriter;
  protected
    FStream: TStream;
    procedure SetStream(vStream: TStream);
  public
    SLError: TStringList;
    OpcodeEOF: Word;

    procedure Clear;
    procedure RegisterObj(MyPers: TData);
    procedure Write;
    constructor Create;
    destructor Destroy; override;
    property Stream: TStream read FStream write SetStream;
  end;

  TBOF = class(TData)
    constructor Create;
    procedure Write(W: TMyWriter); override;
  end;

  TDimension = class(TData)
  public
    MinSaveRecs, MaxSaveRecs, MinSaveCols, MaxSaveCols: Word;

    constructor Create;
    procedure Write(W: TMyWriter); override;
  end;

  TCellClass = class of TCell;

  TCell = class(TData)
  protected
    FAtribut: array [0..2] of Byte;

    procedure SetAtribut(Value: TSetOfAtribut);
  public
    Col, Row: Word;

    property Atribut: TSetOfAtribut write SetAtribut;
    constructor Create; virtual; abstract;
    procedure Write(W: TMyWriter); override;
  end;

  TDoubleCell = class(TCell)
  public
    Value: Double;

    procedure Write(W: TMyWriter); override;
    constructor Create; override;
  end;

  TWordCell = class(TCell)
  public
    Value: Word;

    procedure Write(W: TMyWriter); override;
    constructor Create; override;
  end;

  TIntegerCell = class(TCell)
  public
    Value: Integer;

    procedure Write(W: TMyWriter); override;
    constructor Create; override;
  end;

  TStrCell = class(TCell)
  public
    Value: string;

    procedure Write(W: TMyWriter); override;
    constructor Create; override;
  end;

  TXLSFile = class(TComponent)
  private
    FFileName: string;
    Dispatcher: TDispatcher;
    BOF: TBOF;
    Dimension: TDimension;
    function AddCell(vCol, vRow: Word; vAtribut: TSetOfAtribut; CellRef: TCellClass): TCell;
    procedure AddData(D: TData);
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure AddWordCell(vCol, vRow: Word; vAtribut: TSetOfAtribut; aValue: Word);
    procedure AddIntegerCell(vCol, vRow: Word; vAtribut: TSetOfAtribut; aValue: Word);
    procedure AddDoubleCell(vCol, vRow: Word; vAtribut: TSetOfAtribut; aValue: Double);
    procedure AddStrCell(vCol, vRow: Word; vAtribut: TSetOfAtribut; aValue: String);
    procedure Write;
    procedure Clear;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property FileName: String read FFileName write FFileName;
  end;

procedure Register;

implementation

procedure TMyWriter.WriteByte(b:byte);
begin
  Stream.write(b,1);
end;

procedure TMyWriter.WriteWord(w:word);
begin
  Stream.write(w,2);
end;

procedure TMyWriter.WriteSingleStr(s:string);
begin
  Stream.write(s[1],length(s));
end;

procedure TMyWriter.WriteStr(s:string);
var
   panjang:integer;
begin
  panjang:=length(s);
  WriteWord(panjang);
  Stream.write(s[1],panjang);
end;

procedure TMyWriter.WriteDouble(d:double);
begin
  Stream.write(d, 8);
end;

procedure TMyWriter.WriteInt(i:integer);
begin
  Stream.write(i, 4);
end;

procedure TDispatcher.Clear;
var
   i:integer;
begin
  for i:=0 to StrList.count-1 do
  begin
    TData(StrList.Objects[i]).Free;
  end;
  StrList.Clear;
  SLError.Clear;
end;

procedure TDispatcher.SetStream(vStream:TStream);
begin
  FStream:=vStream;
  Writer.stream:=Fstream;
end;

constructor TDispatcher.create;
begin
  OpCodeEOF:=999;
  StrList:=TStringlist.create;
  Writer:=TMyWriter.create;
  SLError:=TStringList.create;
end;

destructor TDispatcher.destroy;
begin
  Clear;
  StrList.free;
  Writer.free;
  SLError.free;
  inherited;
end;

procedure TDispatcher.RegisterObj(MyPers:TData);
begin
  StrList.AddObject(IntToStr(MyPers.opCode),MyPers);
end;

procedure TDispatcher.Write;
var
   i:integer;
   pos,length:longint;
begin
  for i:=0 to StrList.Count-1 do begin
    Writer.WriteWord(TData(StrList.objects[i]).Opcode);
    Writer.WriteWord(0);
    pos:=Stream.Position;
    TData(StrList.Objects[i]).Write(Writer);

    length:=Stream.Position-pos;
    Stream.Seek(-(length+2),soFromCurrent);
    Writer.WriteWord(length);
    Stream.Seek(length,soFromCurrent);
  end;
  //penutup
  Writer.WriteWord(opCodeEOF);
  Writer.WriteWord(0);
end;

constructor TXLSFile.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Dispatcher := TDispatcher.Create;
  Dispatcher.opcodeEOF := BIFF_EOF;
  Clear;
end;

destructor TXLSFile.destroy;
begin
  Dispatcher.Free;
  inherited;
end;

function TXLSFile.AddCell(vCol,vRow:word;vAtribut:TSetOfAtribut;CellRef:TCellClass):TCell;
var
   C:TCell;
begin
  C:=CellRef.create;
  with C do
  begin
    Col:=vCol-1;
    Row:=vRow-1;
    Atribut:=vAtribut;
  end;
  AddData(C);
  Result:=C;
end;

procedure TXLSFile.AddWordCell(vCol,vRow:word;vAtribut:TSetOfAtribut;aValue:word);
begin
  with TWordCell(AddCell(vCol,vRow,vAtribut,TWordCell)) do
    value:=aValue;
end;

procedure TXLSFile.AddIntegerCell(vCol, vRow: Word;
  vAtribut: TSetOfAtribut; aValue: Word);
begin
  with TIntegerCell(AddCell(vCol, vRow, vAtribut, TIntegerCell)) do
    Value := aValue;
end;

procedure TXLSFile.AddDoubleCell(vCol,vRow:word;vAtribut:TSetOfAtribut;aValue:double);
begin
  with TDoubleCell(AddCell(vCol,vRow,vAtribut,TDoubleCell)) do
    value:=aValue;
end;

procedure TXLSFile.AddStrCell(vCol,vRow:word;vAtribut:TSetOfAtribut;aValue:String);
begin
  with TStrCell(AddCell(vCol,vRow,vAtribut,TStrCell)) do
    value:=aValue;
end;

procedure TXLSFile.AddData(D: TData);
begin
  Dispatcher.RegisterObj(D);
end;

procedure TXLSFile.write;
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(FFileName, fmCreate);
  try
    Dispatcher.Stream := stream;
    Dispatcher.Write;
  finally
    stream.Free;
  end;
end;

procedure TXLSFile.clear;
begin
  Dispatcher.Clear;
  BOF := TBOF.Create;
  Dimension := TDimension.Create;
  Dispatcher.RegisterObj(BOF);
  Dispatcher.RegisterObj(Dimension);
end;

//TBOF  ********************************************************************
constructor TBOF.create;
begin
  opCOde := BOF_BIFF5;
end;

procedure TBOF.write(W:TMyWriter);
begin
  with W do begin
    writeWord(0); //versi
    writeWord(DOCTYPE_XLS);
    writeWord(0);
  end;
end;

//TDimension ****************************************************************
procedure TDimension.write(W: TMyWriter);
begin
  with w do begin
    WriteWord(MinSaveRecs);
    WriteWord(MaxSaveRecs);
    WriteWord(MinSaveCols);
    WriteWord(MaxSaveCols);
  end;
end;

constructor TDimension.create;
begin
  opCode := DIMENSIONS;
  MinSaveRecs := 0; MaxSaveRecs := 1000;
  MinSaveCols := 0; MaxSaveCols := 100;
end;

//TCell ******************************************************************
procedure TCell.SetAtribut(value: TSetOfAtribut);
var
  i: Integer;
begin
  //reset
  for i:=0 to High(FAtribut) do
      FAtribut[i]:=0;

  {Byte Offset  Bit   Description         Contents
      0    7   Cell is hidden         1b
           6   Cell is locked         1b
         5-0   Reserved, must be 0       000000b
    1    7-6   Font number (4 possible)
         5-0   Cell format code
      2    7   Cell is shaded         1b
           6   Cell has a bottom border     1b
           5   Cell has a top border     1b
           4   Cell has a right border      1b
           3   Cell has a left border    1b
         2-0   Cell alignment code
            general        000b
            left           001b
            center         010b
            right          011b
            fill           100b
            Multiplan default align.   111b
  }

  if  acHiddenFormula in value then  //byte 0 bit 7:
      FAtribut[0] := FAtribut[0] + 128;

  if  acLocked in value then //byte 0 bit 6:
      FAtribut[0] := FAtribut[0] + 64 ;

  if  acShaded in value then //byte 2 bit 7:
      FAtribut[2] := FAtribut[2] + 128;

  if  acBottomBorder in value then //byte 2 bit 6
      FAtribut[2] := FAtribut[2] + 64 ;

  if  acTopBorder in value then //byte 2 bit 5
      FAtribut[2] := FAtribut[2] + 32;

  if  acRightBorder in value then //byte 2 bit 4
      FAtribut[2] := FAtribut[2] + 16;

  if  acLeftBorder in value then //byte 2 bit 3
      FAtribut[2] := FAtribut[2] + 8;

  if  acLeft in value then //byte 2 bit 1
      FAtribut[2] := FAtribut[2] + 1
  else
  if  acCenter in value then //byte 2 bit 1
      FAtribut[2] := FAtribut[2] + 2
  else if acRight in value then //byte 2, bit 0 dan bit 1
      FAtribut[2] := FAtribut[2] + 3;
  if acFill in value then //byte 2, bit 0
      FAtribut[2] := FAtribut[2] + 4;
end;

procedure TCell.write(W:TMyWriter);
var
   i:integer;
begin
  with w do begin
    WriteWord(Row);
    WriteWord(Col);
    for i:=0 to 2 do writeByte(FAtribut[i]);
  end;
end;

//TWordCell **************************************************************
procedure TWordCell.write(W:TMyWriter);
begin
  inherited write(W);
  w.WriteWord(value);
end;

constructor TWordCell.create;
begin
  opCode:=2;
end;

{ TIntegerCell }

constructor TIntegerCell.Create;
begin
  opCode := 2;
end;

procedure TIntegerCell.Write(W: TMyWriter);
begin
  inherited Write(W);
  w.WriteInt(Value);
end;

//TDoubleCell **************************************************************

procedure TDoubleCell.write(W:TMyWriter);
begin
  inherited write(W);
  w.writeDouble(value);
end;

constructor TDoubleCell.create;
begin
  opCode:=3;
end;

//TStrCell ***************************************************************
procedure TStrCell.write(W:TMyWriter);
begin
  inherited Write(W);
  w.WriteByte(length(value));
  w.WriteSIngleStr(value);
end;

constructor TStrCell.create;
begin
  opCode:=4;
end;

procedure Register;
begin
  RegisterComponents('BS Common', [TXLSFile]);
end;

end.
