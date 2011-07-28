unit XLSFile;

{$I compiler.inc}

interface

uses
  VCLStub;

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

  TCellAttribute = (
    acHiddenFormula, acLocked, acShaded, acBottomBorder, acTopBorder,
    acRightBorder, acLeftBorder, acLeft, acCenter, acRight, acFill);

  TCellAttributeSet = set of TCellAttribute;

  TBIFFWriter = class(TObject)
  public
    Stream: TStream;

    procedure WriteStr(s: AnsiString);
    procedure WriteByte(b: Byte);
    procedure WriteDouble(d: Double);
    procedure WriteInt(i: Integer);
    procedure WriteWord(w: Word);
  end;

  TData = class(TObject)
  public
    opCode: Word;
    procedure Write(W: TBIFFWriter); virtual; abstract;
  end;

  TBOF = class(TData)
  public
    constructor Create;
    procedure Write(W: TBIFFWriter); override;
  end;

  TDimension = class(TData)
  public
    MinSaveRecs, MaxSaveRecs, MinSaveCols, MaxSaveCols: Word;

    constructor Create;
    procedure Write(W: TBIFFWriter); override;
  end;

  TCellClass = class of TCell;

  TCell = class(TData)
  protected
    FAttribute: array [0..2] of Byte;

    procedure SetAttribute(Value: TCellAttributeSet);
  public
    Col, Row: Word;

    constructor Create; virtual; 
    procedure Write(W: TBIFFWriter); override;

    property Attribute: TCellAttributeSet write SetAttribute;
  end;

  TDoubleCell = class(TCell)
  public
    Value: Double;

    constructor Create; override;
    procedure Write(W: TBIFFWriter); override;
  end;

  TWordCell = class(TCell)
  public
    Value: Word;

    constructor Create; override;
    procedure Write(W: TBIFFWriter); override;
  end;

  TIntegerCell = class(TCell)
  public
    Value: Integer;

    constructor Create; override;
    procedure Write(W: TBIFFWriter); override;
  end;

  TStrCell = class(TCell)
  public
    Value: AnsiString;

    constructor Create; override;
    procedure Write(W: TBIFFWriter); override;
  end;

  TXLSFile = class(TObject)
  private
    BOF: TBOF;
    FDimension: TDimension;
    FList: TList;
    FWriter: TBIFFWriter;

    OpcodeEOF: Word;

    function AddCell(vCol, vRow: Word; vAttribute: TCellAttributeSet; CellRef: TCellClass): TCell;
    procedure RegisterObj(MyPers: TData);
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddWordCell(vCol, vRow: Word; vAttribute: TCellAttributeSet; aValue: Word);
    procedure AddIntegerCell(vCol, vRow: Word; vAttribute: TCellAttributeSet; aValue: Integer);
    procedure AddDoubleCell(vCol, vRow: Word; vAttribute: TCellAttributeSet; aValue: Double);
    procedure AddStrCell(vCol, vRow: Word; vAttribute: TCellAttributeSet; aValue: AnsiString);
    procedure Clear;
    procedure SaveToFile(const FileName: string);
  end;

implementation

{ TBIFFWriter }

procedure TBIFFWriter.WriteByte(b: Byte);
begin
  Stream.Write(b, 1);
end;

procedure TBIFFWriter.WriteWord(w: Word);
begin
  Stream.Write(w, 2);
end;

procedure TBIFFWriter.WriteStr(s: AnsiString);
var
  len: Integer;
  {$IFDEF DELPHI_NET}
  i: Integer;
  {$ENDIF}
begin
  len := Length(s);
  WriteByte(len);

  {$IFNDEF DELPHI_NET}
  Stream.Write(s[1], len);
  {$ELSE}
  for i := 1 to len do Stream.Write(Byte(s[i]));
  {$ENDIF}
end;

procedure TBIFFWriter.WriteDouble(d: Double);
begin
  Stream.Write(d, 8);
end;

procedure TBIFFWriter.WriteInt(i: Integer);
begin
  Stream.Write(i, 4);
end;

{ TXLSFile }

constructor TXLSFile.Create;
begin
  inherited Create();

  opcodeEOF := BIFF_EOF;
  FList := TList.Create;
  FWriter := TBIFFWriter.Create;

  Clear;
end;

destructor TXLSFile.Destroy;
begin
  Clear;
  FList.Free;
  FWriter.Free;

  inherited Destroy;
end;

function TXLSFile.AddCell(vCol, vRow: Word; vAttribute: TCellAttributeSet; CellRef: TCellClass): TCell;
var
  C: TCell;
begin
  C := CellRef.Create;
  with C do begin
    Col := vCol - 1;
    Row := vRow - 1;
    Attribute := vAttribute;
  end;

  RegisterObj(C);

  Result := C;
end;

procedure TXLSFile.AddWordCell(vCol, vRow: word; vAttribute: TCellAttributeSet; aValue: Word);
begin
  with TWordCell(AddCell(vCol, vRow, vAttribute, TWordCell)) do Value := aValue;
end;

procedure TXLSFile.AddIntegerCell(vCol, vRow: Word; vAttribute: TCellAttributeSet; aValue: Integer);
begin
  with TIntegerCell(AddCell(vCol, vRow, vAttribute, TIntegerCell)) do Value := aValue;
end;

procedure TXLSFile.AddDoubleCell(vCol, vRow: Word; vAttribute: TCellAttributeSet; aValue: Double);
begin
  with TDoubleCell(AddCell(vCol, vRow, vAttribute, TDoubleCell)) do Value := aValue;
end;

procedure TXLSFile.AddStrCell(vCol, vRow: Word; vAttribute: TCellAttributeSet; aValue: AnsiString);
begin
  with TStrCell(AddCell(vCol, vRow, vAttribute, TStrCell)) do Value := aValue;
end;

procedure TXLSFile.RegisterObj(MyPers: TData);
begin
  FList.Add(MyPers);
end;

procedure TXLSFile.SaveToFile(const FileName: string);
var
  stream: TFileStream;
  i: Integer;
  pos, length: Longint;
  data: TData;
begin
  stream := TFileStream.Create(FileName, fmCreate);
  try
    FWriter.Stream := stream;

    for i := 0 to FList.Count - 1 do begin
      data := TData(FList[i]);

      FWriter.WriteWord(data.Opcode);
      FWriter.WriteWord(0);
      pos := stream.Position;
      data.Write(FWriter);

      length := stream.Position - pos;
      stream.Seek(-(length + 2), soCurrent);
      FWriter.WriteWord(length);
      stream.Seek(length, soCurrent);
    end;

    FWriter.WriteWord(opCodeEOF);
    FWriter.WriteWord(0);
  finally
    stream.Free;
  end;
end;

procedure TXLSFile.Clear;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do TData(FList[i]).Free;
  FList.Clear;

  BOF := TBOF.Create;
  FDimension := TDimension.Create;

  RegisterObj(BOF);
  RegisterObj(FDimension);
end;

{ TBOF }

constructor TBOF.Create;
begin
  inherited Create;
  opCode := BOF_BIFF5;
end;

procedure TBOF.Write(W: TBIFFWriter);
begin
  with W do begin
    WriteWord(0); //versi
    WriteWord(DOCTYPE_XLS);
    WriteWord(0);
  end;
end;

{ TDimension }

constructor TDimension.Create;
begin
  inherited Create;
  opCode := DIMENSIONS;
  MinSaveRecs := 0;
  MaxSaveRecs := 1000;
  MinSaveCols := 0;
  MaxSaveCols := 100;
end;

procedure TDimension.Write(W: TBIFFWriter);
begin
  with w do begin
    WriteWord(MinSaveRecs);
    WriteWord(MaxSaveRecs);
    WriteWord(MinSaveCols);
    WriteWord(MaxSaveCols);
  end;
end;

{ TCell }

constructor TCell.Create;
begin
  inherited Create;
end;

procedure TCell.SetAttribute(Value: TCellAttributeSet);
var
  i: Integer;
begin
  //reset
  for i := 0 to High(FAttribute) do
    FAttribute[i] := 0;

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

  if acHiddenFormula in value then  //byte 0 bit 7:
      FAttribute[0] := FAttribute[0] + 128;

  if acLocked in value then //byte 0 bit 6:
      FAttribute[0] := FAttribute[0] + 64 ;

  if acShaded in value then //byte 2 bit 7:
      FAttribute[2] := FAttribute[2] + 128;

  if acBottomBorder in value then //byte 2 bit 6
      FAttribute[2] := FAttribute[2] + 64 ;

  if acTopBorder in value then //byte 2 bit 5
      FAttribute[2] := FAttribute[2] + 32;

  if acRightBorder in value then //byte 2 bit 4
      FAttribute[2] := FAttribute[2] + 16;

  if acLeftBorder in value then //byte 2 bit 3
      FAttribute[2] := FAttribute[2] + 8;

  if acLeft in value then //byte 2 bit 1
      FAttribute[2] := FAttribute[2] + 1
  else

  if (acCenter in value) then //byte 2 bit 1
      FAttribute[2] := FAttribute[2] + 2
  else

  if (acRight in value) then //byte 2, bit 0 dan bit 1
      FAttribute[2] := FAttribute[2] + 3;

  if (acFill in value) then //byte 2, bit 0
      FAttribute[2] := FAttribute[2] + 4;
end;

procedure TCell.Write(W: TBIFFWriter);
var
  i: Integer;
begin
  with w do begin
    WriteWord(Row);
    WriteWord(Col);
    for i := 0 to 2 do WriteByte(FAttribute[i]);
  end;
end;

{ TWordCell }

constructor TWordCell.Create;
begin
  inherited Create;
  opCode := 2;
end;

procedure TWordCell.Write(W: TBIFFWriter);
begin
  inherited Write(W);
  w.WriteWord(value);
end;

{ TIntegerCell }

constructor TIntegerCell.Create;
begin
  inherited Create;
  opCode := 2;
end;

procedure TIntegerCell.Write(W: TBIFFWriter);
begin
  inherited Write(W);
  w.WriteInt(Value);
end;

{ TDoubleCell }

constructor TDoubleCell.Create;
begin
  inherited Create;
  opCode := 3;
end;

procedure TDoubleCell.Write(W: TBIFFWriter);
begin
  inherited Write(W);
  w.WriteDouble(value);
end;

{ TStrCell }

constructor TStrCell.Create;
begin
  inherited Create;
  opCode := 4;
end;

procedure TStrCell.Write(W: TBIFFWriter);
begin
  inherited Write(W);
  w.WriteStr(value);
end;

end.
