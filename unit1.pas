unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Generics.Collections;

type
  TMyRec = packed record
    StrVal: String;
    IntVal: SizeInt;
  end;

   { TMyRecList }

   TMyRecList = class
  private
    FListLength: SizeInt;
    FMyRec: array of TMyRec; // Динамический массив записей
    function GetCapacity: SizeInt;
    function GetCount: SizeInt;
    function GetMyRec(Index: SizeInt): TMyRec;
    procedure SetCapacity(AValue: SizeInt);
    procedure SetCount(AValue: SizeInt);
    procedure SetMyRec(Index: SizeInt; AValue: TMyRec);
  public
    constructor Create;
    destructor Destroy; override;
    property Capacity: SizeInt read GetCapacity write SetCapacity;
    property MyRec[Index: SizeInt]: TMyRec read GetMyRec write SetMyRec; // Свойство для доступа к элементу массива
    property Count: SizeInt read GetCount write SetCount; // Свойство для получения количества записей в массиве
    procedure AddMyRec(const AMyRec: TMyRec); // Метод для добавления элемента массива
    procedure Clear;
    function First: TMyRec; inline;
    function Last: TMyRec; inline;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    btnFirstItem: TButton;
    btnLastItem: TButton;
    CheckBox1: TCheckBox;
    Memo1: TMemo;
    procedure btnFirstItemClick(Sender: TObject);
    procedure btnLastItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Memo1DblClick(Sender: TObject);
  private
    FMyRec: TMyRec;
    FMyRecList: TMyRecList;

  public
    property MyRecList: TMyRecList read FMyRecList;
    property MyRec: TMyRec read FMyRec;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TMyRecList }

function TMyRecList.GetCount: SizeInt;
begin
  FListLength:= Length(FMyRec);
  Result := FListLength;
end;

function TMyRecList.GetCapacity: SizeInt;
begin
  Result := Length(FMyRec);
end;

function TMyRecList.GetMyRec(Index: SizeInt): TMyRec;
begin
  if (Index >= 0) and (Index < Length(FMyRec)) then
    Result := FMyRec[Index]
  else
    raise Exception.Create('Индекс вне диапазона');
end;

procedure TMyRecList.SetCapacity(AValue: SizeInt);
begin
  if (AValue < Count) then Count:= AValue;

  SetLength(FMyRec, AValue);
end;

procedure TMyRecList.SetCount(AValue: SizeInt);
begin
  FListLength := AValue;
end;

procedure TMyRecList.SetMyRec(Index: SizeInt; AValue: TMyRec);
begin
  if (Index >= 0) and (Index < Length(FMyRec)) then
    FMyRec[Index] := AValue
  else
    raise Exception.Create('Индекс вне диапазона');
end;

constructor TMyRecList.Create;
begin
  inherited Create;
  Clear;
end;

destructor TMyRecList.Destroy;
begin
  SetCapacity(0);
  inherited Destroy;
end;

procedure TMyRecList.AddMyRec(const AMyRec: TMyRec);
begin
  SetLength(FMyRec, Length(FMyRec) + 1);
  FMyRec[High(FMyRec)] := AMyRec;
end;

procedure TMyRecList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

function TMyRecList.First: TMyRec;
begin
  Result := FMyRec[0];
end;

function TMyRecList.Last: TMyRec;
begin
  Result := FMyRec[Pred(Count)];
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  //MyRec: TMyRec;
  i: Integer;
begin
  FMyRecList:= TMyRecList.Create;
  Memo1.ScrollBars:= ssBoth;
  //MyRec:= Default(TMyRec);

  for i := 0 to Pred(8) do
  begin
    FMyRec.StrVal:= 'value_' + IntToStr(i);
    FMyRec.IntVal:= i;
    FMyRecList.AddMyRec(MyRec);
  end;
end;

procedure TForm1.btnFirstItemClick(Sender: TObject);
var
  tmpRec: TMyRec;
begin
  if (MyRecList.Count = 0) then Exit;
  tmpRec:= MyRecList.First;

  Memo1.Lines.Add('');
  Memo1.Lines.Add('=================');
  Memo1.Lines.Add(Format('First item: %s ~ %d',[tmpRec.StrVal, tmpRec.IntVal]));
end;

procedure TForm1.btnLastItemClick(Sender: TObject);
var
  tmpRec: TMyRec;
begin
  if (MyRecList.Count = 0) then Exit;
  tmpRec:= MyRecList.Last;

  Memo1.Lines.Add('');
  Memo1.Lines.Add('=================');
  Memo1.Lines.Add(Format('Last item: %s ~ %d',[tmpRec.StrVal, tmpRec.IntVal]));
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FMyRecList.Free;
end;

procedure TForm1.FormShow(Sender: TObject);
var
  i: Integer;
begin
  Memo1.Clear;
  for i := 0 to Pred(MyRecList.Count) do
  begin
    Memo1.Lines.Add(Format('%s ~ %d',[MyRecList.MyRec[i].StrVal, MyRecList.MyRec[i].IntVal]));
  end;
end;

procedure TForm1.Memo1DblClick(Sender: TObject);
var
  i: Integer;
  //MyRec: TMyRec;
begin
  Memo1.Clear;

  if CheckBox1.Checked then FMyRecList.Clear;

  for i := 0 to Pred(8) do
  begin
    FMyRec.StrVal:= 'other_value_' + IntToStr(i);
    FMyRec.IntVal:= i;
    FMyRecList.AddMyRec(MyRec);
  end;

  for i := 0 to Pred(MyRecList.Count) do
  begin
    Memo1.Lines.Add(Format('%s ~ %d',[MyRecList.MyRec[i].StrVal, MyRecList.MyRec[i].IntVal]));
  end;
end;

end.

