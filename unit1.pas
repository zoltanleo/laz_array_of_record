unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  TMyRec = packed record
    StrVal: String;
    IntVal: SizeInt;
  end;

   { TMyRecList }

   TMyRecList = class
  private
    FMyRec: array of TMyRec; // Динамический массив записей
    function GetCount: Integer;
    function GetMyRec(Index: Integer): TMyRec;
    procedure SetMyRec(Index: Integer; AValue: TMyRec);
  public
    property MyRec[Index: Integer]: TMyRec read GetMyRec write SetMyRec; // Свойство для доступа к студентам
    property Count: Integer read GetCount; // Свойство для получения количества студентов
    procedure AddMyRec(const AMyRec: TMyRec); // Метод для добавления студента
  end;

  { TForm1 }

  TForm1 = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FMyRecList: TMyRecList;

  public
    property MyRecList: TMyRecList read FMyRecList;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TMyRecList }

function TMyRecList.GetCount: Integer;
begin
  Result := Length(FMyRec);
end;

function TMyRecList.GetMyRec(Index: Integer): TMyRec;
begin
  if (Index >= 0) and (Index < Length(FMyRec)) then
    Result := FMyRec[Index]
  else
    raise Exception.Create('Индекс вне диапазона');
end;

procedure TMyRecList.SetMyRec(Index: Integer; AValue: TMyRec);
begin
  if (Index >= 0) and (Index < Length(FMyRec)) then
    FMyRec[Index] := AValue
  else
    raise Exception.Create('Индекс вне диапазона');
end;

procedure TMyRecList.AddMyRec(const AMyRec: TMyRec);
begin
  SetLength(FMyRec, Length(FMyRec) + 1);
  FMyRec[High(FMyRec)] := AMyRec;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  MyRec: TMyRec;
  i: Integer;
begin
  FMyRecList:= TMyRecList.Create;

  MyRec:= Default(TMyRec);

  for i := 0 to Pred(8) do
  begin
    MyRec.StrVal:= 'value_' + IntToStr(i);
    MyRec.IntVal:= i;
    FMyRecList.AddMyRec(MyRec);
  end;
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

end.

