program htmltable;

uses
  sysutils, SimpleTpl, memds, db, Classes;

type

  { THtmlTest }

  THtmlTest = class
    Lines: TStringList;
    MemData: TMemDataset;
    Tpl: TSimpleTemplate;
    procedure InitMemData;
    procedure TplGetValue(Sender: TObject; const AValueName: String; var AValue: String);
    procedure TplGetIfCondition(Sender: TObject; const ACondition: String; var Result: Boolean);
    procedure TplGetLoopCount(Sender: TObject; const ALoop: String; var LoopCount: Integer);
    procedure TplEndLoop(Sender: TObject; const ALoopName: String; const ALoopIndex: Integer; var ABreak: Boolean);
    procedure RunTest;
  end;

{ THtmlTest }

procedure THtmlTest.InitMemData;
var
  I: Integer;
begin
  MemData.FieldDefs.Add('id', ftInteger);
  MemData.FieldDefs.Add('name', ftString, 20);
  MemData.FieldDefs.Add('field1', ftString, 20);
  MemData.FieldDefs.Add('field2', ftString, 20);
  MemData.CreateTable;
  MemData.Open;

  for I := 0 to 19 do
    MemData.AppendRecord([I, 'Name #' + IntToStr(I), 'Field 1: ' + IntToStr(I), 'Field 2: ' + IntToStr(I)]);

  MemData.First;
end;

procedure THtmlTest.TplGetValue(Sender: TObject; const AValueName: String;
  var AValue: String);
var
  I: Integer;
begin
  I := Tpl.GetLoopCounter('column');
  if I < 0 then
    Exit;
  case AValueName of
    'value': AValue := MemData.Fields[Tpl.GetLoopCounter('column')].Text;
    'column_name': AValue := MemData.Fields[Tpl.GetLoopCounter('column')].FieldName;
  end;
end;

procedure THtmlTest.TplGetIfCondition(Sender: TObject;
  const ACondition: String; var Result: Boolean);
begin
  case ACondition of
    'oddrow': Result := (Tpl.GetLoopCounter('row') mod 2) = 1;
  end;
end;

procedure THtmlTest.TplGetLoopCount(Sender: TObject; const ALoop: String;
  var LoopCount: Integer);
begin
  case ALoop of
    'row': LoopCount := MaxInt;
    'column': LoopCount := MemData.FieldCount;
  end;
end;

procedure THtmlTest.TplEndLoop(Sender: TObject; const ALoopName: String;
  const ALoopIndex: Integer; var ABreak: Boolean);
begin
  if ALoopName = 'row' then
  begin
    MemData.Next;
    ABreak := MemData.EOF;
  end;
end;

procedure THtmlTest.RunTest;
begin
  MemData := TMemDataset.Create(nil);
  InitMemData;
  Lines := TStringList.Create;
  Lines.LoadFromFile('htmltable.tpl');
  Tpl := TSimpleTemplate.Create;
  Tpl.OnGetValue := @TplGetValue;
  Tpl.OnGetCondition := @TplGetIfCondition;
  Tpl.OnGetLoopCount := @TplGetLoopCount;
  Tpl.OnEndLoop := @TplEndLoop;
  Tpl.Prepare(Lines.Text);
  Lines.Text := Tpl.Run;
  Lines.SaveToFile('htmltable.html');
  MemData.Close;
  MemData.Free;
  Lines.Free;
  Tpl.Free;
end;

var
  HTest: THtmlTest;

begin
  HTest := THtmlTest.Create;
  HTest.RunTest;
  HTest.Free;
end.

