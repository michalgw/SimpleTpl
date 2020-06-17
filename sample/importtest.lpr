program importtest;

uses
  sysutils, SimpleTpl, memds, db, Classes;

type
    TImportTest = class
      Lines: TStringList;
      Tpl: TSimpleTemplate;
      procedure RunTest;
    end;


procedure TImportTest.RunTest;
begin
  Lines := TStringList.Create;
  Lines.LoadFromFile('importtest.tpl');
  Tpl := TSimpleTemplate.Create;
  {Tpl.OnGetValue := @TplGetValue;
  Tpl.OnGetCondition := @TplGetIfCondition;
  Tpl.OnGetLoopCount := @TplGetLoopCount;
  Tpl.OnEndLoop := @TplEndLoop;}
  Tpl.Prepare(Lines.Text);
  Lines.Text := Tpl.Run;
  Lines.SaveToFile('importtest.html');
  Lines.Free;
  Tpl.Free;
end;

var
  ITest: TImportTest;

begin
  ITest := TImportTest.Create;
  ITest.RunTest;
  ITest.Free
end.

