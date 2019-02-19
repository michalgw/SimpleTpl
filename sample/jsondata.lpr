program jsondata;

uses
  fpjson, SimpleTplJSON, jsonparser, Classes, LazUTF8, fpexprpars;

type

  { TJSONTest }

  TJSONTest = class
    Lines: TStringList;
    JSONTpl: TSimpleJSONTemplate;
    JSONData: TJSONData;
    procedure RunTest;
  end;

procedure TJSONTest.RunTest;
begin
  Lines := TStringList.Create;
  Lines.LoadFromFile('jsondata.json');
  JSONData := GetJSON(Lines.Text);
  Lines.LoadFromFile('jsondata.tpl');
  JSONTpl := TSimpleJSONTemplate.Create;
  JSONTpl.ExpressionParser.BuiltIns := [bcStrings, bcDateTime, bcMath, bcBoolean, bcConversion];
  JSONTpl.Values.Values['title'] := 'Address book';
  JSONTpl.AddPart('somepart', '<p>{{addressbook.name}}</p>');
  JSONTpl.Prepare(Lines.Text, False);
  JSONTpl.JSONData := JSONData;
  Lines.Text := JSONTpl.Run;
  Lines.SaveToFile('jsondata.html');
  Lines.Free;
  JSONTpl.Free;
  JSONData.Free;
end;

{ TJSONTest }

begin
  with TJSONTest.Create do
  begin
    RunTest;
    Free;
  end;
end.

