program importtest;

uses
  sysutils, SimpleTpl, memds, db, Classes;

type
    TImportTest = class
      Lines: TStringList;
      Tpl: TSimpleTemplate;
      procedure RunTest;
    end;

begin
end.

