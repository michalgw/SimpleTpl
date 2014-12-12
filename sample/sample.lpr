program sample;

uses
  SimpleTpl, sysutils;

type

  { TTest1 }

  TTest1 = class
    Tpl: TSimpleTemplate;
    procedure TplGetValue(Sender: TObject; const AValueName: String; var AValue: String);
    procedure TplGetIfCondition(Sender: TObject; const ACondition: String; var Result: Boolean);
    procedure TplGetLoopCount(Sender: TObject; const ALoop: String; var LoopCount: Integer);
    function RunSingleTest(ATemplate: String): String;
    procedure RunTest;
  end;

const
  TESTS: array[0..21] of String = (
    'Siple value 1: {{value1}} and value 2: {{value2}}',
    'If test true: {{if condition_t}}TRUE1{{endif}}',
    'If test false: {{if condition_f}}TRUE1{{endif}}',
    'If else test true: {{if condition_t}}TRUE2{{else}}FALSE2{{endif}}',
    'If else test false: {{if condition_f}}TRUE2{{else}}FALSE2{{endif}}',
    'If test true with value: {{if condition_t}}{{value_t}}{{endif}}',
    'If test false with value: {{if condition_f}}{{value_t}}{{endif}}',
    'If else true with value: {{if condition_t}}{{value_t}}{{else}}{{value_t}}{{endif}}',
    'If else false with value: {{if condition_t}}{{value_f}}{{else}}{{value_t}}{{endif}}',
    'If if test true true: {{if condition_t}}TRUE3{{if condition2_t}}TRUE3_2{{endif}}{{endif}}',
    'If if test false true: {{if condition_f}}TRUE3{{if condition2_t}}TRUE3_2{{endif}}{{endif}}',
    'If if test true false: {{if condition_t}}TRUE3{{if condition2_f}}TRUE3_2{{endif}}{{endif}}',
    'If if test false false: {{if condition_f}}TRUE3{{if condition2_f}}TRUE3_2{{endif}}{{endif}}',
    'If else if else test true true: {{if condition_t}}TRUE3{{if condition2_t}}TRUE3_2{{endif}}{{endif}}',
    'If if test false true: {{if condition_f}}TRUE3{{if condition2_t}}TRUE3_2{{endif}}{{endif}}',
    'If if test true false: {{if condition_t}}TRUE3{{if condition2_f}}TRUE3_2{{endif}}{{endif}}',
    'If if test false false: {{if condition_f}}TRUE3{{if condition2_f}}TRUE3_2{{endif}}{{endif}}',
    'If else if else test true true: {{if condition_t}}TRUE3{{if condition2_t}}TRUE3_2{{else}}FALSE3_2{{endif}}{{else}}FALSE3_3{{endif}}',
    'If else if else test false true: {{if condition_f}}TRUE3{{if condition2_t}}TRUE3_2{{else}}FALSE3_2{{endif}}{{else}}FALSE3_3{{endif}}',
    'If else if else test true false: {{if condition_f}}TRUE3{{if condition2_f}}TRUE3_2{{else}}FALSE3_2{{endif}}{{else}}FALSE3_3{{endif}}',
    'If else if else test false false: {{if condition_t}}TRUE3{{if condition2_t}}TRUE3_2{{else}}FALSE3_2{{endif}}{{else}}FALSE3_3{{endif}}',
    '{{loop lp1}}Loop1: {{loopval1}};{{loop lp2}} Loop2: {{loopval2}};{{endloop}} End loop1 {{endloop}}');


{ TTest1 }

procedure TTest1.TplGetValue(Sender: TObject; const AValueName: String;
  var AValue: String);
begin
  case AValueName of
    'value1': AValue := '"first value"';
    'value2': AValue := '"second value"';
    'value_t': AValue := '"True"';
    'value_f': AValue := '"False"';
    'loopval1': AValue := IntToStr(Tpl.GetLoopCounter('lp1'));
    'loopval2': AValue := IntToStr(Tpl.GetLoopCounter('lp2'));
  end;
end;

procedure TTest1.TplGetIfCondition(Sender: TObject; const ACondition: String;
  var Result: Boolean);
begin
  case ACondition of
    'condition_t', 'condition2_t': Result := True;
    'condition_f', 'condition2_f': Result := False;
  end;
end;

procedure TTest1.TplGetLoopCount(Sender: TObject; const ALoop: String;
  var LoopCount: Integer);
begin
  case ALoop of
    'lp1': LoopCount := 5;
    'lp2': LoopCount := 6;
  end;
end;

function TTest1.RunSingleTest(ATemplate: String): String;
begin
  WriteLn('Template:');
  WriteLn(ATemplate);
  WriteLn('Otput:');
  Tpl.Prepare(ATemplate);
  WriteLn(Tpl.Run);
  WriteLn;
end;

procedure TTest1.RunTest;
var
  I: Integer;
begin
  Tpl := TSimpleTemplate.Create;
  Tpl.OnGetValue := @TplGetValue;
  Tpl.OnGetCondition := @TplGetIfCondition;
  Tpl.OnGetLoopCount := @TplGetLoopCount;

  for I := 0 to 21 do
    RunSingleTest(TESTS[I]);

  Tpl.Free;
end;

var
  Test: TTest1;

begin
  Test := TTest1.Create;
  Test.RunTest;
  Test.Free;
end.

