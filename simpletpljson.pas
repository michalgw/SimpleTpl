{ SimpleTplJSON - simple template engine with json imput

  Copyright (C) 2019 Michał Gawrycki info..gmsystems.pl

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit SimpleTplJSON;

{$mode objfpc}{$H+}

interface

uses
  SimpleTpl, fpjson, Classes, fpexprpars;

type

  { TSimpleJSONTemplate }

  TSimpleJSONTemplate = class(TSimpleTemplate)
  private
    FExprChar: Char;
    FExprNoEcho: Char;
    FExprSeparator: Char;
    FLoopSeparator: Char;
    FValues: TStringList;
    FJSONData: TJSONData;
    FExprParser: TFPExpressionParser;
    procedure SetJSONData(AValue: TJSONData);
    procedure ExpFuncAsInteger(var Result: TFPExpressionResult; const Args: TExprParameterArray);
    procedure ExpFuncAsFloat(var Result: TFPExpressionResult; const Args: TExprParameterArray);
    procedure ExpFuncAsString(var Result: TFPExpressionResult; const Args: TExprParameterArray);
    procedure ExpFuncAsDateTime(var Result: TFPExpressionResult; const Args: TExprParameterArray);
    procedure ExpFuncAsBoolean(var Result: TFPExpressionResult; const Args: TExprParameterArray);
    procedure ExpFuncArrayLen(var Result: TFPExpressionResult; const Args: TExprParameterArray);
    procedure ExpFuncValueExist(var Result: TFPExpressionResult; const Args: TExprParameterArray);
    procedure ExpFuncValueType(var Result: TFPExpressionResult; const Args: TExprParameterArray);
    procedure ExpFuncIsNull(var Result: TFPExpressionResult; const Args: TExprParameterArray);
    procedure ExpFuncAddVar(var Result: TFPExpressionResult; const Args: TExprParameterArray);
    procedure ExpFuncDelVar(var Result: TFPExpressionResult; const Args: TExprParameterArray);
    procedure ExpFuncClrVar(var Result: TFPExpressionResult; const {%H-}Args: TExprParameterArray);
    procedure ExpFuncSetVar_(var Result: TFPExpressionResult; const Args: TExprParameterArray);
    procedure SetValues(AValue: TStringList);
  protected
    procedure DoAfterRun; override;
    procedure DoGetValue(const AValueName: String; var AValue: String); override;
    procedure DoGetLoopCount(const ALoopName: String; var ALoopCount: Integer); override;
    procedure DoStartLoop(const ALoopName: String; const ALoopIndex: Integer; var ABreak: Boolean); override;
    procedure DoEndLoop(const ALoopName: String; const ALoopIndex: Integer; var ABreak: Boolean); override;
    procedure DoGetIfCondition(const ACondition: String; var AResult: Boolean); override;
    function ParseCounters(AExpression: String): String;
    procedure ClearExprVars;
  public
    constructor Create;
    destructor Destroy; override;
    property JSONData: TJSONData read FJSONData write SetJSONData;
    property ExpressionParser: TFPExpressionParser read FExprParser;
    property Values: TStringList read FValues write SetValues;
    property ExprSeparator: Char read FExprSeparator write FExprSeparator;
    property LoopSeparator: Char read FLoopSeparator write FLoopSeparator;
    property ExprChar: Char read FExprChar write FExprChar;
    property ExprNoEcho: Char read FExprNoEcho write FExprNoEcho;
  end;

var
  DefaultExprSep: Char = ';';
  DefaultLoopSep: Char = '|';
  DefaultExprChar: Char = '&';
  DefaultExprNoEcho: Char = '%';

implementation

uses
  SysUtils, DateUtils;

const
  RFC3339DateTimeFormat = 'yyyy"-"mm"-"dd"T"hh":"nn":"ss';

function StrIsNumber(AStr: String): Boolean;
var
  I: Integer;
begin
  if AStr = '' then
    Exit(False);
  for I := 1 to Length(AStr) do
    if not (AStr[I] in ['0','1','2','3','4','5','6','7','8','9']) then
      Exit(False);
  Result := True;
end;

{ TSimpleJSONTemplate }

procedure TSimpleJSONTemplate.SetJSONData(AValue: TJSONData);
begin
  FJSONData := AValue;
end;

procedure TSimpleJSONTemplate.ExpFuncAsInteger(var Result: TFPExpressionResult;
  const Args: TExprParameterArray);
var
  S: String;
  J: TJSONData;
begin
  Result.ResInteger := 0;
  S := Trim(ParseCounters(Args[0].ResString));
  if S = '' then
    Exit;
  if StrIsNumber(S) then
    Result.ResInteger := StrToInt(S)
  else if (FValues.IndexOfName(S) >= 0) and
    TryStrToInt64(FValues.Values[Args[0].ResString], Result.ResInteger) then
  else if Assigned(FJSONData) then
  begin
    J := TJSONObject(FJSONData).FindPath(S);
    if Assigned(J) then
      case J.JSONType of
        jtNull, jtUnknown: Result.ResInteger := 0;
        jtArray, jtObject: Result.ResInteger := J.Count;
        jtString: Result.ResInteger := StrToIntDef(J.AsString, 0);
        jtBoolean: if J.AsBoolean then Result.ResInteger := 1 else Result.ResInteger := 0;
        jtNumber: Result.ResInteger := J.AsInteger;
      end;
  end;
end;

procedure TSimpleJSONTemplate.ExpFuncAsFloat(var Result: TFPExpressionResult;
  const Args: TExprParameterArray);
var
  S: String;
  J: TJSONData;
begin
  Result.ResFloat := 0;
  if (FValues.IndexOfName(Args[0].ResString) >= 0) and
    TryStrToFloat(FValues.Values[Args[0].ResString], Result.ResFloat) then
  else if Assigned(FJSONData) then
  begin
    S := ParseCounters(Args[0].ResString);
    J := TJSONObject(FJSONData).FindPath(S);
    if Assigned(J) then
      case J.JSONType of
        jtNull, jtUnknown: Result.ResFloat := 0;
        jtArray, jtObject: Result.ResFloat := J.Count;
        jtString: Result.ResFloat := StrToFloatDef(J.AsString, 0);
        jtBoolean: if J.AsBoolean then Result.ResFloat := 1 else Result.ResFloat := 0;
        jtNumber: Result.ResFloat := J.AsFloat;
      end;
  end;
end;

procedure TSimpleJSONTemplate.ExpFuncAsString(var Result: TFPExpressionResult;
  const Args: TExprParameterArray);
var
  S: String;
  J: TJSONData;
begin
  Result.ResString := '';
  if (FValues.IndexOfName(Args[0].ResString) >= 0) then
    Result.ResString := FValues.Values[Args[0].ResString]
  else if Assigned(FJSONData) then
  begin
    S := ParseCounters(Args[0].ResString);
    J := TJSONObject(FJSONData).FindPath(S);
    if Assigned(J) then
      case J.JSONType of
        jtNull, jtUnknown: Result.ResString := '';
        jtArray, jtObject: Result.ResString := IntToStr(J.Count);
        jtString: Result.ResString := J.AsString;
        jtBoolean: Result.ResString := BoolToStr(J.AsBoolean);
        jtNumber: Result.ResString := FloatToStr(J.AsFloat);
      end;
  end;
end;

procedure TSimpleJSONTemplate.ExpFuncAsDateTime(
  var Result: TFPExpressionResult; const Args: TExprParameterArray);
var
  S: String;
  J: TJSONData;
begin
  Result.ResDateTime := 0;
  if (FValues.IndexOfName(Args[0].ResString) >= 0) and
    TryStrToDateTime(FValues.Values[Args[0].ResString], Result.ResDateTime) then
  else if Assigned(FJSONData) then
  begin
    S := ParseCounters(Args[0].ResString);
    J := TJSONObject(FJSONData).FindPath(S);
    if Assigned(J) then
      case J.JSONType of
        jtNull, jtUnknown: Result.ResDateTime := 0;
        jtArray, jtObject: Result.ResDateTime := J.Count;
        jtString: begin
          if not TryStrToDateTime(J.AsString, Result.ResDateTime) then
            if not TryStrToDate(J.AsString, Result.ResDateTime) then
              try
                Result.ResDateTime := ScanDateTime(RFC3339DateTimeFormat, J.AsString);
              except
              end;
        end;
        jtBoolean: if J.AsBoolean then Result.ResDateTime := 1 else Result.ResDateTime := 0;
        jtNumber: Result.ResDateTime := J.AsFloat;
      end;
  end;
end;

procedure TSimpleJSONTemplate.ExpFuncAsBoolean(var Result: TFPExpressionResult;
  const Args: TExprParameterArray);
var
  S: String;
  J: TJSONData;
begin
  Result.ResBoolean := False;
  if (FValues.IndexOfName(Args[0].ResString) >= 0) and
    TryStrToBool(FValues.Values[Args[0].ResString], Result.ResBoolean) then
  else  if Assigned(FJSONData) then
  begin
    S := ParseCounters(Args[0].ResString);
    J := TJSONObject(FJSONData).FindPath(S);
    if Assigned(J) then
      case J.JSONType of
        jtNull, jtUnknown: Result.ResBoolean := False;
        jtArray, jtObject: Result.ResBoolean := J.Count > 0;
        jtString: Result.ResBoolean := StrToBoolDef(J.AsString, False);
        jtBoolean: Result.ResBoolean := J.AsBoolean;
        jtNumber: Result.ResBoolean := J.AsFloat <> 0;
      end;
  end;
end;

procedure TSimpleJSONTemplate.ExpFuncArrayLen(var Result: TFPExpressionResult;
  const Args: TExprParameterArray);
var
  S: String;
  J: TJSONData;
begin
  Result.ResInteger := 0;
  if Assigned(FJSONData) then
  begin
    S := ParseCounters(Args[0].ResString);
    J := TJSONObject(FJSONData).FindPath(S);
    if Assigned(J) and (J.JSONType in [jtArray, jtObject]) then
      Result.ResInteger := J.Count;
  end;
end;

procedure TSimpleJSONTemplate.ExpFuncValueExist(
  var Result: TFPExpressionResult; const Args: TExprParameterArray);
var
  S: String;
begin
  Result.ResBoolean := False;
  if FValues.IndexOfName(Args[0].ResString) >= 0 then
    Result.ResBoolean := True
  else if Assigned(FJSONData) then
  begin
    S := ParseCounters(Args[0].ResString);
    Result.ResBoolean := Assigned(TJSONObject(FJSONData).FindPath(S));
  end;
end;

procedure TSimpleJSONTemplate.ExpFuncValueType(var Result: TFPExpressionResult;
  const Args: TExprParameterArray);
var
  S: String;
  J: TJSONData;
begin
  Result.ResInteger := 0;
  if FValues.IndexOfName(Args[0].ResString) >= 0 then
    Result.ResInteger := Integer(jtString)
  else if Assigned(FJSONData) then
  begin
    S := ParseCounters(Args[0].ResString);
    J := TJSONObject(FJSONData).FindPath(S);
    if Assigned(J) then
      Result.ResInteger := Integer(J.JSONType);
  end;
end;

procedure TSimpleJSONTemplate.ExpFuncIsNull(var Result: TFPExpressionResult;
  const Args: TExprParameterArray);
var
  S: String;
  J: TJSONData;
begin
  Result.ResBoolean := True;
  if Assigned(FJSONData) then
  begin
    S := ParseCounters(Args[0].ResString);
    J := TJSONObject(FJSONData).FindPath(S);
    if Assigned(J) then
      Result.ResBoolean := J.IsNull;
  end;
end;

procedure TSimpleJSONTemplate.ExpFuncAddVar(var Result: TFPExpressionResult;
  const Args: TExprParameterArray);
var
  Id: TFPExprIdentifierDef;
  RT: TResultType;
begin
  Id := FExprParser.IdentifierByName(Args[0].ResString);
  if Id = nil then
  begin
    if Args[1].ResultType = rtInteger then
      RT := TResultType(Args[1].ResInteger)
    else if (Args[1].ResultType = rtString) and (Length(Args[1].ResString) > 0) then
      RT := CharToResultType(Args[1].ResString[1])
    else
    begin
      Result.ResBoolean := False;
      Exit;
    end;
    case RT of
      rtBoolean: FExprParser.Identifiers.AddBooleanVariable(Args[0].ResString, False);
      rtInteger: FExprParser.Identifiers.AddIntegerVariable(Args[0].ResString, 0);
      rtFloat: FExprParser.Identifiers.AddFloatVariable(Args[0].ResString, 0);
      rtDateTime: FExprParser.Identifiers.AddDateTimeVariable(Args[0].ResString, 0);
      rtString: FExprParser.Identifiers.AddStringVariable(Args[0].ResString, '');
      rtCurrency: FExprParser.Identifiers.AddCurrencyVariable(Args[0].ResString, 0);
    end;
    Result.ResBoolean := True;
  end
  else
    Result.ResBoolean := False;
end;

procedure TSimpleJSONTemplate.ExpFuncDelVar(var Result: TFPExpressionResult;
  const Args: TExprParameterArray);
var
  I: Integer;
begin
  I := FExprParser.Identifiers.IndexOfIdentifier(Args[0].ResString);
  if I >= 0 then
  begin
    FExprParser.Identifiers.Delete(I);
    Result.ResBoolean := True;
  end
  else
    Result.ResBoolean := False;
end;

procedure TSimpleJSONTemplate.ExpFuncClrVar(var Result: TFPExpressionResult;
  const Args: TExprParameterArray);
begin
  ClearExprVars;
  Result.ResBoolean := True;
end;

procedure TSimpleJSONTemplate.ExpFuncSetVar_(var Result: TFPExpressionResult;
  const Args: TExprParameterArray);
var
  Id: TFPExprIdentifierDef;
begin
  Result.ResBoolean := False;
  Id := FExprParser.Identifiers.FindIdentifier(Args[0].ResString);
  if Assigned(Id) and (Id.ResultType = Args[1].ResultType) then
  begin
    case Args[1].ResultType of
      rtBoolean: Id.AsBoolean := Args[1].ResBoolean;
      rtInteger: Id.AsInteger := Args[1].ResInteger;
      rtFloat: Id.AsFloat := Args[1].ResFloat;
      rtDateTime: Id.AsDateTime := Args[1].ResDateTime;
      rtString: Id.AsString := Args[1].ResString;
      rtCurrency: Id.AsCurrency := Args[1].ResCurrency;
    end;
    Result.ResBoolean := True;
  end;
end;

procedure TSimpleJSONTemplate.SetValues(AValue: TStringList);
begin
  if FValues = AValue then Exit;
  FValues.Assign(AValue);
end;

procedure TSimpleJSONTemplate.DoAfterRun;
begin
  ClearExprVars;
end;

procedure TSimpleJSONTemplate.DoGetValue(const AValueName: String;
  var AValue: String);
var
  S: String;
  J: TJSONObject;
  R: TFPExpressionResult;
  C: Char;
  SA: TStringArray;
begin
  if FValues.IndexOfName(AValueName) >= 0 then
    AValue := FValues.Values[AValueName]
  else if (AValueName <> '') and Assigned(FJSONData) then
  begin
    S := ParseCounters(AValueName);
    C := S[1];
    if CharInSet(C, [FExprChar, FExprNoEcho]) then
    begin
      SA := Copy(S, 2).Split(FExprSeparator);
      for S in SA do
        if Trim(S) <> '' then
        begin
          FExprParser.Expression := S;
          R := FExprParser.Evaluate;
          if C = FExprChar then
            case FExprParser.ResultType of
              rtBoolean: AValue := AValue + BoolToStr(R.ResBoolean);
              rtString: AValue := AValue + R.ResString;
              rtInteger: AValue := AValue + IntToStr(R.ResInteger);
              rtFloat: AValue := AValue + FloatToStr(R.ResFloat);
              rtCurrency: AValue := AValue + CurrToStr(R.ResCurrency);
              rtDateTime: AValue := AValue + DateTimeToStr(R.ResDateTime);
            end;
        end;
    end
    else
    begin
      J := TJSONObject(FJSONData.FindPath(S));
      if Assigned(J) then
        AValue := J.AsString;
    end;
  end;
  inherited DoGetValue(AValueName, AValue);
end;

procedure TSimpleJSONTemplate.DoGetLoopCount(const ALoopName: String;
  var ALoopCount: Integer);
var
  J: TJSONObject;
  SA: TStringArray;
  Id: TFPExprIdentifierDef;
  R: TFPExpressionResult;
begin
  if ALoopName[1] = FExprChar then
  begin
    SA := Copy(ALoopName, 2).Split([FLoopSeparator]);
    if (Length(SA) > 0) and (Trim(SA[0]) <> '') then
    begin
      FExprParser.Expression := SA[0];
      R := FExprParser.Evaluate;
      if R.ResultType = rtInteger then
        ALoopCount := R.ResInteger
    end
    else
      ALoopCount := MaxInt;
    if (Length(SA) >= 4) and (Trim(SA[3]) <> '') then
    begin
      Id := FExprParser.IdentifierByName(Trim(SA[3]));
      if Assigned(Id) then
      begin
        if (Id.IdentifierType = itVariable) and (Id.ResultType = rtInteger) then
          Id.AsInteger := 0
        else
          raise Exception.Create('Identifier exists: ' + SA[1]);
      end
      else
        FExprParser.Identifiers.AddIntegerVariable(Trim(SA[3]), 0);
    end;
  end
  else if Assigned(FJSONData) then
  begin
    SA := ALoopName.Trim.Split([FLoopSeparator]);
    if Length(SA) = 2 then
    begin
      J := TJSONObject(FJSONData.FindPath(ParseCounters(SA[0])));
      if Assigned(J) and (J.JSONType = jtArray) then
      begin
        ALoopCount := J.Count;
        if ALoopCount > 0 then
        begin
          SA[1] := Trim(SA[1]);
          Id := FExprParser.IdentifierByName(SA[1]);
          if Assigned(Id) then
          begin
            if (Id.IdentifierType = itVariable) and (Id.ResultType = rtInteger) then
              Id.AsInteger := 0
            else
              raise Exception.Create('Identifier exists: ' + SA[1]);
          end
          else
            FExprParser.Identifiers.AddIntegerVariable(SA[1], 0);
        end;
      end;
    end;
  end;
  inherited DoGetLoopCount(ALoopName, ALoopCount);
end;

procedure TSimpleJSONTemplate.DoStartLoop(const ALoopName: String;
  const ALoopIndex: Integer; var ABreak: Boolean);
var
  SA: TStringArray;
  Id: TFPExprIdentifierDef;
  R: TFPExpressionResult;
begin
  if ALoopName.Trim[1] = FExprChar then
  begin
    SA := Copy(ALoopName.Trim, 2).Split([FLoopSeparator]);
    if (Length(SA) >= 2) and (Trim(SA[1]) <> '') then
    begin
      FExprParser.Expression := SA[1];
      R := FExprParser.Evaluate;
      case R.ResultType of
        rtBoolean: ABreak := R.ResBoolean;
        rtInteger: ABreak := R.ResInteger <> 0;
        rtFloat: ABreak := R.ResFloat <> 0;
      end;
    end;
    if (Length(SA) >= 4) then
    begin
      Id := FExprParser.IdentifierByName(Trim(SA[3]));
      if Assigned(Id) and (Id.IdentifierType = itVariable) and (Id.ResultType = rtInteger) then
        Id.AsInteger := ALoopIndex;
    end;
  end
  else if Assigned(FJSONData) then
  begin
    SA := ALoopName.Trim.Split([FLoopSeparator]);
    if (Length(SA) = 2) then
    begin
      Id := FExprParser.IdentifierByName(Trim(SA[1]));
      if Assigned(Id) and (Id.IdentifierType = itVariable) and (Id.ResultType = rtInteger) then
        Id.AsInteger := ALoopIndex;
    end;
  end;
  inherited DoEndLoop(ALoopName, ALoopIndex, ABreak);
end;

procedure TSimpleJSONTemplate.DoEndLoop(const ALoopName: String;
  const ALoopIndex: Integer; var ABreak: Boolean);
var
  SA: TStringArray;
  R: TFPExpressionResult;
begin
  if ALoopName.Trim[1] = FExprChar then
  begin
    SA := Copy(ALoopName.Trim, 2).Split([FLoopSeparator]);
    if (Length(SA) >= 3) and (Trim(SA[2]) <> '') then
    begin
      FExprParser.Expression := SA[2];
      R := FExprParser.Evaluate;
      case R.ResultType of
        rtBoolean: ABreak := R.ResBoolean;
        rtInteger: ABreak := R.ResInteger <> 0;
        rtFloat: ABreak := R.ResFloat <> 0;
      end;
    end;
  end;
  inherited DoEndLoop(ALoopName, ALoopIndex, ABreak);
end;

procedure TSimpleJSONTemplate.DoGetIfCondition(const ACondition: String;
  var AResult: Boolean);
var
  S: String;
  J: TJSONObject;
  R: TFPExpressionResult;
begin
  if Assigned(FJSONData) then
  begin
    S := ParseCounters(ACondition);
    if S[1] = FExprChar then
    begin
      S := Copy(S, 2);
      begin
        FExprParser.Expression := S;
        R := FExprParser.Evaluate;
        case FExprParser.ResultType of
          rtBoolean: AResult := R.ResBoolean;
          rtString: AResult := StrToBoolDef(R.ResString, ((Length(R.ResString) > 0) and (StrToIntDef(R.ResString, 0) <> 0)));
          rtInteger: AResult := R.ResInteger <> 0;
          rtFloat: AResult := R.ResFloat <> 0;
          rtCurrency: AResult := R.ResCurrency <> 0;
          rtDateTime: AResult := R.ResDateTime <> 0;
        end;
      end;
    end
    else
    begin
      J := TJSONObject(FJSONData.FindPath(S));
      if Assigned(J) then
        case J.JSONType of
          jtObject, jtArray: AResult := J.Count > 0;
          jtString: AResult := StrToBoolDef(J.AsString, ((Length(J.AsString) > 0) and (StrToIntDef(J.AsString, 0) <> 0)));
          jtNull, jtUnknown: AResult := False;
          jtNumber: AResult := J.AsFloat <> 0;
          jtBoolean: AResult := J.AsBoolean;
        end;
    end;
  end;
  inherited DoGetIfCondition(ACondition, AResult);
end;

function TSimpleJSONTemplate.ParseCounters(AExpression: String): String;
var
  I, J: Integer;
  Params: TStringArray;
  Id: TFPExprIdentifierDef;
  S: String;
begin
  SetLength(Params{%H-}, 0);
  I := Pos('[', AExpression);
  while I > 0 do
  begin
    J := Pos(']', AExpression, I);
    if J > 0 then
    begin
      SetLength(Params, Succ(Length(Params)));
      Params[Pred(Length(Params))] := Copy(AExpression, I, J - I + 1);
    end
    else
      Break;
    I := Pos('[', AExpression, Succ(J));
  end;
  Result := Trim(AExpression);
  for I := 0 to Length(Params) - 1 do
  begin
    S := Trim(Copy(Params[I], 2, Length(Params[I]) - 2));
    if not StrIsNumber(S) then
    begin
      Id := FExprParser.IdentifierByName(S);
      if Assigned(Id) and (Id.ResultType = rtInteger) then
        Result := StringReplace(Result, Params[I], '[' + IntToStr(Id.AsInteger) + ']', [rfReplaceAll]);
    end;
  end;
end;

procedure TSimpleJSONTemplate.ClearExprVars;
var
  I: Integer;
begin
  for I := FExprParser.Identifiers.Count - 1 downto 0 do
    if FExprParser.Identifiers[I].IdentifierType = itVariable then
      FExprParser.Identifiers.Delete(I);
end;

constructor TSimpleJSONTemplate.Create;
begin
  inherited Create;
  FExprChar := DefaultExprChar;
  FExprNoEcho := DefaultExprNoEcho;
  FExprSeparator := DefaultExprSep;
  FLoopSeparator := DefaultLoopSep;
  FValues := TStringList.Create;
  FExprParser := TFPExpressionParser.Create(nil);
  FExprParser.Identifiers.AddFunction('AsInteger',  'I', 'S',  @ExpFuncAsInteger);
  FExprParser.Identifiers.AddFunction('AsString',   'S', 'S',  @ExpFuncAsString);
  FExprParser.Identifiers.AddFunction('AsFloat',    'F', 'S',  @ExpFuncAsFloat);
  FExprParser.Identifiers.AddFunction('AsBoolean',  'B', 'S',  @ExpFuncAsBoolean);
  FExprParser.Identifiers.AddFunction('AsDateTime', 'D', 'S',  @ExpFuncAsDateTime);
  FExprParser.Identifiers.AddFunction('ArrayLen',   'I', 'S',  @ExpFuncArrayLen);
  FExprParser.Identifiers.AddFunction('ValueExist', 'B', 'S',  @ExpFuncValueExist);
  FExprParser.Identifiers.AddFunction('ValueType',  'I', 'S',  @ExpFuncValueType);
  FExprParser.Identifiers.AddFunction('IsNull',     'B', 'S',  @ExpFuncIsNull);
  FExprParser.Identifiers.AddFunction('AddVar',     'B', 'SI', @ExpFuncAddVar);
  FExprParser.Identifiers.AddFunction('AddVarT',    'B', 'SS', @ExpFuncAddVar);
  FExprParser.Identifiers.AddFunction('DelVar',     'B', 'S',  @ExpFuncDelVar);
  FExprParser.Identifiers.AddFunction('ClearVars',  'B', '',   @ExpFuncClrVar);
  FExprParser.Identifiers.AddFunction('SetVarB',    'B', 'SB', @ExpFuncSetVar_);
  FExprParser.Identifiers.AddFunction('SetVarI',    'B', 'SI', @ExpFuncSetVar_);
  FExprParser.Identifiers.AddFunction('SetVarF',    'B', 'SF', @ExpFuncSetVar_);
  FExprParser.Identifiers.AddFunction('SetVarD',    'B', 'SD', @ExpFuncSetVar_);
  FExprParser.Identifiers.AddFunction('SetVarS',    'B', 'SS', @ExpFuncSetVar_);
  FExprParser.Identifiers.AddFunction('SetVarC',    'B', 'SC', @ExpFuncSetVar_);
end;

destructor TSimpleJSONTemplate.Destroy;
begin
  FExprParser.Free;
  FValues.Free;
  inherited Destroy;
end;

end.
