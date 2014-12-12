{ SimpleTpl - simple template engine

  Copyright (C) 2014 Michał Gawrycki info..gmsystems.pl

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

unit SimpleTpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
  EParserError = class(Exception);

  { Events }

  TGetValueEvent = procedure(Sender: TObject; const AValueName: String; var AValue: String) of object;
  TGetIfConditionEvent = procedure(Sender: TObject; const ACondition: String; var AResult: Boolean) of object;
  TGetLoopCountEvent = procedure(Sender: TObject; const ALoopName: String; var ALoopCount: Integer) of object;
  TLoopEvent = procedure(Sender: TObject; const ALoopName: String; const ALoopIndex: Integer; var ABreak: Boolean) of object;

  TBlock = class;
  TBlocks = specialize TFPGObjectList<TBlock>;

  { TBlock }

  TBlock = class
    Parent: TBlock;
    Items: TBlocks;
    Text: String;
    constructor Create(AParent: TBlock);
    destructor Destroy; override;
  end;

  TValueBlock = class(TBlock);

  { TIfBlock }

  TIfBlock = class(TBlock)
    ElseItems: TBlocks;
    IsElseIf: Boolean;
    constructor Create(AParent: TBlock);
    destructor Destroy; override;
  end;

  { TLoopBlock }

  TLoopBlock = class(TBlock)
    CurrentIndex: Integer;
    constructor Create(AParent: TBlock);
  end;

  { TSimpleTemplate }

  TSimpleTemplate = class
  private
    FBlocks: TBlock;
    FLoops: TBlocks;
    FIfs: TBlocks;
    FValues: TBlocks;
    FPrepared: Boolean;
    FStartTag: String;
    FEndTag: String;
    FIfTag: String;
    FElseTag: String;
    FEndIfTag: String;
    FLoopTag: String;
    FEndLoopTag: String;
    FOnGetValue: TGetValueEvent;
    FIsRunning: Boolean;
    FStopping: Boolean;
    FOnGetCondition: TGetIfConditionEvent;
    FOnGetLoopCount: TGetLoopCountEvent;
    FOnStartLoop: TLoopEvent;
    FOnEndLoop: TLoopEvent;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Prepare(ATemplate: String);
    function Run: String;
    procedure Stop;
    function GetLoopCounter(ALoopName: String): Integer;
    procedure GetLoops(AList: TStrings; OnlyActive: Boolean = False);
    procedure GetIfs(AList: TStrings);
    procedure GetValues(AList: TStrings);
  published
    property StartTag: String read FStartTag write FStartTag;
    property EndTag: String read FEndTag write FEndTag;
    property IfTag: String read FIfTag write FIfTag;
    property ElseIfTag: String read FElseTag write FElseTag;
    property EndIfTag: String read FEndIfTag write FEndIfTag;
    property LoopTag: String read FLoopTag write FLoopTag;
    property EndLoopTag: String read FEndLoopTag write FEndLoopTag;
    property IsRunning: Boolean read FIsRunning;
    property Stopping: Boolean read FStopping;
    property OnGetValue: TGetValueEvent read FOnGetValue write FOnGetValue;
    property OnGetCondition: TGetIfConditionEvent read FOnGetCondition write FOnGetCondition;
    property OnGetLoopCount: TGetLoopCountEvent read FOnGetLoopCount write FOnGetLoopCount;
    property OnStartLoop: TLoopEvent read FOnStartLoop write FOnStartLoop;
    property OnEndLoop: TLoopEvent read FOnEndLoop write FOnEndLoop;
  end;

var
  DefaultStartTag: String = '{{';
  DefaultEndTag: String = '}}';
  DefaultIfTag: String = 'if';
  DefaultElseTag: String = 'else';
  DefaultEndIfTag: String = 'endif';
  DefaultLoopTag: String = 'loop';
  DefaultEndLoopTag: String = 'endloop';

implementation

uses
  strutils;

{ TBlock }

constructor TBlock.Create(AParent: TBlock);
begin
  Items := TBlocks.Create(True);
  Parent := AParent;
end;

destructor TBlock.Destroy;
begin
  Items.Free;
  inherited Destroy;
end;

{ TIfBlock }

constructor TIfBlock.Create(AParent: TBlock);
begin
  inherited Create(AParent);
  ElseItems := TBlocks.Create(True);
  IsElseIf := False;
end;

destructor TIfBlock.Destroy;
begin
  ElseItems.Free;
  inherited Destroy;
end;

{ TLoopBlock }

constructor TLoopBlock.Create(AParent: TBlock);
begin
  inherited Create(AParent);
  CurrentIndex := -1;
end;

{ TSimpleTemplate }

constructor TSimpleTemplate.Create;
begin
  inherited;
  FBlocks := TBlock.Create(nil);
  FLoops := TBlocks.Create(False);
  FIfs := TBlocks.Create(False);
  FValues := TBlocks.Create(False);
  FStartTag := DefaultStartTag;
  FEndTag := DefaultEndTag;
  FIfTag := DefaultIfTag;
  FElseTag := DefaultElseTag;
  FEndIfTag := DefaultEndIfTag;
  FLoopTag := DefaultLoopTag;
  FEndLoopTag := DefaultEndLoopTag;
  FPrepared := False;
  FIsRunning := False;
  FStopping := False;
end;

destructor TSimpleTemplate.Destroy;
begin
  FBlocks.Free;
  FLoops.Free;
  FIfs.Free;
  FValues.Free;
  inherited Destroy;
end;

procedure TSimpleTemplate.Prepare(ATemplate: String);
var
  CurrentObject, NewObject: TBlock;
  CurPos: Integer;
  TagStart, TagEnd: Integer;
  TagText: String;
begin
  if FIsRunning then
    raise EParserError.Create('Parser is running.');
  FPrepared := False;
  FBlocks.Items.Clear;
  FLoops.Clear;
  FIfs.Clear;
  FValues.Clear;
  CurrentObject := FBlocks;
  CurPos := 1;
  while CurPos < Length(ATemplate) do
  begin
    TagStart := PosEx(FStartTag, ATemplate, CurPos);
    if (TagStart = 0) then
    begin
      NewObject := TBlock.Create(CurrentObject);
      CurrentObject.Items.Add(NewObject);
      NewObject.Text := Copy(ATemplate, CurPos, Length(ATemplate));
      Break;
    end;
    TagEnd := PosEx(FEndTag, ATemplate, TagStart + Length(FStartTag));
    if TagEnd = 0 then
      raise EParserError.Create('Unclosed tag');
    TagText := Trim(Copy(ATemplate, TagStart + Length(FStartTag), TagEnd - TagStart - Length(FStartTag)));
    if TagStart > CurPos then
    begin
      NewObject := TBlock.Create(CurrentObject);
      if (CurrentObject is TIfBlock) and (TIfBlock(CurrentObject).IsElseIf) then
        TIfBlock(CurrentObject).ElseItems.Add(NewObject)
      else
        CurrentObject.Items.Add(NewObject);
      NewObject.Text := Copy(ATemplate, CurPos, TagStart - CurPos);
    end;
    if Pos(FIfTag + ' ', TagText) = 1 then
    begin
      NewObject := TIfBlock.Create(CurrentObject);
      FIfs.Add(NewObject);
      CurrentObject.Items.Add(NewObject);
      CurrentObject := NewObject;
      CurrentObject.Text := Copy(TagText, Pos(FIfTag, TagText) + Length(FIfTag) + 1, Length(TagText));
      CurPos := TagEnd + Length(FEndTag);
      Continue;
    end;
    if Pos(FEndIfTag, TagText) = 1 then
    begin
      if not (CurrentObject is TIfBlock) then
        raise EParserError.Create('Unexcepted "endif" tag.');
      CurrentObject := CurrentObject.Parent;
      CurPos := TagEnd + Length(FEndTag);
      Continue;
    end;
    if Pos(FElseTag, TagText) = 1 then
    begin
      if not (CurrentObject is TIfBlock) or TIfBlock(CurrentObject).IsElseIf then
        raise EParserError.Create('Unexcepted "elseif" tag.');
      TIfBlock(CurrentObject).IsElseIf := True;
      CurPos := TagEnd + Length(FEndTag);
      Continue;
    end;
    if Pos(FLoopTag + ' ', TagText) = 1 then
    begin
      TagText := Copy(TagText, Pos(FLoopTag, TagText) + Length(FLoopTag) + 1, Length(TagText));
      NewObject := CurrentObject;
      while NewObject.Parent <> nil do
      begin
        if (NewObject is TLoopBlock) and (NewObject.Text = TagText) then
          raise EParserError.Create('Loop name same as parent loop.');
        NewObject := NewObject.Parent;
      end;
      NewObject := TLoopBlock.Create(CurrentObject);
      if (CurrentObject is TIfBlock) and (TIfBlock(CurrentObject).IsElseIf) then
        TIfBlock(CurrentObject).ElseItems.Add(NewObject)
      else
        CurrentObject.Items.Add(NewObject);
      NewObject.Text := TagText;
      CurrentObject := NewObject;
      FLoops.Add(CurrentObject);
      CurPos := TagEnd + Length(FEndTag);
      Continue;
    end;
    if Pos(FEndLoopTag, TagText) = 1 then
    begin
      if not (CurrentObject is TLoopBlock) then
        raise EParserError.Create('Unexcepted "endloop" tag.');
      CurrentObject := CurrentObject.Parent;
      CurPos := TagEnd + Length(FEndTag);
      Continue;
    end;
    NewObject := TValueBlock.Create(CurrentObject);
    if (CurrentObject is TIfBlock) and (TIfBlock(CurrentObject).IsElseIf) then
      TIfBlock(CurrentObject).ElseItems.Add(NewObject)
    else
      CurrentObject.Items.Add(NewObject);
    NewObject.Text := TagText;
    FValues.Add(NewObject);
    CurPos := TagEnd + Length(FEndTag);
  end;
  FPrepared := True;
end;

function TSimpleTemplate.Run: String;

procedure DoRun(AItems: TBlocks);
var
  I, J: Integer;
  ResCond: Boolean;
  ResVal: String;
  LoopCnt: Integer;
  CurItem: TBlock;
begin
  for I := 0 to AItems.Count - 1 do
  begin
    if FStopping then
      Exit;
    CurItem := AItems[I];
    if CurItem is TLoopBlock then
    begin
      LoopCnt := 0;
      if Assigned(FOnGetLoopCount) then
        FOnGetLoopCount(Self, CurItem.Text, LoopCnt);
      for J := 0 to LoopCnt - 1 do
      begin
        TLoopBlock(CurItem).CurrentIndex := J;
        ResCond := False;
        if Assigned(FOnStartLoop) then
          FOnStartLoop(Self, CurItem.Text, J, ResCond);
        if ResCond then
          Break;
        DoRun(CurItem.Items);
        if Assigned(FOnEndLoop) then
          FOnEndLoop(Self, CurItem.Text, J, ResCond);
        if ResCond then
          Break;
      end;
      TLoopBlock(CurItem).CurrentIndex := -1;
      Continue;
    end;
    if CurItem is TIfBlock then
    begin
      ResCond := False;
      if Assigned(FOnGetCondition) then
        FOnGetCondition(Self, CurItem.Text, ResCond);
      if ResCond then
        DoRun(CurItem.Items)
      else
        DoRun(TIfBlock(CurItem).ElseItems);
      Continue;
    end;
    if CurItem is TValueBlock then
    begin
      ResVal := '';
      if Assigned(FOnGetValue) then
        OnGetValue(Self, CurItem.Text, ResVal);
      Run := Run + ResVal;
      Continue;
    end;
    if CurItem is TBlock then
      Run := Run + CurItem.Text;
  end;
end;

begin
  if not FPrepared then
    Exit;
  FIsRunning := True;
  FStopping := False;
  Result := '';
  DoRun(FBlocks.Items);
  FIsRunning := False;
  FStopping := False;
end;

procedure TSimpleTemplate.Stop;
begin
  if FIsRunning then
    FStopping := True;
end;

function TSimpleTemplate.GetLoopCounter(ALoopName: String): Integer;
var
  I: Integer;
begin
  Result := -1;
  if not FIsRunning then
    Exit;
  for I := 0 to FLoops.Count - 1 do
    if FLoops[I].Text = ALoopName then
    begin
      Result := TLoopBlock(FLoops[I]).CurrentIndex;
      if Result > -1 then
        Break;
    end;
end;

procedure TSimpleTemplate.GetLoops(AList: TStrings; OnlyActive: Boolean);
var
  I: Integer;
begin
  for I := 0 to FLoops.Count - 1 do
    if (not OnlyActive) or (TLoopBlock(FLoops[I]).CurrentIndex <> -1) then
      AList.Add(FLoops[I].Text);
end;

procedure TSimpleTemplate.GetIfs(AList: TStrings);
var
  I: Integer;
begin
  for I := 0 to FIfs.Count - 1 do
    AList.Add(FIfs[I].Text);
end;

procedure TSimpleTemplate.GetValues(AList: TStrings);
var
  I: Integer;
begin
  for I := 0 to FValues.Count - 1 do
    AList.Add(FValues[I].Text);
end;

end.

