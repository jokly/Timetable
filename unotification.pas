unit UNotification;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

TProtectRecord = record
  IndexTable, RecordID: Integer;
end;

{ TNotification }

TNotification = class(TObject)
  public
     class procedure Subscribe(AEvent: TNotifyEvent);
     class procedure UpdateDirectoryForms();
     class procedure AddToProtect(ATable: Integer);
     class procedure DeleteFromProtect(ATable: Integer);
     class function IsEditable(ATable: Integer): Boolean;
     class procedure AddToProtect(ATable, ARecId: Integer);
     class procedure DeleteFromProtect(ATable, ARecId: Integer);
     class function IsEditable(ATable, ARecId: Integer): Boolean;
end;

implementation

var
  Events: array of TNotifyEvent;
  ProtectTables: array of Integer;
  ProtectRecords:array of TProtectRecord;

{ TNotification }

class procedure TNotification.Subscribe(AEvent: TNotifyEvent);
begin
  SetLength(Events, Length(Events) + 1);
  Events[High(Events)]:= AEvent;
end;

class procedure TNotification.UpdateDirectoryForms;
var
  i: Integer;
begin
  for i:= 0 to High(Events) do
    Events[i](Nil);
end;

class procedure TNotification.AddToProtect(ATable: Integer);
begin
  SetLength(ProtectTables, Length(ProtectTables) + 1);
  ProtectTables[High(ProtectTables)]:= ATable;
end;

class procedure TNotification.DeleteFromProtect(ATable: Integer);
var
  i, j: Integer;
begin
  for i:= 0 to High(ProtectTables) do begin
    if ATable = ProtectTables[i] then begin
      for j:= i to High(ProtectTables) - 1 do
        ProtectTables[j]:= ProtectTables[j + 1];
      SetLength(ProtectTables, Length(ProtectTables) - 1);
      Exit;
    end;
  end;
end;

class function TNotification.IsEditable(ATable: Integer): Boolean;
var
  i: Integer;
begin
  for i:= 0 to High(ProtectTables) do
    if ProtectTables[i] = ATable then
      Exit(False);
  Result:= True;
end;

class procedure TNotification.AddToProtect(ATable, ARecId: Integer);
begin
  SetLength(ProtectRecords, Length(ProtectRecords) + 1);
  with ProtectRecords[High(ProtectRecords)] do begin
    IndexTable:= ATable;
    RecordID:= ARecId;
  end;
end;

class procedure TNotification.DeleteFromProtect(ATable, ARecId: Integer);
var
  i, j: Integer;
begin
  for i:= 0 to High(ProtectRecords) do begin
    if (ATable = ProtectRecords[i].IndexTable) and (ARecId = ProtectRecords[i].RecordID) then begin
      for j:= i to High(ProtectRecords) - 1 do
        ProtectRecords[j]:= ProtectRecords[j + 1];
      SetLength(ProtectRecords, Length(ProtectRecords) - 1);
      Exit;
    end;
  end;
end;

class function TNotification.IsEditable(ATable, ARecId: Integer): Boolean;
var
  i: Integer;
begin
  for i:= 0 to High(ProtectRecords) do
    if (ProtectRecords[i].IndexTable = ATable) and (ProtectRecords[i].RecordID = ARecId) then
      Exit(False);
  Result:= True;
end;

end.

