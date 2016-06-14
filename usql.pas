unit USQL;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UMetadata;

type

  TColumn = record
    TableID, FieldID: Integer;
  end;

  TCondition = record
    Field, Operation: String;
  end;

  { TSQL }

  TSQL = class(TObject)
    public
      Query: String;
      TableID: Integer;
      Columns: array of TColumn;
      function SelectAllFrom(ATableId: Integer): TSQL;
      function SelectFrom(ATableId: Integer; AColumns: array of String): TSQL;
      function InnerJoin(): TSQL;
      function Where(AConditions: array of TCondition): TSQL;
      function OrderBy(AFields: array of String): TSQL;
      function DeleteRecord(ATableIndex: Integer; ACondition: TCondition): TSQL;
      function InsertRecord(ATableIndex, ANumValues: Integer): TSQL;
      function UpdateRecord(ATableIndex: Integer; AFieldsName: array of String;
        ACond: TCondition): TSQL;
    private
      procedure AddColumn(ATableID, AFieldID: Integer);
  end;

implementation

{ TSQL }

procedure TSQL.AddColumn(ATableID, AFieldID: Integer);
begin
  SetLength(Columns, Length(Columns) + 1);

  with Columns[High(Columns)] do begin
    TableID:= ATableID;
    FieldID:= AFieldID;
  end;
end;

function TSQL.SelectAllFrom(ATableId: Integer): TSQL;
var
  i: Integer;
begin
  TableID:= ATableId;
  SetLength(Columns, 0);
  Query:= 'SELECT ';
  with Tables[ATableId] do begin
    for i:= 0 to High(Fields) do begin
      if Fields[i].Visible then begin
         Query+= TDBName + '.' + Fields[i].FDBName + ',';
         AddColumn(ATableId, i);
      end;
    end;
    Delete(Query, High(Query), 1);
    Query+= ' FROM ' + TDBName;
  end;
  Result:= Self;
end;

function TSQL.SelectFrom(ATableId: Integer; AColumns: array of String): TSQL;
var
  i, j: Integer;
  IsFind: Boolean;
begin
  TableID:= ATableId;
  SetLength(Columns, 0);
  Query:= 'SELECT ';
  with Tables[ATableId] do begin
    for i:= 0 to High(Fields) do begin
      IsFind:= False;
      for j:= 0 to High(AColumns) do
        if Fields[i].FDBName = AColumns[j] then
           IsFind:= True;
      if (Fields[i].Visible) and (IsFind) then begin
         Query+= TDBName + '.' + Fields[i].FDBName + ',';
         AddColumn(ATableId, i);
      end;
    end;
    Delete(Query, High(Query), 1);
    Query+= ' FROM ' + TDBName;
  end;
  Result:= Self;
end;

function TSQL.InnerJoin(): TSQL;
var
  i, j: Integer;
  Field: TField;
  IsFind: Boolean;
  FColumns: array of TColumn;
begin
  SetLength(FColumns, Length(Columns));
  for i:= 0 to High(Columns) do
    FColumns[i]:= Columns[i];
  SetLength(Columns, 0);
  Query:= 'SELECT ';
  with Tables[TableId] do begin
    for i:= 0 to High(Fields) do begin
      IsFind:= False;
      for j:= 0 to High(FColumns) do begin
        if (FColumns[j].TableID = TableID) and
          (Fields[FColumns[j].FieldID].FDBName = Fields[i].FDBName) then begin
             IsFind:= True;
             Break;
          end;
      end;
      if IsFind and (Fields[i] is TLink) then begin
         with Fields[i] as TLink do begin
           for j:= 0 to High(Tables[RefTable].Fields) do begin
             Field:= Tables[RefTable].Fields[j];
             if (Field.Visible) and (Field.FDBName <> 'ID') then begin
                Query+= Tables[RefTable].TDBName + '.' + Tables[RefTable].Fields[j].FDBName + ',';
                AddColumn(RefTable, j);
             end;
           end;
         end;
      end
      else if IsFind and Fields[i].Visible then begin
        Query+= TDBName + '.' + Fields[i].FDBName + ',';
        AddColumn(TableID, i);
      end;
    end;
    Delete(Query, High(Query), 1);
    Query+= ' FROM ' + TDBName;
    for i:= 0 to High(Fields) do begin
      if Fields[i] is TLink then begin
         with Fields[i] as TLink do begin
           Query+= ' INNER JOIN ' + Tables[RefTable].TDBName + ' ON ' +
              TDBName + '.' + FDBName + '=' +
              Tables[RefTable].TDBName + '.' + Tables[RefTable].Fields[RefField].FDBName;
         end;
      end;
    end;
  end;
  Result:= Self;
end;

function TSQL.Where(AConditions: array of TCondition): TSQL;
var
  i: Integer;
begin
  if Length(AConditions) = 0 then
     Exit(Self);

  Query+= ' WHERE ';
  for i:= 0 to High(AConditions) do
    with AConditions[i] do
      Query+= Field + ' ' + Operation + ' ' + ':p' + IntToStr(i) + ' AND ';

  Delete(Query, High(Query) - 3, 4);
  Result:= Self;
end;

function TSQL.OrderBy(AFields: array of String): TSQL;
var
  i: Integer;
begin
  if Length(AFields) = 0 then
     Exit(Self);

  Query+= ' ORDER BY ';
  for i:= 0 to High(AFields) do
    Query+= AFields[i] + ',';

  Delete(Query, High(Query), 1);
  Result:= Self;
end;

function TSQL.DeleteRecord(ATableIndex: Integer; ACondition: TCondition): TSQL;
begin
  Query:= 'DELETE FROM ' + Tables[ATableIndex].TDBName + ' WHERE ' + ACondition.Field + ACondition.Operation + ':p0';
  Result:= Self;
end;

function TSQL.InsertRecord(ATableIndex, ANumValues: Integer): TSQL;
var
  i: Integer;
begin
  Query:= 'INSERT INTO ' + Tables[ATableIndex].TDBName + ' VALUES(1';
  for i:= 0 to ANumValues - 1 do
      Query+= ',:p' + IntToStr(i);
  Query+= ')';
  Result:= Self;
end;

function TSQL.UpdateRecord(ATableIndex: Integer;
  AFieldsName: array of String; ACond: TCondition): TSQL;
var
  i: Integer;
begin
  Query:= 'UPDATE ' + Tables[ATableIndex].TDBName + ' SET ';
  for i:= 0 to High(AFieldsName) do
    Query+= AFieldsName[i] + '=' + ':p' + IntToStr(i) + ',';
  Delete(Query, High(Query), 1);
  Query+= ' WHERE ' + Tables[ATableIndex].TDBName + '.' + ACond.Field + ACond.Operation
     + ':p' + IntToStr(i + 1);
  Result:= Self;
end;

end.

