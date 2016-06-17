unit UConflicts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  DBGrids, sqldb, db, USQL, UMetadata, UDBConnection, UEditCard;

type

  { TConflictsForm }

  TConflictsForm = class(TForm)
    DataSource: TDataSource;
    SQLQuery: TSQLQuery;
    TreeView: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
  private
    FBSQL: TSQL;
    VisibleIDs: array of Integer;
    class procedure CheckOverflowConflict(ACurrTConf: Integer);
    class procedure AddToConflicts(AConflictType, ACurrConflict, AId: Integer);
    procedure UpdateTreeView(Sender: TObject);
    function GetConflictInfo(AConflictType, AID: Integer): String;
    procedure DeleteEmptyBranch(ALevel: Integer);
  public
    constructor Create(AIDs: array of Integer); overload;
    class procedure CheckConflicts();
    class function IsRecConflict(AID: Integer): Boolean;
    class function CheckIntersect(AStart1, AEnd1, AStart2, AEnd2: TDate): Boolean;
  end;

var
  ConflictsForm: TConflictsForm;
  Conflicts: array of array of array of Integer;

implementation

const
  TimeTableID = 7;

{$R *.lfm}

{ TConflictsForm }

class function CreateDataSet(AQuery: String): TDataSet;
var
  FSQLQuery: TSQLQuery;
  FDataSource: TDataSource;
begin
  FSQLQuery:= TSQLQuery.Create(Nil);
  FSQLQuery.DataBase:= DBConnection.IBConnection;
  FSQLQuery.Transaction:= DBConnection.SQLTransaction;
  FDataSource:= TDataSource.Create(FSQLQuery);
  FDataSource.DataSet:= FSQLQuery;

  FSQLQuery.Close;
  FSQLQuery.SQL.Text:= AQuery;
  FSQLQuery.Open;

  Result:= FDataSource.DataSet;
end;

class procedure TConflictsForm.CheckConflicts();
var
  i, j, k, g, CurrConflict, RecCount: Integer;
  CompConf: TCompareConf;
  FFBSQL: TSQL;
  DataSet: TDataSet;
  SameFields, DifFields: array of Integer;
  StartPer, EndPer: TDate;
  UsedRecord: array of Boolean;
  IsConflict: Boolean;
begin
  SetLength(Conflicts, 0);
  SetLength(Conflicts, Length(ConflictTypes));

  FFBSQL:= TSQL.Create;
  DataSet:= CreateDataSet(FFBSQL.SelectAllFrom(TimeTableID).Query);
  FFBSQL.Free;

  RecCount:= 0;
  DataSet.First;
  while not DataSet.EOF do begin
    Inc(RecCount);
    DataSet.Next;
  end;

  SetLength(UsedRecord, RecCount);

  for i:= 0 to High(ConflictTypes) do begin
    if ConflictTypes[i] is TOverflowConf then begin
      CheckOverflowConflict(i);
      Continue;
    end
    else if ConflictTypes[i] is TCompareConf then
      CompConf:= ConflictTypes[i] as TCompareConf
    else
      Continue;

    CurrConflict:= -1;
    for g:= 0 to High(UsedRecord) do
      UsedRecord[g]:= False;

    for j:= 0 to RecCount - 2 do begin
      DataSet.First;
      DataSet.MoveBy(j);
      if UsedRecord[j] then
          Continue;

      StartPer:= DataSet.FieldByName('START_PERIOD').AsDateTime;
      EndPer:= DataSet.FieldByName('END_PERIOD').AsDateTime;
      SetLength(SameFields, Length(CompConf.SameColumns) + 1);
      for g:= 0 to High(CompConf.SameColumns) do
        SameFields[g]:= DataSet.FieldByName(CompConf.SameColumns[g]).AsInteger;
      SameFields[g + 1]:= DataSet.FieldByName('ID').AsInteger;

      SetLength(DifFields, Length(CompConf.DifferentColumns));
      for g:= 0 to High(CompConf.DifferentColumns) do
        DifFields[g]:= DataSet.FieldByName(CompConf.DifferentColumns[g]).AsInteger;

      for k:= j + 1 to RecCount - 1 do begin
        DataSet.Next;
        if UsedRecord[k] then
          Continue;
        if not CheckIntersect(StartPer, EndPer, DataSet.FieldByName('START_PERIOD').AsDateTime,
          DataSet.FieldByName('END_PERIOD').AsDateTime) then
              Continue;
        IsConflict:= True;
        for g:= 0 to High(CompConf.SameColumns) do begin
          if SameFields[g] <> DataSet.FieldByName(CompConf.SameColumns[g]).AsInteger then begin
            IsConflict:= False;
            Break;
          end;
        end;
        if IsConflict then begin
          for g:= 0 to High(CompConf.DifferentColumns) do begin
            if DifFields[g] = DataSet.FieldByName(CompConf.DifferentColumns[g]).AsInteger then begin
              IsConflict:= False;
              Break;
            end;
          end;
        end;
        if IsConflict then begin
          if not UsedRecord[j] then begin
            Inc(CurrConflict);
            AddToConflicts(i, CurrConflict, SameFields[High(SameFields)]);
          end;
          if not UsedRecord[k] then
            AddToConflicts(i, CurrConflict, DataSet.FieldByName('ID').AsInteger);
          UsedRecord[j]:= True;
          UsedRecord[k]:= True;
        end;
      end;
    end;
  end;
end;

class function TConflictsForm.IsRecConflict(AID: Integer): Boolean;
var
  IsConf: Boolean;
  j, k, g: Integer;
begin
  IsConf:= False;
  for j:= 0 to High(Conflicts) do begin
    for k:= 0 to High(Conflicts[j]) do begin
      for g:= 0 to High(Conflicts[j][k]) do begin
        if Conflicts[j][k][g] = AID then begin
          IsConf:= True;
          Break;
        end;
      end;
    end;
  end;
  Result:= IsConf;
end;

class function TConflictsForm.CheckIntersect(AStart1, AEnd1, AStart2,
  AEnd2: TDate): Boolean;
begin
  Result:= (AStart1 <= AEnd2) and (AEnd1 >= AStart2);
end;

class procedure TConflictsForm.AddToConflicts(AConflictType, ACurrConflict, AId: Integer);
begin
  if High(Conflicts[AConflictType]) < ACurrConflict then
    SetLength(Conflicts[AConflictType], ACurrConflict + 1);

  SetLength(Conflicts[AConflictType][High(Conflicts[AConflictType])],
    Length(Conflicts[AConflictType][High(Conflicts[AConflictType])]) + 1);

  Conflicts[AConflictType][High(Conflicts[AConflictType])]
    [High(Conflicts[AConflictType][High(Conflicts[AConflictType])])]:= AId;
end;

class procedure TConflictsForm.CheckOverflowConflict(ACurrTConf: Integer);
var
  FFBSQL: TSQL;
  DataSet, DataSetInfo: TDataSet;
  OverflowConf: TOverflowConf;
  StartPer, EndPer: TDate;
  i, Capacity, Students, CurrConf: Integer;
  CurrRecord, SameRecs: array of Integer;
  OrderedCols: array of String;
  IsSame: Boolean;
begin
  FFBSQL:= TSQL.Create;
  OverflowConf:= ConflictTypes[ACurrTConf] as TOverflowConf;
  SetLength(OrderedCols, Length(OverflowConf.SameColumns) + 2);
  for i:= 0 to High(OverflowConf.SameColumns) do
    OrderedCols[i]:= OverflowConf.SameColumns[i];
  OrderedCols[High(OrderedCols) - 1]:= 'START_PERIOD';
  OrderedCols[High(OrderedCols)]:= 'END_PERIOD';
  DataSet:= CreateDataSet(FFBSQL.SelectAllFrom(TimeTableID).OrderBy(OrderedCols).Query);
  DataSetInfo:= CreateDataSet(FFBSQL.SelectAllFrom(TimeTableID).InnerJoin().OrderBy(OrderedCols).Query);

  SetLength(CurrRecord, Length(OverflowConf.SameColumns));
  for i:= 0 to High(OverflowConf.SameColumns) do
    CurrRecord[i]:= DataSet.FieldByName(OverflowConf.SameColumns[i]).AsInteger;
  DataSetInfo.Locate('ID', DataSet.Fields.Fields[0].AsInteger, []);
  Capacity:= DataSetInfo.FieldByName(OverflowConf.CapacityColumn).AsInteger;
  Students:= DataSetInfo.FieldByName(OverflowConf.CountColumn).AsInteger;
  SetLength(SameRecs, 1);
  SameRecs[0]:= DataSet.Fields.Fields[0].AsInteger;
  StartPer:= DataSet.FieldByName('START_PERIOD').AsDateTime;
  EndPer:= DataSet.FieldByName('END_PERIOD').AsDateTime;

  DataSet.Next;
  CurrConf:= 0;
  while(not DataSet.EOF) do begin
    IsSame:= True;
    if not CheckIntersect(StartPer, EndPer,
      DataSet.FieldByName('START_PERIOD').AsDateTime, DataSet.FieldByName('END_PERIOD').AsDateTime) then
          IsSame:= False;

    if IsSame then begin;
      for i:= 0 to High(CurrRecord) do begin
        if CurrRecord[i] <> DataSet.FieldByName(OverflowConf.SameColumns[i]).AsInteger then begin
          IsSame:= False;
          Break;
        end;
      end;
    end;

    if IsSame then begin
      SetLength(SameRecs, Length(SameRecs) + 1);
      SameRecs[High(SameRecs)]:= DataSet.Fields.Fields[0].AsInteger;
      DataSetInfo.Locate('ID', SameRecs[High(SameRecs)], []);
      Students+= DataSetInfo.FieldByName(OverflowConf.CountColumn).AsInteger;
    end
    else begin
      if Students > Capacity then begin
        for i:= 0 to High(SameRecs) do
          AddToConflicts(ACurrTConf, CurrConf, SameRecs[i]);

        Inc(CurrConf);
      end;

      for i:= 0 to High(OverflowConf.SameColumns) do
        CurrRecord[i]:= DataSet.FieldByName(OverflowConf.SameColumns[i]).AsInteger;
      DataSetInfo.Locate('ID', DataSet.Fields.Fields[0].AsInteger, []);
      Capacity:= DataSetInfo.FieldByName(OverflowConf.CapacityColumn).AsInteger;
      Students:= DataSetInfo.FieldByName(OverflowConf.CountColumn).AsInteger;
      SetLength(SameRecs, 1);
      SameRecs[0]:= DataSet.Fields.Fields[0].AsInteger;
      StartPer:= DataSet.FieldByName('START_PERIOD').AsDateTime;
      EndPer:= DataSet.FieldByName('END_PERIOD').AsDateTime;
    end;

    DataSet.Next;
  end;
end;

procedure TConflictsForm.FormCreate(Sender: TObject);
begin
  UpdateTreeView(Application);
  Visible:= True;
end;

constructor TConflictsForm.Create(AIDs: array of Integer);
var
  i: Integer;
begin
  SetLength(VisibleIDs, Length(AIDs));
  for i:= 0 to High(AIDs) do
    VisibleIDs[i]:= AIDs[i];
  i:= Length(VisibleIDs);

  inherited Create(Application);
end;

procedure TConflictsForm.UpdateTreeView(Sender: TObject);
var
  RecStr: String;
  SelectedCols: array of String;
  IsFind: Boolean;
  ID: ^Integer;
  i, j, k, g: Integer;
  TypeNode, ConfNode, Node, DelNode: TTreeNode;
  IsDelete: Boolean;
begin
  TConflictsForm.CheckConflicts();
  TreeView.Items.Clear;

  FBSQL:= TSQL.Create;

  for i:= 0 to High(Conflicts) do begin
    TypeNode:= TreeView.Items.Add(Nil, ConflictTypes[i].Name);
    SetLength(SelectedCols, 0);
    for j:= 0 to High(Tables[TimeTableID].Fields) do begin
      IsFind:= False;
      for k:= 0 to High(ConflictTypes[i].SameColumns) do begin
        if ConflictTypes[i].SameColumns[k] = Tables[TimeTableID].Fields[j].FDBName then begin
          IsFind:= True;
          Break;
        end;
      end;
      if not IsFind then begin
        SetLength(SelectedCols, Length(SelectedCols) + 1);
        SelectedCols[High(SelectedCols)]:= Tables[TimeTableID].Fields[j].FDBName;
      end;
    end;
    SQLQuery.Close;
    SQLQuery.SQL.Text:= FBSQL.SelectFrom(TimeTableID, SelectedCols).InnerJoin().OrderBy(['ID']).Query;
    SQLQuery.Open;
    for j:= 0 to High(Conflicts[i]) do begin
      ConfNode:= TreeView.Items.AddChild(TypeNode, GetConflictInfo(i, Conflicts[i][j][0]));
      for k:= 0 to High(Conflicts[i][j]) do begin
        DataSource.DataSet.Locate('ID', Conflicts[i][j][k], []);
        RecStr:= '';
        if Length(ConflictTypes[i].SameColumns) = (Length(Tables[TimeTableID].Fields) - 1) then
          RecStr:= DataSource.DataSet.Fields.Fields[0].AsString + ' ';
        for g:= 0 to DataSource.DataSet.FieldCount - 1 do
          RecStr += DataSource.DataSet.Fields.Fields[g].AsString + ' ';
        New(ID);
        ID^:= DataSource.DataSet.Fields.Fields[0].AsInteger;
        TreeView.Items.AddChildObject(ConfNode, RecStr, ID);
      end;
    end;
  end;

  if Length(VisibleIDs) = 0 then
      Exit;

  Node := TreeView.Items.GetFirstNode;
  while Assigned(Node) do begin
    if (Node.Data <> Nil) then begin
      IsDelete:= True;
      for i:= 0 to High(VisibleIDs) do begin
        if Integer(Node.Data^) = VisibleIDs[i] then begin
          IsDelete:= False;
          Break;
        end;
      end;
      if IsDelete then begin
        i:= Integer(Node.Data^);
        DelNode:= Node;
        Node := Node.GetPrev;
        TreeView.Items.Delete(DelNode);
      end;
    end;
    Node := Node.GetNext;
  end;

  DeleteEmptyBranch(1);
end;

procedure TConflictsForm.DeleteEmptyBranch(ALevel: Integer);
var
  Node, DelNode: TTreeNode;
begin
  Node := TreeView.Items.GetFirstNode;

  while Assigned(Node) do begin
    if (Node.Level = ALevel) and (Node.Count = 0) then begin
      DelNode:= Node;
      Node := Node.GetPrev;
      TreeView.Items.Delete(DelNode);
    end;
    Node:= Node.GetNext;
  end;
end;

function TConflictsForm.GetConflictInfo(AConflictType, AID: Integer): String;
var
  i, j: Integer;
  Info: array of TColumn;
  DataSet: TDataSet;
begin
  SetLength(Info, Length(ConflictTypes[AConflictType].SameColumns));
  FBSQL:= TSQL.Create;
  DataSet:= CreateDataSet(FBSQL.SelectAllFrom(TimeTableID).Query);
  DataSet.Locate('ID', AID, []);

  for i:= 0 to High(Info) do begin
    Info[i].FieldID:= DataSet.FieldByName(ConflictTypes[AConflictType].SameColumns[i]).AsInteger;
    for j:= 0 to High(Tables[TimeTableID].Fields) do
      if (Tables[TimeTableID].Fields[j] is TLink) and
        (Tables[TimeTableID].Fields[j].FDBName = ConflictTypes[AConflictType].SameColumns[i]) then
          Info[i].TableID:= (Tables[TimeTableID].Fields[j] as TLink).RefTable;
  end;

  Result:= '';
  for i:= 0 to High(Info) do begin
    DataSet:= CreateDataSet(FBSQL.SelectAllFrom(Info[i].TableID).Query);
    DataSet.Locate('ID', Info[i].FieldID, []);
    for j:= 1 to DataSet.FieldCount - 1 do
      Result+= DataSet.Fields.Fields[j].AsString + ' ';
  end;
end;

procedure TConflictsForm.TreeViewChange(Sender: TObject; Node: TTreeNode);
var
  ID: Integer;
begin
  if Assigned(Node) and (TreeView.Selected.Level = 2) then begin
    ID:= Integer(TreeView.Selected.Data^);
    TEditCard.Create(7, @UpdateTreeView, ID);
  end;
  TreeView.Selected:= Nil;
end;

end.

