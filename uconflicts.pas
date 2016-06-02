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
    VisibleIDs: array of Integer;
    class procedure AddToConflicts(AConflictType, ACurrConflict, AId: Integer);
    procedure UpdateTreeView(Sender: TObject);
    function GetConflictInfo(AConflictType, AID: Integer): String;
    procedure DeleteEmptyBranch(ALevel: Integer);
  public
    constructor Create(AIDs: array of Integer); overload;
    class procedure CheckConflicts();
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
  FBSQL: TSQL;
  DataSet: TDataSet;
  SameFields, DifFields: array of Integer;
  IsConflict: Boolean;
  UsedRecord: array of Boolean;
begin
  SetLength(Conflicts, 0);
  SetLength(Conflicts, Length(ConflictTypes));

  FBSQL:= TSQL.Create;
  DataSet:= CreateDataSet(FBSQL.SelectAllFrom(TimeTableID).Query);

  RecCount:= 0;
  DataSet.First;
  while not DataSet.EOF do begin
    Inc(RecCount);
    DataSet.Next;
  end;

  SetLength(UsedRecord, RecCount);

  for i:= 0 to High(ConflictTypes) do begin
    CurrConflict:= -1;
    for g:= 0 to High(UsedRecord) do
      UsedRecord[g]:= False;

    for j:= 0 to RecCount - 2 do begin
      DataSet.First;
      DataSet.MoveBy(j);
      if UsedRecord[j] then
          Continue;

      SetLength(SameFields, Length(ConflictTypes[i].SameColumns) + 1);
      for g:= 0 to High(ConflictTypes[i].SameColumns) do
        SameFields[g]:= DataSet.FieldByName(ConflictTypes[i].SameColumns[g]).AsInteger;
      SameFields[g + 1]:= DataSet.FieldByName('ID').AsInteger;

      SetLength(DifFields, Length(ConflictTypes[i].DifferentColumns));
      for g:= 0 to High(ConflictTypes[i].DifferentColumns) do
        DifFields[g]:= DataSet.FieldByName(ConflictTypes[i].DifferentColumns[g]).AsInteger;

      for k:= j + 1 to RecCount - 1 do begin
        DataSet.Next;
        if UsedRecord[k] then
          Continue;
        IsConflict:= True;
        for g:= 0 to High(ConflictTypes[i].SameColumns) do begin
          if SameFields[g] <> DataSet.FieldByName(ConflictTypes[i].SameColumns[g]).AsInteger then begin
            IsConflict:= False;
            Break;
          end;
        end;
        if IsConflict then begin
          for g:= 0 to High(ConflictTypes[i].DifferentColumns) do begin
            if DifFields[g] = DataSet.FieldByName(ConflictTypes[i].DifferentColumns[g]).AsInteger then begin
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

class procedure TConflictsForm.AddToConflicts(AConflictType, ACurrConflict, AId: Integer);
begin
  if High(Conflicts[AConflictType]) < ACurrConflict then
    SetLength(Conflicts[AConflictType], ACurrConflict + 1);

  SetLength(Conflicts[AConflictType][High(Conflicts[AConflictType])],
    Length(Conflicts[AConflictType][High(Conflicts[AConflictType])]) + 1);

  Conflicts[AConflictType][High(Conflicts[AConflictType])]
    [High(Conflicts[AConflictType][High(Conflicts[AConflictType])])]:= AId;
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
  FBSQL: TSQL;
  RecStr: String;
  ID: ^Integer;
  i, j, k, g: Integer;
  TypeNode, ConfNode, Node, DelNode: TTreeNode;
  IsDelete: Boolean;
begin
  TConflictsForm.CheckConflicts();
  TreeView.Items.Clear;

  FBSQL:= TSQL.Create;
  SQLQuery.Close;
  SQLQuery.SQL.Text:= FBSQL.SelectAllFrom(7).InnerJoin().OrderBy(['ID']).Query;
  SQLQuery.Open;

  for i:= 0 to High(Conflicts) do begin
    TypeNode:= TreeView.Items.Add(Nil, ConflictTypes[i].Name);
    for j:= 0 to High(Conflicts[i]) do begin
      ConfNode:= TreeView.Items.AddChild(TypeNode, GetConflictInfo(i, Conflicts[i][j][0]));
      for k:= 0 to High(Conflicts[i][j]) do begin
        DataSource.DataSet.Locate('ID', Conflicts[i][j][k], []);
        RecStr:= '';
        for g:= 1 to DataSource.DataSet.FieldCount - 1 do
          RecStr += DataSource.DataSet.Fields.Fields[g].AsString + ' | ';
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
  FBSQL: TSQL;
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

