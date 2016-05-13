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
    class procedure AddToConflicts(AConflictType, ACurrConflict, AId: Integer);
    procedure UpdateTreeView(Sender: TObject);
    function GetConflictInfo(AConflictType, AID: Integer): String;
  public
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
  CurrRecord: array of Integer;
  IsConflict: Boolean;
  UsedRecord: array of Boolean;
  st: String;
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
      SetLength(CurrRecord, Length(ConflictTypes[i].Columns) + 1);
      for g:= 0 to High(ConflictTypes[i].Columns) do
        CurrRecord[g]:= DataSet.FieldByName(ConflictTypes[i].Columns[g]).AsInteger;
      CurrRecord[g + 1]:= DataSet.FieldByName('ID').AsInteger;
      st += IntToStr(CurrRecord[g + 1]) + ' ';
      for k:= j + 1 to RecCount - 1 do begin
        DataSet.Next;
        IsConflict:= True;
        for g:= 0 to High(ConflictTypes[i].Columns) do
          if CurrRecord[g] <> DataSet.FieldByName(ConflictTypes[i].Columns[g]).AsInteger then begin
            IsConflict:= False;
            Break;
          end;
        if IsConflict then begin
          if not UsedRecord[j] then begin
            Inc(CurrConflict);
            AddToConflicts(i, CurrConflict, CurrRecord[High(CurrRecord)]);
          end;
          if not UsedRecord[k] then
            AddToConflicts(i, CurrConflict, DataSet.FieldByName('ID').AsInteger);
          UsedRecord[j]:= True;
          UsedRecord[k]:= True;
        end;
      end;
    end;
  end;

  st:= '';
  for i:= 0 to High(Conflicts) do begin
    st += IntToStr(i) + '{';
    for j:= 0 to High(Conflicts[i]) do begin
      st += IntToStr(j) + ': ';
      for k:= 0 to High(Conflicts[i][j]) do
        st += IntToStr(Conflicts[i][j][k]) + ' ';
      st+= '| '
    end;
    st += '}';
  end;
  SetLength(Conflicts, Length(Conflicts));
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

procedure TConflictsForm.UpdateTreeView(Sender: TObject);
var
  FBSQL: TSQL;
  RecStr: String;
  ID: ^Integer;
  i, j, k, g: Integer;
  TypeNode, ConfNode: TTreeNode;
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
        for g:= 0 to DataSource.DataSet.FieldCount - 1 do
          RecStr += DataSource.DataSet.Fields.Fields[g].AsString + ' ';
        New(ID);
        ID^:= DataSource.DataSet.Fields.Fields[0].AsInteger;
        TreeView.Items.AddChildObject(ConfNode, RecStr, ID);
      end;
    end;
  end;
end;

function TConflictsForm.GetConflictInfo(AConflictType, AID: Integer): String;
var
  i, j: Integer;
  Info: array of TColumn;
  DataSet: TDataSet;
  FBSQL: TSQL;
begin
  SetLength(Info, Length(ConflictTypes[AConflictType].Columns));
  FBSQL:= TSQL.Create;
  DataSet:= CreateDataSet(FBSQL.SelectAllFrom(TimeTableID).Query);
  DataSet.Locate('ID', AID, []);

  for i:= 0 to High(Info) do begin
    Info[i].FieldID:= DataSet.FieldByName(ConflictTypes[AConflictType].Columns[i]).AsInteger;
    for j:= 0 to High(Tables[TimeTableID].Fields) do
      if (Tables[TimeTableID].Fields[j] is TLink) and
        (Tables[TimeTableID].Fields[j].FDBName = ConflictTypes[AConflictType].Columns[i]) then
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

