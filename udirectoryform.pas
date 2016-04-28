unit UDirectoryForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBGrids, Menus, Buttons, StdCtrls, CheckLst, ExtCtrls, UMetadata, UFilter,
  USQL, UEditCard, UDBConnection, UNotification;

type

  { TDirectoryForm }

  TDirectoryForm = class(TForm)
    FAddButton: TButton;
    FApplyButton: TButton;
    RAddButton: TButton;
    RChangeButton: TButton;
    RDeleteButton: TButton;
    CheckListSort: TCheckListBox;
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    FiltersGroupBox: TGroupBox;
    RecordsGroupBox: TGroupBox;
    Label1: TLabel;
    LSortBy: TLabel;
    Panel: TPanel;
    RightScrollBox: TScrollBox;
    LeftScrollBox: TScrollBox;
    SQLQuery: TSQLQuery;
    constructor Create(ATableId: Integer); overload;
    constructor Create(ATableId: Integer; ASysConditions: array of TSystemCondition);
    procedure CheckListSortItemClick(Sender: TObject; Index: Integer);
    procedure DBGridCellClick(Column: TColumn);
    procedure DBGridDblClick(Sender: TObject);
    procedure FAddButtonClick(Sender: TObject);
    procedure FApplyButtonClick(Sender: TObject);
    procedure RAddButtonClick(Sender: TObject);
    procedure RChangeButtonClick(Sender: TObject);
    procedure RDeleteButtonClick(Sender: TObject);
  private
    FBSQL: TSQL;
    Filters: TFilters;
    SelectedRow: Integer;
    procedure OnChangeFilter(Sender: TObject);
    procedure UpdateGrid();
    procedure ChangeCursorPosition(Sender: TObject);
  public
    { public declarations }
  end;

var
  DirectoryForm: TDirectoryForm;

implementation

{$R *.lfm}

{ TDirectoryForm }

procedure TDirectoryForm.CheckListSortItemClick(Sender: TObject; Index: Integer
  );
begin
  FApplyButton.Enabled:= True;
end;

constructor TDirectoryForm.Create(ATableId: Integer);
var
  i: Integer;
begin
  inherited Create(Application);

  Caption:= Tables[ATableId].TAppName;
  Tag:= ATableId;
  SelectedRow:= 1;
  TNotification.Subscribe(@FApplyButtonClick);

  Filters:= TFilters.Create(Tag);

  FBSQL:= TSQL.Create;
  FApplyButton.Click;

  UpdateGrid();

  for i:= 1 to High(FBSQL.Columns) do
    with FBSQL.Columns[i] do
      CheckListSort.Items.Add(Tables[TableID].Fields[FieldID].FAppName);
end;

constructor TDirectoryForm.Create(ATableId: Integer; ASysConditions: array of TSystemCondition);
begin
  Create(ATableId);
  Filters.SetSystemConditions(ASysConditions);
  FApplyButton.Click;
end;

procedure TDirectoryForm.DBGridCellClick(Column: TColumn);
begin
  SelectedRow:= DBGrid.DataSource.DataSet.RecNo;
end;

procedure TDirectoryForm.DBGridDblClick(Sender: TObject);
begin
  RChangeButton.Click;
end;

procedure TDirectoryForm.FAddButtonClick(Sender: TObject);
begin
  Filters.AddFilter(RightScrollBox, @OnChangeFilter,
    CheckListSort.Left + CheckListSort.Width);
end;

procedure TDirectoryForm.FApplyButtonClick(Sender: TObject);
var
  i: Integer;
  ToOrder: array of String;
begin
  for i:= 0 to CheckListSort.Items.Count - 1 do begin
    if CheckListSort.Checked[i] then begin
      SetLength(ToOrder, Length(ToOrder) + 1);
      with FBSQL.Columns[i + 1] do
        ToOrder[High(ToOrder)]:= Tables[TableID].TDBName + '.' + Tables[TableID].Fields[FieldID].FDBName;
    end;
  end;

  SQLQuery.Close;
  SQLQuery.SQL.Text:= FBSQL.SelectAllFrom(Tag).InnerJoin().Where(Filters.ToConditions()).OrderBy(ToOrder).Query;
  SQLQuery.Prepare;
  for i:= 0 to High(Filters.Filters) do
    SQLQuery.Params[i].AsString:= Filters.Filters[i].Constant.Text;
  for i:= 0 to High(Filters.SystemConditions) do
    SQLQuery.Params[i + Length(Filters.Filters)].AsString:= Filters.SystemConditions[i].Value;
  SQLQuery.Open;

  UpdateGrid();
  FApplyButton.Enabled:= False;
end;

procedure TDirectoryForm.ChangeCursorPosition(Sender: TObject);
begin
  DBGrid.DataSource.DataSet.RecNo:= SelectedRow;
end;

procedure TDirectoryForm.RAddButtonClick(Sender: TObject);
begin
  TEditCard.Create(Tag, @ChangeCursorPosition);
end;

procedure TDirectoryForm.RChangeButtonClick(Sender: TObject);
var
  FieldID: Integer;
  EditCard: TEditCard;
begin
  SelectedRow:= DBGrid.DataSource.DataSet.RecNo;
  FieldID:= DBGrid.DataSource.DataSet.Fields.Fields[0].AsInteger;
  if not TNotification.IsEditable(Tag) then
     ShowMessage('Эта таблица используется в данный момент!')
  else if not TNotification.IsEditable(Tag, FieldID) then
     ShowMessage('Эта запись уже редактируется!')
  else begin
    EditCard:= TEditCard.Create(Tag, @ChangeCursorPosition, FieldID);
    EditCard.SetCursorPosition:= @ChangeCursorPosition;
  end;
end;

procedure TDirectoryForm.RDeleteButtonClick(Sender: TObject);
var
  TempSQLQuery: TSQLQuery;
  Con: TCondition;
  ButtonSel, FieldID: Integer;
begin
  FieldID:= DBGrid.DataSource.DataSet.Fields.Fields[0].AsInteger;
  if not TNotification.IsEditable(Tag) then begin
     ShowMessage('Эта таблица используется в данный момент!');
     Exit;
  end
  else if not TNotification.IsEditable(Tag, FieldID) then begin
    ShowMessage('Эта запись редактируется!');
    Exit;
  end;
  ButtonSel:= messagedlg('Вы точно хотите удалить запись?', mtCustom, [mbYes,mbNo], 0);
  if (ButtonSel = 6) and (TNotification.IsEditable(Tag)) then begin
    TempSQLQuery:= TSQLQuery.Create(Self);
    TempSQLQuery.DataBase:= DBConnection.IBConnection;
    TempSQLQuery.SQLTransaction:= DBConnection.SQLTransaction;
    TempSQLQuery.Close;
    Con.Field:= 'ID';
    Con.Operation:= '=';
    TempSQLQuery.SQL.Text:= FBSQL.DeleteRecord(Tag, Con).Query;
    TempSQLQuery.Params[0].AsInteger:= FieldID;
    TempSQLQuery.ExecSQL;
    DBConnection.SQLTransaction.Commit;
    SelectedRow:= 1;
    TNotification.UpdateDirectoryForms();
  end;
end;

procedure TDirectoryForm.OnChangeFilter(Sender: TObject);
begin
  FApplyButton.Enabled:= True;
end;

procedure TDirectoryForm.UpdateGrid;
var
  i: Integer;
begin
  for i:= 0 to DBGrid.Columns.Count - 1 do begin
    with FBSQL.Columns[i], Tables[TableID].Fields[FieldID] do begin
      DBGrid.Columns[i].Title.Caption:= FAppName;
      DBGrid.Columns[i].Width:= FieldWidth;
    end;
  end;
end;

end.

