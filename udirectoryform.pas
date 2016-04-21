unit UDirectoryForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, db, FileUtil, Forms, Controls, Graphics, Dialogs,
  DBGrids, Menus, Buttons, StdCtrls, CheckLst, ExtCtrls, UMetadata, USQL,
  UEditCard, UDBConnection, UNotification;

type

  TFilter = record
    Fields: TComboBox;
    Operations: TComboBox;
    Constant: TEdit;
    DeleteFilter: TBitBtn;
  end;

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
    procedure CheckListSortItemClick(Sender: TObject; Index: Integer);
    constructor Create(ATableId: Integer); overload;
    procedure DBGridCellClick(Column: TColumn);
    procedure DBGridDblClick(Sender: TObject);
    procedure FAddButtonClick(Sender: TObject);
    procedure FApplyButtonClick(Sender: TObject);
    procedure RAddButtonClick(Sender: TObject);
    procedure RChangeButtonClick(Sender: TObject);
    procedure RDeleteButtonClick(Sender: TObject);
  private
    FBSQL: TSQL;
    Filters: array of TFilter;
    CurrentFilterY, SelectedRow: Integer;
    procedure OnChangeFilter(Sender: TObject);
    procedure DeleteFilter(Sender: TObject; Button: TMouseButton;
       Shift: TShiftState; X, Y: Integer);
    procedure UpdateGrid();
    procedure ChangeCursorPosition(Sender: TObject);
  public
    { public declarations }
  end;

var
  DirectoryForm: TDirectoryForm;

implementation

const
  MarginTopFilters = 10;
  MarginLeftFilters = 10;

var
  Operations: array[0..6] of String = ('>', '<', '=', '>=', '<=' , '<>', 'LIKE');

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
  CurrentFilterY:= 0;
  SelectedRow:= 1;
  TNotification.Subscribe(@FApplyButtonClick);

  FBSQL:= TSQL.Create;
  FApplyButton.Click;

  UpdateGrid();

  for i:= 1 to High(FBSQL.Columns) do
    with FBSQL.Columns[i] do
      CheckListSort.Items.Add(Tables[TableID].Fields[FieldID].FAppName);
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
var
  Filter: TFilter;
  i: Integer;
begin
  Filter.Fields:= TComboBox.Create(RightScrollBox);
  with Filter.Fields do begin
      Left:= CheckListSort.Left + CheckListSort.Width + MarginLeftFilters;
      Top:= CurrentFilterY + MarginTopFilters;
      for i:= 1 to DBGrid.Columns.Count - 1 do
          Items.Add(DBGrid.Columns[i].Title.Caption);

      ItemIndex:= 0;
      ReadOnly:= True;
      Parent:= RightScrollBox;
      OnChange:= @OnChangeFilter;
  end;

  Filter.Operations:= TComboBox.Create(RightScrollBox);
  with Filter.Operations do begin
      Left:= Filter.Fields.Left + Filter.Fields.Width + MarginLeftFilters;
      Top:= Filter.Fields.Top;
      for i:= 0 to High(Operations) do
          Items.Add(Operations[i]);
      ItemIndex:= 0;
      ReadOnly:= True;
      Parent:= RightScrollBox;
      OnChange:= @OnChangeFilter;
  end;

  Filter.Constant:= TEdit.Create(RightScrollBox);
  with Filter.Constant do begin
      Left:= Filter.Operations.Left + Filter.Operations.Width + MarginLeftFilters;
      Top:= Filter.Fields.Top;
      Parent:= RightScrollBox;
      OnChange:= @OnChangeFilter;
  end;

  Filter.DeleteFilter:= TBitBtn.Create(RightScrollBox);
  with Filter.DeleteFilter do begin
      Left:= Filter.Constant.Left + Filter.Constant.Width + MarginLeftFilters;
      Top:= Filter.Fields.Top;
      Glyph.LoadFromFile('img/delete.bmp');
      Spacing:= 0;
      Height:= 25;
      Width:= 25;
      Parent:= RightScrollBox;
      Tag:= Length(Filters);
      OnMouseUp:= @DeleteFilter;
  end;

  SetLength(Filters, Length(Filters) + 1);
  Filters[High(Filters)]:= Filter;
  CurrentFilterY:= Filter.Fields.Top + Filter.Fields.Height;
  FApplyButton.Enabled:= True;
end;

procedure TDirectoryForm.FApplyButtonClick(Sender: TObject);
var
  i: Integer;
  Conditions: array of TCondition;
  ToOrder: array of String;
begin
  SetLength(Conditions, Length(Filters));
  for i:= 0 to High(Conditions) do begin
      with Conditions[i], FBSQL.Columns[Filters[i].Fields.ItemIndex + 1] do begin
          Field:= Tables[TableID].TDBName + '.' + Tables[TableID].Fields[FieldID].FDBName;
          Operation:= Filters[i].Operations.Text;
      end;
  end;

  for i:= 0 to CheckListSort.Items.Count - 1 do begin
    if CheckListSort.Checked[i] then begin
      SetLength(ToOrder, Length(ToOrder) + 1);
      with FBSQL.Columns[i + 1] do
          ToOrder[High(ToOrder)]:= Tables[TableID].TDBName + '.' + Tables[TableID].Fields[FieldID].FDBName;
    end;
  end;

  SQLQuery.Close;
  SQLQuery.SQL.Text:= FBSQL.SelectAllFrom(Tag).InnerJoin().Where(Conditions).OrderBy(ToOrder).Query;
  SQLQuery.Prepare;
  for i:= 0 to High(Filters) do
      SQLQuery.Params[i].AsString:= Filters[i].Constant.Text;
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

procedure TDirectoryForm.DeleteFilter(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  for i:= (Sender as TBitBtn).Tag to High(Filters) - 1 do begin
      Filters[i].Fields.ItemIndex:= Filters[i + 1].Fields.ItemIndex;
      Filters[i].Operations.ItemIndex:= Filters[i + 1].Operations.ItemIndex;
      Filters[i].Constant.Text:= Filters[i + 1].Constant.Text;
      Filters[i].DeleteFilter.Tag:= i;
  end;

  with Filters[High(Filters)] do begin
    Fields.Free;
    Operations.Free;
    Constant.Free;
    DeleteFilter.Free;
  end;

  SetLength(Filters, Length(Filters) - 1);

  if Length(Filters) = 0 then
     CurrentFilterY:= 0
  else
     CurrentFilterY:= Filters[High(Filters)].Fields.Top + Filters[High(Filters)].Fields.Height;

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

