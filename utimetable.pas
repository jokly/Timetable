unit UTimeTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, ExtCtrls, Menus, sqldb, math, Types, UMetadata, USQL, UFilter,
  UDirectoryForm, UEditCard, UNotification, UConflicts, UConverter;

type

  TButType = (OpenDirectory, AddRecord, EditRecord, DeleteRecord, Warning, None);

  TManageButton = record
    ButType: TButType;
    RecordID: Integer;
  end;

  TTableCell = record
    Row, Col: Integer;
  end;

  TCellField = record
    NameField: String;
    isVisible: Boolean;
  end;

  TCellButtons = record
    Table, Add, Warning: TRect;
    Edits: array of TRect;
    Deletes: array of TRect;
  end;

  { TTimetableForm }

  TTimetableForm = class(TForm)
    ButShowTable: TButton;
    ButtonAddFilter: TButton;
    CheckBoxDisplayEmptyRow: TCheckBox;
    CheckBoxDisplayFieldName: TCheckBox;
    CheckBoxDisplayEmptyCol: TCheckBox;
    ComboBoxCol: TComboBox;
    ComboBoxRow: TComboBox;
    DrawGrid: TDrawGrid;
    GroupBoxFilters: TGroupBox;
    GroupBoxOptions: TGroupBox;
    GroupBoxDimensions: TGroupBox;
    LabelHor: TLabel;
    LabelVer: TLabel;
    MainMenu: TMainMenu;
    MConflicts: TMenuItem;
    MSaveAs: TMenuItem;
    MFile: TMenuItem;
    MTreeConflicts: TMenuItem;
    MDirectoryConflicts: TMenuItem;
    Panel: TPanel;
    PanelLeft: TPanel;
    SaveDialog: TSaveDialog;
    ScrollBoxFilters: TScrollBox;
    SQLQuery: TSQLQuery;
    procedure ButShowTableClick(Sender: TObject);
    procedure ButtonAddFilterClick(Sender: TObject);
    constructor Create(TableIndex: Integer); overload;
    procedure DrawGridDblClick(Sender: TObject);
    procedure DrawGridDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure DrawGridDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure DrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure DrawGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawGridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure MSaveAsClick(Sender: TObject);
    procedure MTreeConflictsClick(Sender: TObject);
    procedure OnChangeOption(Sender: TObject);
  private
    { private declarations }
    FTable: TTimeTable;
    FieldsName: TFieldsName;
    ColTableIndex, RowTableIndex, RecordHeight: Integer;
    RowsCaption, ColumnsCaption: TCaps;
    Filters: TFilters;
    SelectedCell: TTableCell;
    CellClick: TManageButton;
    CellsButtons: array of array of TCellButtons;
    procedure FillDimensionsComboBox();
    function FillCaptions(TableIndex: Integer; var AFillArray: TCaps): Integer;
    procedure FillTable();
    procedure GetNameFields();
    procedure FillCell(X, Y: Integer);
    procedure SetSizeCells();
    procedure SetWidthCol(AString: String; aCol: Integer);
  public
    { public declarations }
  end;

var
  TimetableForm: TTimetableForm;

implementation

const
  IconSize = 16;

var
  CautionPic, TablePic, AddPic, EditPic, DeletePic, WarningPic: TPicture;

{$R *.lfm}

{ TTimetableForm }

constructor TTimetableForm.Create(TableIndex: Integer);
begin
  inherited Create(Application);
  TNotification.Subscribe(@ButShowTableClick);
  Tag:= TableIndex;
  Filters:= TFilters.Create(Tag);

  CautionPic:= TPicture.Create;
  CautionPic.LoadFromFile('img/caution.bmp');
  TablePic:= TPicture.Create;
  TablePic.LoadFromFile('img/table.bmp');
  AddPic:= TPicture.Create;
  AddPic.LoadFromFile('img/add.bmp');
  EditPic:= TPicture.Create;
  EditPic.LoadFromFile('img/edit.bmp');
  DeletePic:= TPicture.Create;
  DeletePic.LoadFromFile('img/deleteRecord.bmp');
  WarningPic:= TPicture.Create;
  WarningPic.LoadFromFile('img/warning.bmp');

  FillDimensionsComboBox();
  TConflictsForm.CheckConflicts();
end;

procedure TTimetableForm.DrawGridDblClick(Sender: TObject);
var
  i: Integer;
  SysConditions: array of TSystemCondition;
begin
  if (SelectedCell.Row = 0) or (SelectedCell.Col = 0) then
    Exit;

  SetLength(SysConditions, 2 + Length(Filters.Filters));
  SysConditions[0].Condition.Field:= Tables[ColTableIndex].TDBName + '.ID';
  SysConditions[0].Condition.Operation:= '=';
  SysConditions[0].Value:= IntToStr(ColumnsCaption[SelectedCell.Col - 1].ID);

  SysConditions[1].Condition.Field:= Tables[RowTableIndex].TDBName + '.ID';
  SysConditions[1].Condition.Operation:= '=';
  SysConditions[1].Value:= IntToStr(RowsCaption[SelectedCell.Row - 1].ID);

  for i:= 0 to High(Filters.Filters) do begin
    SysConditions[i + 2].Condition.Field:= Filters.FFields[Filters.Filters[i].Fields.ItemIndex].DBName;
    SysConditions[i + 2].Condition.Operation:= Filters.Filters[i].Operations.Text;
    SysConditions[i + 2].Value:= Filters.Filters[i].Constant.Text;
  end;

  TDirectoryForm.Create(Tag, SysConditions);
end;

procedure TTimetableForm.DrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
const
  Margin = 3;
var
  i, j, k, g, MarginTop, TextWidth: Integer;
  FRecord: array of String;
  Str: String;
begin
  if (aRow = 0) and (aCol = 0) then
    Exit;

  DrawGrid.Canvas.FillRect(aRect);
  if aCol = 0 then begin
    DrawGrid.Canvas.TextOut(aRect.Left + 5, aRect.Top + 2, RowsCaption[aRow - 1].Value);
  end
  else if aRow = 0 then begin
    DrawGrid.Canvas.TextOut(aRect.Left + 5, aRect.Top + 2, ColumnsCaption[aCol - 1].Value);
  end
  else begin
    MarginTop:= 16;
    TextWidth:= 0;
    DrawGrid.Canvas.Pen.Style:= psDash;
    for i:= 0 to High(FTable[aRow - 1][aCol - 1]) do begin
      FRecord:= FTable[aRow - 1][aCol - 1][i].Rec;
      for j:= 0 to High(FRecord) do begin
        Str:= '';
        if CheckBoxDisplayFieldName.Checked then
          Str:= FieldsName[j] + ': ';
        Str += FRecord[j];
        DrawGrid.Canvas.TextOut(aRect.Left + 5, aRect.Top + MarginTop, Str);
        TextWidth:= Max(TextWidth, DrawGrid.Canvas.TextWidth(Str));
        MarginTop+= 16;
      end;
      MarginTop+= 8;

      for j:= 0 to High(Conflicts) do begin
        for k:= 0 to High(Conflicts[j]) do begin
          for g:= 0 to High(Conflicts[j][k]) do begin
            if Conflicts[j][k][g] = FTable[aRow - 1][aCol - 1][i].ID then begin
              //Warning icon
              CellsButtons[aRow - 1][aCol - 1].Warning:= Rect(
                aRect.Right - IconSize - Margin,
                aRect.Top + Margin,
                aRect.Right - Margin, aRect.Top + IconSize + Margin);
              DrawGrid.Canvas.Draw(aRect.Right - IconSize -  Margin,
                aRect.Top + Margin, WarningPic.Graphic);
            end;
          end;
        end;
      end;

      //Delete icon
      CellsButtons[aRow - 1][aCol - 1].Deletes[i]:= Rect(
        aRect.Right - IconSize - Margin,
        aRect.Top + MarginTop - Margin - IconSize,
        aRect.Right - Margin, aRect.Top + MarginTop - Margin);
      DrawGrid.Canvas.Draw(
        aRect.Right - IconSize -  Margin,
        aRect.Top + MarginTop - Margin - IconSize, DeletePic.Graphic);

      //Edit icon
      CellsButtons[aRow - 1][aCol - 1].Edits[i]:= Rect(aRect.Right - 2 * (IconSize + Margin),
        aRect.Top + MarginTop - Margin - IconSize, aRect.Right - IconSize - Margin ,
        aRect.Top + MarginTop - Margin);
      DrawGrid.Canvas.Draw(aRect.Right - 2 * (IconSize + Margin),
       aRect.Top + MarginTop - Margin - IconSize, EditPic.Graphic);

      //Line
      DrawGrid.Canvas.Line(aRect.Left, aRect.Top + MarginTop,
        aRect.Left + DrawGrid.ColWidths[aCol], aRect.Top + MarginTop);
      if i = 0 then
        RecordHeight:= MarginTop;
      MarginTop+= 8;
    end;

    TextWidth+= 5;
    MarginTop-= 8;
    if (DrawGrid.RowHeights[aRow] < MarginTop) or (TextWidth > DrawGrid.ColWidths[aCol]) then begin
      DrawGrid.Canvas.Draw(aRect.Right - IconSize -  Margin,
        aRect.Bottom - IconSize - Margin, CautionPic.Graphic);
    end;
    //Table icon
    CellsButtons[aRow - 1][aCol - 1].Table:= Rect(aRect.Right - 2 * (Margin + IconSize),
      aRect.Bottom - IconSize - Margin, aRect.Right - 2 * Margin - IconSize, aRect.Bottom - Margin);
    DrawGrid.Canvas.Draw(aRect.Right - 2 * (Margin + IconSize),
      aRect.Bottom - IconSize - Margin, TablePic.Graphic);

    //Add icon
    if (RowTableIndex = ColTableIndex) and (ColumnsCaption[aCol - 1].ID <> RowsCaption[aRow - 1].ID) then
      Exit;
    CellsButtons[aRow - 1][aCol - 1].Add:= Rect(aRect.Right - 3 * (Margin + IconSize),
      aRect.Bottom - IconSize - Margin, aRect.Right - 3 * Margin - IconSize, aRect.Bottom - Margin);
    DrawGrid.Canvas.Draw(aRect.Right - 3 * (Margin + IconSize),
      aRect.Bottom - IconSize - Margin, AddPic.Graphic);
  end;
end;

procedure TTimetableForm.DrawGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i, aCol, aRow: Integer;
  CanSelect: Boolean;
  CellButtons: TCellButtons;
  Rects: array of TRect;
  FRect: TRect;
begin
  DrawGrid.MouseToCell(X, Y, aCol, aRow);
  DrawGridSelectCell(Sender, aCol, aRow, CanSelect);
  CellClick.ButType:= TButType.None;
  if (SelectedCell.Col = 0) or (SelectedCell.Row = 0) then
    Exit;
  CellButtons:= CellsButtons[SelectedCell.Row - 1][SelectedCell.Col - 1];
  if PtInRect(CellButtons.Table, Point(X, Y)) then
    CellClick.ButType:= TButType.OpenDirectory
  else if PtInRect(CellButtons.Add, Point(X, Y)) then
    CellClick.ButType:= TButType.AddRecord
  else if PtInRect(CellButtons.Warning, Point(X, Y)) then
    CellClick.ButType:= TButType.Warning
  else begin
    Rects:= CellButtons.Edits;
    for i:= 0 to High(Rects) do
      if PtInRect(Rects[i], Point(X, Y)) then begin
        CellClick.ButType:= TButType.EditRecord;
        CellClick.RecordID:= FTable[SelectedCell.Row - 1][SelectedCell.Col - 1][i].ID;
        Exit;
      end;

    Rects:= CellButtons.Deletes;
    for i:= 0 to High(Rects) do
      if PtInRect(Rects[i], Point(X, Y)) then begin
        CellClick.ButType:= TButType.DeleteRecord;
        CellClick.RecordID:= FTable[SelectedCell.Row - 1][SelectedCell.Col - 1][i].ID;
        Exit;
      end;
  end;

  //Drag mode start
  if Button = mbRight then begin;
    for i:= 0 to High(FTable[SelectedCell.Row - 1][SelectedCell.Col - 1]) do begin
      FRect:= DrawGrid.CellRect(SelectedCell.Col, SelectedCell.Row);
      FRect:= Rect(FRect.Left, FRect.Top + i * RecordHeight,
        FRect.Left + DrawGrid.ColWidths[SelectedCell.Col],FRect.Top + (i + 1) * RecordHeight);
      if PtInRect(FRect, Point(X, Y)) then begin
        DrawGrid.Canvas.Brush.Style:= bsClear;
        DrawGrid.Canvas.Pen.Color:= clBlue;
        DrawGrid.Canvas.Rectangle(FRect);
        CellClick.RecordID:= FTable[SelectedCell.Row - 1][SelectedCell.Col - 1][i].ID;
        DrawGrid.BeginDrag(True);
        Exit;
      end;
    end;
  end;
end;

procedure TTimetableForm.DrawGridDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  aCol, aRow: Integer;
begin
  DrawGrid.MouseToCell(X, Y, aCol, aRow);
  Accept:= (aCol > 0) and (aRow > 0);
end;

procedure TTimetableForm.DrawGridDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  aCol, aRow:Integer;
  DefaultValues: TDefaultValues;
  EditCard: TEditCard;
begin
  DrawGrid.MouseToCell(X, Y, aCol, aRow);
  SetLength(DefaultValues, 2);
  DefaultValues[0].TableID:= RowTableIndex;
  DefaultValues[0].FieldID:= RowsCaption[aRow - 1].ID;
  DefaultValues[1].TableID:= ColTableIndex;
  DefaultValues[1].FieldID:= ColumnsCaption[aCol - 1].ID;
  EditCard:= TEditCard.Create(Tag, CellClick.RecordID, DefaultValues);
  EditCard.Visible:= False;
  EditCard.EditButClick(Sender);
end;

procedure TTimetableForm.DrawGridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  DefaultValues: TDefaultValues;
  FBSQL: TSQL;
  Con: TCondition;
  i, ButtonSel: Integer;
  FIDs: array of Integer;
begin
  if CellClick.ButType = TButType.OpenDirectory then
    DrawGridDblClick(Sender)
  else if CellClick.ButType = TButType.AddRecord then begin
    SetLength(DefaultValues, 2);
    DefaultValues[0].TableID:= RowTableIndex;
    DefaultValues[0].FieldID:= RowsCaption[SelectedCell.Row - 1].ID;
    DefaultValues[1].TableID:= ColTableIndex;
    DefaultValues[1].FieldID:= ColumnsCaption[SelectedCell.Col - 1].ID;
    TEditCard.Create(Tag, DefaultValues);
    TConflictsForm.CheckConflicts();
  end
  else if CellClick.ButType = TButType.EditRecord then begin
    if not TNotification.IsEditable(Tag) then
     ShowMessage('Эта таблица используется в данный момент!')
    else if not TNotification.IsEditable(Tag, CellClick.RecordID) then
     ShowMessage('Эта запись уже редактируется!')
    else
      TEditCard.Create(Tag, Nil, CellClick.RecordID);
    TConflictsForm.CheckConflicts();
  end
  else if CellClick.ButType = TButType.DeleteRecord then begin
    if not TNotification.IsEditable(Tag) then begin
     ShowMessage('Эта таблица используется в данный момент!');
     Exit;
    end
    else if not TNotification.IsEditable(Tag, CellClick.RecordID) then begin
      ShowMessage('Эта запись редактируется!');
      Exit;
    end;
    ButtonSel:= messagedlg('Вы точно хотите удалить запись?', mtCustom, [mbYes,mbNo], 0);
    if ButtonSel = 6 then begin
      SQLQuery.Close;
      Con.Field:= 'ID';
      Con.Operation:= '=';
      FBSQL:= TSQL.Create;
      SQLQuery.SQL.Text:= FBSQL.DeleteRecord(Tag, Con).Query;
      SQLQuery.Params[0].AsInteger:= CellClick.RecordID;
      SQLQuery.ExecSQL;
      TNotification.UpdateDirectoryForms();
    end;
    TConflictsForm.CheckConflicts();
  end
  else if CellClick.ButType = TButType.Warning then begin
    SetLength(FIDs, Length(FTable[SelectedCell.Row - 1][SelectedCell.Col - 1]));
    for i:= 0 to High(FIDs) do
      FIDs[i]:= FTable[SelectedCell.Row - 1][SelectedCell.Col - 1][i].ID;

    TConflictsForm.Create(FIDs);
  end;
end;

procedure TTimetableForm.DrawGridSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  SelectedCell.Col:= aCol;
  SelectedCell.Row:= aRow;
end;

procedure TTimetableForm.MSaveAsClick(Sender: TObject);
begin
  if not SaveDialog.Execute then
    Exit;

  case SaveDialog.FilterIndex of
    1: Converter.SaveToHtml(SaveDialog.FileName, RowsCaption, ColumnsCaption,
      FTable, FieldsName);
    2: Converter.SaveToExcel(SaveDialog.FileName, RowsCaption, ColumnsCaption,
      FTable, FieldsName);
  end;
end;

procedure TTimetableForm.MTreeConflictsClick(Sender: TObject);
begin
  TConflictsForm.Create(Self);
end;

procedure TTimetableForm.SetSizeCells;
var
  aRow, aCol, i, j: Integer;
  FRecord: array of String;
  Str: String;
begin
  for aRow:= 1 to DrawGrid.RowCount - 1 do begin
    if (CheckBoxDisplayEmptyRow.Checked = False) and (RowsCaption[aRow - 1].isEmpty = True) then begin
      DrawGrid.RowHeights[aRow]:= 0;
      Continue;
    end;
    DrawGrid.RowHeights[aRow]:= DrawGrid.DefaultRowHeight;
    SetWidthCol(RowsCaption[aRow - 1].Value, 0);
  end;

  DrawGrid.RowHeights[0]:= 50;
  for aCol:= 1 to DrawGrid.ColCount - 1 do begin
    if (CheckBoxDisplayEmptyCol.Checked = False) and (ColumnsCaption[aCol - 1].isEmpty = True) then begin
      DrawGrid.ColWidths[aCol]:= 0;
      Continue;
    end;
    SetWidthCol(ColumnsCaption[aCol - 1].Value, aCol);
  end;

  for aRow:= 1 to DrawGrid.RowCount - 1 do begin
    for aCol:= 1 to DrawGrid.ColCount - 1 do begin
      for i:= 0 to High(FTable[aRow - 1][aCol - 1]) do begin
        FRecord:= FTable[aRow - 1][aCol - 1][i].Rec;
        for j:= 0 to High(FRecord) do begin
          Str:= '';
          if CheckBoxDisplayFieldName.Checked then
            Str:= FieldsName[j] + ': ';
          Str += FRecord[j];
          SetWidthCol(Str, aCol);
        end;
      end;
    end;
  end;
end;

procedure TTimetableForm.SetWidthCol(AString: String; aCol: Integer);
var
  WidthCell: Integer;
begin
  WidthCell:= DrawGrid.Canvas.TextWidth(AString);
  if ((WidthCell > DrawGrid.ColWidths[aCol]) or (ColumnsCaption[aCol - 1].isEmpty)) and
    (WidthCell > DrawGrid.DefaultColWidth) then
    DrawGrid.ColWidths[aCol]:= WidthCell + 10;
end;

procedure TTimetableForm.OnChangeOption(Sender: TObject);
begin
  ButShowTable.Enabled:= True;
end;

procedure TTimetableForm.ButShowTableClick(Sender: TObject);
begin
  RowTableIndex:= PtrUInt(ComboBoxRow.Items.Objects[ComboBoxRow.ItemIndex]);
  ColTableIndex:= PtrUInt(ComboBoxCol.Items.Objects[ComboBoxCol.ItemIndex]);

  DrawGrid.RowCount:= FillCaptions(RowTableIndex, RowsCaption) + 1;
  DrawGrid.ColCount:= FillCaptions(ColTableIndex, ColumnsCaption) + 1;

  FillTable();
  SetSizeCells();
  DrawGrid.Invalidate;
  ButShowTable.Enabled:= False;
end;

procedure TTimetableForm.ButtonAddFilterClick(Sender: TObject);
begin
  Filters.AddFilter(ScrollBoxFilters, @OnChangeOption, 0);
end;

procedure TTimetableForm.FillDimensionsComboBox;
var
  i: Integer;
  LField: TLink;
begin
  for i:= 1 to High(Tables[Tag].Fields) do begin
    LField:= Tables[Tag].Fields[i] as TLink;
    ComboBoxCol.AddItem(LField.FAppName, TObject(PtrUInt(LField.RefTable)));
    ComboBoxRow.AddItem(LField.FAppName, TObject(PtrUInt(LField.RefTable)));
  end;

  ComboBoxCol.ItemIndex:= 0;
  ComboBoxRow.ItemIndex:= 0;
end;

function TTimetableForm.FillCaptions(TableIndex: Integer;
  var AFillArray: TCaps): Integer;
var
  i: Integer;
  Cap: String;
  FBSQL: TSQL;
begin
  FBSQL:= TSQL.Create;

  SQLQuery.Close;
  SQLQuery.SQL.Text:= FBSQL.SelectAllFrom(TableIndex).Query;
  SQLQuery.Open;

  SetLength(AFillArray, 0);
  SQLQuery.First;
  while (not SQLQuery.EOF) do begin
    Cap:= '';
    for i:= 1 to SQLQuery.FieldCount - 1 do
      Cap += SQLQuery.Fields[i].AsString + ' ';
    SetLength(AFillArray, Length(AFillArray) + 1);
    AFillArray[High(AFillArray)].Value:= Cap;
    AFillArray[High(AFillArray)].ID:= SQLQuery.Fields[0].AsInteger;
    AFillArray[High(AFillArray)].isEmpty:= True;
    SQLQuery.Next;
  end;

  Result:= Length(AFillArray);
end;

procedure TTimetableForm.FillTable();
var
  i, j: Integer;
begin
  GetNameFields();
  SetLength(FTable, 0);
  SetLength(CellsButtons, 0);

  SetLength(FTable, Length(RowsCaption));
  SetLength(CellsButtons, Length(RowsCaption));
  for i:= 0 to High(FTable) do begin
    SetLength(FTable[i], Length(ColumnsCaption));
    SetLength(CellsButtons[i], Length(ColumnsCaption));
  end;

  for i:= 0 to High(FTable) do
    for j:= 0 to High(FTable[i]) do
      FillCell(j, i);
end;

procedure TTimetableForm.GetNameFields;
var
  i, j: Integer;
  Ref: TLink;
begin
  SetLength(FieldsName, 0);

  for i:= 1 to High(Tables[Tag].Fields) do begin
    if Tables[Tag].Fields[i] is TLink then begin
      Ref:= Tables[Tag].Fields[i] as TLink;
      for j:= 1 to High(Tables[Ref.RefTable].Fields) do begin
        SetLength(FieldsName, Length(FieldsName) + 1);
        FieldsName[High(FieldsName)]:= Tables[Ref.RefTable].Fields[j].FAppName;
      end;
    end;
  end;
end;

procedure TTimetableForm.FillCell(X, Y: Integer);
var
  FBSQL: TSQL;
  Conds: TConditions;
  CondRow, CondCol: TCondition;
  i, iRecord: Integer;
begin
  FBSQL:= TSQL.Create;

  CondRow.Field:= Tables[RowTableIndex].TDBName + '.' + 'ID';
  CondRow.Operation:= '=';
  CondCol.Field:= Tables[ColTableIndex].TDBName + '.' + 'ID';
  CondCol.Operation:= '=';
  Conds:= Filters.ToConditions();
  SetLength(Conds, Length(Conds) + 2);
  Conds[High(Conds) - 1]:= CondCol;
  Conds[High(Conds)]:= CondRow;

  SQLQuery.Close;
  SQLQuery.SQL.Text:= FBSQL.SelectAllFrom(Tag).InnerJoin().Where(Conds).Query;
  SQLQuery.Prepare;
  for i:= 0 to High(Filters.Filters) do
      SQLQuery.Params[i].AsString:= Filters.Filters[i].Constant.Text;
  SQLQuery.Params[High(Conds) - 1].AsInteger:= ColumnsCaption[X].ID;
  SQLQuery.Params[High(Conds)].AsInteger:= RowsCaption[Y].ID;
  SQLQuery.Open;

  SQLQuery.First;
  while (not SQLQuery.EOF) do begin
    SetLength(FTable[Y][X], Length(FTable[Y][X]) + 1);
    iRecord:= High(FTable[Y][X]);
    SetLength(FTable[Y][X][iRecord].Rec, SQLQuery.FieldCount - 1);
    FTable[Y][X][iRecord].ID:= SQLQuery.Fields[0].AsInteger;
    for i:= 1 to SQLQuery.FieldCount - 1 do
      FTable[Y][X][iRecord].Rec[i - 1]:= SQLQuery.Fields[i].AsString;
    SQLQuery.Next;
  end;

  if Length(FTable[Y][X]) > 0 then begin
    ColumnsCaption[X].isEmpty:= False;
    RowsCaption[Y].isEmpty:= False;
  end;

  SetLength(CellsButtons[Y][X].Edits, Length(FTable[Y][X]));
  SetLength(CellsButtons[Y][X].Deletes, Length(FTable[Y][X]));
end;

end.

