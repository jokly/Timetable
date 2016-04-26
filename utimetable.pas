unit UTimeTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, ExtCtrls, sqldb, UMetadata, USQL, UFilter, UDirectoryForm;

type

  TCell = array of array of String;

  TCap = record
    Value: String;
    ID: Integer;
    isEmpty: Boolean;
  end;

  TCaps = array of TCap;

  TTableCell = record
    Row, Col: Integer;
  end;

  TCellField = record
    NameField: String;
    isVisible: Boolean;
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
    Panel: TPanel;
    PanelLeft: TPanel;
    ScrollBoxFilters: TScrollBox;
    SQLQuery: TSQLQuery;
    procedure ButShowTableClick(Sender: TObject);
    procedure ButtonAddFilterClick(Sender: TObject);
    constructor Create(TableIndex: Integer); overload;
    procedure DrawGridDblClick(Sender: TObject);
    procedure DrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure DrawGridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure OnChangeOption(Sender: TObject);
  private
    { private declarations }
    FTable: array of array of TCell;
    FieldsName: array of String;
    ColTableIndex, RowTableIndex: Integer;
    RowsCaption: TCaps;
    ColumnsCaption: TCaps;
    Filters: TFilters;
    SelectedCell: TTableCell;
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

{$R *.lfm}

{ TTimetableForm }

constructor TTimetableForm.Create(TableIndex: Integer);
begin
  inherited Create(Application);
  Tag:= TableIndex;
  Filters:= TFilters.Create(Tag);
  FillDimensionsComboBox();
end;

procedure TTimetableForm.DrawGridDblClick(Sender: TObject);
var
  i: Integer;
  SysConditions: array of TSystemCondition;
begin
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
var
  i, j, MarginTop, MarginRight: Integer;
  FRecord: array of String;
  Str: String;
  Picture: TPicture;
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
    MarginTop:= 8;
    for i:= 0 to High(FTable[aRow - 1][aCol - 1]) do begin
      FRecord:= FTable[aRow - 1][aCol - 1][i];
      for j:= 0 to High(FRecord) do begin
        Str:= '';
        if CheckBoxDisplayFieldName.Checked then
          Str:= FieldsName[j] + ': ';
        Str += FRecord[j];
        DrawGrid.Canvas.TextOut(aRect.Left + 5, aRect.Top + MarginTop, Str);
        MarginTop+= 2 + 16;
      end;
      MarginTop+= 8;
    end;

    MarginTop-= 8;
    MarginRight:= 3;
    if DrawGrid.RowHeights[aRow] < MarginTop then begin
      Picture:= TPicture.Create;
      Picture.LoadFromFile('img/caution.bmp');
      DrawGrid.Canvas.Draw(aRect.Right - IconSize - MarginRight, aRect.Bottom - IconSize - 3, Picture.Graphic);
      MarginRight:= IconSize + 5;
      Picture.Free;
    end;
    Picture:= TPicture.Create;
    Picture.LoadFromFile('img/table.bmp');
    DrawGrid.Canvas.Draw(aRect.Right - IconSize - MarginRight, aRect.Bottom - IconSize - 3, Picture.Graphic);
    Picture.Free;
  end;
end;

procedure TTimetableForm.DrawGridSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  SelectedCell.Col:= aCol;
  SelectedCell.Row:= aRow;
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
        FRecord:= FTable[aRow - 1][aCol - 1][i];
        for j:= 0 to High(FRecord) do begin
          Str:= '';
          if CheckBoxDisplayFieldName.Checked then
            Str:= FieldsName[j] + ': ';
          Str += FRecord[j];
          if aCol = 4 then
            Str:= Str;
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
  if (WidthCell > DrawGrid.ColWidths[aCol]) or (ColumnsCaption[aCol - 1].isEmpty) then
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

  SetLength(FTable, Length(RowsCaption));
  for i:= 0 to High(FTable) do
    SetLength(FTable[i], Length(ColumnsCaption));

  for i:= 0 to High(FTable) do begin
    for j:= 0 to High(FTable[i]) do begin
      FillCell(j, i);
    end;
  end;
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
    SetLength(FTable[Y][X][iRecord], SQLQuery.FieldCount - 1);
    for i:= 1 to SQLQuery.FieldCount - 1 do
      FTable[Y][X][iRecord][i - 1]:= SQLQuery.Fields[i].AsString;
    SQLQuery.Next;
  end;

  if  Length(FTable[Y][X]) > 0 then begin
    ColumnsCaption[X].isEmpty:= False;
    RowsCaption[Y].isEmpty:= False;
  end;
end;

end.

