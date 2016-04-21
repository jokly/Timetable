unit UTimeTable;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, ExtCtrls, sqldb, UMetadata, USQL, UFilter;

type

  TCell = array of array of String;

  TCap = record
    Value: String;
    ID: Integer;
  end;

  TCaps = array of TCap;

  { TTimetableForm }

  TTimetableForm = class(TForm)
    ButShowTable: TButton;
    ButtonAddFilter: TButton;
    CheckBoxDisplayFieldName: TCheckBox;
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
    procedure DrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure OnChangeOption(Sender: TObject);
  private
    { private declarations }
    FTable: array of array of TCell;
    FieldsName: array of String;
    ColTableIndex, RowTableIndex: Integer;
    RowsCaption: TCaps;
    ColumnsCaption: TCaps;
    Filters: TFilters;
    procedure FillDimensionsComboBox();
    function FillCaptions(TableIndex: Integer; var AFillArray: TCaps): Integer;
    procedure FillTable();
    procedure GetNameFields();
    procedure FillCell(X, Y: Integer);
    procedure SetWidthCell(AString: String; aCol: Integer);
  public
    { public declarations }
  end;

var
  TimetableForm: TTimetableForm;

implementation

{$R *.lfm}

{ TTimetableForm }

constructor TTimetableForm.Create(TableIndex: Integer);
begin
  inherited Create(Application);
  Tag:= TableIndex;
  Filters:= TFilters.Create(Tag, ButShowTable);
  FillDimensionsComboBox();
end;

procedure TTimetableForm.DrawGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  i, j, MarginTop, HeightCell, WidthCell: Integer;
  FRecord: array of String;
  Str: String;
  PicCaution: TPicture;
begin
  if (aRow = 0) and (aCol = 0) then
    Exit;

  HeightCell:= 200;

  if aCol = 0 then begin
    DrawGrid.RowHeights[aRow]:= HeightCell;
    SetWidthCell(RowsCaption[aRow - 1].Value, aCol);
    DrawGrid.Canvas.TextOut(aRect.Left + 5, aRect.Top + 2, RowsCaption[aRow - 1].Value);
  end
  else if aRow = 0 then begin
    DrawGrid.RowHeights[aRow]:= 50;
    SetWidthCell(ColumnsCaption[aCol - 1].Value, aCol);
    DrawGrid.Canvas.TextOut(aRect.Left + 5, aRect.Top + 2, ColumnsCaption[aCol - 1].Value);
  end
  else begin
    DrawGrid.RowHeights[aRow]:= HeightCell;
    MarginTop:= 16;
    for i:= 0 to High(FTable[aRow - 1][aCol - 1]) do begin
      FRecord:= FTable[aRow - 1][aCol - 1][i];
      for j:= 0 to High(FRecord) do begin
        Str:= '';
        if CheckBoxDisplayFieldName.Checked then
          Str:= FieldsName[j] + ': ';
        Str += FRecord[j];
        SetWidthCell(Str, aCol);
        DrawGrid.Canvas.TextOut(aRect.Left + 5, aRect.Top + MarginTop, Str);
        MarginTop+= 2 + 16;
      end;
      MarginTop+= 8;
    end;

    if MarginTop > HeightCell then begin
      PicCaution:= TPicture.Create;
      PicCaution.LoadFromFile('img/caution.bmp');
      DrawGrid.Canvas.Draw(aRect.Left, aRect.Top, PicCaution.Graphic);
    end;
  end;
end;

procedure TTimetableForm.SetWidthCell(AString: String; aCol: Integer);
var
  WidthCell: Integer;
begin
  WidthCell:= DrawGrid.Canvas.TextWidth(AString);
  if WidthCell > DrawGrid.ColWidths[aCol] then
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

  DrawGrid.RowCount:= FillCaptions(RowTableIndex,
    RowsCaption) + 1;
  DrawGrid.ColCount:= FillCaptions(ColTableIndex,
    ColumnsCaption) + 1;

  FillTable();
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
end;

end.

