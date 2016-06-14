unit UConverter;

{$mode objfpc}{$H+}

interface

uses
  //Classes, SysUtils, UMetadata, comobj, UFilter, UConflicts;

  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  ActnList, StdCtrls, IBConnection, sqldb, DB, DBCtrls, DBGrids, Menus,
  ExtCtrls, Math, Buttons, types, comobj, Windows, UFilter, UConflicts, UMetadata;

type

  TBorders = (All, Square, None);

  { TConverter }

  TConverter = class(TObject)
    public
      procedure SaveToHtml(AFileName: String; ARowsCaption, AColumnsCaption: TCaps;
        ATable: TTimeTable; AFieldsName: TFieldsName; IsShowFields: Boolean;
        Filters: TFiltersStrings);
      procedure SaveToExcel(AFileName: String; ARowsCaption, AColumnsCaption: TCaps;
        ATable: TTimeTable; AFieldsName: TFieldsName; IsShowFields: Boolean;
        Filters: TFiltersStrings);
    private
      function ConvertToHtml(ARowsCaption, AColumnsCaption: TCaps;
        ATable: TTimeTable; AFieldsName: TFieldsName; IsShowFields: Boolean;
        Filters: TFiltersStrings): TStringList;
      function GetStyles(): String;
      function OpenTag(ATag: String): String;
      function CloseTag(ATag: String): String;
      function SimpleTag(ATag: String): String;
      procedure SetExcelFont(XL: olevariant; TextHorizontalAlign: integer;
        TextVerticalAlign: integer; Merge: boolean; Bold: boolean; Size: integer; Warp: boolean);
      procedure SetExcelBorders(XL: olevariant; KindOfBorders: TBorders);
  end;

var
  Converter: TConverter;

implementation

const
  Enter = #13#10;
  xlTop = -4160;
  xlCenter = -4108;
  xlLeft = -4131;

{ TConvert }

procedure TConverter.SaveToHtml(AFileName: String; ARowsCaption, AColumnsCaption: TCaps;
        ATable: TTimeTable; AFieldsName: TFieldsName; IsShowFields: Boolean;
        Filters: TFiltersStrings);
begin
  ConvertToHtml(
    ARowsCaption, AColumnsCaption, ATable, AFieldsName, IsShowFields, Filters).
      SaveToFile(AFileName);
end;

procedure TConverter.SaveToExcel(AFileName: String; ARowsCaption, AColumnsCaption: TCaps;
        ATable: TTimeTable; AFieldsName: TFieldsName; IsShowFields: Boolean; Filters:
        TFiltersStrings);
var
  XL, Sheet: olevariant;
  FilePath: String;
  i, j, row, col, counter: Integer;
  PreviousMaxRowNumber, MaxRowNumber, BeginRow, MaxRow: Integer;
  TextColor: TColor;
begin
  FilePath:= ExtractFilePath(AFileName);

  BeginRow:= 1;
  MaxRow:= BeginRow;

  XL:= CreateOleObject('Excel.Application');
  XL.WorkBooks.add(-4167);
  Sheet:= XL.Workbooks[1].WorkSheets[1];
  Sheet.Name:= WideString(UTF8Decode('Расписание'));

  if Length(Filters) > 0 then begin
    XL.Workbooks[1].WorkSheets[1].Cells.Item[BeginRow, 1]:= WideString(UTF8Decode('Название поля'));
    XL.Workbooks[1].WorkSheets[1].Cells.Item[BeginRow, 2]:= WideString(UTF8Decode('Условие'));
    XL.Workbooks[1].WorkSheets[1].Cells.Item[BeginRow, 3]:= WideString(UTF8Decode('Константа'));

    XL.ActiveWorkBook.WorkSheets[1].Range[XL.Workbooks[1].WorkSheets[1].Cells.Item[1, 1],
      XL.Workbooks[1].WorkSheets[1].Cells.Item[1, 3]].Select;
    SetExcelBorders(XL, All);
    SetExcelFont(XL, xlCenter, xlCenter, False, True, 10, True);

    for i:= 0 to High(Filters) do begin
      Inc(MaxRow);
      XL.Workbooks[1].WorkSheets[1].Cells.Item[MaxRow, 1]:= WideString(UTF8Decode(Filters[i].AppField));
      XL.Workbooks[1].WorkSheets[1].Cells.Item[MaxRow, 2]:= WideString(UTF8Decode(Filters[i].Operation));
      XL.Workbooks[1].WorkSheets[1].Cells.Item[MaxRow, 3]:= WideString(UTF8Decode(Filters[i].Value));
    end;

     XL.ActiveWorkBook.WorkSheets[1].Range[XL.Workbooks[1].WorkSheets[1].Cells.Item[BeginRow, 1],
      XL.Workbooks[1].WorkSheets[1].Cells.Item[MaxRow, 3]].Select;
    SetExcelBorders(XL, All);
    SetExcelFont(XL, xlCenter, xlCenter, False, False, 10, True);
    MaxRow+= 2;
    BeginRow:= MaxRow;
  end;

  for i := 0 to High(AColumnsCaption) do
    Sheet.Cells[BeginRow, i + 2] := WideString(UTF8Decode(AColumnsCaption[i].Value));

  XL.ActiveWorkBook.WorkSheets[1].Range[Sheet.Cells.Item[BeginRow, 1],
    Sheet.Cells.Item[BeginRow, High(AColumnsCaption) + 2]].Select;
  SetExcelBorders(Xl, All);
  SetExcelFont(XL, xlCenter, xlCenter, False, True, 10, True);

  PreviousMaxRowNumber := BeginRow;
  MaxRowNumber := BeginRow;
  Sheet.Columns[1].ColumnWidth:= 17;

  for row:= 0 to High(ATable) do begin
    counter := PreviousMaxRowNumber;
    for col:= 0 to High(ATable[row]) do begin
      Sheet.Columns[col + 1 + 1].ColumnWidth := 25;
      for i := 0 to High(ATable[row][col]) do begin
        if TConflictsForm.IsRecConflict(ATable[row][col][i].ID) then
          TextColor:= clRed
        else
          TextColor:= clBlack;

        for j:= 0 to High(ATable[row][col][i].Rec) do begin
          if IsShowFields then
            Sheet.Cells[counter + 1, col + 2]:=
              WideString(UTF8Decode(AFieldsName[j] + ': ' + ATable[row][col][i].Rec[j]))
          else
            Sheet.Cells[counter + 1, col + 2]:=
              WideString(UTF8Decode(ATable[row][col][i].Rec[j]));

          Sheet.Cells[counter + 1, col + 2].Font.color:= TextColor;
          Inc(counter);
        end;
        Sheet.Cells[counter + 1, col + 2]:= Enter;
        Inc(counter);
      end;
      MaxRowNumber := max(counter, MaxRowNumber);
      counter := PreviousMaxRowNumber;
    end;

    XL.ActiveWorkBook.WorkSheets[1].Range[Sheet.Cells.Item[PreviousMaxRowNumber + 1, 1],
    Sheet.Cells.Item[MaxRowNumber, 1]].Select;
    SetExcelBorders(Xl, Square);
    SetExcelFont(XL, xlCenter, xlTop, True, True, 10, True);
    XL.Selection := WideString(UTF8Decode(ARowsCaption[row].Value));

    XL.ActiveWorkBook.WorkSheets[1].Range[Sheet.Cells.Item[PreviousMaxRowNumber + 1, 2],
      Sheet.Cells.Item[MaxRowNumber, High(AColumnsCaption) + 2]].Select;
    SetExcelBorders(Xl, Square);
    PreviousMaxRowNumber := MaxRowNumber;
  end;

   XL.ActiveWorkBook.WorkSheets[1].Range[Sheet.Cells.Item[BeginRow + 1, 2],
    Sheet.Cells.Item[MaxRowNumber, High(AColumnsCaption) + 2]].Select;

  for i := 2 to High(AColumnsCaption) + 2 do
  begin
    XL.ActiveWorkBook.WorkSheets[1].Range[Sheet.Cells.Item[BeginRow, i],
      Sheet.Cells.Item[MaxRowNumber + 1, i]].Select;
    SetExcelFont(XL, xlLeft, xlTop, False, False, 10, True);
    SetExcelBorders(XL, Square);
  end;

  XL.Workbooks[1].SaveAs(WideString(AFileName));
  XL.DisplayAlerts := False;
  XL.Quit;
end;

procedure TConverter.SetExcelFont(XL: olevariant; TextHorizontalAlign: integer;
  TextVerticalAlign: integer; Merge: boolean; Bold: boolean; Size: integer;
  Warp: boolean);
begin
  if Merge then
    XL.Selection.Merge;
  if Bold then
    XL.Selection.Font.Bold:= True;
  if Warp then
    XL.Selection.WrapText:= True;

  XL.Selection.HorizontalAlignment:= TextHorizontalAlign;
  XL.Selection.VerticalAlignment:= TextVerticalAlign;
  XL.Selection.Font.Size:= Size;
end;

procedure TConverter.SetExcelBorders(XL: olevariant; KindOfBorders: TBorders);
begin
  case KindOfBorders of
    None: XL.Selection.Interior.Color:= RGB(255, 255, 255);
    All:
    begin
      XL.Selection.Borders.LineStyle := 1;
      XL.Selection.Borders.Weight := 2;
    end;
    Square:
    begin
      XL.Selection.Interior.Color:= RGB(255, 255, 255);
      XL.Selection.Borders[8].LineStyle:= 1;
      XL.Selection.Borders[8].Weight:= 2;
      XL.Selection.Borders[9].LineStyle:= 1;
      XL.Selection.Borders[9].Weight:= 2;
      XL.Selection.Borders[7].LineStyle:= 1;
      XL.Selection.Borders[7].Weight:= 2;
      XL.Selection.Borders[10].LineStyle:= 1;
      XL.Selection.Borders[10].Weight:= 2;
    end;
  end;
end;

function TConverter.ConvertToHtml(ARowsCaption, AColumnsCaption: TCaps;
  ATable: TTimeTable; AFieldsName: TFieldsName; IsShowFields: Boolean;
  Filters: TFiltersStrings): TStringList;
var
  row, col, i, j, k, g: Integer;
  HtmlText: String;
begin
  HtmlText:= '<!DOCTYPE html>' + Enter + OpenTag('html') + OpenTag('head') +
    SimpleTag('meta charset="utf-8"') + OpenTag('title') + 'Расписание' +
    Enter + CloseTag('title') + GetStyles() + CloseTag('head') + OpenTag('body');

  if Length(Filters) > 0 then begin
    HtmlText+= OpenTag('table') + OpenTag('caption') + 'Фильтры' + CloseTag('caption');
    HtmlText+= OpenTag('tr');
    HtmlText+= OpenTag('th') + 'Поле' + CloseTag('th');
    HtmlText+= OpenTag('th') + 'Операция' + CloseTag('th');
    HtmlText+= OpenTag('th') + 'Значение' + CloseTag('th') + CloseTag('tr');
    for i:= 0 to High(Filters) do begin
      HtmlText+= OpenTag('tr');
      HtmlText+= OpenTag('td') + Filters[i].AppField + CloseTag('td');
      HtmlText+= OpenTag('td') + Filters[i].Operation + CloseTag('td');
      HtmlText+= OpenTag('td') + Filters[i].Value + CloseTag('td');
      HtmlText+= CloseTag('tr');
    end;
    HtmlText+= CloseTag('table') + SimpleTag('br') + SimpleTag('br');
  end;

  HtmlText+= OpenTag('table') + OpenTag('tr') + OpenTag('th') + CloseTag('th');

  for i:= 0 to High(AColumnsCaption) do
    HtmlText+= OpenTag('th') + AColumnsCaption[i].Value + CloseTag('th');

  HtmlText+= CloseTag('tr');

  for row:= 0 to High(ATable) do begin
    HtmlText+= OpenTag('tr');
    HtmlText+= OpenTag('th') + ARowsCaption[row].Value + Enter + CloseTag('th');

    for col:= 0 to High(ATable[row]) do begin
      HtmlText+= OpenTag('td');

      for i:= 0 to High(ATable[row][col]) do begin
        if TConflictsForm.IsRecConflict(ATable[row][col][i].ID) then
          HtmlText+= OpenTag('p style = "display: block; background: #FF7B00;"')
        else
          HtmlText+= OpenTag('p');

        for j:= 0 to High(ATable[row][col][i].Rec) do begin
          if IsShowFields then
            HtmlText+= AFieldsName[j] + ': ';
          HtmlText+= ATable[row][col][i].Rec[j] + SimpleTag('br');
        end;
        HtmlText+= CloseTag('p') + SimpleTag('hr');
      end;
      HtmlText+= CloseTag('td');
    end;
    HtmlText+= CloseTag('tr');
  end;

  HtmlText+= CloseTag('table') + CloseTag('body') + CloseTag('html');

  Result:= TStringList.Create;
  Result.Add(HtmlText);

end;

function TConverter.GetStyles: String;
begin
  Result:= OpenTag('style type="text/css"') + Enter;
  Result+= 'table {' +
    'font-family: "Lucida Sans Unicode", "Lucida Grande", Sans-Serif;' +
    'font-size: 14px;' +
    'border-collapse: collapse;' +
    'text-align: center;' +
    '}' +
    'th, td:first-child {' +
    'background: #AFCDE7;' +
    'color: #fff;' +
    'padding: 10px 20px;' +
    '}' +
    'th, td {' +
    'border-style: solid;' +
    'border-width: 0 1px 1px 0;' +
    'border-color: #fff;' +
    '}' +
    'td {' +
    'background: #D8E6F3;' +
    '}' +
    'th:first-child, td:first-child {' +
    'text-align: left;' +
    '}';
  Result+= CloseTag('style');
end;

function TConverter.OpenTag(ATag: String): String;
begin
  Result:= '<' + ATag + '>' + Enter;
end;

function TConverter.CloseTag(ATag: String): String;
begin
  Result:= Enter + '</' + ATag + '>' + Enter;
end;

function TConverter.SimpleTag(ATag: String): String;
begin
  Result:= '<' + ATag + '/>' + Enter;
end;

initialization
  Converter := TConverter.Create();

end.

