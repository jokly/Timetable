unit UConverter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UMetadata, comobj, UFilter;

type

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
  end;

var
  Converter: TConverter;

implementation

const
  Enter = #13#10;

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
  Excel: Variant;
  TempFileName, FilePath: String;
begin
  TempFileName:= 'tmp.html';

  FilePath:= ExtractFilePath(AFileName);
  Excel:= CreateOleObject('Excel.Application');
  Excel.Visible:= False;
  Excel.DisplayAlerts := False;

  SaveToHtml(FilePath + TempFileName, ARowsCaption, AColumnsCaption, ATable,
    AFieldsName, IsShowFields, Filters);
  Excel.WorkBooks.Open(WideString(FilePath + TempFileName));
  Excel.WorkBooks.Item[1].SaveAs(WideString(AFileName), 51);

  DeleteFile(PChar(FilePath + TempFileName));
  Excel.Quit;
end;

function TConverter.ConvertToHtml(ARowsCaption, AColumnsCaption: TCaps;
  ATable: TTimeTable; AFieldsName: TFieldsName; IsShowFields: Boolean;
  Filters: TFiltersStrings): TStringList;
var
  row, col, i, j: Integer;
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
    'color: white;' +
    'padding: 10px 20px;' +
    '}' +
    'th, td {' +
    'border-style: solid;' +
    'border-width: 0 1px 1px 0;' +
    'border-color: white;' +
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

