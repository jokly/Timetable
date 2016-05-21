unit UConverter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UMetadata, comobj;

type

  { TConverter }

  TConverter = class(TObject)
    public
      procedure SaveToHtml(AFileName: String; ARowsCaption, AColumnsCaption: TCaps;
        ATable: TTimeTable; AFieldsName: TFieldsName);
      procedure SaveToExcel(AFileName: String; ARowsCaption, AColumnsCaption: TCaps;
        ATable: TTimeTable; AFieldsName: TFieldsName);
    private
      function ConvertToHtml(ARowsCaption, AColumnsCaption: TCaps;
        ATable: TTimeTable; AFieldsName: TFieldsName): TStringList;
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
        ATable: TTimeTable; AFieldsName: TFieldsName);
begin
  ConvertToHtml(ARowsCaption, AColumnsCaption, ATable, AFieldsName).
    SaveToFile(AFileName);
end;

procedure TConverter.SaveToExcel(AFileName: String; ARowsCaption, AColumnsCaption: TCaps;
        ATable: TTimeTable; AFieldsName: TFieldsName);
var
  Excel: Variant;
  TempFileName, FilePath: String;
begin
  TempFileName:= 'tmp.html';

  FilePath:=  ExtractFilePath(AFileName);
  Excel:= CreateOleObject('Excel.Application');
  Excel.Visible:= False;
  Excel.DisplayAlerts := False;

  SaveToHtml(FilePath + TempFileName, ARowsCaption, AColumnsCaption, ATable, AFieldsName);
  Excel.WorkBooks.Open(WideString(FilePath + TempFileName));
  Excel.WorkBooks.Item[1].SaveAs(WideString(AFileName), 51);

  DeleteFile(PChar(FilePath + TempFileName));
  Excel.Quit;
end;

function TConverter.ConvertToHtml(ARowsCaption, AColumnsCaption: TCaps;
  ATable: TTimeTable; AFieldsName: TFieldsName): TStringList;
var
  row, col, i, j: Integer;
  HtmlText: String;
begin
  HtmlText:= '<!DOCTYPE html>' + Enter + OpenTag('html') + OpenTag('head') +
    SimpleTag('meta charset="utf-8"') + OpenTag('title') + 'Расписание' +
    Enter + CloseTag('title') + GetStyles() + CloseTag('head') + OpenTag('body');

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
          HtmlText+= AFieldsName[j] + ': ' + ATable[row][col][i].Rec[j] + SimpleTag('br');
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
  Result+= 'table, th, td { border: 1px solid black; }' + Enter;
  Result+= 'table { width: 100%; border-collapse: collapse;}' + Enter;
  Result+= 'td { background: #fff; vertical-align: top; } ' + Enter;
  Result+= 'th { background: #C9C9C9; } ' + Enter;
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

