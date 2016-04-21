CREATE DATABASE 'localhost:C:\Users\slast\Documents\Projects\Timetable\db\FEFU.FDB' USER 'SYSDBA' PASSWORD 'masterkey' 
	DEFAULT CHARACTER SET UTF8;
CREATE GENERATOR GroupsIdGenerator;
CREATE GENERATOR LessonsIdGenerator;
CREATE GENERATOR TeachersIdGenerator;
CREATE GENERATOR ClassRoomsIdGenerator;
CREATE GENERATOR LessonsTimesIdGenerator;
CREATE GENERATOR WeekDaysIdGenerator;
CREATE GENERATOR TimeTableIdGenerator;
CREATE GENERATOR LessonsTypesIdGenerator;

CREATE TABLE Groups(
	id INTEGER NOT NULL PRIMARY KEY,
	name VARCHAR(10)
);

CREATE TABLE Lessons(
	id INTEGER NOT NULL PRIMARY KEY,
	name VARCHAR(100)
);

CREATE TABLE Teachers(
	id INTEGER NOT NULL PRIMARY KEY,
	last_name VARCHAR(20),
	first_name VARCHAR(20), 
    middle_name VARCHAR(20)
);

CREATE TABLE Classrooms(
	id INTEGER NOT NULL PRIMARY KEY,
	name VARCHAR(10)
);

CREATE TABLE Lessons_Times(
	id INTEGER NOT NULL PRIMARY KEY,
	begin_ TIME,
	end_ TIME
);

CREATE TABLE Weekdays(
	id INTEGER NOT NULL PRIMARY KEY,
	name VARCHAR(20)
);

CREATE TABLE Lessons_Types(
	id INTEGER NOT NULL PRIMARY KEY,
	name VARCHAR(50)
);

CREATE TABLE Timetable(
	id INTEGER NOT NULL PRIMARY KEY,
	lesson_id INTEGER REFERENCES Lessons(id) ON UPDATE CASCADE ON DELETE CASCADE,
	lesson_type_id INTEGER REFERENCES Lessons_Types(id) ON UPDATE CASCADE ON DELETE CASCADE,
	teacher_id INTEGER REFERENCES Teachers(id) ON UPDATE CASCADE ON DELETE CASCADE,
	group_id INTEGER REFERENCES Groups(id) ON UPDATE CASCADE ON DELETE CASCADE,
	classroom_id INTEGER REFERENCES Classrooms(id) ON UPDATE CASCADE ON DELETE CASCADE,
	weekday_id INTEGER REFERENCES Weekdays(id) ON UPDATE CASCADE ON DELETE CASCADE,
	lesson_time_id INTEGER REFERENCES Lessons_Times(id) ON UPDATE CASCADE ON DELETE CASCADE
);

SET TERM ^ ;

CREATE TRIGGER GroupsIdTrigger FOR Groups ACTIVE BEFORE INSERT
AS BEGIN
new.id = NEXT VALUE FOR GroupsIdGenerator;
END^

CREATE TRIGGER TeachersIdTrigger FOR Teachers ACTIVE BEFORE INSERT
AS BEGIN
new.id = NEXT VALUE FOR TeachersIdGenerator;
END^

CREATE TRIGGER LessonsIdTrigger FOR Lessons ACTIVE BEFORE INSERT
AS BEGIN
new.id = NEXT VALUE FOR LessonsIdGenerator;
END^

CREATE TRIGGER LessonsTimesIdTrigger FOR Lessons_Times ACTIVE BEFORE INSERT
AS BEGIN
new.id = NEXT VALUE FOR LessonsTimesIdGenerator;
END^

CREATE TRIGGER WeekDaysIdTrigger FOR Weekdays ACTIVE BEFORE INSERT
AS BEGIN
new.id = NEXT VALUE FOR WeekDaysIdGenerator;
END^

CREATE TRIGGER ClassRoomsIdTrigger FOR Classrooms ACTIVE BEFORE INSERT
AS BEGIN
new.id = NEXT VALUE FOR ClassRoomsIdGenerator;
END^

CREATE TRIGGER LessonsTypesIdTrigger FOR Lessons_Types ACTIVE BEFORE INSERT
AS BEGIN
new.id = NEXT VALUE FOR LessonsTypesIdGenerator;
END^

CREATE TRIGGER TimeTableIdTrigger FOR Timetable ACTIVE BEFORE INSERT
AS BEGIN
new.id = NEXT VALUE FOR TimeTableIdGenerator;
END^

SET TERM ; ^

COMMIT;

INSERT INTO Groups VALUES(1, 'Б8103а1');
INSERT INTO Groups VALUES(2, 'Б8103а2');
INSERT INTO Groups VALUES(3, 'Б8103б1');
INSERT INTO Groups VALUES(4, 'Б8103б2');
INSERT INTO Groups VALUES(5, 'Б8102-1');
INSERT INTO Groups VALUES(6, 'Б8102-2');
INSERT INTO Groups VALUES(7, 'С3107в');

INSERT INTO Lessons VALUES(1, 'Математический анализ');
INSERT INTO Lessons VALUES(2, 'Дискретная математика и математическая логика');
INSERT INTO Lessons VALUES(3, 'Алгебра и геометрия');
INSERT INTO Lessons VALUES(4, 'Иностранный язык');
INSERT INTO Lessons VALUES(5, 'Практикум на ЭВМ');
INSERT INTO Lessons VALUES(6, 'Языки и методы программирования');
INSERT INTO Lessons VALUES(7, 'Базы данных');
INSERT INTO Lessons VALUES(8, 'Архитектура компьютеров');
INSERT INTO Lessons VALUES(9, 'Элективные курсы по физической культуре');
INSERT INTO Lessons VALUES(10, 'Software engineering');
INSERT INTO Lessons VALUES(11, 'Теоретическая математика и физика');
INSERT INTO Lessons VALUES(12, 'Русский язык и культура речи');
INSERT INTO Lessons VALUES(13, 'Алгебра');
INSERT INTO Lessons VALUES(14, 'История');
INSERT INTO Lessons VALUES(15, 'Теоретическая механика');
INSERT INTO Lessons VALUES(16, 'Начертательная геометрия и инженерная графика');
INSERT INTO Lessons VALUES(17, 'Физика');
INSERT INTO Lessons VALUES(18, 'Информационные технологии в строительстве');
INSERT INTO Lessons VALUES(19, 'Инженерная геология');

INSERT INTO Teachers VALUES(1, 'Кленин', 'Александр', 'Сергеевич');
INSERT INTO Teachers VALUES(2, 'Клевчихин', 'Юрий', 'Александрович');
INSERT INTO Teachers VALUES(3, 'Прилепкина', 'Елена', 'Гумаровна');
INSERT INTO Teachers VALUES(4, 'Спорышев', 'Максим', 'Сергеевич');
INSERT INTO Teachers VALUES(5, 'Машенцев', 'Владимир', 'Юрьевич');
INSERT INTO Teachers VALUES(6, 'Пак', 'Геннадий', 'Константинович');
INSERT INTO Teachers VALUES(7, 'Трикашная', 'Наталья', 'Вячеславовна');
INSERT INTO Teachers VALUES(8, 'Кириленко', 'Лариса', 'Михайловна');
INSERT INTO Teachers VALUES(9, 'Баранов', 'Андрей', 'Александрович');
INSERT INTO Teachers VALUES(10, 'Алексанин', 'Григорий', 'Анатольевич');
INSERT INTO Teachers VALUES(11, 'Веремеева', 'Ирина', 'Феликсовна');
INSERT INTO Teachers VALUES(12, 'Абстрактный', 'Учитель', 'Физкультуры');
INSERT INTO Teachers VALUES(13, 'Пак', 'С.', 'Б.');
INSERT INTO Teachers VALUES(14, 'Сущенко', 'А.', 'А.');
INSERT INTO Teachers VALUES(15, 'Малыкина', 'Ирина', 'Анатольевна');
INSERT INTO Teachers VALUES(16, 'Чеканов', 'Сергей', 'Геннадьевич');
INSERT INTO Teachers VALUES(17, 'Мишаков', 'Александр', 'Владиславович');
INSERT INTO Teachers VALUES(18, 'Сторожок', 'Евгений', 'Анатольевич');
INSERT INTO Teachers VALUES(19, 'Перцевая', 'Ксения', 'Александровна');
INSERT INTO Teachers VALUES(20, 'Степанова', 'Алена', 'Андреевна');
INSERT INTO Teachers VALUES(21, 'Сакун', 'Р.', 'А.');
INSERT INTO Teachers VALUES(22, 'Крикунова', 'Юлия', 'Анатольевна');
INSERT INTO Teachers VALUES(23, 'Исхакова', 'Ольга', 'Дмитриевна');
INSERT INTO Teachers VALUES(24, 'Бойко', 'Людмила', 'Александровна');
INSERT INTO Teachers VALUES(25, 'Шустикова', 'Татьяна', 'Валентиновна');
INSERT INTO Teachers VALUES(26, 'Мацуцин', 'Артур', 'Алексеевич');
INSERT INTO Teachers VALUES(27, 'Фоменко', 'Елена', 'Львовна');
INSERT INTO Teachers VALUES(28, 'Проценко', 'Виктория', 'Владимировна');
INSERT INTO Teachers VALUES(29, 'Василенко', 'Геннадий', 'Петрович');
INSERT INTO Teachers VALUES(30, 'Аверкова', 'Галина', 'Владимировны');
INSERT INTO Teachers VALUES(31, 'Руковишникова', 'Валентина', 'Ивановна');

INSERT INTO Lessons_Times VALUES(1, '08:30:00.0000', '10:00:00.0000');
INSERT INTO Lessons_Times VALUES(2, '10:10:00.0000', '11:40:00.0000');
INSERT INTO Lessons_Times VALUES(3, '11:50:00.0000', '13:20:00.0000');
INSERT INTO Lessons_Times VALUES(4, '13:30:00.0000', '15:00:00.0000');
INSERT INTO Lessons_Times VALUES(5, '15:10:00.0000', '16:40:00.0000');
INSERT INTO Lessons_Times VALUES(6, '16:50:00.0000', '18:20:00.0000');
INSERT INTO Lessons_Times VALUES(7, '18:30:00.0000', '20:00:00.0000');
INSERT INTO Lessons_Times VALUES(8, '20:10:00.0000', '21:40:00.0000');

INSERT INTO Weekdays VALUES(1, 'Понедельник');
INSERT INTO Weekdays VALUES(2, 'Вторник');
INSERT INTO Weekdays VALUES(3, 'Среда');
INSERT INTO Weekdays VALUES(4, 'Четверг');
INSERT INTO Weekdays VALUES(5, 'Пятница');
INSERT INTO Weekdays VALUES(6, 'Суббота');
INSERT INTO Weekdays VALUES(7, 'Воскресенье');

INSERT INTO Classrooms VALUES(1, 'D733');
INSERT INTO Classrooms VALUES(2, 'D733a');
INSERT INTO Classrooms VALUES(3, 'D654/752');
INSERT INTO Classrooms VALUES(4, 'D412/542');
INSERT INTO Classrooms VALUES(5, 'S');
INSERT INTO Classrooms VALUES(6, 'D547');
INSERT INTO Classrooms VALUES(7, 'D741');
INSERT INTO Classrooms VALUES(8, 'D743');
INSERT INTO Classrooms VALUES(9, 'D549a');
INSERT INTO Classrooms VALUES(10, 'D746');
INSERT INTO Classrooms VALUES(11, 'D546');
INSERT INTO Classrooms VALUES(12, 'D548');
INSERT INTO Classrooms VALUES(13, 'D820');
INSERT INTO Classrooms VALUES(14, 'D546а');
INSERT INTO Classrooms VALUES(15, 'D734а');
INSERT INTO Classrooms VALUES(16, 'D945');
INSERT INTO Classrooms VALUES(17, 'D734');
INSERT INTO Classrooms VALUES(18, 'C406');
INSERT INTO Classrooms VALUES(19, 'C408');
INSERT INTO Classrooms VALUES(20, 'E425');
INSERT INTO Classrooms VALUES(21, 'E320/224');
INSERT INTO Classrooms VALUES(22, 'D817');
INSERT INTO Classrooms VALUES(23, 'E720');
INSERT INTO Classrooms VALUES(24, 'E323/236');
INSERT INTO Classrooms VALUES(25, 'E702/802');
INSERT INTO Classrooms VALUES(26, 'E809');
INSERT INTO Classrooms VALUES(27, 'E706');
INSERT INTO Classrooms VALUES(28, 'E809');
INSERT INTO Classrooms VALUES(29, 'E541/442');

INSERT INTO Lessons_Types VALUES(1, 'Практическое занятие');
INSERT INTO Lessons_Types VALUES(2, 'Лекция');
INSERT INTO Lessons_Types VALUES(3, 'Лабораторное занятие');

INSERT INTO Timetable VALUES(1, 1, 2, 3, 1, 3, 1, 2);
INSERT INTO Timetable VALUES(2, 1, 2, 3, 2, 3, 1, 2);
INSERT INTO Timetable VALUES(3, 1, 2, 3, 3, 3, 1, 2);
INSERT INTO Timetable VALUES(4, 1, 2, 3, 4, 3, 1, 2);
INSERT INTO Timetable VALUES(5, 2, 2, 6, 1, 4, 1, 3);
INSERT INTO Timetable VALUES(6, 2, 2, 6, 2, 4, 1, 3);
INSERT INTO Timetable VALUES(7, 2, 2, 6, 3, 4, 1, 3);
INSERT INTO Timetable VALUES(8, 2, 2, 6, 4, 4, 1, 3);
INSERT INTO Timetable VALUES(9, 3, 2, 6, 1, 4, 1, 4);
INSERT INTO Timetable VALUES(10, 3, 2, 6, 2, 4, 1, 4);
INSERT INTO Timetable VALUES(11, 3, 2, 6, 3, 4, 1, 4);
INSERT INTO Timetable VALUES(12, 3, 2, 6, 4, 4, 1, 4);
INSERT INTO Timetable VALUES(13, 9, 1, 12, 1, 5, 1, 5);
INSERT INTO Timetable VALUES(14, 9, 1, 12, 2, 5, 1, 5);
INSERT INTO Timetable VALUES(15, 9, 1, 12, 3, 5, 1, 5);
INSERT INTO Timetable VALUES(16, 9, 1, 12, 4, 5, 1, 5);
INSERT INTO Timetable VALUES(17, 1, 1, 2, 1, 6, 2, 1);
INSERT INTO Timetable VALUES(18, 1, 1, 2, 2, 6, 2, 1);
INSERT INTO Timetable VALUES(19, 4, 1, 8, 1, 8, 2, 2);
INSERT INTO Timetable VALUES(20, 4, 1, 8, 2, 8, 2, 2);
INSERT INTO Timetable VALUES(21, 4, 1, 11, 1, 7, 2, 2);
INSERT INTO Timetable VALUES(22, 4, 1, 11, 2, 7, 2, 2);
INSERT INTO Timetable VALUES(23, 5, 3, 4, 1, 1, 2, 3);
INSERT INTO Timetable VALUES(24, 5, 3, 4, 1, 1, 2, 4);
INSERT INTO Timetable VALUES(25, 6, 3, 9, 2, 2, 2, 3);
INSERT INTO Timetable VALUES(26, 2, 1, 6, 1, 6, 3, 1);
INSERT INTO Timetable VALUES(27, 2, 1, 6, 2, 6, 3, 1);
INSERT INTO Timetable VALUES(28, 3, 1, 7, 1, 7, 3, 2);
INSERT INTO Timetable VALUES(29, 3, 1, 7, 2, 7, 3, 2);
INSERT INTO Timetable VALUES(30, 3, 1, 7, 1, 7, 3, 3);
INSERT INTO Timetable VALUES(31, 3, 1, 7, 2, 7, 3, 3);
INSERT INTO Timetable VALUES(32, 5, 3, 4, 2, 1, 4, 3);
INSERT INTO Timetable VALUES(33, 5, 3, 4, 2, 1, 4, 4);
INSERT INTO Timetable VALUES(34, 9, 1, 12, 1, 5, 4, 5);
INSERT INTO Timetable VALUES(35, 9, 1, 12, 2, 5, 4, 5);
INSERT INTO Timetable VALUES(36, 9, 1, 12, 3, 5, 4, 5);
INSERT INTO Timetable VALUES(37, 9, 1, 12, 4, 5, 4, 5);
INSERT INTO Timetable VALUES(38, 7, 3, 1, 1, 1, 5, 1);
INSERT INTO Timetable VALUES(39, 7, 3, 1, 1, 1, 5, 2);
INSERT INTO Timetable VALUES(40, 8, 3, 5, 1, 2, 5, 2);
INSERT INTO Timetable VALUES(41, 8, 3, 5, 1, 2, 5, 3);
INSERT INTO Timetable VALUES(42, 8, 3, 5, 2, 2, 5, 4);
INSERT INTO Timetable VALUES(43, 8, 3, 5, 2, 2, 5, 5);
INSERT INTO Timetable VALUES(44, 7, 2, 1, 1, 3, 6, 1);
INSERT INTO Timetable VALUES(45, 6, 2, 1, 1, 3, 6, 2);
INSERT INTO Timetable VALUES(46, 8, 2, 1, 1, 3, 6, 2);
INSERT INTO Timetable VALUES(47, 6, 1, 9, 1, 11, 6, 3);
INSERT INTO Timetable VALUES(48, 7, 3, 10, 2, 9, 6, 3);
INSERT INTO Timetable VALUES(49, 7, 3, 10, 2, 9, 6, 4);
INSERT INTO Timetable VALUES(50, 1, 1, 2, 3, 6, 2, 2);
INSERT INTO Timetable VALUES(51, 1, 1, 2, 4, 6, 2, 2);
INSERT INTO Timetable VALUES(52, 4, 1, 8, 3, 8, 2, 3);
INSERT INTO Timetable VALUES(53, 4, 1, 8, 4, 8, 2, 3);
INSERT INTO Timetable VALUES(54, 5, 3, 13, 3, 11, 2, 4);
INSERT INTO Timetable VALUES(55, 5, 3, 13, 3, 11, 2, 5);
INSERT INTO Timetable VALUES(56, 5, 3, 9, 4, 2, 2, 4);
INSERT INTO Timetable VALUES(57, 5, 3, 9, 4, 2, 2, 5);
INSERT INTO Timetable VALUES(58, 3, 1, 7, 3, 7, 3, 1);
INSERT INTO Timetable VALUES(59, 3, 1, 7, 4, 7, 3, 1);
INSERT INTO Timetable VALUES(60, 2, 1, 6, 3, 6, 3, 2);
INSERT INTO Timetable VALUES(61, 2, 1, 6, 4, 6, 3, 2);
INSERT INTO Timetable VALUES(62, 3, 1, 7, 3, 7, 3, 3);
INSERT INTO Timetable VALUES(63, 3, 1, 7, 4, 7, 3, 3);
INSERT INTO Timetable VALUES(64, 7, 1, 18, 3, 14, 3, 4);
INSERT INTO Timetable VALUES(65, 7, 1, 18, 4, 14, 3, 4);
INSERT INTO Timetable VALUES(66, 6, 3, 13, 3, 2, 4, 1);
INSERT INTO Timetable VALUES(67, 8, 3, 21, 4, 14, 4, 1);
INSERT INTO Timetable VALUES(68, 8, 3, 21, 3, 14, 4, 2);
INSERT INTO Timetable VALUES(69, 7, 3, 18, 3, 14, 4, 2);
INSERT INTO Timetable VALUES(70, 7, 3, 18, 4, 14, 4, 2);
INSERT INTO Timetable VALUES(71, 8, 3, 21, 4, 14, 4, 2);
INSERT INTO Timetable VALUES(72, 8, 3, 21, 3, 14, 4, 3);
INSERT INTO Timetable VALUES(73, 6, 3, 9, 4, 2, 4, 3);
INSERT INTO Timetable VALUES(74, 7, 3,  18, 3, 14, 5, 1);
INSERT INTO Timetable VALUES(75, 7, 3,  18, 3, 14, 5, 2);
INSERT INTO Timetable VALUES(76, 7, 3,  18, 4, 14, 5, 2);
INSERT INTO Timetable VALUES(77, 7, 3,  18, 4, 14, 5, 3);
INSERT INTO Timetable VALUES(78, 7, 2, 1, 2, 3, 6, 1);
INSERT INTO Timetable VALUES(79, 6, 2, 1, 2, 3, 6, 2);
INSERT INTO Timetable VALUES(80, 8, 2, 1, 2, 3, 6, 2);
INSERT INTO Timetable VALUES(81, 7, 2, 1, 3, 3, 6, 1);
INSERT INTO Timetable VALUES(82, 6, 2, 1, 3, 3, 6, 2);
INSERT INTO Timetable VALUES(83, 8, 2, 1, 3, 3, 6, 2);
INSERT INTO Timetable VALUES(84, 7, 2, 1, 4, 3, 6, 1);
INSERT INTO Timetable VALUES(85, 6, 2, 1, 4, 3, 6, 2);
INSERT INTO Timetable VALUES(86, 8, 2, 1, 4, 3, 6, 2);
INSERT INTO Timetable VALUES(87, 1, 2, 3, 5, 3, 1, 2);
INSERT INTO Timetable VALUES(88, 1, 2, 3, 6, 3, 1, 2);
INSERT INTO Timetable VALUES(89, 10, 1, 14, 5, 1, 1, 3);
INSERT INTO Timetable VALUES(90, 10, 1, 14, 6, 1, 1, 3);
INSERT INTO Timetable VALUES(91, 5, 3, 15, 5, 17, 1, 3);
INSERT INTO Timetable VALUES(92, 5, 3, 14, 6, 1, 1, 3);
INSERT INTO Timetable VALUES(93, 5, 3, 15, 5, 17, 1, 4);
INSERT INTO Timetable VALUES(94, 5, 3, 14, 6, 1, 1, 4);
INSERT INTO Timetable VALUES(95, 9, 1, 12, 5, 5, 1, 5);
INSERT INTO Timetable VALUES(96, 9, 1, 12, 6, 5, 1, 5);
INSERT INTO Timetable VALUES(97, 2, 1, 16, 5, 12, 2, 2);
INSERT INTO Timetable VALUES(98, 2, 1, 16, 6, 12, 2, 2);
INSERT INTO Timetable VALUES(99, 1, 1, 3, 5, 12, 2, 3);
INSERT INTO Timetable VALUES(100, 1, 1, 3, 6, 12, 2, 3);
INSERT INTO Timetable VALUES(101, 4, 1, 8, 5, 8, 2, 4);
INSERT INTO Timetable VALUES(102, 4, 1, 8, 6, 8, 2, 4);
INSERT INTO Timetable VALUES(103, 11, 2, 17, 5, 13, 3, 2);
INSERT INTO Timetable VALUES(104, 11, 2, 17, 6, 13, 3, 2);
INSERT INTO Timetable VALUES(105, 11, 1, 17, 5, 13, 3, 3);
INSERT INTO Timetable VALUES(106, 11, 1, 17, 6, 13, 3, 3);
INSERT INTO Timetable VALUES(107, 11, 3, 17, 5, 13, 3, 3);
INSERT INTO Timetable VALUES(108, 11, 3, 17, 6, 13, 3, 3);
INSERT INTO Timetable VALUES(109, 7, 2, 18, 5, 14, 3, 4);
INSERT INTO Timetable VALUES(110, 7, 2, 18, 6, 14, 3, 4);
INSERT INTO Timetable VALUES(112, 7, 3, 18, 5, 14, 3, 5);
INSERT INTO Timetable VALUES(113, 7, 3, 18, 6, 14, 3, 5);
INSERT INTO Timetable VALUES(114, 6, 3, 14, 5, 1, 4, 2);
INSERT INTO Timetable VALUES(115, 12, 1, 19, 5, 7, 4, 2);
INSERT INTO Timetable VALUES(116, 12, 1, 19, 6, 7, 4, 2);
INSERT INTO Timetable VALUES(117, 2, 2, 16, 5, 13, 4, 3);
INSERT INTO Timetable VALUES(118, 2, 2, 16, 6, 13, 4, 3);
INSERT INTO Timetable VALUES(119, 7, 1, 18, 5, 15, 4, 4);
INSERT INTO Timetable VALUES(120, 7, 1, 18, 6, 15, 4, 4);
INSERT INTO Timetable VALUES(121, 9, 1, 12, 5, 5, 4, 5);
INSERT INTO Timetable VALUES(122, 9, 1, 12, 6, 5, 4, 5);
INSERT INTO Timetable VALUES(123, 13, 2, 20, 5, 16, 5, 1);
INSERT INTO Timetable VALUES(124, 13, 2, 20, 6, 16, 5, 1);
INSERT INTO Timetable VALUES(125, 13, 1, 20, 5, 16, 5, 2);
INSERT INTO Timetable VALUES(126, 13, 1, 20, 6, 16, 5, 2);
INSERT INTO Timetable VALUES(127, 13, 3, 20, 5, 16, 5, 2);
INSERT INTO Timetable VALUES(128, 13, 3, 20, 6, 16, 5, 2);
INSERT INTO Timetable VALUES(129, 6, 3, 21, 6, 1, 5, 3);
INSERT INTO Timetable VALUES(130, 6, 2, 1, 5, 3, 6, 2);
INSERT INTO Timetable VALUES(131, 6, 2, 1, 6, 3, 6, 2);
INSERT INTO Timetable VALUES(132, 1, 2, 31, 7, 21, 1, 2);
INSERT INTO Timetable VALUES(133, 4, 1, 22, 7, 19, 1, 4);
INSERT INTO Timetable VALUES(134, 14, 1, 23, 7, 19, 2, 1);
INSERT INTO Timetable VALUES(135, 15, 1, 24, 7, 19, 2, 2);
INSERT INTO Timetable VALUES(136, 9, 1, 12, 7, 5, 2, 3);
INSERT INTO Timetable VALUES(137, 16, 1, 25, 7, 18, 3, 1);
INSERT INTO Timetable VALUES(138, 16, 1, 25, 7, 18, 3, 2);
INSERT INTO Timetable VALUES(139, 17, 1, 26, 7, 20, 3, 3);
INSERT INTO Timetable VALUES(140, 17, 2, 26, 7, 21, 4, 2);
INSERT INTO Timetable VALUES(141, 3, 2, 31, 7, 21, 4, 2);
INSERT INTO Timetable VALUES(142, 17, 3, 27, 7, 22, 4, 3);
INSERT INTO Timetable VALUES(143, 18, 3, 28, 7, 23, 4, 3);
INSERT INTO Timetable VALUES(144, 17, 3, 27, 7, 22, 4, 4);
INSERT INTO Timetable VALUES(145, 18, 3, 28, 7, 23, 4, 4);
INSERT INTO Timetable VALUES(146, 14, 2, 23, 7, 24, 5, 2);
INSERT INTO Timetable VALUES(147, 9, 1, 12, 7, 5, 5, 3);
INSERT INTO Timetable VALUES(148, 19, 2, 29, 7, 25, 5, 4);
INSERT INTO Timetable VALUES(149, 19, 1, 29, 7, 27, 5, 5);
INSERT INTO Timetable VALUES(150, 3, 1, 30, 7, 26, 6, 1);
INSERT INTO Timetable VALUES(151, 15, 2, 24, 7, 29, 6, 1);
INSERT INTO Timetable VALUES(152, 1, 1, 30, 7, 26, 6, 2);

COMMIT;
