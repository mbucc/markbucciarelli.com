PRAGMA foreign_keys = ON
;
DELETE FROM workson
;
DELETE FROM task
;
DELETE FROM workday
;
--
--	10/20
--
INSERT INTO workday VALUES ('2016-10-20')
;
INSERT INTO task (name) VALUES ('write oop vs. cqrs blog entry')
;
INSERT INTO task (name) VALUES ('get plate glass at amherst glass')
;
INSERT INTO task (name) VALUES ('clean up workshop')
;
INSERT INTO task (name) VALUES ('condition plane')
;
INSERT INTO workson (task_name, date, first_estimate, actual) 
     VALUES ('write oop vs. cqrs blog entry', '2016-10-20', 4, 4)
;
INSERT INTO workson (task_name, date, first_estimate) 
     VALUES ('get plate glass at amherst glass', '2016-10-20', 1)
;
INSERT INTO workson (task_name, date, first_estimate) 
     VALUES ('clean up workshop', '2016-10-20', 2)
;
INSERT INTO workson (task_name, date, first_estimate) 
     VALUES ('condition plane', '2016-10-20', 2)
;

--
--	10/22
--
INSERT INTO workday VALUES ('2016-10-22')
;
INSERT INTO task (name) VALUES ('codewrap patch for tufte-css')
;
INSERT INTO task (name) VALUES ('car in to dealer')
;
INSERT INTO workson (task_name, date, first_estimate, actual) 
     VALUES ('write oop vs. cqrs blog entry', '2016-10-22', 4, 2)
;
INSERT INTO workson (task_name, date, planned, first_estimate, actual, completed) 
     VALUES ('codewrap patch for tufte-css', '2016-10-22', 'N', 3, 3, 'Y')
;
INSERT INTO workson (task_name, date, planned, first_estimate, actual, completed) 
     VALUES ('car in to dealer', '2016-10-22', 'N', 2, 2, 'Y')
;
