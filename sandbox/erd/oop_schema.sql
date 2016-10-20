DROP TABLE IF EXISTS task
;
CREATE TABLE task(
  name       TEXT PRIMARY KEY,
  cancelled  TEXT NOT NULL CHECK (cancelled IN ('Y', 'N')) DEFAULT 'N'
)
;
DROP TABLE IF EXISTS workday
;
CREATE TABLE workday(
  date       TEXT PRIMARY KEY
)
;
DROP TABLE IF EXISTS workson
;
CREATE TABLE workson(
  task_name       TEXT    NOT NULL REFERENCES task (name)          ON UPDATE CASCADE,
  date            TEXT    NOT NULL REFERENCES workday (date)       ON UPDATE CASCADE,
  first_estimate  INTEGER NOT NULL CHECK (first_estimate > 0), 
  planned         TEXT    NOT NULL CHECK (planned IN ('Y', 'N'))   DEFAULT 'Y',
  interruptions   INTEGER NOT NULL CHECK (interruptions >= 0)      DEFAULT  0, 
  completed       TEXT    NOT NULL CHECK (completed IN ('Y', 'N')) DEFAULT 'N',
  second_estimate INTEGER          CHECK (second_estimate > 0), 
  third_estimate  INTEGER          CHECK (third_estimate IS NULL 
                                          OR (second_estimate IS NOT NULL 
                                              AND third_estimate > 0)),
  actual          INTEGER          CHECK (actual > 0), 
  PRIMARY KEY(task_name, date)
)
;

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
INSERT INTO workson (task_name, date, first_estimate) 
     VALUES ('write oop vs. cqrs blog entry', '2016-10-20', 4)
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
UPDATE workson SET actual = 4
 WHERE task_name = 'write oop vs. cqrs blog entry'
   AND date = '2016-10-20'
;
DROP VIEW IF EXISTS daily_report
;
CREATE VIEW daily_report AS
WITH actuals 
        AS (
        SELECT   date
               , sum(actual) n
               , sum(interruptions) int
          FROM workson
         GROUP BY date),
     planned 
        AS (
        SELECT date, count(*) n
          FROM workson
         WHERE planned = 'Y'
         GROUP BY date),
     unplanned 
        AS (
        SELECT date, count(*) n
          FROM workson
         WHERE planned = 'N'
         GROUP BY date),
     completed
        AS (
        SELECT date, count(*) n
          FROM workson
         WHERE completed = 'Y'
         GROUP BY date),
     under
        AS (
        SELECT date, count(*) n
          FROM workson
         WHERE second_estimate IS NOT NULL
         GROUP BY date),
     over
        AS (
        SELECT   date
               , SUM(first_estimate) - actual n
          FROM workson
         WHERE completed = 'Y'
           AND first_estimate > actual
         GROUP BY date)
SELECT   actuals.date
       , IFNULL(actuals.n,   0) AS pomodoros
       , IFNULL(actuals.int, 0) AS interruptions
       , IFNULL(planned.n,   0) AS planned_tasks
       , IFNULL(unplanned.n, 0) AS unplanned_tasks
       , IFNULL(under.n,     0) AS under_estimated_tasks
       , IFNULL(over.n,      0) AS over_estimated_tasks
  FROM actuals
  LEFT OUTER JOIN planned   ON planned.date = actuals.date
  LEFT OUTER JOIN unplanned ON unplanned.date = actuals.date
  LEFT OUTER JOIN under     ON under.date = actuals.date
  LEFT OUTER JOIN over      ON over.date = actuals.date
;
SELECT * FROM daily_report
;  
CREATE VIEW todo AS
SELECT name
  FROM task
 WHERE name NOT IN (SELECT DISTINCT task_name
                      FROM workson
                     WHERE completed = 'Y')
;
SELECT * FROM todo
;

