PRAGMA foreign_keys = ON
;
DROP TABLE IF EXISTS workson
;
DROP TABLE IF EXISTS task
;
DROP TABLE IF EXISTS workday
;
CREATE TABLE task(
  name       TEXT PRIMARY KEY,
  cancelled  TEXT NOT NULL CHECK (cancelled IN ('Y', 'N')) DEFAULT 'N'
)
;
CREATE TABLE workday(
  date       TEXT PRIMARY KEY
)
;
CREATE TABLE workson(
  task_name       TEXT    NOT NULL REFERENCES task    (name)       ON UPDATE CASCADE,
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
DROP VIEW IF EXISTS record
;
CREATE VIEW record AS
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
               , SUM(first_estimate) - sum(actual) n
          FROM workson
         WHERE completed = 'Y'
           AND first_estimate > actual
         GROUP BY date)
SELECT   workday.date
       , IFNULL(actuals.n,   0) AS pomodoros
       , IFNULL(actuals.int, 0) AS interruptions
       , IFNULL(planned.n,   0) AS planned_tasks
       , IFNULL(unplanned.n, 0) AS unplanned_tasks
       , IFNULL(under.n,     0) AS under_estimated_tasks
       , IFNULL(over.n,      0) AS total_over_estimate
  FROM workday
  LEFT OUTER JOIN actuals   ON actuals.date   = workday.date
  LEFT OUTER JOIN planned   ON planned.date   = workday.date
  LEFT OUTER JOIN unplanned ON unplanned.date = workday.date
  LEFT OUTER JOIN under     ON under.date     = workday.date
  LEFT OUTER JOIN over      ON over.date      = workday.date
;
DROP VIEW IF EXISTS inventory
;
CREATE VIEW inventory AS
SELECT name
  FROM task
 WHERE name NOT IN (SELECT DISTINCT task_name
                      FROM workson
                     WHERE completed = 'Y')
   AND cancelled = 'N'
;

