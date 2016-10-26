PRAGMA foreign_keys = ON
;
DROP TABLE IF EXISTS task_added
;
DROP TABLE IF EXISTS task_estimated
;
DROP TABLE IF EXISTS task_done
;
DROP TABLE IF EXISTS pomodoro_started
;
DROP TABLE IF EXISTS pomodoro_done
;
DROP TABLE IF EXISTS work_day_ended
;
DROP TABLE IF EXISTS event
;
DROP TABLE IF EXISTS inventory
;
DROP TABLE IF EXISTS record
;
DROP TABLE IF EXISTS error
;
CREATE TABLE event(
  id         INTEGER PRIMARY KEY,
  date       TEXT     NOT NULL,
  -- current_timestamp is in UTC timezone
  timestamp  DATETIME NOT NULL DEFAULT current_timestamp,
  type       TEXT     NOT NULL
)
;
CREATE TABLE task_added(
  event_id     INTEGER NOT NULL REFERENCES event (id),
  task_name    TEXT    NOT NULL,
  planned      TEXT    NOT NULL CHECK (planned IN ('Y', 'N'))   DEFAULT 'Y'
)
;
CREATE TABLE task_estimated(
  event_id     INTEGER NOT NULL REFERENCES event (id),
  task_name    TEXT    NOT NULL,
  estimate     INTEGER NOT NULL CHECK (estimate > 0)
)
;
CREATE TABLE task_done(
  event_id     INTEGER NOT NULL REFERENCES event (id),
  task_name    TEXT    NOT NULL
)
;
CREATE TABLE pomodoro_started(
  event_id     INTEGER NOT NULL REFERENCES event (id),
  task_name    TEXT    NOT NULL
)
;
CREATE TABLE pomodoro_done(
  event_id     INTEGER NOT NULL REFERENCES event (id),
  task_name    TEXT    NOT NULL
)
;
CREATE TABLE work_day_ended(
  event_id     INTEGER NOT NULL REFERENCES event (id)
)
;
CREATE TABLE inventory(
  date                  TEXT     NOT NULL,
  pomodoros             INTEGER,
  interruptions         INTEGER,
  planned_tasks         INTEGER  NOT NULL CHECK (planned_tasks > 0),
  unplanned_tasks       INTEGER,
  under_estimated_tasks INTEGER,
  total_over_estimate   INTEGER
)
;
CREATE TABLE record(
  task_name    TEXT    NOT NULL
)
;
CREATE TABLE error(
  command_id  INTEGER NOT NULL,
  text        TEXT NOT NULL
)
;
