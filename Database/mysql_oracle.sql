-- ############################
-- # MySQL  
-- ############################

  /* ----------------------------------------------------------
  	MySQL SAMPLE 1 - load teleclient survey data transaction
  */
  
  start transaction;
    use cust_service;
    
    select count(*) row_count
    from cust_service.teleclient
    order by RespID DESC;
    
    -- identify most recent survey
    select * from teleclient
    where term = 'uw'
    order by RespID DESC
    -- limit 1
    ;
    
    -- load client telehealth survey data from csv.  
    -- export csv from SurveyMonkey as 'condensed' columns and 'numerical' cells.  
    LOAD DATA LOCAL INFILE 'G:/Telehealth Surveys/Data/20200902_vo_client.csv'
    INTO TABLE cust_service.teleclient
    FIELDS TERMINATED BY ','
    ENCLOSED BY '"'
    LINES TERMINATED by '\n'
    IGNORE 2 ROWS
    (RespID, CollectorID, @StartSurv, @EndSurv, @IPAddy, @EMAddy, @FName, @LName, @Custom1, 
    @did_audio, @did_video, @void_dox, @void_duo, @void_idk, @void_other, 
    @plat_zoom, @plat_doxy, @plat_duo, @plat_idk, @plat_other, 
    @fav_vid, @fav_other, @serv_therp, @serv_med_mgmt, @serv_cm, @serv_other, 
    rate_access, rate_travel, rate_same, rate_easytalk, rate_progress, rate_effective,
    would_cont, @why_cont, @term)
    SET StartSurv = STR_TO_DATE(@StartSurv, '%m/%d/%Y %H:%i'),
    EndSurv = STR_TO_DATE(@EndSurv, '%m/%d/%Y %H:%i'), 
    IPAddy = NULLIF(@IPAddy, ''), 
    EMAddy = NULLIF(@EMAddy, ''), 
    FName = NULLIF(@FName, ''), 
    LName = NULLIF(@LName, ''), 
    Custom1 = NULLIF(@Custom1, ''), 
    did_audio = NULLIF(@did_audio, ''),
    did_video = NULLIF(@did_video, ''), 
    void_dox = NULLIF(@void_dox, ''), 
    void_duo = NULLIF(@void_duo, ''), 
    void_idk = NULLIF(@void_idk, ''),  
    void_other = NULLIF(@void_other, ''),
    plat_zoom = NULLIF(@plat_zoom, ''), 
    plat_doxy = NULLIF(@plat_doxy, ''), 
    plat_duo = NULLIF(@plat_duo, ''), 
    plat_idk = NULLIF(@plat_idk, ''), 
    plat_other = NULLIF(@plat_other, ''), 
    fav_vid = NULLIF(@fav_vid, ''), 
    fav_other = NULLIF(@fav_other, ''), 
    serv_therp = NULLIF(@serv_therp, ''), 
    serv_med_mgmt = NULLIF(@serv_med_mgmt, ''), 
    serv_cm = NULLIF(@serv_cm, ''), 
    serv_other = NULLIF(@serv_other, ''), 
    why_cont = NULLIF(@why_cont, ''), 
    term = TRIM(TRAILING('\r') from @term)
    ;
    
    select count(*) row_count
    from cust_service.teleclient
    order by RespID DESC;
    
    select * from teleclient order by RespID DESC;
  
  -- rollback;
  commit;
  
  
  
  /* --------------------------------------------------------
  	MySQL SAMPLE 2 - from tutorial, simple bank transaction
  */
  
  delimiter $$
  create procedure `withdraw`(in  account_id int, in amount numeric(7, 2), out success bool)
  begin
  declare current_balance numeric(7, 2) default 0.0;  
      
  declare exit handler for sqlexception 
    begin
      show errors;
    end; 
      
  declare exit handler for sqlwarning
    begin
      show warnings;
    end; 
  
  start transaction; 
    select balance into current_balance from accounts where id=account_id for update; 
  
    if current_balance >= amount then
      update accounts set balance = balance - amount where id=account_id;
      set success = true;
    else 
      set success = false;
    end if;
  commit; 
  end$$
  delimiter ;



-- ############################
-- # ORACLE/PLSQL 
-- ############################
  /*  
  	Sample DDL for creation of simple 
  	database, with sequencing for foreign keys.  
  */
  
  REM Creating Contact Table
  CREATE TABLE USERINSC521SU17_DED34.Contact (
    Cid INTEGER NOT NULL,
    Client_id INTEGER,
    Inter_id INTEGER,
    Fname VARCHAR(50) NOT NULL,
    Lname VARCHAR(50) NOT NULL,
    Title VARCHAR(50) NOT NULL,
    Phone VARCHAR(14),
    Influence VARCHAR(25),
    PRIMARY KEY (Cid)
  );
  
  REM Creating Client Table
  CREATE TABLE USERINSC521SU17_DED34.Client (
    Sid INTEGER NOT NULL,
    Ctype VARCHAR(20) NOT NULL,
    Name VARCHAR(100) NOT NULL,
    Reference VARCHAR(1) NOT NULL,
    Add_id VARCHAR(20) NOT NULL,
    Con_id INTEGER NOT NULL,
    PRIMARY KEY (Sid),
    FOREIGN KEY (Con_id) REFERENCES USERINSC521SU17_DED34.Contact(Cid)
  );
  
  REM Creating Intermediary Table
  CREATE TABLE USERINSC521SU17_DED34.Intermediary (
    Iid INTEGER NOT NULL,
    Name VARCHAR(100) NOT NULL,
    Con_id INTEGER NOT NULL,
    PRIMARY KEY (Iid),
    FOREIGN KEY (Con_id) REFERENCES USERINSC521SU17_DED34.Contact(Cid)
  );
  
  CREATE TABLE USERINSC521SU17_DED34.Represents (
    Iid INTEGER NOT NULL,
    Client_id INTEGER NOT NULL,
    PRIMARY KEY (Iid, Client_id),
    Startdate DATE,
    Enddate DATE,
    FOREIGN KEY (Iid) REFERENCES USERINSC521SU17_DED34.Intermediary (Iid),
    FOREIGN KEY (Client_id) REFERENCES USERINSC521SU17_DED34.Client (Sid) 
  );
  
  REM Creating Employee Table
  CREATE TABLE USERINSC521SU17_DED34.Employee (
    Eid INTEGER NOT NULL,
    Etype VARCHAR(20) NOT NULL,
    Fname VARCHAR(50) NOT NULL,
    Mname VARCHAR(50),
    Lname VARCHAR(50) NOT NULL,
    Ssn VARCHAR(11) NOT NULL,
    Rate REAL NOT NULL,
    Add_id INTEGER NOT NULL,
    PRIMARY KEY (Eid)
  );
  
  REM Creating Client Address Table
  CREATE TABLE USERINSC521SU17_DED34.ClientAddress (
    Aid INTEGER NOT NULL,
    Client_id INTEGER NOT NULL,
    Address_type VARCHAR(20) NOT NULL,
    Street_add1 VARCHAR(100) NOT NULL,
    Street_add2 VARCHAR(100),
    City VARCHAR(50),
    State VARCHAR(50),
    Zip_code VARCHAR(10),
    PRIMARY KEY (Aid, Client_id),
    FOREIGN KEY (Client_id) REFERENCES USERINSC521SU17_DED34.Client (Sid) 
      ON DELETE CASCADE
  );
  
  REM Creating Intermediary Address Table
  CREATE TABLE USERINSC521SU17_DED34.IntAddress (
    Aid INTEGER NOT NULL,
    Inter_id INTEGER NOT NULL,
    Address_type VARCHAR(20) NOT NULL,
    Street_add1 VARCHAR(100) NOT NULL,
    Street_add2 VARCHAR(100),
    City VARCHAR(50),
    State VARCHAR(50),
    Zip_code VARCHAR(10),
    PRIMARY KEY (Aid, Inter_id),
    FOREIGN KEY (Inter_id) REFERENCES USERINSC521SU17_DED34.Intermediary (Iid) 
      ON DELETE CASCADE
  );
  
  REM Creating Employee Address Table
  CREATE TABLE USERINSC521SU17_DED34.EmpAddress (
    Aid INTEGER NOT NULL,
    Emp_id INTEGER NOT NULL,
    Address_type VARCHAR(20) NOT NULL,
    Street_add1 VARCHAR(100) NOT NULL,
    Street_add2 VARCHAR(100),
    City VARCHAR(50),
    State VARCHAR(50),
    Zip_code VARCHAR(10),
    PRIMARY KEY (Aid, Emp_id),
    FOREIGN KEY (Emp_id) REFERENCES USERINSC521SU17_DED34.Employee (Eid) 
      ON DELETE CASCADE
  );
  
  REM Creating Phone Table
  CREATE TABLE USERINSC521SU17_DED34.Phone (
    Pid INTEGER NOT NULL,
    Cid INTEGER,
    Iid INTEGER,
    Eid INTEGER,
    Phone VARCHAR(14),
    PRIMARY KEY (Pid, Phone), 
    FOREIGN KEY (Cid) REFERENCES USERINSC521SU17_DED34.Client (Sid) 
      ON DELETE CASCADE,
    FOREIGN KEY (Iid) REFERENCES USERINSC521SU17_DED34.Intermediary (Iid) 
      ON DELETE CASCADE,
    FOREIGN KEY (Eid) REFERENCES USERINSC521SU17_DED34.Employee (Eid) 
      ON DELETE CASCADE
  );
  
  REM Creating Invoice Table
  CREATE TABLE USERINSC521SU17_DED34.Invoice (
    Invoice_id INTEGER NOT NULL,
    Invoice_line INTEGER NOT NULL,
    Time_id INTEGER NOT NULL,
    Time_line INTEGER NOT NULL,
    Emp_id INTEGER NOT NULL,
    Client_id INTEGER NOT NULL,
    Hours_billed REAL NOT NULL,
    Time_period DATE NOT NULL,
    Status VARCHAR(10) NOT NULL,
    PRIMARY KEY (Invoice_id, Invoice_line),
    FOREIGN KEY (Client_id) REFERENCES USERINSC521SU17_DED34.Client(Sid), 
    FOREIGN KEY (Emp_id) REFERENCES USERINSC521SU17_DED34.Employee (Eid) 
  );
  
  REM Creating Timesheet Table
  CREATE TABLE USERINSC521SU17_DED34.Timesheet (
    Time_id INTEGER NOT NULL,
    Time_line INTEGER NOT NULL,
    Invoice_id INTEGER NOT NULL,
    Invoice_line INTEGER NOT NULL,
    Emp_id INTEGER NOT NULL,
    Client_id INTEGER NOT NULL,
    Hours_billed REAL NOT NULL,
    Billable_rate REAL NOT NULL,
    Time_period DATE NOT NULL,
    Approved VARCHAR(1),
    PRIMARY KEY (Time_id,Time_line),
    FOREIGN KEY (Emp_id) REFERENCES USERINSC521SU17_DED34.Employee(Eid),
    FOREIGN KEY (Client_id) REFERENCES USERINSC521SU17_DED34.Client(Sid)
  );
  
  REM Creating Bills_against Table
  CREATE TABLE USERINSC521SU17_DED34.Bills_against (
    Emp_id INTEGER,
    Client_id INTEGER,
    PRIMARY KEY (Emp_id,Client_id),
    FOREIGN KEY (Emp_id) REFERENCES USERINSC521SU17_DED34.Employee(Eid),
    FOREIGN KEY (Client_id) REFERENCES USERINSC521SU17_DED34.Client(Sid)
  );
  
  commit;
  
  REM Inserting tuples into Employee
  REM (Eid, Etype, Fname, Mname, Lname, Ssn, Rate, Phone, Add_id);
  INSERT INTO USERINSC521SU17_DED34.Employee VALUES (7000001, 'Full Time', 'David', 'Lee', 'Fugate', '111-11-1111', 70.00, 1000001);
  INSERT INTO USERINSC521SU17_DED34.Employee VALUES (7000002, 'Full Time', 'Carrie', 'Beth', 'Hille', '222-22-2222', 50.00, 1000002);
  INSERT INTO USERINSC521SU17_DED34.Employee VALUES (7000003, 'Billable Resource', 'Tom', 'NMN', 'Jones', '333-33-3333', 65.00, 1000005);
  
  REM Inserting tuples into Contact
  REM (Cid, Client_id, Inter_id, Fname, LName, Title, Phone, Influence)
  INSERT INTO USERINSC521SU17_DED34.Contact VALUES (9000001, NULL, 3000001, 'Dawn', 'West', 'VP of Recruiting', '409-987-0021', 'Contract Authority');
  INSERT INTO USERINSC521SU17_DED34.Contact VALUES (9000002, 5000002, NULL, 'Mary', 'Jones', 'ECM Supervisor', '814-923-9901', 'Time Approver');
  INSERT INTO USERINSC521SU17_DED34.Contact VALUES (9000003, 5000003, NULL, 'Kelly', 'Norton', 'VAMMIS Manager', '804-869-5513', 'Champion');
  INSERT INTO USERINSC521SU17_DED34.Contact VALUES (9000004, 5000001, NULL, 'David', 'Fugate', 'Owner', '423-443-7523', 'Company Approval');
  INSERT INTO USERINSC521SU17_DED34.Contact VALUES (9000005, NULL, 3000002, 'Todd', 'Henry', 'ACS Supervisor', '570-231-8877', 'Contract Authority');
  
  REM Inserting tuples into Client
  REM (Sid, Ctype, Name, Reference, Phone, Add_id, Con_id)
  INSERT INTO USERINSC521SU17_DED34.Client VALUES (5000001, 'Overhead', 'CCO', 'Y', 1000006, 9000004);
  INSERT INTO USERINSC521SU17_DED34.Client VALUES (5000002, 'Overhead', 'Erie Insurance', 'Y', 1000003, 9000002);
  INSERT INTO USERINSC521SU17_DED34.Client VALUES (5000003, 'Overhead', 'VAMMIS', 'Y',  1000008, 9000003);
  INSERT INTO USERINSC521SU17_DED34.Client VALUES (5000004, 'Overhead', 'Giant Consulting', 'Y', 1000003, 9000002);
  INSERT INTO USERINSC521SU17_DED34.Client VALUES (5000005, 'Overhead', 'Acme Consulting', 'Y', 1000003, 9000002);
  INSERT INTO USERINSC521SU17_DED34.Client VALUES (5000006, 'Overhead', 'Home Depot Consulting', 'Y', 1000003, 9000002);
  
  
  REM Inserting tuples into Intermediary
  REM (Iid, Name, Phone, contact)
  INSERT INTO USERINSC521SU17_DED34.Intermediary VALUES (3000001, 'ACS', 9000005);
  INSERT INTO USERINSC521SU17_DED34.Intermediary VALUES (3000002, 'AdeptSource', 9000001);
  INSERT INTO USERINSC521SU17_DED34.Intermediary VALUES (3000003, 'Pi Hat', 9000002);
  INSERT INTO USERINSC521SU17_DED34.Intermediary VALUES (3000004, 'Phenom', 9000003);
  INSERT INTO USERINSC521SU17_DED34.Intermediary VALUES (3000005, 'Nexus', 9000004);
  
  REM Inserting tuples into Represents
  REM (Iid, Sid, startdate, enddate)
  INSERT INTO USERINSC521SU17_DED34.Represents VALUES (3000001, 5000001, '15-JAN-05', NULL);
  INSERT INTO USERINSC521SU17_DED34.Represents VALUES (3000002, 5000002, '15-FEB-04', NULL);
  INSERT INTO USERINSC521SU17_DED34.Represents VALUES (3000003, 5000005, '12-JAN-10', NULL);
  INSERT INTO USERINSC521SU17_DED34.Represents VALUES (3000003, 5000006, '15-FEB-05', NULL);
  INSERT INTO USERINSC521SU17_DED34.Represents VALUES (3000004, 5000005, '15-JAN-05', NULL);
  INSERT INTO USERINSC521SU17_DED34.Represents VALUES (3000004, 5000006, '02-JAN-05', NULL);
  INSERT INTO USERINSC521SU17_DED34.Represents VALUES (3000004, 5000004, '15-JAN-05', NULL);
  
  REM Inserting tuples into Address Tables
  REM (Aid, ***ID, Address_type, Street_add1, Street_add2, City, State, Zip_code)
  INSERT INTO USERINSC521SU17_DED34.EmpAddress VALUES (1000001, 7000001, 'Employee', '2328 Midland Drive', NULL, 'Erie', 'PA', '16506');
  INSERT INTO USERINSC521SU17_DED34.EmpAddress VALUES (1000002, 7000002, 'Employee', '437 Woodies Lane', 'Suite D', 'Bremen', 'IN', '46506');
  INSERT INTO USERINSC521SU17_DED34.ClientAddress VALUES (1000003, 5000002, 'Billing', '100 Erie Way', 'PO BOX 12301', 'Erie', 'PA', '16501');
  INSERT INTO USERINSC521SU17_DED34.ClientAddress VALUES (1000004, 5000002, 'Physical', '516 5th Street', NULL, 'Erie', 'PA', '16501');
  INSERT INTO USERINSC521SU17_DED34.EmpAddress VALUES (1000005, 7000003, 'Billable Resource', '213 7th Street', NULL, 'Pittsburgh', 'PA', '15122');
  INSERT INTO USERINSC521SU17_DED34.ClientAddress VALUES (1000006, 5000001, 'Company', '100 Irelend Avenue', 'Suite 102', 'South Bend', 'IN', '46601');
  INSERT INTO USERINSC521SU17_DED34.IntAddress VALUES (1000007, 3000001, 'Billing', '10200 SR 35', NULL, 'Atlanta', 'GA', '30314');
  INSERT INTO USERINSC521SU17_DED34.ClientAddress VALUES (1000008, 5000003, 'Physical', '570 Willow Lane', '3rd Floor', 'Richmond', 'VA', '23173');
  INSERT INTO USERINSC521SU17_DED34.ClientAddress VALUES (1000009, 5000003, 'Billing', '570 Willow Lane', 'Attn: AP', 'Richmond', 'VA', '23173');
  INSERT INTO USERINSC521SU17_DED34.IntAddress VALUES (1000010, 3000002, 'Billing', '100 Sun Drive', 'Suite 201', 'Phoenix', 'AZ', '85029');
  
  REM Inserting tuples into Phone Table
  REM (Pid, Cid, Iid, Eid, Phone)
  INSERT INTO USERINSC521SU17_DED34.Phone VALUES (1, NULL, 3000001, NULL, '570-321-9876');
  INSERT INTO USERINSC521SU17_DED34.Phone VALUES (2, NULL, 3000002, NULL, '409-987-0978');
  INSERT INTO USERINSC521SU17_DED34.Phone VALUES (3, NULL, 3000003, NULL, '333-223-2323');
  INSERT INTO USERINSC521SU17_DED34.Phone VALUES (4, NULL, 3000004, NULL, '355-993-4443');
  INSERT INTO USERINSC521SU17_DED34.Phone VALUES (5, NULL, 3000005, NULL, '234-432-2663');
  
  INSERT INTO USERINSC521SU17_DED34.Phone VALUES (6, NULL, NULL, 7000001, '423-443-7523');
  INSERT INTO USERINSC521SU17_DED34.Phone VALUES (7, NULL, NULL, 7000002, '574-460-4675');
  INSERT INTO USERINSC521SU17_DED34.Phone VALUES (8, NULL, NULL, 7000003, '814-545-4321');
  
  INSERT INTO USERINSC521SU17_DED34.Phone VALUES (9, 5000001, NULL, NULL, '222-993-4443');
  INSERT INTO USERINSC521SU17_DED34.Phone VALUES (10, 5000002, NULL, NULL, '222-432-2663');
  INSERT INTO USERINSC521SU17_DED34.Phone VALUES (11, 5000003, NULL, NULL, '333-321-9876');
  INSERT INTO USERINSC521SU17_DED34.Phone VALUES (12, 5000004, NULL, NULL, '333-987-0978');
  INSERT INTO USERINSC521SU17_DED34.Phone VALUES (13, 5000005, NULL, NULL, '399-223-2323');
  INSERT INTO USERINSC521SU17_DED34.Phone VALUES (14, 5000006, NULL, NULL, '745-993-4443');
  
  
  REM Inserting tuples into Timesheet
  REM (Time_id, Time_line, Invoice_id, Invoice_line, Emp_id, Client_id, Hours_billed, Billabe_rate, Time_period, Approved)
  INSERT INTO USERINSC521SU17_DED34.Timesheet VALUES (2000001, 101, 4000001, 101, 7000001, 5000001, 27.5, 90.00, '29-MAR-05', 'Y');
  INSERT INTO USERINSC521SU17_DED34.Timesheet VALUES (2000001, 102, 4000002, 101, 7000002, 5000003, 13.5, 90.00, '22-MAR-05', 'Y');
  INSERT INTO USERINSC521SU17_DED34.Timesheet VALUES (2000002, 101, 4000001, 101, 7000003, 5000004, 27.5, 90.00, '29-MAR-05', 'Y');
  INSERT INTO USERINSC521SU17_DED34.Timesheet VALUES (2000003, 102, 4000002, 101, 7000001, 5000005, 13.5, 90.00, '22-MAR-05', 'Y');
  INSERT INTO USERINSC521SU17_DED34.Timesheet VALUES (2000004, 101, 4000001, 101, 7000001, 5000002, 27.5, 90.00, '15-MAR-05', 'Y');
  INSERT INTO USERINSC521SU17_DED34.Timesheet VALUES (2000005, 102, 4000002, 101, 7000002, 5000003, 13.5, 90.00, '15-MAR-05', 'Y');
  INSERT INTO USERINSC521SU17_DED34.Timesheet VALUES (2000006, 101, 4000001, 101, 7000002, 5000002, 27.5, 90.00, '08-MAR-05', 'Y');
  INSERT INTO USERINSC521SU17_DED34.Timesheet VALUES (2000007, 102, 4000002, 101, 7000002, 5000004, 13.5, 90.00, '08-MAR-05', 'Y');
  INSERT INTO USERINSC521SU17_DED34.Timesheet VALUES (2000008, 101, 4000001, 101, 7000001, 5000005, 27.5, 90.00, '15-APR-05', 'Y');
  INSERT INTO USERINSC521SU17_DED34.Timesheet VALUES (2000009, 102, 4000002, 101, 7000003, 5000006, 13.5, 90.00, '15-JAN-05', 'Y');
  INSERT INTO USERINSC521SU17_DED34.Timesheet VALUES (2000010, 101, 4000001, 101, 7000003, 5000006, 27.5, 90.00, '08-FEB-05', 'Y');
  INSERT INTO USERINSC521SU17_DED34.Timesheet VALUES (2000011, 102, 4000002, 101, 7000003, 5000006, 13.5, 90.00, '08-APR-05', 'Y');
  
  INSERT INTO USERINSC521SU17_DED34.Timesheet VALUES (2000012, 101, 4000001, 101, 7000003, 5000002, 27.5, 90.00, '29-JAN-16', 'Y');
  INSERT INTO USERINSC521SU17_DED34.Timesheet VALUES (2000013, 102, 4000002, 101, 7000002, 5000003, 13.5, 90.00, '22-MAR-16', 'Y');
  INSERT INTO USERINSC521SU17_DED34.Timesheet VALUES (2000014, 101, 4000001, 101, 7000002, 5000002, 27.5, 90.00, '29-MAR-16', 'Y');
  INSERT INTO USERINSC521SU17_DED34.Timesheet VALUES (2000015, 102, 4000002, 101, 7000001, 5000003, 13.5, 90.00, '22-JUN-16', 'Y');
  INSERT INTO USERINSC521SU17_DED34.Timesheet VALUES (2000016, 101, 4000001, 101, 7000001, 5000002, 27.5, 90.00, '15-JUL-16', 'Y');
  INSERT INTO USERINSC521SU17_DED34.Timesheet VALUES (2000017, 102, 4000002, 101, 7000003, 5000003, 13.5, 90.00, '15-AUG-16', 'Y');
  INSERT INTO USERINSC521SU17_DED34.Timesheet VALUES (2000018, 101, 4000001, 101, 7000002, 5000002, 27.5, 90.00, '08-DEC-16', 'Y');
  INSERT INTO USERINSC521SU17_DED34.Timesheet VALUES (2000019, 102, 4000002, 101, 7000001, 5000003, 13.5, 90.00, '08-DEC-16', 'Y');
  INSERT INTO USERINSC521SU17_DED34.Timesheet VALUES (2000020, 101, 4000001, 101, 7000001, 5000002, 27.5, 90.00, '15-APR-16', 'Y');
  INSERT INTO USERINSC521SU17_DED34.Timesheet VALUES (2000021, 102, 4000002, 101, 7000002, 5000003, 13.5, 90.00, '15-JAN-16', 'Y');
  INSERT INTO USERINSC521SU17_DED34.Timesheet VALUES (2000022, 101, 4000001, 101, 7000002, 5000002, 27.5, 90.00, '08-FEB-16', 'Y');
  INSERT INTO USERINSC521SU17_DED34.Timesheet VALUES (2000023, 102, 4000002, 101, 7000002, 5000003, 13.5, 90.00, '08-APR-16', 'Y');
  
  REM Inserting tuples into Invoice 
  REM (Invoice_id, Invoice_line, Time_id, Time_line, Emp_id, Client_id, Hours_billed, Time_period, Status)
  INSERT INTO USERINSC521SU17_DED34.Invoice VALUES (4000001, 101, 2000001, 101, 7000001, 5000002, 27.5, '15-APR-05', 'Open');
  INSERT INTO USERINSC521SU17_DED34.Invoice VALUES (4000002, 101, 2000001, 102, 7000001, 5000003, 13.5, '15-MAY-06', 'Mailed');
  
  REM Inserting tuples into Bills_against
  REM (Emp_id, Client_id)
  INSERT INTO USERINSC521SU17_DED34.Bills_against VALUES (7000001, 5000001);
  INSERT INTO USERINSC521SU17_DED34.Bills_against VALUES (7000001, 5000002);
  INSERT INTO USERINSC521SU17_DED34.Bills_against VALUES (7000001, 5000003);
  INSERT INTO USERINSC521SU17_DED34.Bills_against VALUES (7000002, 5000001);
  INSERT INTO USERINSC521SU17_DED34.Bills_against VALUES (7000003, 5000002);
  INSERT INTO USERINSC521SU17_DED34.Bills_against VALUES (7000003, 5000003);
  
  commit;
  
  /*  QUERIES; */
  REM  Query a:  
  REM    Employees who have billed against one or more client.
  
  SELECT DISTINCT 
    USERINSC521SU17_DED34.Employee.Fname,
    USERINSC521SU17_DED34.Employee.Mname,
    USERINSC521SU17_DED34.Employee.Lname
  FROM 
    USERINSC521SU17_DED34.Employee,
    USERINSC521SU17_DED34.Bills_against
  WHERE 
    USERINSC521SU17_DED34.Employee.Eid = 
      USERINSC521SU17_DED34.Bills_against.Emp_id;
    
    
  REM  Query b  
  REM     Revenues last year by week.  
    
   SELECT TO_CHAR(USERINSC521SU17_DED34.Timesheet.Time_period, 'IW') AS Wk,
      SUM (USERINSC521SU17_DED34.Timesheet.Hours_billed *
          USERINSC521SU17_DED34.Timesheet.Billable_rate) AS Amt
      FROM USERINSC521SU17_DED34.Timesheet
      WHERE USERINSC521SU17_DED34.Timesheet.Time_period > '01-JAN-16'
        AND USERINSC521SU17_DED34.Timesheet.Time_period < '31-DEC-16'
      GROUP BY  TO_CHAR(USERINSC521SU17_DED34.Timesheet.Time_period, 'IW') 
      ORDER BY TO_CHAR(USERINSC521SU17_DED34.Timesheet.Time_period, 'IW');
      
  REM  Query c:  
  REM    Highest billed clients March '05.
  
  SELECT * FROM
  (SELECT USERINSC521SU17_DED34.Client.Name, 
      SUM (USERINSC521SU17_DED34.Timesheet.Hours_billed ) AS Hrs
  FROM USERINSC521SU17_DED34.Client, USERINSC521SU17_DED34.Timesheet
  WHERE USERINSC521SU17_DED34.Timesheet.Time_period > '01-MAR-05'
        AND USERINSC521SU17_DED34.Timesheet.Time_period < '31-MAR-05'
        AND USERINSC521SU17_DED34.Client.Ctype = 'Overhead'
        AND USERINSC521SU17_DED34.Timesheet.Client_id = USERINSC521SU17_DED34.Client.Sid
  GROUP BY  USERINSC521SU17_DED34.Client.Name
  ORDER BY Hrs desc) 
  WHERE rownum <= 3;
     
  REM  Query d:  
  REM    Intermediaries for Acme and Home Depot, but not Giant.
  
  SELECT 
    USERINSC521SU17_DED34.Intermediary.Name,
    USERINSC521SU17_DED34.Phone.Phone 
  FROM 
    USERINSC521SU17_DED34.Intermediary,
    USERINSC521SU17_DED34.Represents,
    USERINSC521SU17_DED34.Phone
  WHERE 
    USERINSC521SU17_DED34.Represents.Client_id = 5000005 
    AND
    USERINSC521SU17_DED34.Represents.Iid = 
      USERINSC521SU17_DED34.Intermediary.Iid
    AND
    USERINSC521SU17_DED34.Phone.Iid = 
      USERINSC521SU17_DED34.Intermediary.Iid
  
  INTERSECT
  
  SELECT 
    USERINSC521SU17_DED34.Intermediary.Name,
    USERINSC521SU17_DED34.Phone.Phone 
  FROM 
    USERINSC521SU17_DED34.Intermediary,
    USERINSC521SU17_DED34.Represents,
    USERINSC521SU17_DED34.Phone
  WHERE 
    USERINSC521SU17_DED34.Represents.Client_id = 5000006 
    AND
    USERINSC521SU17_DED34.Represents.Iid = 
      USERINSC521SU17_DED34.Intermediary.Iid
    AND
    USERINSC521SU17_DED34.Phone.Iid = 
      USERINSC521SU17_DED34.Intermediary.Iid
  
  MINUS 
  
  SELECT 
    USERINSC521SU17_DED34.Intermediary.Name,
    USERINSC521SU17_DED34.Phone.Phone 
  FROM 
    USERINSC521SU17_DED34.Intermediary,
    USERINSC521SU17_DED34.Represents, 
    USERINSC521SU17_DED34.Phone
  WHERE 
    USERINSC521SU17_DED34.Represents.Client_id = 5000004 
    AND
    USERINSC521SU17_DED34.Represents.Iid = 
      USERINSC521SU17_DED34.Intermediary.Iid
    AND
    USERINSC521SU17_DED34.Phone.Iid = 
      USERINSC521SU17_DED34.Intermediary.Iid;
      
      
  REM  Query e
  REM    # intermediaries with more than one client in March 2005.  
     
  SELECT COUNT(*) FROM
  (SELECT COUNT(*) AS TotalCounts
  FROM USERINSC521SU17_DED34.Intermediary, 
      USERINSC521SU17_DED34.Represents,
      USERINSC521SU17_DED34.Client
  WHERE 
      USERINSC521SU17_DED34.Represents.Startdate < '31-MAR-05'
      AND (USERINSC521SU17_DED34.Represents.Enddate > '01-MAR-05' OR
      USERINSC521SU17_DED34.Represents.Enddate IS NULL)
      AND USERINSC521SU17_DED34.Client.Sid = USERINSC521SU17_DED34.Represents.Client_id
      AND USERINSC521SU17_DED34.Intermediary.Iid = 
      USERINSC521SU17_DED34.Represents.Iid
  GROUP BY USERINSC521SU17_DED34.Intermediary.Name
  HAVING COUNT(*) > 1);      
  */
  
  /*
  REM Testing and clean-up.
      
  SELECT * FROM USERINSC521SU17_DED34.Invoice;
  SELECT * FROM USERINSC521SU17_DED34.EmpAddress;
  SELECT * FROM USERINSC521SU17_DED34.ClientAddress;
  SELECT * FROM USERINSC521SU17_DED34.IntAddress;
  SELECT * FROM USERINSC521SU17_DED34.Employee;
  SELECT * FROM USERINSC521SU17_DED34.Contact;
  SELECT * FROM USERINSC521SU17_DED34.Client;
  SELECT * FROM USERINSC521SU17_DED34.Intermediary;
  SELECT * FROM USERINSC521SU17_DED34.Represents;
  SELECT * FROM USERINSC521SU17_DED34.Phone;
  SELECT * FROM USERINSC521SU17_DED34.Timesheet;
  SELECT * FROM USERINSC521SU17_DED34.Bills_against;
  
  DROP TABLE USERINSC521SU17_DED34.Invoice;
  DROP TABLE USERINSC521SU17_DED34.Employee;
  DROP TABLE USERINSC521SU17_DED34.Contact;
  DROP TABLE USERINSC521SU17_DED34.Client;
  DROP TABLE USERINSC521SU17_DED34.Intermediary;
  DROP TABLE USERINSC521SU17_DED34.Represents;
  DROP TABLE USERINSC521SU17_DED34.EmpAddress;
  DROP TABLE USERINSC521SU17_DED34.ClientAddress;
  DROP TABLE USERINSC521SU17_DED34.IntAddress;
  DROP TABLE USERINSC521SU17_DED34.Phone;
  DROP TABLE USERINSC521SU17_DED34.Timesheet;
  DROP TABLE USERINSC521SU17_DED34.Bills_against;
  
  BEGIN
  
  FOR c IN (SELECT table_name FROM user_tables) LOOP
  EXECUTE IMMEDIATE ('DROP TABLE "' || c.table_name || '" CASCADE CONSTRAINTS');
  END LOOP;
  
  FOR s IN (SELECT sequence_name FROM user_sequences) LOOP
  EXECUTE IMMEDIATE ('DROP SEQUENCE ' || s.sequence_name);
  END LOOP;
  
  END;
  */
  
  SPOOL OFF

