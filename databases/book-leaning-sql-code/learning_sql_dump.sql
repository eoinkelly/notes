CREATE DATABASE  IF NOT EXISTS `learning_sql_book` /*!40100 DEFAULT CHARACTER SET utf8 */;
USE `learning_sql_book`;
-- MySQL dump 10.13  Distrib 5.6.13, for osx10.6 (i386)
--
-- Host: localhost    Database: learning_sql_book
-- ------------------------------------------------------
-- Server version	5.6.22

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `account`
--

DROP TABLE IF EXISTS `account`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `account` (
  `account_id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `product_cd` varchar(10) NOT NULL,
  `cust_id` int(10) unsigned NOT NULL,
  `open_date` date NOT NULL,
  `close_date` date DEFAULT NULL,
  `last_activity_date` date DEFAULT NULL,
  `status` enum('ACTIVE','CLOSED','FROZEN') DEFAULT NULL,
  `open_branch_id` smallint(5) unsigned DEFAULT NULL,
  `open_emp_id` smallint(5) unsigned DEFAULT NULL,
  `avail_balance` float(10,2) DEFAULT NULL,
  `pending_balance` float(10,2) DEFAULT NULL,
  PRIMARY KEY (`account_id`),
  KEY `fk_product_cd` (`product_cd`),
  KEY `fk_a_cust_id` (`cust_id`),
  KEY `fk_a_branch_id` (`open_branch_id`),
  KEY `fk_a_emp_id` (`open_emp_id`),
  CONSTRAINT `fk_a_branch_id` FOREIGN KEY (`open_branch_id`) REFERENCES `branch` (`branch_id`),
  CONSTRAINT `fk_a_cust_id` FOREIGN KEY (`cust_id`) REFERENCES `customer` (`cust_id`),
  CONSTRAINT `fk_a_emp_id` FOREIGN KEY (`open_emp_id`) REFERENCES `employee` (`emp_id`),
  CONSTRAINT `fk_product_cd` FOREIGN KEY (`product_cd`) REFERENCES `product` (`product_cd`)
) ENGINE=InnoDB AUTO_INCREMENT=30 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `account`
--

LOCK TABLES `account` WRITE;
/*!40000 ALTER TABLE `account` DISABLE KEYS */;
INSERT INTO `account` VALUES (1,'CHK',1,'2000-01-15',NULL,'2005-01-04','ACTIVE',2,10,1057.75,1057.75),(2,'SAV',1,'2000-01-15',NULL,'2004-12-19','ACTIVE',2,10,500.00,500.00),(3,'CD',1,'2004-06-30',NULL,'2004-06-30','ACTIVE',2,10,3000.00,3000.00),(4,'CHK',2,'2001-03-12',NULL,'2004-12-27','ACTIVE',2,10,2258.02,2258.02),(5,'SAV',2,'2001-03-12',NULL,'2004-12-11','ACTIVE',2,10,200.00,200.00),(7,'CHK',3,'2002-11-23',NULL,'2004-11-30','ACTIVE',3,13,1057.75,1057.75),(8,'MM',3,'2002-12-15',NULL,'2004-12-05','ACTIVE',3,13,2212.50,2212.50),(10,'CHK',4,'2003-09-12',NULL,'2005-01-03','ACTIVE',1,1,534.12,534.12),(11,'SAV',4,'2000-01-15',NULL,'2004-10-24','ACTIVE',1,1,767.77,767.77),(12,'MM',4,'2004-09-30',NULL,'2004-11-11','ACTIVE',1,1,5487.09,5487.09),(13,'CHK',5,'2004-01-27',NULL,'2005-01-05','ACTIVE',4,16,2237.97,2897.97),(14,'CHK',6,'2002-08-24',NULL,'2004-11-29','ACTIVE',1,1,122.37,122.37),(15,'CD',6,'2004-12-28',NULL,'2004-12-28','ACTIVE',1,1,10000.00,10000.00),(17,'CD',7,'2004-01-12',NULL,'2004-01-12','ACTIVE',2,10,5000.00,5000.00),(18,'CHK',8,'2001-05-23',NULL,'2005-01-03','ACTIVE',4,16,3487.19,3487.19),(19,'SAV',8,'2001-05-23',NULL,'2004-10-12','ACTIVE',4,16,387.99,387.99),(21,'CHK',9,'2003-07-30',NULL,'2004-12-15','ACTIVE',1,1,125.67,125.67),(22,'MM',9,'2004-10-28',NULL,'2004-10-28','ACTIVE',1,1,9345.55,9845.55),(23,'CD',9,'2004-06-30',NULL,'2004-06-30','ACTIVE',1,1,1500.00,1500.00),(24,'CHK',10,'2002-09-30',NULL,'2004-12-15','ACTIVE',4,16,23575.12,23575.12),(25,'BUS',10,'2002-10-01',NULL,'2004-08-28','ACTIVE',4,16,0.00,0.00),(27,'BUS',11,'2004-03-22',NULL,'2004-11-14','ACTIVE',2,10,9345.55,9345.55),(28,'CHK',12,'2003-07-30',NULL,'2004-12-15','ACTIVE',4,16,38552.05,38552.05),(29,'SBL',13,'2004-02-22',NULL,'2004-12-17','ACTIVE',3,13,50000.00,50000.00);
/*!40000 ALTER TABLE `account` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `branch`
--

DROP TABLE IF EXISTS `branch`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `branch` (
  `branch_id` smallint(5) unsigned NOT NULL AUTO_INCREMENT,
  `name` varchar(20) NOT NULL,
  `address` varchar(30) DEFAULT NULL,
  `city` varchar(20) DEFAULT NULL,
  `state` varchar(2) DEFAULT NULL,
  `zip` varchar(12) DEFAULT NULL,
  PRIMARY KEY (`branch_id`)
) ENGINE=InnoDB AUTO_INCREMENT=5 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `branch`
--

LOCK TABLES `branch` WRITE;
/*!40000 ALTER TABLE `branch` DISABLE KEYS */;
INSERT INTO `branch` VALUES (1,'Headquarters','3882 Main St.','Waltham','MA','02451'),(2,'Woburn Branch','422 Maple St.','Woburn','MA','01801'),(3,'Quincy Branch','125 Presidential Way','Quincy','MA','02169'),(4,'So. NH Branch','378 Maynard Ln.','Salem','NH','03079');
/*!40000 ALTER TABLE `branch` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `business`
--

DROP TABLE IF EXISTS `business`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `business` (
  `cust_id` int(10) unsigned NOT NULL,
  `name` varchar(40) NOT NULL,
  `state_id` varchar(10) NOT NULL,
  `incorp_date` date DEFAULT NULL,
  PRIMARY KEY (`cust_id`),
  CONSTRAINT `fk_b_cust_id` FOREIGN KEY (`cust_id`) REFERENCES `customer` (`cust_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `business`
--

LOCK TABLES `business` WRITE;
/*!40000 ALTER TABLE `business` DISABLE KEYS */;
INSERT INTO `business` VALUES (10,'Chilton Engineering','12-345-678','1995-05-01'),(11,'Northeast Cooling Inc.','23-456-789','2001-01-01'),(12,'Superior Auto Body','34-567-890','2002-06-30'),(13,'AAA Insurance Inc.','45-678-901','1999-05-01');
/*!40000 ALTER TABLE `business` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `customer`
--

DROP TABLE IF EXISTS `customer`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `customer` (
  `cust_id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `fed_id` varchar(12) NOT NULL,
  `cust_type_cd` enum('I','B') NOT NULL,
  `address` varchar(30) DEFAULT NULL,
  `city` varchar(20) DEFAULT NULL,
  `state` varchar(20) DEFAULT NULL,
  `postal_code` varchar(10) DEFAULT NULL,
  PRIMARY KEY (`cust_id`)
) ENGINE=InnoDB AUTO_INCREMENT=14 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `customer`
--

LOCK TABLES `customer` WRITE;
/*!40000 ALTER TABLE `customer` DISABLE KEYS */;
INSERT INTO `customer` VALUES (1,'111-11-1111','I','47 Mockingbird Ln','Lynnfield','MA','01940'),(2,'222-22-2222','I','372 Clearwater Blvd','Woburn','MA','01801'),(3,'333-33-3333','I','18 Jessup Rd','Quincy','MA','02169'),(4,'444-44-4444','I','12 Buchanan Ln','Waltham','MA','02451'),(5,'555-55-5555','I','2341 Main St','Salem','NH','03079'),(6,'666-66-6666','I','12 Blaylock Ln','Waltham','MA','02451'),(7,'777-77-7777','I','29 Admiral Ln','Wilmington','MA','01887'),(8,'888-88-8888','I','472 Freedom Rd','Salem','NH','03079'),(9,'999-99-9999','I','29 Maple St','Newton','MA','02458'),(10,'04-1111111','B','7 Industrial Way','Salem','NH','03079'),(11,'04-2222222','B','287A Corporate Ave','Wilmington','MA','01887'),(12,'04-3333333','B','789 Main St','Salem','NH','03079'),(13,'04-4444444','B','4772 Presidential Way','Quincy','MA','02169');
/*!40000 ALTER TABLE `customer` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `department`
--

DROP TABLE IF EXISTS `department`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `department` (
  `dept_id` smallint(5) unsigned NOT NULL AUTO_INCREMENT,
  `name` varchar(20) NOT NULL,
  PRIMARY KEY (`dept_id`)
) ENGINE=InnoDB AUTO_INCREMENT=4 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `department`
--

LOCK TABLES `department` WRITE;
/*!40000 ALTER TABLE `department` DISABLE KEYS */;
INSERT INTO `department` VALUES (1,'Operations'),(2,'Loans'),(3,'Administration');
/*!40000 ALTER TABLE `department` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `employee`
--

DROP TABLE IF EXISTS `employee`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `employee` (
  `emp_id` smallint(5) unsigned NOT NULL AUTO_INCREMENT,
  `fname` varchar(20) NOT NULL,
  `lname` varchar(20) NOT NULL,
  `start_date` date NOT NULL,
  `end_date` date DEFAULT NULL,
  `superior_emp_id` smallint(5) unsigned DEFAULT NULL,
  `dept_id` smallint(5) unsigned DEFAULT NULL,
  `title` varchar(20) DEFAULT NULL,
  `assigned_branch_id` smallint(5) unsigned DEFAULT NULL,
  PRIMARY KEY (`emp_id`),
  KEY `fk_e_emp_id` (`superior_emp_id`),
  KEY `fk_dept_id` (`dept_id`),
  KEY `fk_e_branch_id` (`assigned_branch_id`),
  CONSTRAINT `fk_dept_id` FOREIGN KEY (`dept_id`) REFERENCES `department` (`dept_id`),
  CONSTRAINT `fk_e_branch_id` FOREIGN KEY (`assigned_branch_id`) REFERENCES `branch` (`branch_id`),
  CONSTRAINT `fk_e_emp_id` FOREIGN KEY (`superior_emp_id`) REFERENCES `employee` (`emp_id`)
) ENGINE=InnoDB AUTO_INCREMENT=19 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `employee`
--

LOCK TABLES `employee` WRITE;
/*!40000 ALTER TABLE `employee` DISABLE KEYS */;
INSERT INTO `employee` VALUES (1,'Michael','Smith','2005-06-22',NULL,NULL,3,'President',1),(2,'Susan','Barker','2006-09-12',NULL,1,3,'Vice President',1),(3,'Robert','Tyler','2005-02-09',NULL,1,3,'Treasurer',1),(4,'Susan','Hawthorne','2006-04-24',NULL,3,1,'Operations Manager',1),(5,'John','Gooding','2007-11-14',NULL,4,2,'Loan Manager',1),(6,'Helen','Fleming','2008-03-17',NULL,4,1,'Head Teller',1),(7,'Chris','Tucker','2008-09-15',NULL,6,1,'Teller',1),(8,'Sarah','Parker','2006-12-02',NULL,6,1,'Teller',1),(9,'Jane','Grossman','2006-05-03',NULL,6,1,'Teller',1),(10,'Paula','Roberts','2006-07-27',NULL,4,1,'Head Teller',2),(11,'Thomas','Ziegler','2004-10-23',NULL,10,1,'Teller',2),(12,'Samantha','Jameson','2007-01-08',NULL,10,1,'Teller',2),(13,'John','Blake','2004-05-11',NULL,4,1,'Head Teller',3),(14,'Cindy','Mason','2006-08-09',NULL,13,1,'Teller',3),(15,'Frank','Portman','2007-04-01',NULL,13,1,'Teller',3),(16,'Theresa','Markham','2005-03-15',NULL,4,1,'Head Teller',4),(17,'Beth','Fowler','2006-06-29',NULL,16,1,'Teller',4),(18,'Rick','Tulman','2006-12-12',NULL,16,1,'Teller',4);
/*!40000 ALTER TABLE `employee` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `individual`
--

DROP TABLE IF EXISTS `individual`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `individual` (
  `cust_id` int(10) unsigned NOT NULL,
  `fname` varchar(30) NOT NULL,
  `lname` varchar(30) NOT NULL,
  `birth_date` date DEFAULT NULL,
  PRIMARY KEY (`cust_id`),
  CONSTRAINT `fk_i_cust_id` FOREIGN KEY (`cust_id`) REFERENCES `customer` (`cust_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `individual`
--

LOCK TABLES `individual` WRITE;
/*!40000 ALTER TABLE `individual` DISABLE KEYS */;
INSERT INTO `individual` VALUES (1,'James','Hadley','1977-04-22'),(2,'Susan','Tingley','1973-08-15'),(3,'Frank','Tucker','1963-02-06'),(4,'John','Hayward','1971-12-22'),(5,'Charles','Frasier','1976-08-25'),(6,'John','Spencer','1967-09-14'),(7,'Margaret','Young','1951-03-19'),(8,'George','Blake','1982-07-01'),(9,'Richard','Farley','1973-06-16');
/*!40000 ALTER TABLE `individual` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `officer`
--

DROP TABLE IF EXISTS `officer`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `officer` (
  `officer_id` smallint(5) unsigned NOT NULL AUTO_INCREMENT,
  `cust_id` int(10) unsigned NOT NULL,
  `fname` varchar(30) NOT NULL,
  `lname` varchar(30) NOT NULL,
  `title` varchar(20) DEFAULT NULL,
  `start_date` date NOT NULL,
  `end_date` date DEFAULT NULL,
  PRIMARY KEY (`officer_id`),
  KEY `fk_o_cust_id` (`cust_id`),
  CONSTRAINT `fk_o_cust_id` FOREIGN KEY (`cust_id`) REFERENCES `business` (`cust_id`)
) ENGINE=InnoDB AUTO_INCREMENT=8 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `officer`
--

LOCK TABLES `officer` WRITE;
/*!40000 ALTER TABLE `officer` DISABLE KEYS */;
INSERT INTO `officer` VALUES (1,10,'John','Chilton','President','1995-05-01',NULL),(2,11,'Paul','Hardy','President','2001-01-01',NULL),(3,12,'Carl','Lutz','President','2002-06-30',NULL),(4,13,'Stanley','Cheswick','President','1999-05-01',NULL);
/*!40000 ALTER TABLE `officer` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `product`
--

DROP TABLE IF EXISTS `product`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `product` (
  `product_cd` varchar(10) NOT NULL,
  `name` varchar(50) NOT NULL,
  `product_type_cd` varchar(10) NOT NULL,
  `date_offered` date DEFAULT NULL,
  `date_retired` date DEFAULT NULL,
  PRIMARY KEY (`product_cd`),
  KEY `fk_product_type_cd` (`product_type_cd`),
  CONSTRAINT `fk_product_type_cd` FOREIGN KEY (`product_type_cd`) REFERENCES `product_type` (`product_type_cd`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `product`
--

LOCK TABLES `product` WRITE;
/*!40000 ALTER TABLE `product` DISABLE KEYS */;
INSERT INTO `product` VALUES ('AUT','auto loan','LOAN','2004-01-01',NULL),('BUS','business line of credit','LOAN','2004-01-01',NULL),('CD','certificate of deposit','ACCOUNT','2004-01-01',NULL),('CHK','checking account','ACCOUNT','2004-01-01',NULL),('MM','money market account','ACCOUNT','2004-01-01',NULL),('MRT','home mortgage','LOAN','2004-01-01',NULL),('SAV','savings account','ACCOUNT','2004-01-01',NULL),('SBL','small business loan','LOAN','2004-01-01',NULL);
/*!40000 ALTER TABLE `product` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `product_type`
--

DROP TABLE IF EXISTS `product_type`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `product_type` (
  `product_type_cd` varchar(10) NOT NULL,
  `name` varchar(50) NOT NULL,
  PRIMARY KEY (`product_type_cd`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `product_type`
--

LOCK TABLES `product_type` WRITE;
/*!40000 ALTER TABLE `product_type` DISABLE KEYS */;
INSERT INTO `product_type` VALUES ('ACCOUNT','Customer Accounts'),('INSURANCE','Insurance Offerings'),('LOAN','Individual and Business Loans');
/*!40000 ALTER TABLE `product_type` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `transaction`
--

DROP TABLE IF EXISTS `transaction`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `transaction` (
  `txn_id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `txn_date` datetime NOT NULL,
  `account_id` int(10) unsigned NOT NULL,
  `txn_type_cd` enum('DBT','CDT') DEFAULT NULL,
  `amount` double(10,2) NOT NULL,
  `teller_emp_id` smallint(5) unsigned DEFAULT NULL,
  `execution_branch_id` smallint(5) unsigned DEFAULT NULL,
  `funds_avail_date` datetime DEFAULT NULL,
  PRIMARY KEY (`txn_id`),
  KEY `fk_t_account_id` (`account_id`),
  KEY `fk_teller_emp_id` (`teller_emp_id`),
  KEY `fk_exec_branch_id` (`execution_branch_id`),
  CONSTRAINT `fk_exec_branch_id` FOREIGN KEY (`execution_branch_id`) REFERENCES `branch` (`branch_id`),
  CONSTRAINT `fk_t_account_id` FOREIGN KEY (`account_id`) REFERENCES `account` (`account_id`),
  CONSTRAINT `fk_teller_emp_id` FOREIGN KEY (`teller_emp_id`) REFERENCES `employee` (`emp_id`)
) ENGINE=InnoDB AUTO_INCREMENT=32 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `transaction`
--

LOCK TABLES `transaction` WRITE;
/*!40000 ALTER TABLE `transaction` DISABLE KEYS */;
INSERT INTO `transaction` VALUES (1,'2008-01-05 00:00:00',3,'DBT',100.00,NULL,NULL,'2008-01-05 00:00:00'),(2,'2008-01-05 00:00:00',15,'DBT',100.00,NULL,NULL,'2008-01-05 00:00:00'),(3,'2008-01-05 00:00:00',17,'DBT',100.00,NULL,NULL,'2008-01-05 00:00:00'),(4,'2008-01-05 00:00:00',23,'DBT',100.00,NULL,NULL,'2008-01-05 00:00:00'),(5,'2008-01-05 00:00:00',1,'DBT',100.00,NULL,NULL,'2008-01-05 00:00:00'),(6,'2008-01-05 00:00:00',4,'DBT',100.00,NULL,NULL,'2008-01-05 00:00:00'),(7,'2008-01-05 00:00:00',7,'DBT',100.00,NULL,NULL,'2008-01-05 00:00:00'),(8,'2008-01-05 00:00:00',10,'DBT',100.00,NULL,NULL,'2008-01-05 00:00:00'),(9,'2008-01-05 00:00:00',13,'DBT',100.00,NULL,NULL,'2008-01-05 00:00:00'),(10,'2008-01-05 00:00:00',14,'DBT',100.00,NULL,NULL,'2008-01-05 00:00:00'),(11,'2008-01-05 00:00:00',18,'DBT',100.00,NULL,NULL,'2008-01-05 00:00:00'),(12,'2008-01-05 00:00:00',21,'DBT',100.00,NULL,NULL,'2008-01-05 00:00:00'),(13,'2008-01-05 00:00:00',24,'DBT',100.00,NULL,NULL,'2008-01-05 00:00:00'),(14,'2008-01-05 00:00:00',28,'DBT',100.00,NULL,NULL,'2008-01-05 00:00:00'),(15,'2008-01-05 00:00:00',8,'DBT',100.00,NULL,NULL,'2008-01-05 00:00:00'),(16,'2008-01-05 00:00:00',12,'DBT',100.00,NULL,NULL,'2008-01-05 00:00:00'),(17,'2008-01-05 00:00:00',22,'DBT',100.00,NULL,NULL,'2008-01-05 00:00:00'),(18,'2008-01-05 00:00:00',2,'DBT',100.00,NULL,NULL,'2008-01-05 00:00:00'),(19,'2008-01-05 00:00:00',5,'DBT',100.00,NULL,NULL,'2008-01-05 00:00:00'),(20,'2008-01-05 00:00:00',11,'DBT',100.00,NULL,NULL,'2008-01-05 00:00:00'),(21,'2008-01-05 00:00:00',19,'DBT',100.00,NULL,NULL,'2008-01-05 00:00:00');
/*!40000 ALTER TABLE `transaction` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2015-01-21  7:40:07
