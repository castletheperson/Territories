DROP DATABASE IF EXISTS `territories`;

CREATE DATABASE `territories`;

USE `territories`;

CREATE TABLE `users` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `username` VARCHAR(255) NULL,
  `fname` VARCHAR(255) NULL,
  `lname` VARCHAR(255) NULL,
  PRIMARY KEY (`id`),
  UNIQUE INDEX `username_UNIQUE` (`username` ASC));

INSERT INTO `users` (`username`, `fname`, `lname`)
  VALUES ('4castle', 'Castle', 'Kerr'),
         ('aikman', 'Aikman', 'Ewalt');