DROP DATABASE IF EXISTS `territories`;

CREATE DATABASE `territories`;

USE `territories`;

CREATE TABLE `users` (
  `id` VARCHAR(255) NOT NULL,
  PRIMARY KEY (`id`));

INSERT INTO `users` (`id`) VALUES ('test');