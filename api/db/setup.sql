DROP DATABASE IF EXISTS `territories`;

CREATE DATABASE `territories`;

USE `territories`;

CREATE TABLE `users` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `name` VARCHAR(255) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE(`name`)
);

CREATE TABLE `territories` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `userId` INT NOT NULL,
  `name` VARCHAR(255) NOT NULL,
  `instructions` TEXT,
  `created` TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  `updated` TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`, `userId`),
  UNIQUE (`name`, `userId`)
);

INSERT INTO `users` (`name`) VALUES ('4castle');

INSERT INTO `territories` (`userId`, `name`, `instructions`)
  SELECT
    `id`,
    'C-1',
    'Go left, then right, then right'
  FROM `users`
  WHERE `name` = '4castle';
