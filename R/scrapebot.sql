CREATE TABLE IF NOT EXISTS `data` (
  `uid` int(11) NOT NULL AUTO_INCREMENT,
  `created` datetime DEFAULT NULL,
  `value` text,
  `run_uid` int(11) DEFAULT NULL,
  `step_uid` int(11) DEFAULT NULL,
  PRIMARY KEY (`uid`),
  KEY `run_uid` (`run_uid`),
  KEY `step_uid` (`step_uid`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE IF NOT EXISTS `instance` (
  `uid` int(11) NOT NULL AUTO_INCREMENT,
  `created` datetime DEFAULT NULL,
  `name` varchar(256) DEFAULT NULL,
  `description` text,
  `owner_uid` int(11) DEFAULT NULL,
  PRIMARY KEY (`uid`),
  KEY `owner_uid` (`owner_uid`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE IF NOT EXISTS `log` (
  `uid` int(11) NOT NULL AUTO_INCREMENT,
  `created` datetime DEFAULT NULL,
  `type` enum('info','warning','error') DEFAULT NULL,
  `message` text,
  `run_uid` int(11) DEFAULT NULL,
  PRIMARY KEY (`uid`),
  KEY `run_uid` (`run_uid`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE IF NOT EXISTS `recipe` (
  `uid` int(11) NOT NULL AUTO_INCREMENT,
  `created` datetime DEFAULT NULL,
  `name` varchar(256) DEFAULT NULL,
  `description` text,
  `active` tinyint(1) DEFAULT NULL,
  `cookies` tinyint(1) DEFAULT NULL,
  `interval` int(11) DEFAULT NULL,
  `owner_uid` int(11) DEFAULT NULL,
  PRIMARY KEY (`uid`),
  KEY `owner_uid` (`owner_uid`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE IF NOT EXISTS `recipe2instance` (
  `uid` int(11) NOT NULL AUTO_INCREMENT,
  `created` datetime DEFAULT NULL,
  `recipe_uid` int(11) DEFAULT NULL,
  `instance_uid` int(11) DEFAULT NULL,
  `cookies_from_last_run` text,
  PRIMARY KEY (`uid`),
  KEY `recipe_uid` (`recipe_uid`),
  KEY `instance_uid` (`instance_uid`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE IF NOT EXISTS `recipestep` (
  `uid` int(11) NOT NULL AUTO_INCREMENT,
  `created` datetime DEFAULT NULL,
  `sort` int(11) DEFAULT NULL,
  `type` enum('navigate','find_by_id','find_by_name','find_by_class','find_by_tag','find_by_link','find_by_link_partial','find_by_css','find_by_xpath','random_select','scroll_to','pause','click','write','write_slowly','submit','get_text','get_texts','get_value','get_values','get_attribute','get_attributes','get_pagetitle','get_element_count','get_htmlsource','log','data','post_all_data','post_previous_step_data','execute_js','go_back','go_forward','unset_prior_element','screenshot','sometimes_screenshot','element_screenshot') DEFAULT NULL,
  `value` text,
  `use_random_item_instead_of_value` tinyint(1) DEFAULT NULL,
  `use_data_item_instead_of_value` int(11) DEFAULT '0',
  `active` tinyint(1) DEFAULT NULL,
  `recipe_uid` int(11) DEFAULT NULL,
  PRIMARY KEY (`uid`),
  KEY `recipe_uid` (`recipe_uid`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE IF NOT EXISTS `recipestepitem` (
  `uid` int(11) NOT NULL AUTO_INCREMENT,
  `created` datetime DEFAULT NULL,
  `value` varchar(256) DEFAULT NULL,
  `step_uid` int(11) DEFAULT NULL,
  PRIMARY KEY (`uid`),
  KEY `step_uid` (`step_uid`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE IF NOT EXISTS `run` (
  `uid` int(11) NOT NULL AUTO_INCREMENT,
  `created` datetime DEFAULT NULL,
  `runtime` int(11) DEFAULT NULL,
  `instance_uid` int(11) DEFAULT NULL,
  `recipe_uid` int(11) DEFAULT NULL,
  `status` enum('success','error','config_error','command_not_found','in_progress') DEFAULT NULL,
  PRIMARY KEY (`uid`),
  KEY `instance_uid` (`instance_uid`),
  KEY `recipe_uid` (`recipe_uid`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE IF NOT EXISTS `user` (
  `uid` int(11) NOT NULL AUTO_INCREMENT,
  `created` datetime DEFAULT NULL,
  `email` varchar(150) DEFAULT NULL,
  `name` varchar(80) DEFAULT NULL,
  `password` varchar(128) DEFAULT NULL,
  `active` tinyint(1) DEFAULT NULL,
  PRIMARY KEY (`uid`),
  UNIQUE KEY `email` (`email`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE IF NOT EXISTS `user2instance` (
  `uid` int(11) NOT NULL AUTO_INCREMENT,
  `created` datetime DEFAULT NULL,
  `user_uid` int(11) DEFAULT NULL,
  `instance_uid` int(11) DEFAULT NULL,
  `allowed_to_edit` tinyint(1) DEFAULT NULL,
  PRIMARY KEY (`uid`),
  KEY `user_uid` (`user_uid`),
  KEY `instance_uid` (`instance_uid`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

CREATE TABLE IF NOT EXISTS `user2recipe` (
  `uid` int(11) NOT NULL AUTO_INCREMENT,
  `created` datetime DEFAULT NULL,
  `user_uid` int(11) DEFAULT NULL,
  `recipe_uid` int(11) DEFAULT NULL,
  `allowed_to_edit` tinyint(1) DEFAULT NULL,
  PRIMARY KEY (`uid`),
  KEY `user_uid` (`user_uid`),
  KEY `recipe_uid` (`recipe_uid`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8;
