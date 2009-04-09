<?php

/*
 * This only works on PHP >= 5.3.0 !!!
 */

class ModelBase {

  private $data = array();

  public static function find() {
    if (function_exists('get_called_class')) {
      $classname = get_called_class();
    } else {
      include('getCalledClass.php');
      $classname = my_get_called_class();
    }
    $table_name = $classname . 's'; // model is singular, table name is plural
    if ($db = sqlite_open('test.db', 0666, $sqliteerror)) {
      $query = "SELECT * FROM $table_name;";
      echo $query . "\n";
      $result = sqlite_query($db, $query);
      $objects = array();
      while ($row = sqlite_fetch_array($result, SQLITE_ASSOC)) {
	echo "Row: " . $row . "\n";
	$this_obj = new $classname;
	foreach ($row as $key => $value) {
	  $this_obj->$key = $value;
	}
	array_push($objects, $this_obj);
      }
      return $objects;
    } else {
      die($sqliteerror);
    }
  }

  public function __set($name, $value) {
    $this->data[$name] = $value;
  }

  public function __get($name) {
    if (array_key_exists($name, $this->data)) {
      return $this->data[$name];
    }

    echo "Trying to access an undefined property - very bad! \n";
    return null;
  }

}

class User extends ModelBase {
 
}

/*
 * How to get the following to work?
 * Need to get the invoked class name
 */

$users = User::find();
foreach ($users as $user) {
  echo "User - name: " . $user->name . " phone number: " . $user->phone . ".\n";
}
//$user_class = 'User';
//$user1 = new $user_class;
//$user1->age = 26;
//echo "Age is : ". $user1->age;

?>