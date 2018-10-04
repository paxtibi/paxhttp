<?php
include_once 'dumprequest.php';
printf ( "Method : %s\n", $_SERVER ["REQUEST_METHOD"] );
if (count ( $_REQUEST ) != 0) {
  print_r ( $_REQUEST );
} else {
  echo file_get_contents ( 'php://input' );
}
?>