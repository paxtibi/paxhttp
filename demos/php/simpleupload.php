<?php
include_once 'dumprequest.php';
if (count ( $_FILES ) == 0) {
  http_response_code ( 400 );
} else {
  foreach ( $_FILES as $key => $value ) {
    $path = "c:/wwwroot/test/uploads/";
    echo $key . "->";
    $path = $path . basename ( $value ['name'] );
    if (move_uploaded_file ( $value ['tmp_name'], $path )) {
      echo $path . "<br>";
    }
  }
  http_response_code ( 200 );
}
printf ( "done?" );
?>