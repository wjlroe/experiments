<?php

/*
 * Return called class name
 *
 * @author Michael Grenier
 * @param int $i_level optional
 * @return string
 */
function my_get_called_class ($i_level = 1)
{
  $a_debug = debug_backtrace();
  $a_called = array();
  $a_called_function = $a_debug[$i_level]['function'];
  for ($i = 1, $n = sizeof($a_debug); $i < $n; $i++)
    {
      if (in_array($a_debug[$i]['function'], array('eval')) ||
	  strpos($a_debug[$i]['function'], 'eval()') !== false)
	continue;
      if (in_array($a_debug[$i]['function'], array('__call', '__callStatic')))
	$a_called_function = $a_debug[$i]['args'][0];
      if ($a_debug[$i]['function'] == $a_called_function)
	$a_called = $a_debug[$i];
    }
  if (isset($a_called['object']) && isset($a_called['class']))
    return (string)$a_called['class'];
  $i_line = (int)$a_called['line'] - 1;
  $a_lines = explode("\n", file_get_contents($a_called['file']));
  preg_match("#([a-zA-Z0-9_]+){$a_called['type']}
                {$a_called['function']}( )*\(#", $a_lines[$i_line], $a_match);
  unset($a_debug, $a_called, $a_called_function, $i_line, $a_lines);
  if (sizeof($a_match) > 0)
    $s_class = (string)trim($a_match[1]);
  else
    $s_class = (string)$a_called['class'];
  if ($s_class == 'self')
    return my_get_called_class($i_level + 2);
  return $s_class;
} 

?>